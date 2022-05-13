//! Provides futures that wait for method calls to complete and fetch their results.

use super::sleep;
use core::convert::TryInto;
use core::future::Future;
use core::pin::Pin;
use core::task::{Context, Poll};
use minicbor::{decode, encode, Decode, Encode};
use oc_wasm_safe::{
	component::{
		InvokeEndLengthResult, InvokeEndResult, InvokeResult, Invoker, MethodCall, MethodCallError,
	},
	descriptor::{AsDescriptor, Borrowed},
	error::Error,
	panic_or_trap, Address,
};

/// An object that can be used as a scratch buffer for making component method calls.
pub trait Buffer: AsMut<[u8]> + minicbor::encode::Write {
	/// Encodes a CBOR-encodable object into the buffer.
	///
	/// On success, a sub-slice of the buffer holding the encoded object is returned.
	///
	/// # Panics
	/// This method should panic if the buffer is too small to hold the object and cannot be
	/// resized, or if `x` fails to encode.
	fn encode_into(&mut self, x: impl Encode) -> &[u8];

	/// Tries to end a component call invocation into the buffer.
	///
	/// If the buffer is of insufficient size and is resizable, the implementation should resize
	/// the buffer and try again. In any other case, it should return the
	/// [`InvokeEndResult`](InvokeEndResult) directly.
	///
	/// # Errors
	/// This function can return any error returned by [`MethodCall::end`](MethodCall::end).
	fn end_into<'invoker>(&mut self, call: MethodCall<'invoker>) -> InvokeEndResult<'invoker>;
}

impl Buffer for &mut [u8] {
	fn encode_into(&mut self, x: impl Encode) -> &[u8] {
		// The Write impl for &mut[u8] mutates the reference-to-slice itself to refer to a smaller
		// slice. We don’t want to affect self, since we want to retain knowledge of the whole
		// slice for subsequent uses as a buffer, so make a copy of the reference first and let
		// encode mutate that.
		let mut target: &mut [u8] = self;
		match encode(x, &mut target) {
			Ok(()) => target,
			Err(_) => panic_or_trap!("failed to encode component call parameters"),
		}
	}

	fn end_into<'invoker>(&mut self, call: MethodCall<'invoker>) -> InvokeEndResult<'invoker> {
		call.end(self)
	}
}

#[cfg(feature = "alloc")]
impl Buffer for alloc::vec::Vec<u8> {
	fn encode_into(&mut self, x: impl Encode) -> &[u8] {
		// The Write impl for Vec<u8> appends the bytes to the Vec, so we should clear it first and
		// then return the whole Vec.
		self.clear();
		let result: Result<(), minicbor::encode::Error<core::convert::Infallible>> =
			encode(x, &mut *self);
		match result {
			Ok(()) => self.as_mut(),
			Err(_) => panic_or_trap!("failed to encode component call parameters"),
		}
	}

	fn end_into<'invoker>(&mut self, mut call: MethodCall<'invoker>) -> InvokeEndResult<'invoker> {
		loop {
			self.clear();
			let cap = self.capacity();
			// SAFETY: capacity() returns the length of the allocation, end_ptr() promises to write no
			// more than that many bytes to the buffer.
			let ret = unsafe { call.end_ptr(self.as_mut_ptr(), cap) };
			match ret {
				InvokeEndResult::Done(Ok(n)) => {
					// SAFETY: if end_ptr() returns Done(Ok(n)), then it promises to have written n
					// bytes into the buffer.
					unsafe { self.set_len(n) };
					break InvokeEndResult::Done(Ok(n));
				}
				InvokeEndResult::Done(Err(e)) => break InvokeEndResult::Done(Err(e)),
				InvokeEndResult::BufferTooShort(c) => match c.end_length() {
					InvokeEndLengthResult::Done(Ok((n, c))) => {
						self.reserve(n);
						call = c;
					}
					InvokeEndLengthResult::Done(Err(e)) => break InvokeEndResult::Done(Err(e)),
					InvokeEndLengthResult::Pending(c) => break InvokeEndResult::Pending(c),
				},
				InvokeEndResult::Pending(c) => break InvokeEndResult::Pending(c),
			}
		}
	}
}

/// A future that waits for the method call to complete, then returns the length, in bytes, of the
/// result.
///
/// On success, the length and the [`MethodCall`](MethodCall) are returned, allowing the
/// [`MethodCall`](MethodCall) to be reused to fetch the actual bytes.
///
/// # Errors
/// * [`NoSuchComponent`](MethodCallError::NoSuchComponent) is returned if the method call failed
///   because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](MethodCallError::NoSuchMethod) is returned if the method call failed because
///   the method does not exist on the component.
/// * [`BadParameters`](MethodCallError::BadParameters) is returned if the parameters provided when
///   starting the call are not acceptable for the method.
/// * [`Other`](MethodCallError::Other) is returned if the method call failed.
#[must_use = "A future does nothing unless awaited."]
pub struct EndLength<'invoker>(Option<MethodCall<'invoker>>);

impl<'invoker> Future for EndLength<'invoker> {
	type Output = Result<(usize, MethodCall<'invoker>), MethodCallError<'invoker>>;

	fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
		match self.0.take().unwrap().end_length() {
			InvokeEndLengthResult::Done(result) => Poll::Ready(result),
			InvokeEndLengthResult::Pending(call) => {
				self.0 = Some(call);
				sleep::register_next_timeslice_wakeup(context.waker());
				Poll::Pending
			}
		}
	}
}

/// The result of ending a method call.
pub enum EndResult<'invoker> {
	/// The call returned a result. The number of bytes written into the buffer is returned.
	Done(usize),

	/// The call indicated that the buffer is too short. The [`MethodCall`](MethodCall) is returned
	/// so the operation can be retried with a larger buffer.
	BufferTooShort(MethodCall<'invoker>),
}

/// Returns the result of the method call as a CBOR-encoded data item.
///
/// On success, the CBOR-encoded result is written into `buffer`, and the number of bytes written
/// is returned.
///
/// # Errors
/// * [`BufferTooShort`](MethodCallError::BufferTooShort) is returned if the method call succeeded
///   but the return value could not be fetched because the buffer is too small and not resizable.
/// * [`NoSuchComponent`](MethodCallError::NoSuchComponent) is returned if the method call failed
///   because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](MethodCallError::NoSuchMethod) is returned if the method call failed because
///   the method does not exist on the component.
/// * [`BadParameters`](MethodCallError::BadParameters) is returned if the parameters provided when
///   starting the call are not acceptable for the method.
/// * [`Other`](MethodCallError::Other) is returned if the method call failed.
#[must_use = "A future does nothing unless awaited."]
pub struct EndIntoBuffer<'invoker, 'buffer, B>(Option<(MethodCall<'invoker>, &'buffer mut B)>);

impl<'invoker, 'buffer, B: Buffer> Future for EndIntoBuffer<'invoker, 'buffer, B> {
	type Output = Result<usize, MethodCallError<'invoker>>;

	fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
		// Do the first call.
		let (call, buffer): (MethodCall<'invoker>, &'buffer mut B) = self.0.take().unwrap();
		match buffer.end_into(call) {
			InvokeEndResult::Done(result) => Poll::Ready(result),
			InvokeEndResult::BufferTooShort(_) => Poll::Ready(Err(MethodCallError::BufferTooShort)),
			InvokeEndResult::Pending(call) => {
				self.0 = Some((call, buffer));
				sleep::register_next_timeslice_wakeup(context.waker());
				Poll::Pending
			}
		}
	}
}

pub trait MethodCallExt<'invoker> {
	/// Waits for the method call to complete, then returns the length, in bytes, of the result.
	///
	/// On success, the length and the [`MethodCall`](MethodCall) are returned, allowing the
	/// [`MethodCall`](MethodCall) to be reused to fetch the actual bytes.
	///
	/// # Errors
	/// * [`NoSuchComponent`](MethodCallError::NoSuchComponent) is returned if the method call
	///   failed because the component does not exist or is inaccessible.
	/// * [`NoSuchMethod`](MethodCallError::NoSuchMethod) is returned if the method call failed
	///   because the method does not exist on the component.
	/// * [`BadParameters`](MethodCallError::BadParameters) is returned if the parameters provided
	///   when starting the call are not acceptable for the method.
	/// * [`Other`](MethodCallError::Other) is returned if the method call failed.
	fn end_length(self) -> EndLength<'invoker>;

	/// Returns the result of the method call as a CBOR-encoded data item.
	///
	/// On success, the CBOR-encoded result is written into `buffer`, and the number of bytes
	/// written is returned.
	///
	/// # Errors
	/// * [`BufferTooShort`](MethodCallError::BufferTooShort) is returned if the method call
	///   succeeded but the return value could not be fetched because the buffer is too small and
	///   not resizable.
	/// * [`NoSuchComponent`](MethodCallError::NoSuchComponent) is returned if the method call
	///   failed because the component does not exist or is inaccessible.
	/// * [`NoSuchMethod`](MethodCallError::NoSuchMethod) is returned if the method call failed
	///   because the method does not exist on the component.
	/// * [`BadParameters`](MethodCallError::BadParameters) is returned if the parameters provided
	///   when starting the call are not acceptable for the method.
	/// * [`Other`](MethodCallError::Other) is returned if the method call failed.
	fn end_into_buffer<'buffer, B: Buffer>(
		self,
		buffer: &'buffer mut B,
	) -> EndIntoBuffer<'invoker, 'buffer, B>;
}

impl<'invoker> MethodCallExt<'invoker> for MethodCall<'invoker> {
	fn end_length(self) -> EndLength<'invoker> {
		EndLength(Some(self))
	}

	fn end_into_buffer<'buffer, B: Buffer>(
		self,
		buffer: &'buffer mut B,
	) -> EndIntoBuffer<'invoker, 'buffer, B> {
		EndIntoBuffer(Some((self, buffer)))
	}
}

/// Information about a method to call.
trait Callable {
	/// Starts the method call.
	///
	/// # Errors
	/// * [`CborDecode`](MethodCallError::CborDecode) is returned if the `params` parameter is
	///   present but contains an invalid or unsupported CBOR sequence.
	/// * [`BadDescriptor`](MethodCallError::BadDescriptor) is returned if the parameters contain a
	///   descriptor reference to a descriptor that is not open.
	/// * [`TooManyDescriptors`](MethodCallError::TooManyDescriptors) is returned if the descriptor
	///   table is too full and some descriptors must be closed before another method call can be
	///   made.
	fn start<'invoker>(
		self,
		invoker: &'invoker mut Invoker,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>), Error>;
}

/// A component method call.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct ComponentMethodCallable<'a> {
	address: &'a Address,
	method: &'a str,
}

impl Callable for ComponentMethodCallable<'_> {
	fn start<'invoker>(
		self,
		invoker: &'invoker mut Invoker,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>), Error> {
		invoker.component_method(self.address, self.method, params)
	}
}

/// A value call.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct ValueCallable<'a>(Borrowed<'a>);

impl Callable for ValueCallable<'_> {
	fn start<'invoker>(
		self,
		invoker: &'invoker mut Invoker,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>), Error> {
		invoker.value(&self.0, params)
	}
}

/// A value indexed read.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct ValueIndexedReadCallable<'a>(Borrowed<'a>);

impl Callable for ValueIndexedReadCallable<'_> {
	fn start<'invoker>(
		self,
		invoker: &'invoker mut Invoker,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>), Error> {
		invoker.value_indexed_read(&self.0, params)
	}
}

/// A value indexed write.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct ValueIndexedWriteCallable<'a>(Borrowed<'a>);

impl Callable for ValueIndexedWriteCallable<'_> {
	fn start<'invoker>(
		self,
		invoker: &'invoker mut Invoker,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>), Error> {
		invoker.value_indexed_write(&self.0, params)
	}
}

/// A value method call.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct ValueMethodCallable<'a> {
	value: Borrowed<'a>,
	method: &'a str,
}

impl Callable for ValueMethodCallable<'_> {
	fn start<'invoker>(
		self,
		invoker: &'invoker mut Invoker,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>), Error> {
		invoker.value_method(&self.value, self.method, params)
	}
}

/// Performs a complete method call.
///
/// The `'invoker` lifetime parameter is the lifetime of the method invoker that is performing the
/// call. The `'buffer` lifetime parameter is the lifetime of the scratch buffer. The `Params` type
/// parameter is the type of the parameters to the call. The `Return` type parameter is the type of
/// the return value(s) from the call. The `Call` type is the type of method being invoked.
///
/// # Panics
/// This function panics if `buffer` is too small to hold `params` and cannot be resized, or if
/// `params` fails to encode.
async fn call<
	'invoker,
	'buffer,
	Params: Encode,
	Return: Decode<'buffer>,
	Call: Callable,
	B: Buffer,
>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut B,
	params: Option<&Params>,
	call: Call,
) -> Result<Return, MethodCallError<'invoker>> {
	let params = params.map(|params| buffer.encode_into(params));
	// try_into() will never fail because Callable::start will never return an exception-bearing
	// error.
	let (_, call) = call
		.start(invoker, params)
		.map_err(|e| e.try_into().unwrap())?;
	let len = call.end_into_buffer(buffer).await?;
	decode(&buffer.as_mut()[..len]).or(Err(MethodCallError::CborDecode))
}

/// Performs a complete method call on a component.
///
/// The `'invoker` lifetime parameter is the lifetime of the method invoker that is performing the
/// call. The `'buffer` lifetime parameter is the lifetime of the scratch buffer. The `Params` type
/// parameter is the type of the parameters to the call. The `Return` type parameter is the type of
/// the return value(s) from the call. The `Call` type is the type of method being invoked.
///
/// The `address` parameter identifies the component by its UUID. The `method` parameter identifies
/// the method by its name. The `params` parameter, if present, contains a CBOR-encodable object of
/// parameters to pass to the method.
///
/// # Errors
/// * [`BufferTooShort`](MethodCallError::BufferTooShort) is returned if the call succeeded but the
///   return value could not be fetched because the buffer is too small and not resizable.
/// * [`CborDecode`](MethodCallError::CborDecode) is returned if the `params` parameter is present
///   but contains an invalid or unsupported CBOR sequence.
/// * [`BadDescriptor`](MethodCallError::BadDescriptor) is returned if the parameters contain a
///   descriptor reference to a descriptor that is not open.
/// * [`TooManyDescriptors`](MethodCallError::TooManyDescriptors) is returned if the descriptor
///   table is too full and some descriptors must be closed before another method call can be made.
/// * [`NoSuchComponent`](MethodCallError::NoSuchComponent) is returned if the method call failed
///   because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](MethodCallError::NoSuchMethod) is returned if the method call failed because
///   the method does not exist on the component.
/// * [`BadParameters`](MethodCallError::BadParameters) is returned if the parameters provided when
///   starting the call are not acceptable for the method.
/// * [`Other`](MethodCallError::Other) is returned if the method call failed.
///
/// # Panics
/// This function panics if `buffer` is too small to hold `params` and cannot be resized, or if
/// `params` fails to encode.
pub async fn component_method<
	'invoker,
	'buffer,
	Params: Encode,
	Return: Decode<'buffer>,
	B: Buffer,
>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut B,
	address: &Address,
	method: &str,
	params: Option<&Params>,
) -> Result<Return, MethodCallError<'invoker>> {
	call(
		invoker,
		buffer,
		params,
		ComponentMethodCallable { address, method },
	)
	.await
}

/// Performs a complete call of a value.
///
/// The `'invoker` lifetime parameter is the lifetime of the method invoker that is performing the
/// call. The `'buffer` lifetime parameter is the lifetime of the scratch buffer. The `Params` type
/// parameter is the type of the parameters to the call. The `Return` type parameter is the type of
/// the return value(s) from the call. The `Call` type is the type of method being invoked.
///
/// The `descriptor` parameter identifies the opaque value by its descriptor. The `params`
/// parameter, if present, contains a CBOR-encodable object of parameters to pass to the method.
///
/// # Errors
/// * [`BufferTooShort`](MethodCallError::BufferTooShort) is returned if the call succeeded but the
///   return value could not be fetched because the buffer is too small and not resizable.
/// * [`CborDecode`](MethodCallError::CborDecode) is returned if the `params` parameter is present
///   but contains an invalid or unsupported CBOR sequence.
/// * [`BadDescriptor`](MethodCallError::BadDescriptor) is returned if the parameters contain a
///   descriptor reference to a descriptor that is not open.
/// * [`TooManyDescriptors`](MethodCallError::TooManyDescriptors) is returned if the descriptor
///   table is too full and some descriptors must be closed before another method call can be made.
/// * [`NoSuchComponent`](MethodCallError::NoSuchComponent) is returned if the method call failed
///   because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](MethodCallError::NoSuchMethod) is returned if the method call failed because
///   the method does not exist on the component.
/// * [`BadParameters`](MethodCallError::BadParameters) is returned if the parameters provided when
///   starting the call are not acceptable for the method.
/// * [`Other`](MethodCallError::Other) is returned if the method call failed.
///
/// # Panics
/// This function panics if `buffer` is too small to hold `params` and cannot be resized, or if
/// `params` fails to encode.
pub async fn value<
	'invoker,
	'buffer,
	Params: Encode,
	Return: Decode<'buffer>,
	Descriptor: AsDescriptor,
	B: Buffer,
>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut B,
	descriptor: &Descriptor,
	params: Option<&Params>,
) -> Result<Return, MethodCallError<'invoker>> {
	call(
		invoker,
		buffer,
		params,
		ValueCallable(descriptor.as_descriptor()),
	)
	.await
}

/// Performs a complete indexed read of a value.
///
/// The `'invoker` lifetime parameter is the lifetime of the method invoker that is performing the
/// call. The `'buffer` lifetime parameter is the lifetime of the scratch buffer. The `Params` type
/// parameter is the type of the parameters to the call. The `Return` type parameter is the type of
/// the return value(s) from the call. The `Call` type is the type of method being invoked.
///
/// The `descriptor` parameter identifies the opaque value by its descriptor. The `params`
/// parameter, if present, contains a CBOR-encodable object of parameters to pass to the method.
///
/// # Errors
/// * [`BufferTooShort`](MethodCallError::BufferTooShort) is returned if the call succeeded but the
///   return value could not be fetched because the buffer is too small and not resizable.
/// * [`CborDecode`](MethodCallError::CborDecode) is returned if the `params` parameter is present
///   but contains an invalid or unsupported CBOR sequence.
/// * [`BadDescriptor`](MethodCallError::BadDescriptor) is returned if the parameters contain a
///   descriptor reference to a descriptor that is not open.
/// * [`TooManyDescriptors`](MethodCallError::TooManyDescriptors) is returned if the descriptor
///   table is too full and some descriptors must be closed before another method call can be made.
/// * [`NoSuchComponent`](MethodCallError::NoSuchComponent) is returned if the method call failed
///   because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](MethodCallError::NoSuchMethod) is returned if the method call failed because
///   the method does not exist on the component.
/// * [`BadParameters`](MethodCallError::BadParameters) is returned if the parameters provided when
///   starting the call are not acceptable for the method.
/// * [`Other`](MethodCallError::Other) is returned if the method call failed.
///
/// # Panics
/// This function panics if `buffer` is too small to hold `params` and cannot be resized, or if
/// `params` fails to encode.
pub async fn value_indexed_read<
	'invoker,
	'buffer,
	Params: Encode,
	Return: Decode<'buffer>,
	Descriptor: AsDescriptor,
	B: Buffer,
>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut B,
	descriptor: &Descriptor,
	params: Option<&Params>,
) -> Result<Return, MethodCallError<'invoker>> {
	call(
		invoker,
		buffer,
		params,
		ValueIndexedReadCallable(descriptor.as_descriptor()),
	)
	.await
}

/// Performs a complete indexed write of a value.
///
/// The `'invoker` lifetime parameter is the lifetime of the method invoker that is performing the
/// call. The `'buffer` lifetime parameter is the lifetime of the scratch buffer. The `Params` type
/// parameter is the type of the parameters to the call. The `Return` type parameter is the type of
/// the return value(s) from the call. The `Call` type is the type of method being invoked.
///
/// The `descriptor` parameter identifies the opaque value by its descriptor. The `params`
/// parameter, if present, contains a CBOR-encodable object of parameters to pass to the method.
///
/// # Errors
/// * [`BufferTooShort`](MethodCallError::BufferTooShort) is returned if the call succeeded but the
///   return value could not be fetched because the buffer is too small and not resizable.
/// * [`CborDecode`](MethodCallError::CborDecode) is returned if the `params` parameter is present
///   but contains an invalid or unsupported CBOR sequence.
/// * [`BadDescriptor`](MethodCallError::BadDescriptor) is returned if the parameters contain a
///   descriptor reference to a descriptor that is not open.
/// * [`TooManyDescriptors`](MethodCallError::TooManyDescriptors) is returned if the descriptor
///   table is too full and some descriptors must be closed before another method call can be made.
/// * [`NoSuchComponent`](MethodCallError::NoSuchComponent) is returned if the method call failed
///   because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](MethodCallError::NoSuchMethod) is returned if the method call failed because
///   the method does not exist on the component.
/// * [`BadParameters`](MethodCallError::BadParameters) is returned if the parameters provided when
///   starting the call are not acceptable for the method.
/// * [`Other`](MethodCallError::Other) is returned if the method call failed.
///
/// # Panics
/// This function panics if `buffer` is too small to hold `params` and cannot be resized, or if
/// `params` fails to encode.
pub async fn value_indexed_write<
	'invoker,
	'buffer,
	Params: Encode,
	Return: Decode<'buffer>,
	Descriptor: AsDescriptor,
	B: Buffer,
>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut B,
	descriptor: &Descriptor,
	params: Option<&Params>,
) -> Result<Return, MethodCallError<'invoker>> {
	call(
		invoker,
		buffer,
		params,
		ValueIndexedWriteCallable(descriptor.as_descriptor()),
	)
	.await
}

/// Performs a complete method call on a value.
///
/// The `'invoker` lifetime parameter is the lifetime of the method invoker that is performing the
/// call. The `'buffer` lifetime parameter is the lifetime of the scratch buffer. The `Params` type
/// parameter is the type of the parameters to the call. The `Return` type parameter is the type of
/// the return value(s) from the call. The `Call` type is the type of method being invoked.
///
/// The `descriptor` parameter identifies the opaque value by its descriptor. The `method`
/// parameter identifies the method by its name. The `params` parameter, if present, contains a
/// CBOR-encodable object of parameters to pass to the method.
///
/// # Errors
/// * [`BufferTooShort`](MethodCallError::BufferTooShort) is returned if the call succeeded but the
///   return value could not be fetched because the buffer is too small and not resizable.
/// * [`CborDecode`](MethodCallError::CborDecode) is returned if the `params` parameter is present
///   but contains an invalid or unsupported CBOR sequence.
/// * [`BadDescriptor`](MethodCallError::BadDescriptor) is returned if the parameters contain a
///   descriptor reference to a descriptor that is not open.
/// * [`TooManyDescriptors`](MethodCallError::TooManyDescriptors) is returned if the descriptor
///   table is too full and some descriptors must be closed before another method call can be made.
/// * [`NoSuchComponent`](MethodCallError::NoSuchComponent) is returned if the method call failed
///   because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](MethodCallError::NoSuchMethod) is returned if the method call failed because
///   the method does not exist on the component.
/// * [`BadParameters`](MethodCallError::BadParameters) is returned if the parameters provided when
///   starting the call are not acceptable for the method.
/// * [`Other`](MethodCallError::Other) is returned if the method call failed.
///
/// # Panics
/// This function panics if `buffer` is too small to hold `params` and cannot be resized, or if
/// `params` fails to encode.
pub async fn value_method<
	'invoker,
	'buffer,
	Params: Encode,
	Return: Decode<'buffer>,
	Descriptor: AsDescriptor,
	B: Buffer,
>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut B,
	descriptor: &Descriptor,
	method: &str,
	params: Option<&Params>,
) -> Result<Return, MethodCallError<'invoker>> {
	call(
		invoker,
		buffer,
		params,
		ValueMethodCallable {
			value: descriptor.as_descriptor(),
			method,
		},
	)
	.await
}
