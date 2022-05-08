//! Provides futures that wait for method calls to complete and fetch their results.

use super::sleep;
use alloc::vec::Vec;
use core::future::Future;
use core::hint::unreachable_unchecked;
use core::pin::Pin;
use core::task::{Context, Poll};
use minicbor::{decode, encode, Decode, Encode};
use oc_wasm_safe::{
	component::{InvokeEndLengthResult, InvokeEndResult, InvokeResult, Invoker, MethodCall},
	descriptor::{AsDescriptor, Borrowed},
	error::Result,
	Address,
};

/// A future that waits for the method call to complete, then returns the length, in bytes, of the
/// result.
///
/// On success, the length and the [`MethodCall`](MethodCall) are returned, allowing the
/// [`MethodCall`](MethodCall) to be reused to fetch the actual bytes.
///
/// # Errors
/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the method
///   call failed because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
///   failed because the method does not exist on the component.
/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the parameters
///   provided when starting the call are not acceptable for the method.
/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
#[must_use = "A future does nothing unless awaited."]
pub struct EndLength<'invoker>(Option<MethodCall<'invoker>>);

impl<'invoker> Future for EndLength<'invoker> {
	type Output = Result<(usize, MethodCall<'invoker>)>;

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
/// is returned. If the buffer is not large enough to hold the call result,
/// [`BufferTooShort`](EndResult::BufferTooShort) is returned, containing the `MethodCall` object,
/// allowing the caller to retry fetching the results with a larger buffer, or call
/// [`end_length`](MethodCallExt::end_length) to obtain the needed buffer size.
///
/// # Errors
/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the method
///   call failed because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
///   failed because the method does not exist on the component.
/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the parameters
///   provided when starting the call are not acceptable for the method.
/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
#[must_use = "A future does nothing unless awaited."]
pub struct EndIntoSlice<'invoker, 'buffer>(Option<(MethodCall<'invoker>, &'buffer mut [u8])>);

impl<'invoker, 'buffer> Future for EndIntoSlice<'invoker, 'buffer> {
	type Output = Result<EndResult<'invoker>>;

	fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
		let (call, buffer): (MethodCall<'invoker>, &'buffer mut [u8]) = self.0.take().unwrap();
		match call.end(buffer) {
			InvokeEndResult::Done(Ok(result)) => Poll::Ready(Ok(EndResult::Done(result))),
			InvokeEndResult::Done(Err(e)) => Poll::Ready(Err(e)),
			InvokeEndResult::BufferTooShort(call) => {
				Poll::Ready(Ok(EndResult::BufferTooShort(call)))
			}
			InvokeEndResult::Pending(call) => {
				self.0 = Some((call, buffer));
				sleep::register_next_timeslice_wakeup(context.waker());
				Poll::Pending
			}
		}
	}
}

/// Returns the result of the method call as a CBOR-encoded data item.
///
/// On success, the CBOR-encoded result is written into `buffer`, which is modified to be the exact
/// size of the written bytes.
///
/// # Errors
/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the method
///   call failed because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
///   failed because the method does not exist on the component.
/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the parameters
///   provided when starting the call are not acceptable for the method.
/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
#[must_use = "A future does nothing unless awaited."]
pub struct EndIntoVec<'invoker, 'buffer>(Option<(MethodCall<'invoker>, &'buffer mut Vec<u8>)>);

impl<'invoker, 'buffer> Future for EndIntoVec<'invoker, 'buffer> {
	type Output = Result<()>;

	fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
		let (call, buffer): (MethodCall<'invoker>, &'buffer mut Vec<u8>) = self.0.take().unwrap();
		match call.end_length() {
			InvokeEndLengthResult::Pending(new_call) => {
				self.0 = Some((new_call, buffer));
				sleep::register_next_timeslice_wakeup(context.waker());
				Poll::Pending
			}
			InvokeEndLengthResult::Done(Err(e)) => Poll::Ready(Err(e)),
			InvokeEndLengthResult::Done(Ok((length, call))) => Poll::Ready({
				buffer.resize(length, 0);
				// The code is the same but the comments are different for the two cases and the distinction matters.
				#[allow(clippy::match_same_arms)]
				match call.end(buffer) {
					InvokeEndResult::Done(Ok(_)) => Ok(()),
					InvokeEndResult::Done(Err(e)) => Err(e),
					InvokeEndResult::BufferTooShort(_) =>
					// SAFETY: This is impossible because we just called end_length and resized the
					// buffer to a sufficient size, and nobody else could have cleared the method
					// call and started another one in the mean time because we hold the
					// MethodCall.
					unsafe { unreachable_unchecked() },
					InvokeEndResult::Pending(_) =>
					// SAFETY: This is impossible because we just called end_length and it returned
					// Done, and nobody else could have cleared the method call and started another
					// one in the mean time because we hold the MethodCall.
					unsafe { unreachable_unchecked() },
				}
			}),
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
	/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the
	///   method call failed because the component does not exist or is inaccessible.
	/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
	///   failed because the method does not exist on the component.
	/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the
	///   parameters provided when starting the call are not acceptable for the method.
	/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
	fn end_length(self) -> EndLength<'invoker>;

	/// Returns the result of the method call as a CBOR-encoded data item.
	///
	/// On success, the CBOR-encoded result is written into `buffer`, and a slice referring to the
	/// written bytes is returned. If the buffer is not large enough to hold the call result,
	/// [`BufferTooShort`](EndResult::BufferTooShort) is returned, containing the `MethodCall`
	/// object, allowing the caller to retry fetching the results with a larger buffer, or call
	/// [`end_length`](Self::end_length) to obtain the needed buffer size.
	///
	/// # Errors
	/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the
	///   method call failed because the component does not exist or is inaccessible.
	/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
	///   failed because the method does not exist on the component.
	/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the
	///   parameters provided when starting the call are not acceptable for the method.
	/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
	fn end_into_slice<'buffer>(self, buffer: &'buffer mut [u8]) -> EndIntoSlice<'invoker, 'buffer>;

	/// Returns the result of the method call as a CBOR-encoded data item.
	///
	/// On success, the CBOR-encoded result is written into `buffer`, which is modified to be the
	/// exact size of the written bytes.
	///
	/// # Errors
	/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the
	///   method call failed because the component does not exist or is inaccessible.
	/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
	///   failed because the method does not exist on the component.
	/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the
	///   parameters provided when starting the call are not acceptable for the method.
	/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
	fn end_into_vec<'buffer>(self, buffer: &'buffer mut Vec<u8>) -> EndIntoVec<'invoker, 'buffer>;
}

impl<'invoker> MethodCallExt<'invoker> for MethodCall<'invoker> {
	fn end_length(self) -> EndLength<'invoker> {
		EndLength(Some(self))
	}

	fn end_into_slice<'buffer>(self, buffer: &'buffer mut [u8]) -> EndIntoSlice<'invoker, 'buffer> {
		EndIntoSlice(Some((self, buffer)))
	}

	fn end_into_vec<'buffer>(self, buffer: &'buffer mut Vec<u8>) -> EndIntoVec<'invoker, 'buffer> {
		EndIntoVec(Some((self, buffer)))
	}
}

/// Information about a method to call.
trait Callable {
	/// Starts the method call.
	///
	/// # Errors
	/// * [`CborDecode`](Error::CborDecode) is returned if the `params` parameter is present but
	///   contains an invalid or unsupported CBOR sequence.
	/// * [`BadDescriptor`](Error::BadDescriptor) is returned if the parameters contain a
	///   descriptor reference to a descriptor that is not open.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors) is returned if the descriptor table is
	///   too full and some descriptors must be closed before another method call can be made.
	fn start<'invoker>(
		self,
		invoker: &'invoker mut Invoker,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>)>;
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
	) -> Result<(InvokeResult, MethodCall<'invoker>)> {
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
	) -> Result<(InvokeResult, MethodCall<'invoker>)> {
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
	) -> Result<(InvokeResult, MethodCall<'invoker>)> {
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
	) -> Result<(InvokeResult, MethodCall<'invoker>)> {
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
	) -> Result<(InvokeResult, MethodCall<'invoker>)> {
		invoker.value_method(&self.value, self.method, params)
	}
}

/// Performs a complete method call.
///
/// The `'invoker` lifetime parameter is the lifetime of the method invoker that is performing the
/// call. The `'buffer` lifetime parameter is the lifetime of the scratch buffer. The `Params` type
/// parameter is the type of the parameters to the call. The `Return` type parameter is the type of
/// the return value(s) from the call. The `Call` type is the type of method being invoked.
async fn call<'invoker, 'buffer, Params: Encode, Return: Decode<'buffer>, Call: Callable>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut Vec<u8>,
	params: Option<&Params>,
	call: Call,
) -> Result<Return> {
	let params = params.map(|params| {
		buffer.clear();
		encode(params, &mut *buffer).unwrap();
		buffer.as_slice()
	});
	let (_, call) = call.start(invoker, params)?;
	call.end_into_vec(buffer).await?;
	decode(buffer).or(Err(oc_wasm_safe::error::Error::CborDecode))
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
/// * [`CborDecode`](oc_wasm_safe::error::Error::CborDecode) is returned if the `params` parameter
///   is present but contains an invalid or unsupported CBOR sequence.
/// * [`BadDescriptor`](oc_wasm_safe::error::Error::BadDescriptor) is returned if the parameters
///   contain a descriptor reference to a descriptor that is not open.
/// * [`TooManyDescriptors`](oc_wasm_safe::error::Error::TooManyDescriptors) is returned if the
///   descriptor table is too full and some descriptors must be closed before another method call
///   can be made.
/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the method
///   call failed because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
///   failed because the method does not exist on the component.
/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the parameters
///   provided when starting the call are not acceptable for the method.
/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
pub async fn component_method<'invoker, 'buffer, Params: Encode, Return: Decode<'buffer>>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut Vec<u8>,
	address: &Address,
	method: &str,
	params: Option<&Params>,
) -> Result<Return> {
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
/// * [`CborDecode`](oc_wasm_safe::error::Error::CborDecode) is returned if the `params` parameter
///   is present but contains an invalid or unsupported CBOR sequence.
/// * [`BadDescriptor`](oc_wasm_safe::error::Error::BadDescriptor) is returned if the parameters
///   contain a descriptor reference to a descriptor that is not open.
/// * [`TooManyDescriptors`](oc_wasm_safe::error::Error::TooManyDescriptors) is returned if the
///   descriptor table is too full and some descriptors must be closed before another method call
///   can be made.
/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the method
///   call failed because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
///   failed because the method does not exist on the component.
/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the parameters
///   provided when starting the call are not acceptable for the method.
/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
pub async fn value<
	'invoker,
	'buffer,
	Params: Encode,
	Return: Decode<'buffer>,
	Descriptor: AsDescriptor,
>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut Vec<u8>,
	descriptor: &Descriptor,
	params: Option<&Params>,
) -> Result<Return> {
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
/// * [`CborDecode`](oc_wasm_safe::error::Error::CborDecode) is returned if the `params` parameter
///   is present but contains an invalid or unsupported CBOR sequence.
/// * [`BadDescriptor`](oc_wasm_safe::error::Error::BadDescriptor) is returned if the parameters
///   contain a descriptor reference to a descriptor that is not open.
/// * [`TooManyDescriptors`](oc_wasm_safe::error::Error::TooManyDescriptors) is returned if the
///   descriptor table is too full and some descriptors must be closed before another method call
///   can be made.
/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the method
///   call failed because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
///   failed because the method does not exist on the component.
/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the parameters
///   provided when starting the call are not acceptable for the method.
/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
pub async fn value_indexed_read<
	'invoker,
	'buffer,
	Params: Encode,
	Return: Decode<'buffer>,
	Descriptor: AsDescriptor,
>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut Vec<u8>,
	descriptor: &Descriptor,
	params: Option<&Params>,
) -> Result<Return> {
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
/// * [`CborDecode`](oc_wasm_safe::error::Error::CborDecode) is returned if the `params` parameter
///   is present but contains an invalid or unsupported CBOR sequence.
/// * [`BadDescriptor`](oc_wasm_safe::error::Error::BadDescriptor) is returned if the parameters
///   contain a descriptor reference to a descriptor that is not open.
/// * [`TooManyDescriptors`](oc_wasm_safe::error::Error::TooManyDescriptors) is returned if the
///   descriptor table is too full and some descriptors must be closed before another method call
///   can be made.
/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the method
///   call failed because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
///   failed because the method does not exist on the component.
/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the parameters
///   provided when starting the call are not acceptable for the method.
/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
pub async fn value_indexed_write<
	'invoker,
	'buffer,
	Params: Encode,
	Return: Decode<'buffer>,
	Descriptor: AsDescriptor,
>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut Vec<u8>,
	descriptor: &Descriptor,
	params: Option<&Params>,
) -> Result<Return> {
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
/// * [`CborDecode`](oc_wasm_safe::error::Error::CborDecode) is returned if the `params` parameter
///   is present but contains an invalid or unsupported CBOR sequence.
/// * [`BadDescriptor`](oc_wasm_safe::error::Error::BadDescriptor) is returned if the parameters
///   contain a descriptor reference to a descriptor that is not open.
/// * [`TooManyDescriptors`](oc_wasm_safe::error::Error::TooManyDescriptors) is returned if the
///   descriptor table is too full and some descriptors must be closed before another method call
///   can be made.
/// * [`NoSuchComponent`](oc_wasm_safe::error::Error::NoSuchComponent) is returned if the method
///   call failed because the component does not exist or is inaccessible.
/// * [`NoSuchMethod`](oc_wasm_safe::error::Error::NoSuchMethod) is returned if the method call
///   failed because the method does not exist on the component.
/// * [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) is returned if the parameters
///   provided when starting the call are not acceptable for the method.
/// * [`Other`](oc_wasm_safe::error::Error::Other) is returned if the method call failed.
pub async fn value_method<
	'invoker,
	'buffer,
	Params: Encode,
	Return: Decode<'buffer>,
	Descriptor: AsDescriptor,
>(
	invoker: &'invoker mut Invoker,
	buffer: &'buffer mut Vec<u8>,
	descriptor: &Descriptor,
	method: &str,
	params: Option<&Params>,
) -> Result<Return> {
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
