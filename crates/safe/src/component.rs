//! Types related to examining and calling methods on components attached to or installed in a
//! computer.
//!
//! Some of the APIs in this module are independent and can be used standalone. Invoking methods,
//! however, is more complicated, and must be done as follows:
//!
//! 1. Call [`Invoker::take`](Invoker::take) if not already done (this can only be done once at
//!    program startup) to obtain an [`Invoker`](Invoker).
//! 2. Call one of the methods on the [`Invoker`](Invoker) to start the component call. On success,
//!    this returns an [`InvokeResult`](InvokeResult) indicating whether the call is complete or
//!    not along with a [`MethodCall`](MethodCall) to use to fetch the result.
//! 3. If necessary, wait until the call is complete by returning from `run`.
//! 4. If necessary, call [`MethodCall::end_length`](MethodCall::end_length) to allocate a
//!    sufficient buffer to hold the result.
//! 5. Call one of the methods on [`MethodCall`](MethodCall) to fetch the result, which both fills
//!    the provided buffer and also returns an [`InvokeEndResult`](InvokeEndResult).
//! 6. If [`Done`](InvokeEndResult::Done) is returned with an `Ok` result, examine the result in
//!    the buffer.
//! 7. If [`Done`](InvokeEndResult::Done) is returned with an `Err` result, the
//!    [`MethodCallError`](MethodCallError) can be examined to determine the reason for the
//!    failure, including detailed exception information in the form of a
//!    [`LastException`](LastException) object if applicable.
//! 8. Another method call can only be started once the [`InvokeResult`](InvokeResult),
//!    [`MethodCall`](MethodCall), [`InvokeEndResult`](InvokeEndResult), and
//!    [`LastException`](LastException) have all been dropped. This is enforced by means of
//!    lifetime bindings between those types and the [`Invoker`](Invoker), preventing the latter
//!    from being reused too early.

use super::descriptor::AsDescriptor;
use super::error::{Error, Result};
use super::helpers::{call_buffer_len, call_buffer_str, call_string};
use super::Address;
use crate::panic_or_trap;
use core::convert::TryFrom;
use core::fmt::{Display, Formatter};
use core::marker::PhantomData;
use core::num::NonZeroUsize;
use core::ptr;
use oc_wasm_sys::component as sys;

/// An object that is capable of listing components attached to the computer.
///
/// Because only one component listing can be in progress at a time, only one value of this type
/// can exist. An application written as a state machine should take the instance and store it in a
/// `static` variable. An application using `async` and `await` in which only one task needs to
/// list components should either take the value in that task, or take it in the top-level task and
/// move it into the task that needs it. An application using `async` and `await` in which multiple
/// tasks all need to list components needs to arrange mutual exclusion so that only one task can
/// access the lister at a time.
pub struct Lister(());

impl Lister {
	/// Returns the lister.
	///
	/// This function can only be called once in the lifetime of the application. On the second and
	/// subsequent calls, it will return `None`.
	#[must_use = "A Lister can only be taken once. It needs to be saved. Discarding it means it is impossible to ever list components."]
	pub fn take() -> Option<Self> {
		static mut INSTANCE: Option<Lister> = Some(Self(()));
		// SAFETY: Wasm doesn’t have threads, so only one caller can get here at a time, and the
		// Option will be empty for all but the first caller.
		unsafe { INSTANCE.take() }
	}

	/// Begins listing the components attached to the computer.
	///
	/// The `component_type` parameter, if present, restricts the listing to only return components
	/// of the specified type. If the parameter is absent, all components are returned.
	///
	/// # Panics
	/// This function panics if the underlying syscall fails, because the only reasons it could
	/// fail should be impossible due to the type system.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn start<'lister>(&'lister mut self, component_type: Option<&str>) -> Listing<'lister> {
		let result =
			// SAFETY: list_start permits null or a string pointer/length pair.
			unsafe{call_string(sys::list_start, component_type)};
		// Can’t fail because list_start can only fail due to MemoryFault or StringDecode, and
		// Error::from_i32 already treats those as unreachable.
		result.unwrap_or_else(|_| panic_or_trap!("unreachable"));
		Listing(PhantomData)
	}
}

/// An in-progress component listing.
///
/// The `'lister` lifetime parameter is the lifetime of the component lister that is performing the
/// listing.
#[must_use = "Starting a component listing is only useful if you read the results."]
pub struct Listing<'lister>(PhantomData<&'lister mut Lister>);

impl<'lister> Listing<'lister> {
	/// Returns the next entry in the list of components.
	///
	/// If there is a next entry, its UUID is return. If not, `None` is returned.
	///
	/// # Panics
	/// * This function panics if the underlying syscall fails, because the only reasons it could
	///   fail should be impossible due to the type system.
	/// * This function panics if there is a mismatch between OC-Wasm-safe’s and OpenComputers’s
	///   ideas of the length or formatting of a component address.
	#[allow(clippy::should_implement_trait)] // It’s very like Iterator::next, but can’t be due to lifetimes.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn next<'listing>(&'listing mut self) -> Option<ListEntry<'listing, 'lister>> {
		// SAFETY: list_next permits a writeable buffer pointer and promises to always write a
		// valid UUID. It can only fail due to MemoryFault, which, because we provide it with a
		// valid buffer, is impossible. However we care whether its return value is 0 or 1.
		let mut buf = uuid::Bytes::default();
		let rc = unsafe { sys::list_next(buf.as_mut_ptr()) };
		if rc == 0 {
			None
		} else {
			let address = Address::from_bytes(buf);
			Some(ListEntry {
				address,
				listing: PhantomData,
			})
		}
	}
}

/// A single in from a listing.
///
/// The `'lister` lifetime parameter is the lifetime of the component lister. The `'listing`
/// lifetime parameter is the lifetime of the specific listing being performed.
#[derive(Debug)]
pub struct ListEntry<'listing, 'lister> {
	/// The component address.
	address: Address,

	/// A phantom that allows the `'listing` lifetime to be recorded.
	listing: PhantomData<&'listing mut Listing<'lister>>,
}

impl ListEntry<'_, '_> {
	/// Returns the address of the component.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.address
	}

	/// Returns the length, in bytes, of the component’s type.
	///
	/// # Panics
	/// * This function panics if the underlying syscall fails, because the only reasons it could
	///   fail should be impossible due to the type system.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	#[must_use = "This function is only useful for its return value"]
	pub fn type_name_len(&self) -> NonZeroUsize {
		// SAFETY: list_type permits null.
		let len = unsafe { call_buffer_len(sys::list_type) }
			.unwrap_or_else(|_| panic_or_trap!("unreachable"));
		// SAFETY: A component type can’t be empty.
		unsafe { NonZeroUsize::new_unchecked(len) }
	}

	/// Returns the type of the most recently listed component.
	///
	/// The `buffer` parameter identifies where to store the component type.
	///
	/// The type is written to `buffer` and a string slice referring to it is returned.
	///
	/// # Errors
	/// * [`BufferTooShort`](Error::BufferTooShort) is returned if `buffer` is provided but is not
	///   large enough to hold the component type.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	#[must_use = "This function is only useful for its return value"]
	pub fn type_name<'buffer>(&self, buffer: &'buffer mut [u8]) -> Result<&'buffer mut str> {
		// SAFETY: list_type permits a writeable buffer pointer/length pair and promises to always
		// write a valid UTF-8 string and return its length.
		unsafe { call_buffer_str(sys::list_type, buffer) }
	}
}

/// Returns the length, in bytes, of the type of a component.
///
/// The `address` parameter identifies the component by its UUID.
///
/// # Errors
/// * [`NoSuchComponent`](Error::NoSuchComponent) is returned if the component does not exist or is
///   inaccessible.
#[allow(clippy::module_name_repetitions)] // For parallelism with component_type
#[must_use = "This function is only useful for its return value"]
pub fn component_type_len(address: &Address) -> Result<NonZeroUsize> {
	let address = address.as_bytes();
	let len = Error::from_isize(
		// SAFETY: component_type permits a UUID input pointer and null output pointer/length pair.
		unsafe { sys::component_type(address.as_ptr(), ptr::null_mut(), 0) },
	)?;
	// SAFETY: A component type can’t be empty.
	Ok(unsafe { NonZeroUsize::new_unchecked(len) })
}

/// Returns the type of a component.
///
/// The `address` parameter identifies the component by its UUID. The `buffer` parameter identifies
/// where to store the component type.
///
/// The type is written into `buffer` and a string slice referring to it is returned.
///
/// # Errors
/// * [`BufferTooShort`](Error::BufferTooShort) is returned if `buffer` is provided but is not
///   large enough to hold the component type.
/// * [`NoSuchComponent`](Error::NoSuchComponent) is returned if the component does not exist or is
///   inaccessible.
#[allow(clippy::module_name_repetitions)] // Because just “type” would be a keyword
#[must_use = "This function is only useful for its return value"]
pub fn component_type<'buf>(address: &Address, buffer: &'buf mut [u8]) -> Result<&'buf mut str> {
	let address = address.as_bytes();
	let bytes_written = {
		let buf_len = buffer.len();
		let buf_ptr = buffer.as_mut_ptr();
		Error::from_isize(
			// SAFETY: component_type permits a UUID input pointer and a writeable buffer output
			// pointer/length pair.
			unsafe { sys::component_type(address.as_ptr(), buf_ptr, buf_len) },
		)?
	};
	Ok(
		// SAFETY: component_type promises to always write a valid UTF-8 string and return its
		// length.
		unsafe { core::str::from_utf8_unchecked_mut(buffer.get_unchecked_mut(0..bytes_written)) },
	)
}

/// Returns the slot that a component is installed into.
///
/// The `address` parameter identifies the component by its UUID.
///
/// # Errors
/// * [`NoSuchComponent`](Error::NoSuchComponent) is returned if the component does not exist or is
///   inaccessible.
/// * [`Other`](Error::Other) is returned if the component exists but is not installed in a slot.
#[must_use = "This function is only useful for its return value"]
pub fn slot(address: &Address) -> Result<u32> {
	let address = address.as_bytes();
	Error::from_i32(
		// SAFETY: slot permits a string pointer/length pair.
		unsafe { sys::slot(address.as_ptr(), address.len()) },
	)
}

/// An object that is capable of listing methods on a component or opaque value.
///
/// Because only one method listing can be in progress at a time, only one value of this type can
/// exist. An application written as a state machine should take the instance and store it in a
/// `static` variable. An application using `async` and `await` in which only one task needs to
/// list methods should either take the value in that task, or take it in the top-level task and
/// move it into the task that needs it. An application using `async` and `await` in which multiple
/// tasks all need to list methods needs to arrange mutual exclusion so that only one task can
/// access the lister at a time.
pub struct MethodLister(());

impl MethodLister {
	/// Returns the lister.
	///
	/// This function can only be called once in the lifetime of the application. On the second and
	/// subsequent calls, it will return `None`.
	#[must_use = "A Lister can only be taken once. It needs to be saved. Discarding it means it is impossible to ever list methods."]
	pub fn take() -> Option<Self> {
		static mut INSTANCE: Option<MethodLister> = Some(Self(()));
		// SAFETY: Wasm doesn’t have threads, so only one caller can get here at a time, and the
		// Option will be empty for all but the first caller.
		unsafe { INSTANCE.take() }
	}

	/// Begins iteration over the methods available on a component.
	///
	/// The `address` parameter identifies the component by its UUID.
	///
	/// # Errors
	/// * [`NoSuchComponent`](Error::NoSuchComponent) is returned if the component does not exist
	///   or is inaccessible.
	#[allow(clippy::module_name_repetitions)] // Important to distinguish from methods_start_value
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn start_component<'lister>(
		&'lister mut self,
		address: &Address,
	) -> Result<MethodListing<'lister>> {
		let address = address.as_bytes();
		// SAFETY: methods_start_component permits an input UUID pointer.
		Error::from_i32(unsafe { sys::methods_start_component(address.as_ptr()) })?;
		Ok(MethodListing(PhantomData))
	}

	/// Begins iteration over the methods available on an opaque value.
	///
	/// The `descriptor` parameter identifies the opaque value by its descriptor.
	///
	/// Iteration over methods is not reentrant. Concurrent software must ensure that only one method
	/// iteration at a time is attempted. This is even true if different components are involved, or if
	/// one is over a component and the other over an opaque value.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn start_value(&mut self, descriptor: &impl AsDescriptor) -> MethodListing<'_> {
		// SAFETY: methods_start_value permits a descriptor.
		// SOUNDNESS: Ignoring the return value is sound because methods_start_value can only fail
		// due to BadDescriptor, and BadDescriptor cannot happen because the parameter is a
		// Descriptor object.
		unsafe { sys::methods_start_value(descriptor.as_descriptor().as_raw()) };
		MethodListing(PhantomData)
	}
}

/// The possible attributes of a method.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct MethodAttributes {
	/// The method is direct.
	///
	/// If this value is `true`, the method can be called and completed within a single timeslice.
	/// The caller cannot assume that every invocation of a direct method will complete within a
	/// timeslice, as even direct calls may have call budget limits; however, if this value is
	/// `false`, then no call to the method will ever complete immediately.
	pub direct: bool,

	/// The method is a property getter.
	///
	/// If this value is `true`, the method conceptually reads the value of a property, rather than
	/// performing an action.
	pub getter: bool,

	/// The method is a property setter.
	///
	/// If this value is `true`, the method conceptually writes the value of a property, rather
	/// than performing an action.
	pub setter: bool,
}

impl From<u32> for MethodAttributes {
	fn from(value: u32) -> Self {
		Self {
			direct: (value & 1) != 0,
			getter: (value & 2) != 0,
			setter: (value & 4) != 0,
		}
	}
}

#[must_use = "Starting a method listing is only useful if you read the results."]
pub struct MethodListing<'lister>(PhantomData<&'lister mut MethodLister>);

impl MethodListing<'_> {
	/// Returns the length, in bytes, of the name of the next method in the list of methods.
	///
	/// If there is no next entry, `None` is returned.
	///
	/// # Panics
	/// This function panics if the underlying syscall fails, because the only reasons it could
	/// fail should be impossible due to the type system.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	#[must_use = "This function is only useful for its return value"]
	pub fn next_len(&self) -> Option<NonZeroUsize> {
		let result = Error::from_isize(
			// SAFETY: methods_next permits null for both pointers.
			unsafe { sys::methods_next(ptr::null_mut(), 0, ptr::null_mut()) },
		);
		// Can’t fail because methods_next can only fail due to MemoryFault or StringDecode, and
		// Error::from_isize already treats those as unreachable.
		NonZeroUsize::new(result.unwrap_or_else(|_| panic_or_trap!("unreachable")))
	}

	/// Returns the next method in the list of methods.
	///
	/// The `buffer` parameter identifies where to store the next method name.
	///
	/// If there is a next method, its name is written to `buffer`, a string slice referring to the
	/// name along with the attributes are returned, and the iteration is advanced. If not, `None`
	/// is returned.
	///
	/// # Errors
	/// * [`BufferTooShort`](Error::BufferTooShort) is returned if `buffer` is not large enough to
	///   hold the method name.
	///
	/// On error, the iteration does not advance.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn next<'buffer>(
		&mut self,
		buffer: &'buffer mut [u8],
	) -> Result<Option<(&'buffer mut str, MethodAttributes)>> {
		let mut attributes = 0_u32;
		let bytes_written = {
			let len = buffer.len();
			let ptr = buffer.as_mut_ptr();
			Error::from_isize(
				// SAFETY: methods_next permits a writeable buffer pointer/length pair and a pointer to
				// a single i32 attributes bitmask.
				unsafe { sys::methods_next(ptr, len, &mut attributes) },
			)?
		};
		if bytes_written == 0 {
			Ok(None)
		} else {
			Ok(Some((
				// SAFETY: methods_next promises to always write a valid UTF-8 string and return its
				// length.
				unsafe {
					core::str::from_utf8_unchecked_mut(buffer.get_unchecked_mut(0..bytes_written))
				},
				attributes.into(),
			)))
		}
	}
}

/// Returns the length, in bytes, of the documentation for a method on a component.
///
/// The `address` parameter identifies the component by its UUID. The `method` parameter identifies
/// the method by its name.
///
/// # Errors
/// * [`NoSuchComponent`](Error::NoSuchComponent) is returned if the component does not exist or is
///   inaccessible.
/// * [`NoSuchMethod`](Error::NoSuchMethod) is returned if the method does not exist on the
///   component.
#[must_use = "This function is only useful for its return value"]
pub fn documentation_component_length(address: &Address, method: &str) -> Result<usize> {
	let address = address.as_bytes();
	Error::from_isize(
		// SAFETY: documentation_component permits an input address UUID pointer and method name
		// string pointer/length pair, and a null output pointer/length pair.
		unsafe {
			sys::documentation_component(
				address.as_ptr(),
				method.as_ptr(),
				method.len(),
				ptr::null_mut(),
				0,
			)
		},
	)
}

/// Returns the documentation for a method on a component.
///
/// The `address` parameter identifies the component by its UUID. The `method` parameter identifies
/// the method by its name. The `buffer` parameter identifies where to store the documentation.
///
/// The documentation is written into `buffer` and a string slice referring to it is returned.
///
/// # Errors
/// * [`BufferTooShort`](Error::BufferTooShort) is returned if `buffer` is provided but is not
///   large enough to hold the documentation.
/// * [`NoSuchComponent`](Error::NoSuchComponent) is returned if the component does not exist or is
///   inaccessible.
/// * [`NoSuchMethod`](Error::NoSuchMethod) is returned if the method does not exist on the
///   component.
#[allow(clippy::module_name_repetitions)] // Important to distinguish from documentation_value
#[must_use = "This function is only useful for its return value"]
pub fn documentation_component<'buf>(
	address: &Address,
	method: &str,
	buffer: &'buf mut [u8],
) -> Result<&'buf mut str> {
	let address = address.as_bytes();
	let bytes_written = {
		let buf_len = buffer.len();
		let buf_ptr = buffer.as_mut_ptr();
		Error::from_isize(
			// SAFETY: documentation_component permits an input address UUID pointer and method name
			// string pointer/length pair, and a writeable buffer output pointer/length pair.
			unsafe {
				sys::documentation_component(
					address.as_ptr(),
					method.as_ptr(),
					method.len(),
					buf_ptr,
					buf_len,
				)
			},
		)?
	};
	Ok(
		// SAFETY: documentation_component promises to always write a valid UTF-8 string and return
		// its length.
		unsafe { core::str::from_utf8_unchecked_mut(buffer.get_unchecked_mut(0..bytes_written)) },
	)
}

/// Returns the length, in bytes, of the documentation for a method on a value.
///
/// The `descriptor` parameter identifies the value by its descriptor. The `method` parameter
/// identifies the method by its name.
///
/// # Errors
/// * [`NoSuchMethod`](Error::NoSuchMethod) is returned if the method does not exist on the
///   value.
#[must_use = "This function is only useful for its return value"]
pub fn documentation_value_length(descriptor: &impl AsDescriptor, method: &str) -> Result<usize> {
	Error::from_isize(
		// SAFETY: documentation_value permits two string pointer/length pairs and a null.
		unsafe {
			sys::documentation_value(
				descriptor.as_descriptor().as_raw(),
				method.as_ptr(),
				method.len(),
				ptr::null_mut(),
				0,
			)
		},
	)
}

/// Returns the documentation for a method on a value.
///
/// The `descriptor` parameter identifies the value by its descriptor. The `method` parameter
/// identifies the method by its name. The `buffer` parameter identifies where to store the
/// documentation.
///
/// The documentation is written into `buffer` and a string slice referring to it is returned.
///
/// # Errors
/// * [`BufferTooShort`](Error::BufferTooShort) is returned if `buffer` is provided but is not
///   large enough to hold the documentation.
/// * [`NoSuchMethod`](Error::NoSuchMethod) is returned if the method does not exist on the value.
#[must_use = "This function is only useful for its return value"]
pub fn documentation_value<'buf>(
	descriptor: &impl AsDescriptor,
	method: &str,
	buffer: &'buf mut [u8],
) -> Result<&'buf mut str> {
	let bytes_written = {
		let buf_len = buffer.len();
		let buf_ptr = buffer.as_mut_ptr();
		Error::from_isize(
			// SAFETY: documentation_value permits two string pointer/length pairs and a writeable
			// buffer pointer/length pair.
			unsafe {
				sys::documentation_value(
					descriptor.as_descriptor().as_raw(),
					method.as_ptr(),
					method.len(),
					buf_ptr,
					buf_len,
				)
			},
		)?
	};
	Ok(
		// SAFETY: documentation_value promises to always write a valid UTF-8 string and return its
		// length.
		unsafe { core::str::from_utf8_unchecked_mut(buffer.get_unchecked_mut(0..bytes_written)) },
	)
}

/// An object that is capable of invoking methods.
///
/// Because only one method can be invoked at a time, only one value of this type can exist. An
/// application written as a state machine should take the instance and store it in a `static`
/// variable. An application using `async` and `await` in which only one task needs to make method
/// calls should either take the value in that task, or take it in the top-level task and move it
/// into the task that needs it. An application using `async` and `await` in which multiple tasks
/// all need to make method calls needs to arrange mutual exclusion so that only one task can
/// access the invoker at a time.
pub struct Invoker(());

impl Invoker {
	/// Returns the invoker.
	///
	/// This function can only be called once in the lifetime of the application. On the second and
	/// subsequent calls, it will return `None`.
	#[must_use = "An Invoker can only be taken once. It needs to be saved. Discarding it means it is impossible to ever make a method call."]
	pub fn take() -> Option<Self> {
		static mut INSTANCE: Option<Invoker> = Some(Self(()));
		// SAFETY: Wasm doesn’t have threads, so only one caller can get here at a time, and the
		// Option will be empty for all but the first caller.
		unsafe { INSTANCE.take() }
	}

	/// Starts invoking a method on a component.
	///
	/// The `address` parameter identifies the component by its UUID. The `method` parameter
	/// identifies the method by its name. The `params` parameter, if present, contains a
	/// CBOR-encoded array of parameters to pass to the method.
	///
	/// # Errors
	/// * [`CborDecode`](Error::CborDecode) is returned if the `params` parameter is present but
	///   contains an invalid or unsupported CBOR sequence.
	/// * [`BadDescriptor`](Error::BadDescriptor) is returned if the parameters contain a
	///   descriptor reference to a descriptor that is not open.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors) is returned if the descriptor table is
	///   too full and some descriptors must be closed before another method call can be made.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn component_method<'invoker>(
		&'invoker mut self,
		address: &Address,
		method: &str,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>)> {
		let address = address.as_bytes();
		let params_ptr = params.map_or(ptr::null(), <[u8]>::as_ptr);
		let done = Error::from_i32(
			// SAFETY: invoke_component_method permits an input UUID pointer, method name
			// pointer/length pair, and CBOR pointer which may be null.
			unsafe {
				sys::invoke_component_method(
					address.as_ptr(),
					method.as_ptr(),
					method.len(),
					params_ptr,
				)
			},
		)? != 0;
		Ok((
			if done {
				InvokeResult::Complete
			} else {
				InvokeResult::Incomplete
			},
			MethodCall(PhantomData),
		))
	}

	/// Starts invoking a callable opaque value.
	///
	/// The `descriptor` parameter identifies the opaque value by its descriptor. The `params`
	/// parameter, if present, contains a CBOR-encoded array of parameters to pass to the method.
	///
	/// # Errors
	/// * [`CborDecode`](Error::CborDecode) is returned if the `params` parameter is present but
	///   contains an invalid or unsupported CBOR sequence.
	/// * [`BadDescriptor`](Error::BadDescriptor) is returned if the parameters contain a
	///   descriptor reference to a descriptor that is not open.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors) is returned if the descriptor table is
	///   too full and some descriptors must be closed before another method call can be made.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn value<'invoker>(
		&'invoker mut self,
		descriptor: &impl AsDescriptor,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>)> {
		let params_ptr = params.map_or(ptr::null(), <[u8]>::as_ptr);
		let done = Error::from_i32(
			// SAFETY: invoke_value permits any descriptor and a CBOR pointer which may be null.
			unsafe { sys::invoke_value(descriptor.as_descriptor().as_raw(), params_ptr) },
		)? != 0;
		Ok((
			if done {
				InvokeResult::Complete
			} else {
				InvokeResult::Incomplete
			},
			MethodCall(PhantomData),
		))
	}

	/// Starts reading from an index of an opaque value.
	///
	/// The `descriptor` parameter identifies the opaque value by its descriptor. The `params`
	/// parameter, if present, contains a CBOR-encoded array of parameters to use for indexing.
	///
	/// # Errors
	/// * [`CborDecode`](Error::CborDecode) is returned if the `params` parameter is present but
	///   contains an invalid or unsupported CBOR sequence.
	/// * [`BadDescriptor`](Error::BadDescriptor) is returned if the parameters contain a
	///   descriptor reference to a descriptor that is not open.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors) is returned if the descriptor table is
	///   too full and some descriptors must be closed before another method call can be made.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn value_indexed_read<'invoker>(
		&'invoker mut self,
		descriptor: &impl AsDescriptor,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>)> {
		let params_ptr = params.map_or(ptr::null(), <[u8]>::as_ptr);
		let done = Error::from_i32(
			// SAFETY: invoke_value_indexed_read permits any descriptor and a CBOR pointer which
			// may be null.
			unsafe {
				sys::invoke_value_indexed_read(descriptor.as_descriptor().as_raw(), params_ptr)
			},
		)? != 0;
		Ok((
			if done {
				InvokeResult::Complete
			} else {
				InvokeResult::Incomplete
			},
			MethodCall(PhantomData),
		))
	}

	/// Starts writing to an index of an opaque value.
	///
	/// The `descriptor` parameter identifies the opaque value by its descriptor. The `params`
	/// parameter, if present, contains a CBOR-encoded array of parameters to use for indexing and
	/// the value to write.
	///
	/// # Errors
	/// * [`CborDecode`](Error::CborDecode) is returned if the `params` parameter is present but
	///   contains an invalid or unsupported CBOR sequence.
	/// * [`BadDescriptor`](Error::BadDescriptor) is returned if the parameters contain a
	///   descriptor reference to a descriptor that is not open.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors) is returned if the descriptor table is
	///   too full and some descriptors must be closed before another method call can be made.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn value_indexed_write<'invoker>(
		&'invoker mut self,
		descriptor: &impl AsDescriptor,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>)> {
		let params_ptr = params.map_or(ptr::null(), <[u8]>::as_ptr);
		let done = Error::from_i32(
			// SAFETY: invoke_value_indexed_write permits any descriptor and a CBOR pointer which
			// may be null.
			unsafe {
				sys::invoke_value_indexed_write(descriptor.as_descriptor().as_raw(), params_ptr)
			},
		)? != 0;
		Ok((
			if done {
				InvokeResult::Complete
			} else {
				InvokeResult::Incomplete
			},
			MethodCall(PhantomData),
		))
	}

	/// Starts invoking a method on an opaque value.
	///
	/// The `descriptor` parameter identifies the opaque value by its descriptor. The `method`
	/// parameter identifies the method by its name. The `params` parameter, if present, contains a
	/// CBOR-encoded array of parameters to pass to the method.
	///
	/// # Errors
	/// * [`CborDecode`](Error::CborDecode) is returned if the `params` parameter is present but
	///   contains an invalid or unsupported CBOR sequence.
	/// * [`BadDescriptor`](Error::BadDescriptor) is returned if the parameters contain a
	///   descriptor reference to a descriptor that is not open.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors) is returned if the descriptor table is
	///   too full and some descriptors must be closed before another method call can be made.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn value_method<'invoker>(
		&'invoker mut self,
		descriptor: &impl AsDescriptor,
		method: &str,
		params: Option<&[u8]>,
	) -> Result<(InvokeResult, MethodCall<'invoker>)> {
		let params_ptr = params.map_or(ptr::null(), <[u8]>::as_ptr);
		let done = Error::from_i32(
			// SAFETY: invoke_value_method permits any descriptor, a string pointer/length pair, and a
			// CBOR pointer which may be null.
			unsafe {
				sys::invoke_value_method(
					descriptor.as_descriptor().as_raw(),
					method.as_ptr(),
					method.len(),
					params_ptr,
				)
			},
		)? != 0;
		Ok((
			if done {
				InvokeResult::Complete
			} else {
				InvokeResult::Incomplete
			},
			MethodCall(PhantomData),
		))
	}
}

impl core::fmt::Debug for Invoker {
	fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
		write!(f, "Invoker")
	}
}

impl PartialEq for Invoker {
	fn eq(&self, _: &Self) -> bool {
		// Only one invoker can ever exist, therefore all invokers that exist are equal.
		true
	}
}

impl Eq for Invoker {}

/// The possible results of a successful start to a method call.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum InvokeResult {
	/// The method is complete and its result can be fetched immediately.
	Complete,

	/// The method is not finished yet; its result can be fetched on the next timeslice.
	Incomplete,
}

/// An in-progress method call.
///
/// The `'invoker` lifetime parameter is the lifetime of the method invoker that is performing the
/// call.
///
/// If a value of this type is dropped, the method call is cancelled. If it has not executed yet,
/// it will not execute; if it has already executed, its result is discarded.
#[derive(Debug, Eq, PartialEq)]
#[must_use = "Discarding a MethodCall immediately is buggy. Even if you know the method you are calling is direct and don’t need its return value, direct methods must be run indirectly if the method call cost limit is reached, so you still need to make sure it finishes."]
pub struct MethodCall<'invoker>(PhantomData<&'invoker mut Invoker>);

impl<'invoker> MethodCall<'invoker> {
	/// Returns the length, in bytes, of the result of the method call, or an indication that the
	/// call is not finished.
	///
	/// On success, the length and the `MethodCall` are returned, allowing the `MethodCall` to be
	/// reused to fetch the actual bytes.
	///
	/// # Errors
	/// * [`NoSuchComponent`](Error::NoSuchComponent) is returned if the method call failed because
	///   the component does not exist or is inaccessible.
	/// * [`NoSuchMethod`](Error::NoSuchMethod) is returned if the method call failed because the
	///   method does not exist on the component.
	/// * [`BadParameters`](Error::BadParameters) is returned if the parameters provided when
	///   starting the call are not acceptable for the method.
	/// * [`Other`](Error::Other) is returned if the method call failed.
	#[must_use = "This function is only useful for its return value"]
	pub fn end_length(self) -> InvokeEndLengthResult<'invoker> {
		// SAFETY: invoke_end permits null.
		let ret = unsafe { sys::invoke_end(ptr::null_mut(), 0) };
		match MethodCallError::from_isize(PhantomData, ret) {
			Ok(n) => InvokeEndLengthResult::Done(Ok((n, self))),
			Err(MethodCallError::QueueEmpty) => InvokeEndLengthResult::Pending(self),
			Err(e) => InvokeEndLengthResult::Done(Err(e)),
		}
	}

	/// Returns the result of the method call as a CBOR-encoded data item, or an indication that
	/// the call is not finished.
	///
	/// On success, the result is written into up to `len` bytes pointed to by `buffer`, and the
	/// number of bytes written is returned. If the buffer is not large enough to hold the call
	/// result, [`BufferTooShort`](InvokeEndResult::BufferTooShort) is returned, containing the
	/// `MethodCall` object, allowing the caller to retry fetching the results with a larger
	/// buffer, or call [`end_length`](MethodCall::end_length) to obtain the needed buffer size.
	///
	/// # Errors
	/// * [`NoSuchComponent`](Error::NoSuchComponent) is returned if the method call failed because the
	///   component does not exist or is inaccessible.
	/// * [`NoSuchMethod`](Error::NoSuchMethod) is returned if the method call failed because the
	///   method does not exist on the component.
	/// * [`BadParameters`](Error::BadParameters) is returned if the parameters provided when
	///   starting the call are not acceptable for the method.
	/// * [`Other`](Error::Other) is returned if the method call failed.
	///
	/// # Safety
	/// The caller must ensure that `len` bytes pointed to by `buffer` are writeable.
	pub unsafe fn end_ptr(self, buffer: *mut u8, len: usize) -> InvokeEndResult<'invoker> {
		let result = sys::invoke_end(buffer, len);
		match MethodCallError::from_isize(PhantomData, result) {
			Err(MethodCallError::BufferTooShort) => InvokeEndResult::BufferTooShort(self),
			Err(MethodCallError::QueueEmpty) => InvokeEndResult::Pending(self),
			other => InvokeEndResult::Done(other),
		}
	}

	/// Returns the result of the method call as a CBOR-encoded data item, or an indication that
	/// the call is not finished.
	///
	/// On success, the result is written into `buffer`, and the number of bytes written is
	/// returned. If the buffer is not large enough to hold the call result,
	/// [`BufferTooShort`](InvokeEndResult::BufferTooShort) is returned, containing the
	/// `MethodCall` object, allowing the caller to retry fetching the results with a larger
	/// buffer, or call [`end_length`](MethodCall::end_length) to obtain the needed buffer size.
	///
	/// # Errors
	/// * [`NoSuchComponent`](Error::NoSuchComponent) is returned if the method call failed because the
	///   component does not exist or is inaccessible.
	/// * [`NoSuchMethod`](Error::NoSuchMethod) is returned if the method call failed because the
	///   method does not exist on the component.
	/// * [`BadParameters`](Error::BadParameters) is returned if the parameters provided when
	///   starting the call are not acceptable for the method.
	/// * [`Other`](Error::Other) is returned if the method call failed.
	pub fn end(self, buffer: &mut [u8]) -> InvokeEndResult<'invoker> {
		// SAFETY: invoke_end permits a writeable buffer pointer/length pair and promises to always
		// return the length of data it wrote.
		let result = unsafe { sys::invoke_end(buffer.as_mut_ptr(), buffer.len()) };
		match MethodCallError::from_isize(PhantomData, result) {
			Err(MethodCallError::BufferTooShort) => InvokeEndResult::BufferTooShort(self),
			Err(MethodCallError::QueueEmpty) => InvokeEndResult::Pending(self),
			other => InvokeEndResult::Done(other),
		}
	}
}

impl Drop for MethodCall<'_> {
	fn drop(&mut self) {
		// SAFETY: invoke_cancel is unconditionally safe.
		unsafe { sys::invoke_cancel() }
	}
}

/// An object that is able to retrieve detailed method call error information.
///
/// Certain errors, when returned from a method call (and only from a method call!), are
/// accompanied by additional detailed error information which is only available until the next
/// method call. A value of this type uses the `'invoker` lifetime parameter to prevent additional
/// method calls from being made until the caller has finished examining the detailed error
/// information.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct LastException<'invoker>(PhantomData<&'invoker mut Invoker>);

impl<'invoker> LastException<'invoker> {
	/// Creates a new `LastException`.
	fn new(_: PhantomData<&'invoker mut Invoker>) -> Self {
		Self(PhantomData)
	}

	/// Returns the length of the human-readable message for the error.
	///
	/// # Panics
	/// This function panics if the underlying syscall fails, because the only reasons it could
	/// fail should be impossible due to the type system.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	#[must_use = "This function is only useful for its return value"]
	pub fn message_length(&self) -> usize {
		// SAFETY: last_exception_message permits null.
		let result = unsafe { call_buffer_len(sys::last_exception_message) };
		result.unwrap_or_else(|_| panic_or_trap!("unreachable"))
	}

	/// Returns the human-readable message for the error.
	///
	/// The message is written into `buffer`, and a string slice over the written text is returned.
	///
	/// # Errors
	/// * [`BufferTooShort`](Error::BufferTooShort) is returned if `buffer` is not long enough to
	///   hold the error message.
	///
	/// # Panics
	/// This function panics if the underlying syscall fails for any reason other than
	/// [`BufferTooShort`](Error::BufferTooShort), because the only other reasons it could fail
	/// should be impossible due to the type system.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	pub fn message<'buf>(&self, buffer: &'buf mut [u8]) -> Result<&'buf str> {
		// SAFETY: last_exception permits a writeable buffer output pointer/length pair and
		// promises to always write a valid UTF-8 string and return its length.
		let result = unsafe { call_buffer_str(sys::last_exception_message, buffer) };
		match result {
			Ok(message) => Ok(message),
			Err(Error::BufferTooShort) => Err(Error::BufferTooShort),
			Err(_) => panic_or_trap!("unreachable"),
		}
	}

	/// Checks whether the Java exception underlying the error is of a certain type.
	///
	/// The `class` parameter must be the fully qualified name of a Java class (e.g.
	/// `java.io.IOException`). This function returns `true` if the exception that caused the error
	/// is `class` or a subclass thereof, or `false` if not.
	///
	/// # Panics
	/// This function panics if the underlying syscall fails, because the only reasons it could
	/// fail should be impossible due to the type system.
	#[allow(clippy::unused_self)] // Not used for its value, but used for its lifetime.
	#[must_use = "This function is only useful for its return value"]
	pub fn is_type(&self, class: &str) -> bool {
		// SAFETY: is_type permits a string pointer/length pair.
		let result = unsafe { call_string(sys::last_exception_is_type, Some(class)) };
		result.unwrap_or_else(|_| panic_or_trap!("unreachable")) != 0
	}
}

/// The possible errors that could occur during a method call.
///
/// Some errors carry additional information. This information is only available until the start of
/// the next method call. The `'invoker` lifetime ensures that the invoker cannot be reused to
/// start another method call until a value of this type is dropped and therefore the additional
/// error information is no longer accessible.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum MethodCallError<'invoker> {
	CborDecode,
	BufferTooShort,
	NoSuchComponent,
	NoSuchMethod,
	BadParameters(LastException<'invoker>),
	QueueFull,
	QueueEmpty,
	BadDescriptor,
	TooManyDescriptors,
	Other(LastException<'invoker>),
	Unknown,
}

impl<'invoker> MethodCallError<'invoker> {
	/// Checks a system call return value of type `isize` for an error value.
	///
	/// Returns a `Result` containing a `MethodCallError` if the value is negative, or the
	/// original value if it was nonnegative.
	///
	/// # Errors
	/// This function fails if the parameter is negative, decoding the represented error code.
	///
	/// # Panics
	/// This function panics if the syscall error code is `MemoryFault` or `StringDecode`. These
	/// syscall errors should be impossible in safe code because the type system prohibits them:
	/// `MemoryFault` should be impossible because all memory regions are taken as slices which are
	/// always valid, and `StringDecode` should be impossible because all strings are taken as
	/// string-slices (`&str`) which are always valid UTF-8.
	fn from_isize(
		_: PhantomData<&'invoker mut Invoker>,
		value: isize,
	) -> core::result::Result<usize, MethodCallError<'invoker>> {
		match value {
			-1 => panic_or_trap!("Memory fault"), // Impossible due to memory safety
			-2 => Err(Self::CborDecode),
			-3 => panic_or_trap!("String decode error"), // Impossible due to type safety of &str
			-4 => Err(Self::BufferTooShort),
			-5 => Err(Self::NoSuchComponent),
			-6 => Err(Self::NoSuchMethod),
			-7 => Err(Self::BadParameters(LastException::new(PhantomData))),
			-8 => Err(Self::QueueFull),
			-9 => Err(Self::QueueEmpty),
			-10 => Err(Self::BadDescriptor),
			-11 => Err(Self::TooManyDescriptors),
			-12 => Err(Self::Other(LastException::new(PhantomData))),
			x if x < 0 => Err(Self::Unknown),
			_ => {
				// Cast from isize to usize is safe because the match arm verifies that x ≥ 0.
				#[allow(clippy::cast_sign_loss)]
				Ok(value as usize)
			}
		}
	}

	/// Returns a string describing the error.
	#[must_use = "This function is only useful for its return value"]
	pub fn as_str(self) -> &'static str {
		self.simplify().as_str()
	}

	/// Throws away the additional error information.
	#[must_use = "This function is only useful for its return value"]
	pub fn simplify(self) -> Error {
		match self {
			Self::CborDecode => Error::CborDecode,
			Self::BufferTooShort => Error::BufferTooShort,
			Self::NoSuchComponent => Error::NoSuchComponent,
			Self::NoSuchMethod => Error::NoSuchMethod,
			Self::BadParameters(_) => Error::BadParameters,
			Self::QueueFull => Error::QueueFull,
			Self::QueueEmpty => Error::QueueEmpty,
			Self::BadDescriptor => Error::BadDescriptor,
			Self::TooManyDescriptors => Error::TooManyDescriptors,
			Self::Other(_) => Error::Other,
			Self::Unknown => Error::Unknown,
		}
	}
}

impl Display for MethodCallError<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
		self.simplify().fmt(f)
	}
}

impl From<MethodCallError<'_>> for Error {
	fn from(source: MethodCallError<'_>) -> Self {
		source.simplify()
	}
}

impl TryFrom<Error> for MethodCallError<'static> {
	type Error = ();

	fn try_from(source: Error) -> core::result::Result<Self, Self::Error> {
		match source {
			Error::CborDecode => Ok(Self::CborDecode),
			Error::BufferTooShort => Ok(Self::BufferTooShort),
			Error::NoSuchComponent => Ok(Self::NoSuchComponent),
			Error::NoSuchMethod => Ok(Self::NoSuchMethod),
			Error::QueueFull => Ok(Self::QueueFull),
			Error::QueueEmpty => Ok(Self::QueueEmpty),
			Error::BadDescriptor => Ok(Self::BadDescriptor),
			Error::TooManyDescriptors => Ok(Self::TooManyDescriptors),
			Error::Unknown => Ok(Self::Unknown),
			Error::BadParameters | Error::Other => Err(()),
		}
	}
}

#[cfg(feature = "std")]
impl std::error::Error for MethodCallError<'_> {}

/// The result of a call to [`end_length`](MethodCall::end_length).
///
/// The `'invoker` lifetime parameter is the lifetime of the method invoker that is performing the
/// call.
#[derive(Debug, Eq, PartialEq)]
pub enum InvokeEndLengthResult<'invoker> {
	/// The method call is complete. If the method call completed successfully, the `Result` value
	/// contains both the length of the call result, in bytes, and the [`MethodCall`](MethodCall)
	/// value that can be used to fetch the result if desired. If the method call failed, the
	/// `Result` value contains the error.
	Done(core::result::Result<(usize, MethodCall<'invoker>), MethodCallError<'invoker>>),

	/// The method call is not finished yet. The [`MethodCall`](MethodCall) value is returned so
	/// the caller can continue to monitor progress.
	Pending(MethodCall<'invoker>),
}

/// The result of a call to [`end`](MethodCall::end).
///
/// The `'invoker` lifetime parameter is the lifetime of the method invoker that is performing the
/// call.
#[derive(Debug, Eq, PartialEq)]
pub enum InvokeEndResult<'invoker> {
	/// The method call is complete and the result has been fetched. If the method call completed
	/// successfully, the `Result` value contains the CBOR-encoded result. If the method call
	/// failed, the `Result` value contains an error.
	Done(core::result::Result<usize, MethodCallError<'invoker>>),

	/// The method call is complete but the provided buffer was too short to hold the result. The
	/// [`MethodCall`](MethodCall) value is returned so the caller can retry with a larger buffer.
	BufferTooShort(MethodCall<'invoker>),

	/// The method call is not finished yet. The [`MethodCall`](MethodCall) value is returned so
	/// the caller can continue to monitor progress.
	Pending(MethodCall<'invoker>),
}

impl<'invoker> InvokeEndResult<'invoker> {
	/// Unwraps an `InvokeEndResult`, assuming that the caller already knows that the result is
	/// `Done`. This function is useful if the caller knows that the method call is complete and
	/// that the buffer is large enough to hold any possible return value, or if the caller is not
	/// in a position to handle large return values anyway.
	///
	/// # Errors
	/// * [`BufferTooShort`](MethodCallError::BufferTooShort) is returned if the result was
	///   actually `BufferTooShort`.
	/// * [`QueueEmpty`](MethodCallError::QueueEmpty) is returned if the result was actually
	///   `QueueEmpty`.
	///
	/// In case of any error, because the [`MethodCall`](MethodCall) is consumed, the method call
	/// is cancelled.
	#[must_use = "This function is only useful for its return value"]
	pub fn expect_done(self) -> core::result::Result<usize, MethodCallError<'invoker>> {
		match self {
			Self::Done(result) => result,
			Self::BufferTooShort(_) => Err(MethodCallError::BufferTooShort),
			Self::Pending(_) => Err(MethodCallError::QueueEmpty),
		}
	}
}
