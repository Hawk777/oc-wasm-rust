//! A collection of helper functions and types used by multiple modules in this crate but not
//! exported for public use.

use crate::error::Error;
use minicbor::data::Type;
use minicbor::decode::Decoder;
use minicbor::Decode;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_safe::{component::Invoker, Address};

/// A value which, when CBOR-decoded, does not consume any data items and always successfully does
/// nothing.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Ignore();

impl<'buffer, Context> Decode<'buffer, Context> for Ignore {
	fn decode(_: &mut Decoder<'buffer>, _: &mut Context) -> Result<Self, minicbor::decode::Error> {
		Ok(Self())
	}
}

/// A successful method call result, or an error value decoded from an array of return values whose
/// first entry is null and whose second entry is a string message.
///
/// This is a common convention used by various OpenComputers component APIs to report some (but
/// not all) errors.
///
/// The `'buffer` parameter is the lifetime of the buffer from which the error string is decoded.
/// The `T` parameter is the type to decode if the call was successful.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum NullAndStringOr<'buffer, T> {
	Ok(T),
	Err(&'buffer str),
}

impl<'buffer, Context, T: Decode<'buffer, Context>> Decode<'buffer, Context>
	for NullAndStringOr<'buffer, T>
{
	fn decode(
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
		let mut p = d.probe();
		let is_error = if let Some(length) = p.array()? {
			if length == 2 {
				p.datatype()? == Type::Null
			} else {
				false
			}
		} else {
			false
		};
		if is_error {
			d.array()?;
			d.skip()?;
			Ok(Self::Err(d.str()?))
		} else {
			Ok(Self::Ok(T::decode(d, context)?))
		}
	}
}

/// Returns the larger of two `usize` values in a `const` context.
pub const fn max_usize(x: usize, y: usize) -> usize {
	if x > y {
		x
	} else {
		y
	}
}

/// Enables or disables computer control of a machine.
///
/// If `enable` is `true`, the machine runs or stops based on the most recent call to
/// [`set_enabled`](#set_enabled). If `enable` is `false`, the machine runs or stops based on the
/// redstone signal at the control port.
///
/// # Errors
/// * [`BadComponent`](Error::BadComponent)
/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
pub async fn enable_computer_control(
	address: &Address,
	invoker: &mut Invoker,
	buffer: &mut impl Buffer,
	enable: bool,
) -> Result<(), Error> {
	component_method(
		invoker,
		buffer,
		address,
		"enableComputerControl",
		Some(&(enable,)),
	)
	.await?;
	Ok(())
}

/// Enables or disables a machine.
///
/// This can only be called if the machine is under computer control via a preceding call to
/// [`enable_computer_control`](#enable_computer_control).
///
/// # Errors
/// * [`BadComponent`](Error::BadComponent)
/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
pub async fn set_enabled(
	address: &Address,
	invoker: &mut Invoker,
	buffer: &mut impl Buffer,
	enable: bool,
) -> Result<(), Error> {
	const NOT_COMPUTER_CONTROLLED: &str =
		"Computer control must be enabled to enable or disable the machine";
	let ret: Result<(), oc_wasm_safe::component::MethodCallError<'_>> =
		component_method(invoker, buffer, address, "setEnabled", Some(&(enable,))).await;
	match ret {
		Ok(()) => Ok(()),
		Err(e @ oc_wasm_safe::component::MethodCallError::Other(exp)) => {
			if exp.is_type("java.lang.IllegalStateException") {
				let mut message_buffer = [0_u8; NOT_COMPUTER_CONTROLLED.len()];
				if exp.message(&mut message_buffer) == Ok(NOT_COMPUTER_CONTROLLED) {
					Err(Error::NotComputerControlled)
				} else {
					Err(e.into())
				}
			} else {
				Err(e.into())
			}
		}
		Err(e) => Err(e.into()),
	}
}
