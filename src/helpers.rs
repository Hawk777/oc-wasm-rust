//! A collection of helper functions and types used by multiple modules in this crate but not
//! exported for public use.

use minicbor::data::Type;
use minicbor::decode::Decoder;
use minicbor::{Decode, Encode};

/// A value which, when CBOR-decoded, does not consume any data items and always successfully does
/// nothing.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Ignore();

impl<'buffer> Decode<'buffer> for Ignore {
	fn decode(_: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
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

impl<'buffer, T: Decode<'buffer>> Decode<'buffer> for NullAndStringOr<'buffer, T> {
	fn decode(d: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
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
			Ok(Self::Ok(T::decode(d)?))
		}
	}
}

/// A single value, usable as the parameter list for method calls that take one parameter or the
/// return value for method calls that return one value.
#[derive(Decode, Encode)]
#[cbor(array)]
pub struct OneValue<T>(#[b(0)] pub T);

/// A pair of values, usable as the parameter list for method calls that take two parameters or the
/// return value for method calls that return two values.
#[derive(Decode, Encode)]
#[cbor(array)]
pub struct TwoValues<T, U>(#[b(0)] pub T, #[b(1)] pub U);

/// A triple of values, usable as the parameter list for method calls that take three parameters or
/// the return value for method calls that return three values.
#[derive(Decode, Encode)]
#[cbor(array)]
pub struct ThreeValues<T, U, V>(#[b(0)] pub T, #[b(1)] pub U, #[b(2)] pub V);

/// A quadruple of values, usable as the parameter list for method calls that take four parameters
/// or the return value for method calls that return four values.
#[derive(Decode, Encode)]
#[cbor(array)]
pub struct FourValues<T, U, V, W>(#[b(0)] pub T, #[b(1)] pub U, #[b(2)] pub V, #[b(3)] pub W);

/// A quintuple of values, usable as the parameter list for method calls that take five parameters
/// or the return value for method calls that return five values.
#[derive(Decode, Encode)]
#[cbor(array)]
pub struct FiveValues<T, U, V, W, X>(
	#[b(0)] pub T,
	#[b(1)] pub U,
	#[b(2)] pub V,
	#[b(3)] pub W,
	#[b(4)] pub X,
);

/// Returns the larger of two `usize` values in a `const` context.
pub const fn max_usize(x: usize, y: usize) -> usize {
	if x > y {
		x
	} else {
		y
	}
}

/// Returns the largest of a sequence of `usize` values in a `const` context.
pub const fn max_of_usizes(x: &[usize]) -> usize {
	if let Some((&first, rest)) = x.split_first() {
		max_usize(first, max_of_usizes(rest))
	} else {
		0
	}
}
