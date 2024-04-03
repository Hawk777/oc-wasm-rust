//! A collection of helper functions and types used by multiple modules in this crate but not
//! exported for public use.

use minicbor::decode::Decoder;
use minicbor::Decode;

/// A value which, when CBOR-decoded, does not consume any data items and always successfully does
/// nothing.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Ignore();

impl<'buffer, Context> Decode<'buffer, Context> for Ignore {
	fn decode(_: &mut Decoder<'buffer>, _: &mut Context) -> Result<Self, minicbor::decode::Error> {
		Ok(Self())
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

/// Returns the largest of a sequence of `usize` values in a `const` context.
pub const fn max_of_usizes(x: &[usize]) -> usize {
	if let Some((&first, rest)) = x.split_first() {
		max_usize(first, max_of_usizes(rest))
	} else {
		0
	}
}

/// Decodes a CBOR item that is a signal parameter that should be a `u16`.
pub fn decode_u16_from_signal(d: &mut Decoder<'_>) -> Result<u16, minicbor::decode::Error> {
	// The caller expects a u16, so it should fit.
	#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
	let ret = d.f64()? as u16;
	Ok(ret)
}

/// Decodes a CBOR item that is a signal parameter that should be a `u32`.
pub fn decode_u32_from_signal(d: &mut Decoder<'_>) -> Result<u32, minicbor::decode::Error> {
	// The caller expects a u32, so it should fit.
	#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
	let ret = d.f64()? as u32;
	Ok(ret)
}
