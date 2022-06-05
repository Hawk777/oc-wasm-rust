//! A collection of helper functions and types used by multiple modules in this crate but not
//! exported for public use.

use minicbor::decode::Decoder;
use minicbor::Decode;

/// A value which, when CBOR-decoded, does not consume any data items and always successfully does
/// nothing.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Ignore();

impl<'buffer> Decode<'buffer> for Ignore {
	fn decode(_: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
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
