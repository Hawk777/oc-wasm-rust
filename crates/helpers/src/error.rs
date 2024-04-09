use minicbor::{Decode, Decoder};

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
				p.datatype()? == minicbor::data::Type::Null
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

/// An error returned when converting an invalid value to an enumeration.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TryFromInt;

impl core::fmt::Display for TryFromInt {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
		"invalid value".fmt(f)
	}
}

#[cfg(feature = "std")]
impl std::error::Error for TryFromInt {}
