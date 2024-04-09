//! Out-of-line byte-array and string reference types for more efficient CBOR encoding.
//!
//! This module defines two reference types, one for byte arrays and one for strings. A value of
//! such a type holds a reference to the specified byte array or string. When such a reference is
//! CBOR-encoded, rather than copying the entire byte array or string into the output, a small
//! External Reference object is written containing the pointer to and length of the byte array or
//! string. This CBOR can be passed to OC-Wasm which will read the data directly from its original
//! memory location, eliminating the need to allocate enough memory and copy the data into the CBOR
//! output.

use minicbor::data::Tag;
use minicbor::encode::{Encode, Encoder, Write};

/// The External Reference CBOR tag number.
const EXTERNAL_REFERENCE: Tag = Tag::new(32769);

/// A reference to a byte array.
pub struct Bytes<'a>(&'a [u8]);

impl<'a> Bytes<'a> {
	/// Wraps a byte array in a byte-array external reference.
	///
	/// # Safety
	/// It is not actually unsafe to construct a `Bytes` object. However, if the caller then
	/// CBOR-encodes the resulting object, they must ensure that the `Bytes` object remains in
	/// existence until the CBOR data has been submitted as part of a method call. Failure to do
	/// this would allow the referent to be modified or dropped, resulting in OC-Wasm reading from
	/// that reused memory.
	#[must_use = "This function is only useful for its return value"]
	pub const unsafe fn new(data: &'a [u8]) -> Self {
		Self(data)
	}
}

impl<'a, C> Encode<C> for Bytes<'a> {
	fn encode<W: Write>(
		&self,
		e: &mut Encoder<W>,
		_: &mut C,
	) -> Result<(), minicbor::encode::Error<W::Error>> {
		const BYTE_STRING_MAJOR: u8 = 2;
		// We’re building for WASM which is always 32-bit.
		#[allow(clippy::cast_possible_truncation)]
		e.tag(EXTERNAL_REFERENCE)?
			.array(3)?
			.u8(BYTE_STRING_MAJOR)?
			.u32(self.0.as_ptr() as u32)?
			.u32(self.0.len() as u32)?;
		Ok(())
	}
}

/// A reference to a text string.
pub struct String<'a>(&'a str);

impl<'a> String<'a> {
	/// Wraps a string in a string external reference.
	///
	/// # Safety
	/// It is not actually unsafe to construct a `String` object. However, if the caller then
	/// CBOR-encodes the resulting object, they must ensure that the `String` object remains in
	/// existence until the CBOR data has been submitted as part of a method call. Failure to do
	/// this would allow the referent to be modified or dropped, resulting in OC-Wasm reading from
	/// that reused memory.
	#[must_use = "This function is only useful for its return value"]
	pub const unsafe fn new(data: &'a str) -> Self {
		Self(data)
	}
}

impl<'a, C> Encode<C> for String<'a> {
	fn encode<W: Write>(
		&self,
		e: &mut Encoder<W>,
		_: &mut C,
	) -> Result<(), minicbor::encode::Error<W::Error>> {
		const UNICODE_STRING_MAJOR: u8 = 3;
		// We’re building for WASM which is always 32-bit.
		#[allow(clippy::cast_possible_truncation)]
		e.tag(EXTERNAL_REFERENCE)?
			.array(3)?
			.u8(UNICODE_STRING_MAJOR)?
			.u32(self.0.as_ptr() as u32)?
			.u32(self.0.len() as u32)?;
		Ok(())
	}
}
