//! Helpers to decode structures encoded as CBOR maps with string keys.
//!
//! Various different kinds of structured information (such as item stacks) are encoded in this way
//! by OpenComputers. In Lua they look like tables. In OC-Wasm they look like CBOR maps. In Rust it
//! is more useful to expose them as proper `struct`s with named fields. The helpers in this module
//! assist with implementing [`Decode`](minicbor::Decode) for such data.
//!
//! The typical approach is to implement a “builder structure” whose shape parallels that of the
//! main structure, but with each field being an [`Option`] over the type of the corresponding
//! field in the main structure. The builder structure should then implement [`Builder`].
//!
//! Inheritance between structures is supported. In some cases, an object may look similar to
//! another object but with additional keys added. In this case, the builder of the outer object
//! should contain a builder of the inner object and delegate to it.

use minicbor::data::Type;
use minicbor::decode::{Decoder, Error};

/// A builder that can hold the intermediate state while an object is being decoded from a map.
pub trait Builder<'buffer> {
	/// The type of object that the builder produces on success.
	type Output;

	/// Accepts a single map entry.
	///
	/// The `key` parameter is the string key. The `decoder` parameter is positioned at the
	/// corresponding value.
	///
	/// If this builder (or a subordinate builder for a nested object) understands the key, it
	/// should consume the corresponding value and return `true`. If not, it should return `false`
	/// without consuming the corresponding value. This allows delegation to subordinate builders
	/// to work, as only exactly one builder in the tree will directly return `true` (as opposed to
	/// doing so via delegation), and only that one builder will consume the value.
	///
	/// # Errors
	/// This fails if the value is not acceptable, for example if it is of the wrong data type.
	fn entry<Context>(
		&mut self,
		key: &'buffer str,
		decoder: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<bool, Error>;

	/// Finishes building the object.
	///
	/// # Errors
	/// This fails if the object is not buildable, for example if a required value is missing.
	fn build(self) -> Result<Self::Output, Error>;
}

/// A builder that can also build its object type from a CBOR null data item rather than a map.
pub trait NullableBuilder<'buffer>: Builder<'buffer> {
	/// Builds the object from a CBOR null data item.
	fn build_null() -> Self::Output;
}

/// Decodes a type from a map using a builder.
///
/// Extra keys that the builder does not consume are ignored.
///
/// # Errors
/// This fails if the underlying builder fails, or if the data item is not a definite-length map.
pub fn decode<'buffer, B: Builder<'buffer> + Default, Context>(
	d: &mut Decoder<'buffer>,
	context: &mut Context,
) -> Result<B::Output, Error> {
	let mut builder = B::default();
	let len = d
		.map()?
		.ok_or_else(|| Error::message("indefinite-length maps are not supported"))?;
	for _ in 0..len {
		let key = d.str()?;
		if builder.entry(key, d, context)? {
			// The builder consumed the value.
		} else {
			// The builder did not consume the value. Skip it.
			d.skip()?;
		}
	}
	builder.build()
}

/// Decodes a type from a map or null using a builder.
///
/// Extra keys that the builder does not consume are ignored.
///
/// # Errors
/// This fails if the underlying builder fails while processing map entries, or if the data item is
/// neither a definite-length map, null, nor undefined.
pub fn decode_nullable<'buffer, B: NullableBuilder<'buffer> + Default, Context>(
	d: &mut Decoder<'buffer>,
	context: &mut Context,
) -> Result<B::Output, Error> {
	match d.datatype()? {
		Type::Null => {
			d.null()?;
			Ok(B::build_null())
		}
		Type::Undefined => {
			d.undefined()?;
			Ok(B::build_null())
		}
		_ => decode::<B, Context>(d, context),
	}
}
