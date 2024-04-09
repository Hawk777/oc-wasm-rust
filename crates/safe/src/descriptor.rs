//! Descriptors which refer to opaque values such as open file handles.
//!
//! The types in this module provide RAII-style wrappers around integer descriptors, which in turn
//! are used by OC-Wasm to represent opaque values (such as open file handles) that can be returned
//! by component calls but cannot be represented as pure data in CBOR.

use super::error::{Error, Result};
use core::fmt::{Debug, Formatter};
use core::marker::PhantomData;
use core::mem::forget;
use core::num::NonZeroU32;
use minicbor::data::Tag;
use minicbor::decode::{Decode, Decoder};
use minicbor::encode::{Encode, Encoder, Write};
use oc_wasm_sys::descriptor as sys;

/// The Identifier CBOR tag number.
const IDENTIFIER: Tag = Tag::new(39);

/// CBOR-encodes an opaque value descriptor.
///
/// This produces an integer with the Identifier tag.
fn cbor_encode<W: Write>(
	descriptor: u32,
	e: &mut Encoder<W>,
) -> core::result::Result<(), minicbor::encode::Error<W::Error>> {
	e.tag(IDENTIFIER)?.u32(descriptor)?;
	Ok(())
}

/// A value that can be converted into an opaque value descriptor.
///
/// A value implementing this trait holds, borrows, or is otherwise able to provide an opaque value
/// descriptor as a `u32`.
pub trait AsRaw {
	/// Returns the raw descriptor value.
	#[must_use = "This function is only useful for its return value"]
	fn as_raw(&self) -> u32;
}

/// A value that can be borrowed as an opaque value descriptor.
///
/// A value implementing this trait is able to produce a [`Borrowed`](Borrowed) value referring to
/// a descriptor.
#[allow(clippy::module_name_repetitions)] // This is the best name I could come up with.
pub trait AsDescriptor {
	/// Borrows the descriptor.
	#[must_use = "This function is only useful for its return value"]
	fn as_descriptor(&self) -> Borrowed<'_>;
}

/// A value that can be converted into an opaque value descriptor.
///
/// A value implementing this trait is able to produce an [`Owned`](Owned) value referring to a
/// descriptor by consuming itself.
#[allow(clippy::module_name_repetitions)] // This is the best name I could come up with.
pub trait IntoDescriptor {
	/// Converts to the descriptor.
	#[must_use = "This function is only useful for its return value"]
	fn into_descriptor(self) -> Owned;
}

/// An owned opaque value descriptor.
///
/// A value of this type encapsulates an opaque value descriptor. Cloning it duplicates the
/// descriptor. Dropping it closes the descriptor. CBOR-encoding it yields an integer with the
/// Identifier tag.
#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Owned(NonZeroU32);

impl Owned {
	/// Wraps a raw integer descriptor in a `Descriptor` object.
	///
	/// # Safety
	/// The caller must ensure that the passed-in value is a valid, open descriptor. Passing a
	/// closed descriptor may result in dropping the object closing an unrelated opaque value which
	/// happened to be allocated the same descriptor value. Passing an invalid descriptor value may
	/// violate the niche requirements and result in undefined behaviour.
	///
	/// The caller must ensure that only one `Descriptor` object for a given value exists at a
	/// time, because dropping a `Descriptor` object closes the descriptor.
	#[allow(clippy::must_use_candidate)] // This could be called and immediately dropped to close an unwanted descriptor.
	pub const unsafe fn new(raw: u32) -> Self {
		// SAFETY: The caller is required to pass a valid descriptor. Any valid descriptor is a
		// small nonnegative integer. Therefore, any descriptor plus one is a small positive
		// integer.
		Self(NonZeroU32::new_unchecked(raw + 1))
	}

	/// Destroys a `Descriptor` object and returns the raw value.
	///
	/// The caller must ensure that the descriptor is eventually closed. This function is safe
	/// because Rust’s safety guarantees to not include reliable freeing of resources; however,
	/// care should be taken when calling it.
	#[must_use = "The returned descriptor will leak if not manually closed"]
	pub const fn into_inner(self) -> u32 {
		let ret = self.as_raw();
		forget(self);
		ret
	}

	/// Returns the raw descriptor value.
	#[must_use = "This function is only useful for its return value"]
	pub const fn as_raw(&self) -> u32 {
		self.0.get() - 1
	}

	/// Duplicates the descriptor.
	///
	/// # Errors
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors) is returned if the descriptor table is
	///   too full and some descriptors must be closed.
	pub fn dup(&self) -> Result<Self> {
		// SAFETY: dup can be invoked with any valid descriptor.
		let new_desc = Error::from_i32(unsafe { sys::dup(self.as_raw()) })?;
		// SAFETY: dup returns a fresh, new descriptor on success.
		Ok(unsafe { Self::new(new_desc) })
	}
}

impl AsRaw for Owned {
	fn as_raw(&self) -> u32 {
		self.0.get() - 1
	}
}

impl AsDescriptor for Owned {
	fn as_descriptor(&self) -> Borrowed<'_> {
		Borrowed(self.0, PhantomData)
	}
}

impl IntoDescriptor for Owned {
	fn into_descriptor(self) -> Owned {
		self
	}
}

impl Debug for Owned {
	fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
		self.as_raw().fmt(f)
	}
}

impl Drop for Owned {
	fn drop(&mut self) {
		// SAFETY: The contained descriptor is always valid. There can be only one Owned object in
		// existence for a given open descriptor. There is no safe way to close a descriptor other
		// than dropping the Owned object. Therefore, the descriptor is valid and closing it will
		// not break any other objects.
		unsafe { sys::close(self.as_raw()) };
	}
}

impl<Context> Encode<Context> for Owned {
	fn encode<W: Write>(
		&self,
		e: &mut Encoder<W>,
		_: &mut Context,
	) -> core::result::Result<(), minicbor::encode::Error<W::Error>> {
		cbor_encode(self.as_raw(), e)
	}
}

/// A borrowed opaque value descriptor.
///
/// A value of this type encapsulates an opaque value descriptor. Copying or cloning it produces a
/// new object containing the same descriptor. Dropping it does nothing. CBOR-encoding it yields an
/// integer with the Identifier tag. While a value of this type exists, lifetime rules prevent the
/// modification or dropping of the [`Owned`](Owned) value from which it borrowed its descriptor.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Borrowed<'a>(NonZeroU32, PhantomData<&'a NonZeroU32>);

impl Borrowed<'_> {
	/// Returns the raw descriptor value.
	#[must_use = "This function is only useful for its return value"]
	pub const fn as_raw(self) -> u32 {
		self.0.get() - 1
	}
}

impl AsRaw for Borrowed<'_> {
	fn as_raw(&self) -> u32 {
		self.0.get() - 1
	}
}

impl AsDescriptor for Borrowed<'_> {
	fn as_descriptor(&self) -> Borrowed<'_> {
		*self
	}
}

impl Debug for Borrowed<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
		self.as_raw().fmt(f)
	}
}

impl<Context> Encode<Context> for Borrowed<'_> {
	fn encode<W: Write>(
		&self,
		e: &mut Encoder<W>,
		_: &mut Context,
	) -> core::result::Result<(), minicbor::encode::Error<W::Error>> {
		cbor_encode(self.as_raw(), e)
	}
}

/// A CBOR-decoded opaque value descriptor.
///
/// A value of this type encapsulates an opaque value descriptor. It cannot be cloned. Dropping it
/// does nothing; this may cause a resource leak, but resource leaks are not considered unsafe
/// Rust, and under the circumstances, closing the descriptor could be unsafe (see the safety note
/// on [`into_owned`](Decoded::into_owned) for why). The intended use of this type is to
/// immediately call [`into_owned`](Decoded::into_owned) to convert the value into an
/// [`Owned`](Owned) instead.
#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Decoded(NonZeroU32);

impl Decoded {
	/// Converts a `Decoded` descriptor into an [`Owned`](Owned) descriptor.
	///
	/// # Safety
	/// The caller must ensure that the `Decoded` descriptor is the only reference to the
	/// descriptor it holds, and that that descriptor is valid. Generally, this is accomplished by
	/// obtaining a `Decoded` descriptor by CBOR-decoding the result of a method call, because
	/// OC-Wasm guarantees that any opaque value returned from a method call is represented by a
	/// fresh descriptor.
	///
	/// The reason why this method is unsafe is that a caller could potentially craft an arbitrary
	/// CBOR sequence in a byte buffer, then decode it. If such a decoding operation were to return
	/// an [`Owned`](Owned) directly, this would be unsound, as the caller could decode a second
	/// [`Owned`](Owned) referring to the same descriptor as an existing [`Owned`](Owned) or an
	/// [`Owned`](Owned) referring to a closed descriptor. Instead, CBOR decoding (which is itself
	/// safe) can only create a `Decoded`, which does not claim exclusive ownership (or even
	/// validity) of the contained descriptor but also cannot actually be used as a descriptor; the
	/// caller is forced to promise those properties in order to convert to the actually useful
	/// [`Owned`](Owned) type via this `unsafe` method.
	#[allow(clippy::must_use_candidate)] // If caller doesn’t want the descriptor, they can do this and immediately drop.
	pub unsafe fn into_owned(self) -> Owned {
		Owned(self.0)
	}
}

impl Debug for Decoded {
	fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
		(self.0.get() - 1).fmt(f)
	}
}

impl<'b, Context> Decode<'b, Context> for Decoded {
	fn decode(
		d: &mut Decoder<'b>,
		_: &mut Context,
	) -> core::result::Result<Self, minicbor::decode::Error> {
		let tag = d.tag()?;
		if tag != IDENTIFIER {
			return Err(minicbor::decode::Error::message("expected Identifier tag"));
		}
		Ok(Self(NonZeroU32::new(d.u32()? + 1).unwrap()))
	}
}
