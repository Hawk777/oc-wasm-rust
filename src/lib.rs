//! This crate provides a collection of helpers and utilities used by other OC-Wasm high-level
//! APIs. It is not a useful crate for application developers, unless you are developing an API for
//! a new mod that doesn’t have one yet.
//!
//! # Features
//! * The `alloc` feature enables APIs that require dynamic memory allocation.
//!
//! # Important
//! You *must* depend on [`oc-wasm-futures`](https://gitlab.com/Hawk777/oc-wasm-futures) with the
//! `proper-waker` feature in your own application if your chosen executor requires the
//! `proper-waker` feature.

#![cfg_attr(not(feature = "std"), no_std)]
#![warn(
	// Turn on extra language lints.
	future_incompatible,
	missing_abi,
	nonstandard_style,
	rust_2018_idioms,
	// Disabled due to <https://github.com/rust-lang/rust/issues/69952>.
	// single_use_lifetimes,
	trivial_casts,
	trivial_numeric_casts,
	unused,
	unused_crate_dependencies,
	unused_import_braces,
	unused_lifetimes,
	unused_qualifications,

	// Turn on extra Rustdoc lints.
	rustdoc::all,

	// Turn on extra Clippy lints.
	clippy::cargo,
	clippy::pedantic,
)]
// I’m not a big fan of this style, and it sometimes generates larger code.
#![allow(clippy::option_if_let_else)]
// Nope, tabs thanks.
#![allow(clippy::tabs_in_doc_comments)]

#[cfg(feature = "alloc")]
extern crate alloc;

pub mod error;
pub mod fluid;
pub mod inventory;
pub mod map_decoder;
pub mod sides;

use minicbor::Decode;

/// A component that can be given an [`Invoker`](oc_wasm_safe::component::Invoker) and a byte
/// buffer in order to access its methods.
pub trait Lockable<'invoker, 'buffer, B: oc_wasm_futures::invoke::Buffer> {
	/// The type obtained when locking the component.
	type Locked;

	/// Locks the component so methods can be invoked on it.
	///
	/// The [`Invoker`](oc_wasm_safe::component::Invoker) and a scratch buffer must be provided.
	/// They are released and can be reused once the locked value is dropped.
	#[must_use = "This function is only useful for its return value"]
	fn lock(
		&self,
		invoker: &'invoker mut oc_wasm_safe::component::Invoker,
		buffer: &'buffer mut B,
	) -> Self::Locked;
}

/// Decodes a CBOR map with one-based integer keys into a vector.
///
/// Each element may have a mapping function applied to convert it from its raw decoded type into
/// its final result type, if desired; this avoids the need to allocate a second vector to collect
/// the results of the conversion afterwards.
///
/// # Errors
/// This function fails if the map contains duplicate keys, non-natural keys, or keys outside legal
/// bounds.
#[cfg(feature = "alloc")]
pub fn decode_one_based_map_as_vector<
	'buffer,
	Context,
	DecodedType: Decode<'buffer, Context>,
	ResultType: From<DecodedType>,
>(
	d: &mut minicbor::Decoder<'buffer>,
	context: &mut Context,
) -> Result<alloc::vec::Vec<ResultType>, minicbor::decode::Error> {
	use alloc::vec;
	use alloc::vec::Vec;

	/// A fixed-sized vector, some of whose elements are initialized and some of which are not.
	struct PartialArray<T> {
		/// The storage.
		///
		/// The length of this vector is zero; the data is stored in the extra capacity (which is
		/// the expected length).
		storage: Vec<T>,

		/// A vector indicating which elements have been initialized yet.
		initialized: Vec<bool>,
	}

	impl<T> PartialArray<T> {
		/// Creates a new `PartialArray` of the specified length.
		pub fn new(len: usize) -> Self {
			Self {
				storage: Vec::with_capacity(len),
				initialized: vec![false; len],
			}
		}

		/// Places a new element into the vector in an empty cell.
		///
		/// If the index is not initialized, it becomes initialized with the provided element and
		/// `true` is returned. If the index is already initialized, nothing happens and `false` is
		/// returned.
		pub fn set(&mut self, index: usize, value: T) -> bool {
			if core::mem::replace(&mut self.initialized[index], true) {
				false
			} else {
				// SAFETY: We just verified that the indexth element is uninitialized. The fact
				// that initialized[index] did not panic also means that index is within bounds
				// (because storage.capacity=initialized.len).
				unsafe { self.storage.as_mut_ptr().add(index).write(value) };
				true
			}
		}

		/// Returns the array.
		///
		/// If all positions are initialized, `Some` is returned. If any position is not
		/// initialized, `None` is returned.
		pub fn finish(mut self) -> Option<Vec<T>> {
			if self.initialized.iter().all(|&x| x) {
				let mut storage = core::mem::take(&mut self.storage);
				// SAFETY: We just verified that all positions are initialized.
				unsafe { storage.set_len(storage.capacity()) };
				Some(storage)
			} else {
				None
			}
		}
	}

	impl<T> Drop for PartialArray<T> {
		fn drop(&mut self) {
			for i in 0..self.storage.capacity() {
				if self.initialized[i] {
					// SAFETY: We just verified that the position is initialized.
					unsafe { self.storage.as_mut_ptr().add(i).read() };
					// Drop the value returned by read().
				}
			}
		}
	}

	let len = d.map()?;
	// The CBOR fits in memory, so it must be <2³² elements.
	#[allow(clippy::cast_possible_truncation)]
	let len = len.ok_or_else(|| {
		minicbor::decode::Error::message("indefinite-length maps are not supported")
	})? as usize;
	let mut data = PartialArray::<ResultType>::new(len);
	for _ in 0..len {
		let index = d.u32()? as usize;
		if index == 0 || index > len {
			return Err(minicbor::decode::Error::message("invalid map index"));
		}
		let value = d.decode_with::<Context, DecodedType>(context)?;
		if !data.set(index - 1, value.into()) {
			return Err(minicbor::decode::Error::message("duplicate map key"));
		}
	}
	if let Some(data) = data.finish() {
		Ok(data)
	} else {
		Err(minicbor::decode::Error::message("missing map key"))
	}
}
