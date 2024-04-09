//! This crate provides safe but low-level for the Wasm imports available in the OC-Wasm
//! environment.
//!
//! # Features
//! The `panic` feature controls how certain system call errors which should be impossible are
//! handled. When the feature is enabled, a panic is generated in those situations. When the
//! feature is disabled, a Wasm `unreachable` (trap) instruction is executed instead; this produces
//! smaller code but less useful error messages.
//!
//! The `std` feature controls whether [`error::Error`](error::Error) implements
//! `std::error::Error`, which it cannot do in a `no_std` environment.

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

pub mod component;
pub mod computer;
pub mod descriptor;
pub mod error;
pub mod execute;
pub mod extref;

use core::fmt::{Display, Formatter};
use core::str::FromStr;
use minicbor::{
	data::{Tag, Type},
	decode, encode,
};
use uuid::Uuid;

/// A component address.
///
/// This is just a UUID. It supports `minicbor`. When encoding, it encodes as a byte string tagged
/// with the Binary UUID tag. When decoding, it decodes from an optional Binary UUID (or
/// Identifier, for backwards compatibility) tag followed by either a byte string or a UTF-8
/// string.
#[derive(Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Address(Uuid);

impl Address {
	const TAG: Tag = Tag::new(37);

	#[must_use = "This function is only useful for its return value"]
	pub const fn as_bytes(&self) -> &[u8; 16] {
		self.0.as_bytes()
	}

	#[must_use = "This function is only useful for its return value"]
	pub const fn from_bytes(b: [u8; 16]) -> Self {
		Self(Uuid::from_bytes(b))
	}
}

impl Display for Address {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), core::fmt::Error> {
		self.0.fmt(f)
	}
}

impl FromStr for Address {
	type Err = <Uuid as FromStr>::Err;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		Ok(Self(<Uuid as FromStr>::from_str(s)?))
	}
}

impl<Context> decode::Decode<'_, Context> for Address {
	fn decode(d: &mut decode::Decoder<'_>, _: &mut Context) -> Result<Self, decode::Error> {
		// Check the datatype of the next item.
		let mut datatype = d.datatype()?;

		// If it’s a tag, it must have value Identifier (39) or Binary UUID, and the tagged value
		// is the UUID.
		if datatype == Type::Tag {
			let tag = d.tag()?;
			if tag != Self::TAG && tag != Tag::new(39) {
				return Err(decode::Error::message("expected tag Binary UUID"));
			}
			datatype = d.datatype()?;
		}

		// Now the value must appear as either byte string holding a binary UUID or a UTF-8 string
		// holding a text UUID. Indefinite forms do not need to be supported as OC-Wasm never
		// generates them in data passed to the Wasm module instance.
		match datatype {
			Type::Bytes => {
				let b = d.bytes()?;
				Ok(Self::from_bytes(b.try_into().map_err(|_| {
					decode::Error::message("expected 16 bytes")
				})?))
			}
			Type::String => {
				let s = d.str()?;
				Ok(
					Self::from_str(s)
						.map_err(|_| decode::Error::message("expected UUID string"))?,
				)
			}
			_ => Err(decode::Error::message("expected byte or UTF-8 string")),
		}
	}
}

impl<Context> encode::Encode<Context> for Address {
	fn encode<W: encode::Write>(
		&self,
		e: &mut encode::Encoder<W>,
		_: &mut Context,
	) -> Result<(), encode::Error<W::Error>> {
		e.tag(Self::TAG)?.bytes(self.as_bytes())?;
		Ok(())
	}
}

/// Panics or traps depending on the state of the `panic` feature.
///
/// If the `panic` feature is enabled, this macro panics with the given message. If it is disabled,
/// this macro invokes Wasm `UNREACHABLE` (trap) instruction, instantly terminating execution; the
/// message is ignored.
#[cfg(feature = "panic")]
#[macro_export]
macro_rules! panic_or_trap {
	($message: literal) => {
		core::panic!($message)
	};
}

/// Panics or traps depending on the state of the `panic` feature.
///
/// If the `panic` feature is enabled, this macro panics with the given message. If it is disabled,
/// this macro invokes Wasm `UNREACHABLE` (trap) instruction, instantly terminating execution; the
/// message is ignored.
#[cfg(not(feature = "panic"))]
#[macro_export]
macro_rules! panic_or_trap {
	($message: literal) => {
		core::arch::wasm32::unreachable()
	};
}

mod helpers;
