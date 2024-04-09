//! This crate provides raw FFI bindings for the Wasm imports available in the OC-Wasm environment.
//! Please see the OC-Wasm documentation for details on individual functions.

#![no_std]
#![warn(
	// Turn on extra language lints.
	future_incompatible,
	missing_abi,
	nonstandard_style,
	rust_2018_idioms,
	single_use_lifetimes,
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
pub mod execute;
