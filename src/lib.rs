//! This crate provides a collection of helpers and utilities used by other OC-Wasm high-level
//! APIs. It is not a useful crate for application developers, unless you are developing an API for
//! a new mod that doesn’t have one yet.
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

pub mod error;
pub mod fluid;
pub mod inventory;
pub mod map_decoder;

use minicbor::{Decode, Encode};

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
