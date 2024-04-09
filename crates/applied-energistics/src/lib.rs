//! This crate provides high-level APIs for accessing components provided by [Applied
//! Energistics](https://appliedenergistics.github.io/ae2-site-archive/).
//!
//! As a general rule, APIs in this crate accept an `Invoker` and a `Buffer` scratch buffer, the
//! latter being used for encoding parameters and decoding return values. This buffer can be reused
//! between API calls to reduce heap allocations. In some cases the return value of an API may
//! borrow from the scratch buffer.
//!
//! # Features
//! * The `std` feature enables integration with the Rust standard library that can only be done
//!   outside `no_std` mode.
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

extern crate alloc;

pub mod error;
pub mod interface;
pub mod network_control;

pub use oc_wasm_helpers::{fluid::Fluid, inventory::ItemStack, Lockable};
