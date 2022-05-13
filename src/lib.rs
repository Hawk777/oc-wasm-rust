//! This crate provides a collection of async futures for making method calls and sleeping.
//!
//! # Features
//! The `proper-waker` feature, which is enabled by default, makes the sleep futures fully
//! compliant with the [`Future`](core::future::Future) specification. Disabling the feature uses
//! an alternative implementation which only keeps track of the shortest deadline (rather than the
//! deadlines of all in-progress sleeps); this reduces code size, but only works if your choice of
//! executor polls all tasks on every wakeup (as certain simple executors do) rather than keeping a
//! proper ready-queue and requiring each task to be woken by its own [`Waker`](core::task::Waker).

#![no_std]
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

#[cfg(feature = "alloc")]
extern crate alloc;

pub mod invoke;
pub mod sleep;
