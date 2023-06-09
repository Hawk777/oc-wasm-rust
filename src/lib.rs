//! This crate provides high-level APIs for accessing components provided by
//! [OpenComputers](https://oc.cil.li) in a vanilla Minecraft environment (e.g. redstone blocks,
//! GPUs, screens, etc.).
//!
//! As a general rule, APIs in this crate accept an `Invoker` and a `Buffer` scratch buffer, the
//! latter being used for encoding parameters and decoding return values. This buffer can be reused
//! between API calls to reduce heap allocations. In some cases the return value of an API may
//! borrow from the scratch buffer.
//!
//! # Important
//! You *must* depend on [`oc-wasm-futures`](https://gitlab.com/Hawk777/oc-wasm-futures) with the
//! `proper-waker` feature in your own application if your chosen executor requires the
//! `proper-waker` feature.
//!
//! # Example
//! ```
//! extern crate alloc;
//! use alloc::vec::Vec;
//! use oc_wasm_futures::sleep;
//! use oc_wasm_opencomputers::prelude::*;
//! use oc_wasm_opencomputers::{gpu, screen};
//! use oc_wasm_opencomputers::common::{Dimension, Point};
//! use oc_wasm_safe::{component, computer};
//! use core::panic::PanicInfo;
//!
//! #[global_allocator]
//! static ALLOC: lol_alloc::AssumeSingleThreaded<lol_alloc::LeakingAllocator> =
//! 	unsafe { lol_alloc::AssumeSingleThreaded::new(lol_alloc::LeakingAllocator::new()) };
//!
//! #[panic_handler]
//! fn panic_hook(_: &PanicInfo<'_>) -> ! {
//! 	computer::error("panic occurred");
//! }
//!
//! async fn main_impl() -> Result<(), oc_wasm_opencomputers::error::Error> {
//! 	// Grab the one-and-only resources.
//! 	let mut invoker = component::Invoker::take().unwrap();
//! 	let mut lister = component::Lister::take().unwrap();
//!
//! 	// Find the GPU.
//! 	let mut listing = lister.start(Some("gpu"));
//! 	let gpu = *listing.next().expect("no GPU").address();
//! 	let gpu = gpu::Gpu::new(gpu);
//!
//! 	// Find the screen.
//! 	listing = lister.start(Some("screen"));
//! 	let screen = *listing.next().expect("no screen").address();
//! 	let screen = screen::Screen::new(screen);
//!
//! 	// Allocate a scratch buffer to use for method calls.
//! 	let mut buffer = Vec::<u8>::new();
//!
//! 	// Lock the GPU so method calls can be made on it. For gpu_locked’s lifetime, methods can only
//! 	// be called on the GPU, not on anything else. To make method calls on another component, drop
//! 	// this value and recreate it later.
//! 	let mut gpu_locked = gpu.lock(&mut invoker, &mut buffer);
//!
//! 	// Bind the GPU to the screen.
//! 	gpu_locked.bind(*screen.address(), true).await?;
//!
//! 	// Clear the screen.
//! 	gpu_locked.set_foreground(gpu::Colour::Rgb(gpu::Rgb(0x00_FF_FF_FF))).await?;
//! 	gpu_locked.set_background(gpu::Colour::Rgb(gpu::Rgb(0))).await?;
//! 	gpu_locked.fill(Point{x: 1, y: 1}, Dimension{width: 160, height: 80}, ' ').await?;
//!
//! 	// Say hello.
//! 	gpu_locked.set(Point{x: 1, y: 1}, "Hello World!", gpu::TextDirection::Horizontal).await?;
//!
//! 	// Stop running forever.
//! 	loop {
//! 		sleep::for_uptime(core::time::Duration::from_secs(3600)).await;
//! 	}
//! }
//!
//! async fn main() -> core::convert::Infallible {
//! 	match main_impl().await {
//! 		Ok(()) => computer::error("main task terminated"),
//! 		Err(e) => computer::error(e.as_str()),
//! 	}
//! }
//!
//! #[no_mangle]
//! pub extern "C" fn run(arg: i32) -> i32 {
//!		oc_wasm_cassette::run(arg, async_main)
//! }
//! ```

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

pub mod common;
pub mod eeprom;
pub mod error;
pub mod filesystem;
pub mod gpu;
pub mod inventory;
pub mod keyboard;
pub mod modem;
pub mod prelude;
pub mod redstone;
pub mod robot;
pub mod screen;

mod helpers;
