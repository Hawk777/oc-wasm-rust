//! This crate provides high-level APIs for accessing components provided by
//! [OpenComputers](https://oc.cil.li) in a vanilla Minecraft environment (e.g. redstone blocks,
//! GPUs, screens, etc.).
//!
//! As a general rule, APIs in this crate accept an `Invoker` and a `Vec<u8>` scratch buffer, the
//! latter being used for encoding parameters and decoding return values. This buffer can be reused
//! between API calls to reduce heap allocations. In some cases the return value of an API may
//! borrow from the scratch buffer.
//!
//! = Important =
//! You *must* depend on [`oc-wasm-futures`](https://gitlab.com/Hawk777/oc-wasm-futures) with the
//! `proper-waker` feature in your own application if your chosen executor requires the
//! `proper-waker` feature.
//!
//! = Example =
//! ```
//! extern crate alloc;
//! use alloc::boxed::Box;
//! use alloc::string::String;
//! use alloc::vec::Vec;
//! use cassette::Cassette;
//! use oc_wasm_futures::sleep;
//! use oc_wasm_opencomputers::{gpu, screen};
//! use oc_wasm_opencomputers::common::{Dimension, Point};
//! use oc_wasm_safe::{component, computer};
//! use once_cell::unsync::OnceCell;
//! use core::future::Future;
//! use core::panic::PanicInfo;
//! use core::pin::Pin;
//! use wee_alloc::WeeAlloc;
//!
//! #[global_allocator]
//! static ALLOC: WeeAlloc<'_> = WeeAlloc::INIT;
//!
//! fn panic_hook(info: &PanicInfo<'_>) {
//! 	if let Some(s) = info.payload().downcast_ref::<&str>() {
//! 		computer::error(s);
//! 	} else if let Some(s) = info.payload().downcast_ref::<String>() {
//! 		computer::error(s);
//! 	} else {
//! 		computer::error("panic occurred");
//! 	}
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
//! async fn main() {
//! 	match main_impl().await {
//! 		Ok(()) => (),
//! 		Err(e) => computer::error(e.as_str()),
//! 	}
//! }
//!
//! #[no_mangle]
//! pub extern "C" fn run(_: i32) -> i32 {
//! 	static mut PANIC_HOOK_SET: bool = false;
//! 	static mut EXECUTOR: OnceCell<Cassette<Pin<Box<dyn Future<Output = ()>>>>> = OnceCell::new();
//!
//! 	// SAFETY: run() is not reentrant and never touches the PANIC_HOOK_SET variable anywhere else
//! 	// in its body, so run() will never create a second mutable reference. PANIC_HOOK_SET is local
//! 	// to run(), so nobody else can create a second mutable reference on the same thread. OC-Wasm
//! 	// is single-threaded, so no other threads can call run() at the same time.
//! 	let panic_hook_set = unsafe { &mut PANIC_HOOK_SET };
//! 	if !*panic_hook_set {
//! 		std::panic::set_hook(Box::new(panic_hook));
//! 		*panic_hook_set = true;
//! 	}
//!
//! 	// SAFETY: run() is not reentrant and never touches the EXECUTOR variable anywhere else in its
//! 	// body, so run() will never create a second mutable reference. EXECUTOR is local to run(), so
//! 	// nobody else can create a second mutable reference on the same thread. OC-Wasm is
//! 	// single-threaded, so no other threads can call run() at the same time.
//! 	let executor = unsafe { &mut EXECUTOR };
//! 	executor.get_or_init(|| Cassette::new(Box::pin(main())));
//! 	let executor = executor.get_mut().unwrap_or_else(
//! 		// SAFETY: We just called get_or_init(), so it must be populated.
//! 		|| unsafe { core::hint::unreachable_unchecked() },
//! 	);
//!
//! 	sleep::check_for_wakeups();
//! 	if executor.poll_on().is_some() {
//! 		computer::error("main task terminated");
//! 	}
//! 	sleep::shortest_requested()
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
pub mod redstone;
pub mod robot;
pub mod screen;

mod helpers;
