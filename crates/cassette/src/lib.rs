//! OC-Wasm-Cassette provides a convenient wrapper that makes it easy to use an `async fn` as a
//! top-level function in an OC-Wasm application.
//!
//! Usage is as simple as:
//! ```rust
//! async fn main() -> Infallible {
//!     // Your code here
//! }
//!
//! #[no_mangle]
//! pub extern "C" fn run(arg: i32) -> i32 {
//!     oc_wasm_cassette::run(arg, main)
//! }
//! ```

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

extern crate alloc;
use alloc::boxed::Box;
use cassette::Cassette;
use core::convert::Infallible;
use core::future::Future;
use core::pin::Pin;
use oc_wasm_futures::sleep;
use sync_unsafe_cell::SyncUnsafeCell;

/// All the state of the application, encapsulated into one object.
enum State {
	/// The application has never run before and startup processes need to run.
	Uninitialized,

	/// The application is running.
	Running(Cassette<Pin<Box<dyn Future<Output = Infallible> + Sync>>>),
}

impl State {
	fn run<Fut: Future<Output = Infallible> + Sync + 'static, F: Fn() -> Fut>(
		&mut self,
		_: i32,
		main: F,
	) -> i32 {
		match self {
			Self::Uninitialized => {
				// Construct the executor.
				*self = Self::Running(Cassette::new(Box::pin(main())));

				// Yield for zero time and then execute the application next invocation.
				0
			}
			Self::Running(executor) => {
				sleep::check_for_wakeups();
				let _: Option<Infallible> = executor.poll_on(); // No need to examine return value, it cannot be Some(Infallible).
				sleep::shortest_requested()
			}
		}
	}
}

/// Runs the application for a period of time.
///
/// The `MainFuture` type is the type of future returned by the `main` function. The `Main` type is
/// the type of the `main` function itself. The `arg` parameter is the `i32` parameter that OC-Wasm
/// passes to the top-level `run` function. The `main` parameter is the main function, which is
/// typically an `async fn`. The return value should be returned from the top-level `run` function.
///
/// # Panics
/// This function panics if it is re-entered (i.e. if it is called while it is already in
/// progress).
pub fn run<MainFuture: Future<Output = Infallible> + Sync + 'static, Main: Fn() -> MainFuture>(
	arg: i32,
	main: Main,
) -> i32 {
	// Records whether the run() function is currently in progress.
	static RUNNING: SyncUnsafeCell<bool> = SyncUnsafeCell::new(false);

	// Stores the state that persists from tick to tick.
	static STATE: SyncUnsafeCell<State> = SyncUnsafeCell::new(State::Uninitialized);

	// Check for re-entry.
	//
	// SAFETY: OC-Wasm is single-threaded; therefore, there cannot be another thread accessing
	// RUNNING at the same time. Nothing ever takes a reference to RUNNING (only core::ptr
	// operations are done on it) so reference rules cannot be violated either.
	let reentered = unsafe { core::ptr::replace(RUNNING.get(), true) };
	assert!(!reentered, "oc_wasm_cassette::run() re-entered");

	// Get a mutable reference to STATE.
	//
	// SAFETY: We just checked that this function has not been re-entered, so no other call is in
	// progress. The STATE variable is named exactly once (besides its definition) in this
	// function, on the following line, so no second mutable reference can be created elsewhere in
	// the function. The STATE variable is scoped within the function, so no second mutable
	// reference can be created anywhere else either.
	let state: &mut State = unsafe { &mut *STATE.get() };

	// Delegate to the state.
	let ret = state.run(arg, main);

	// Clear the re-entry flag.
	//
	// SAFETY: OC-Wasm is single-threaded; therefore, there cannot be another thread accessing
	// RUNNING at the same time. Nothing ever takes a reference to RUNNING (only core::ptr
	// operations are done on it) so reference rules cannot be violated either.
	unsafe { core::ptr::write(RUNNING.get(), false) };

	ret
}
