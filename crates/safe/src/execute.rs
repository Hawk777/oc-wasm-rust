//! Functions for loading and executing a new WASM binary.

use super::error::{Error, Result};
use oc_wasm_sys::execute as sys;

/// Clears the execution buffer.
///
/// At the start of a program’s execution, the execution buffer is empty, so loading can commence
/// without invoking this syscall first. However, if the program starts loading a binary then needs
/// to abort and load a different binary, this syscall can be used to discard the first binary.
pub fn clear() {
	// SAFETY: clear is unconditionally safe.
	unsafe { sys::clear() }
}

/// Writes data to the execution buffer.
///
/// The `data` parameter is the portion of the Wasm binary to write into the buffer.
///
/// # Errors
/// * [`Other`](Error::Other) is returned if this call would make the contents of the buffer larger
///   than the computer’s installed RAM.
pub fn add(data: &[u8]) -> Result<()> {
	Error::from_i32(
		// SAFETY: add permits a readable pointer/length pair.
		unsafe { sys::add(data.as_ptr(), data.len()) },
	)?;
	Ok(())
}

/// Executes the Wasm binary contained in the execution buffer.
pub fn execute() -> ! {
	// SAFETY: execute is unconditionally safe.
	unsafe { sys::execute() }
}
