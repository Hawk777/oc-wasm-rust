//! Miscellaneous APIs related to the computer itself.

use super::error::{Error, Result};
use super::helpers::{call_buffer, call_buffer_len, call_buffer_str, call_string};
use super::Address;
use crate::panic_or_trap;
use core::num::NonZeroUsize;
use oc_wasm_sys::computer as sys;
use ordered_float::NotNan;

/// Returns the amount of world time the computer has been running, in seconds.
#[must_use = "This function is only useful for its return value"]
pub fn uptime() -> NotNan<f64> {
	// SAFETY: uptime is unconditionally safe.
	unsafe { sys::uptime() }
}

/// Returns the amount of CPU time that the computer has consumed, in seconds.
#[must_use = "This function is only useful for its return value"]
pub fn cpu_time() -> NotNan<f64> {
	// SAFETY: cpu_time is unconditionally safe.
	unsafe { sys::cpu_time() }
}

/// Returns the current in-game time and date, in game ticks.
#[must_use = "This function is only useful for its return value"]
pub fn world_time() -> u64 {
	// SAFETY: world_time is unconditionally safe.
	unsafe { sys::world_time() }
}

/// Returns the computer’s own UUID address.
#[must_use = "This function is only useful for its return value"]
pub fn address() -> Address {
	// SAFETY: address permits a writeable buffer pointer and promises to always write a valid
	// UUID. It can only fail due to MemoryFault, which, because we provide it with a valid buffer,
	// is impossible.
	let mut buffer = uuid::Bytes::default();
	unsafe { sys::address(buffer.as_mut_ptr()) };
	Address::from_bytes(buffer)
}

/// Returns the UUID address of a filesystem that lives until the computer shuts down and can
/// beused to hold temporarily files.
#[must_use = "This function is only useful for its return value"]
pub fn tmpfs_address() -> Address {
	// SAFETY: tmpfs_address permits a writeable buffer pointer and promises to always write a
	// valid UUID. It can only fail due to MemoryFault, which, because we provide it with a valid
	// buffer, is impossible.
	let mut buffer = uuid::Bytes::default();
	unsafe { sys::tmpfs_address(buffer.as_mut_ptr()) };
	Address::from_bytes(buffer)
}

/// Returns the amount, in bytes, of RAM installed in the computer.
#[must_use = "This function is only useful for its return value"]
pub fn installed_ram() -> u32 {
	// SAFETY: installed_ram is unconditionally safe.
	unsafe { sys::installed_ram() }
}

/// Returns the amount, in bytes, of free RAM in the computer.
#[must_use = "This function is only useful for its return value"]
pub fn free_ram() -> u32 {
	// SAFETY: free_ram is unconditionally safe.
	unsafe { sys::free_ram() }
}

/// Pushes a signal to the signal queue.
///
/// The `signal` parameter contains a CBOR-encoded array representing the signal, which must be a
/// mix of numbers, strings, and maps containing these types, and the first element of which must
/// be a string containing the signal name.
///
/// # Errors
/// * [`CborDecode`](Error::CborDecode) is returned if the `params` pointer is present but contains
///   an invalid or unsupported CBOR sequence.
/// * [`QueueFull`](Error::QueueFull) is returned if the computer’s signal queue is full.
pub fn push_signal(signal: &[u8]) -> Result<()> {
	Error::from_i32(
		// SAFETY: push_signal permits CBOR pointer.
		unsafe { sys::push_signal(signal.as_ptr()) },
	)?;
	Ok(())
}

/// Returns the length, in bytes, of the next signal in the signal queue.
///
/// If there is no next entry, `None` is returned.
///
/// # Panics
/// This function panics if the underlying syscall fails, because the only reasons it could fail
/// should be impossible due to the type system.
#[must_use = "This function is only useful for its return value"]
pub fn pull_signal_length() -> Option<NonZeroUsize> {
	let len =
		// SAFETY: pull_signal permits null.
		unsafe{call_buffer_len(sys::pull_signal)};
	// Can’t fail because pull_signal can only fail due to MemoryFault or StringDecode, and
	// Error::from_isize already treats those as unreachable.
	let len = len.unwrap_or_else(|_| panic_or_trap!("unreachable"));
	NonZeroUsize::new(len)
}

/// Pops a signal from the signal queue.
///
/// The `buffer` parameter identifies where to store the signal data.
///
/// If there is a signal pending, the signal data is written to `buffer` as a CBOR sequence of two
/// elements, the first being the name and the second being an array containing any additional
/// signal parameters; a slice referring to the sequence is returned; and the signal is removed
/// from the queue. If not, `None` is returned.
///
/// # Errors
/// * [`BufferTooShort`](Error::BufferTooShort) is returned if `buffer` is not large enough to hold
///   the signal data.
///
/// On error, the signal remains in the queue.
pub fn pull_signal(buffer: &mut [u8]) -> Result<Option<&mut [u8]>> {
	// SAFETY: pull_signal permits a writeable buffer pointer/length pair and promises to always
	// return the number of bytes written to it.
	let ret = unsafe { call_buffer(sys::pull_signal, buffer) }?;
	Ok(if ret.is_empty() { None } else { Some(ret) })
}

/// Begins iteration over the computer’s access control list.
///
/// Iteration over the access control list is not reentrant. Concurrent software must ensure that
/// only one access control list iteration at a time is attempted.
pub fn acl_start() {
	unsafe { sys::acl_start() }
}

/// Returns the length, in bytes, of the Minecraft username of the next allowed user in the ACL.
///
/// If there is no next entry, `None` is returned.
///
/// # Panics
/// This function panics if the underlying syscall fails, because the only reasons it could fail
/// should be impossible due to the type system.
#[must_use = "This function is only useful for its return value"]
pub fn acl_next_len() -> Option<NonZeroUsize> {
	let len =
		// SAFETY: acl_next permits null.
		unsafe{call_buffer_len(sys::acl_next)};
	// Can’t fail because acl_next can only fail due to MemoryFault or StringDecode, and
	// Error::from_isize already treats those as unreachable.
	let len = len.unwrap_or_else(|_| panic_or_trap!("unreachable"));
	NonZeroUsize::new(len)
}

/// Returns the Minecraft username of the next allowed user in the ACL.
///
/// The `buffer` parameter identifies where to store the next username.
///
/// If there is a next entry, the username is written to `buffer`, a string slice referring to it
/// is returned, and the iteration is advanced. If not, `None` is returned.
///
/// # Errors
/// * [`BufferTooShort`](Error::BufferTooShort) is returned if `buffer` is not large enough to hold
///   the component UUID.
///
/// On error, the iteration does not advance.
pub fn acl_next(buffer: &mut [u8]) -> Result<Option<&mut str>> {
	// SAFETY: acl_next permits a writeable buffer pointer/length pair and promises to always write
	// a valid UTF-8 string and return its length.
	let s = unsafe { call_buffer_str(sys::acl_next, buffer) }?;
	Ok(if s.is_empty() { None } else { Some(s) })
}

/// Grants access to the computer to a user.
///
/// The `name` parameter is the Minecraft username of the user to grant access to.
///
/// # Errors
/// * [`Other`](Error::Other) is returned if adding the user failed.
pub fn add_user(name: &str) -> Result<()> {
	// SAFETY: add_user permits a string pointer/length pair.
	unsafe { call_string(sys::add_user, Some(name)) }?;
	Ok(())
}

/// Revokes access to the computer from a user.
///
/// The `name` parameter is the Minecraft username of the user to revoke access from.
///
/// # Errors
/// * [`Other`](Error::Other) is returned if the user is not on the ACL.
pub fn remove_user(name: &str) -> Result<()> {
	// SAFETY: remove_user permits a string pointer/length pair.
	unsafe { call_string(sys::remove_user, Some(name)) }?;
	Ok(())
}

/// Returns the amount of energy stored in the computer and its network.
#[must_use = "This function is only useful for its return value"]
pub fn energy() -> NotNan<f64> {
	// SAFETY: energy is unconditionally safe.
	unsafe { sys::energy() }
}

/// Returns the maximum amount of energy that can be stored in the computer and its network.
#[must_use = "This function is only useful for its return value"]
pub fn max_energy() -> NotNan<f64> {
	// SAFETY: max_energy is unconditionally safe.
	unsafe { sys::max_energy() }
}

/// Returns the width of a Unicode character, in terminal columns.
///
/// The `ch` parameter is the character to examine.
#[must_use = "This function is only useful for its return value"]
pub fn char_width(ch: char) -> u32 {
	// SAFETY: all Unicode scalars fit in a 32-bit integer.
	let ch = ch as u32;
	// SAFETY: char_width is unconditionally safe.
	unsafe { sys::char_width(ch) }
}

/// Plays a beep.
///
/// The `frequency` parameter is the frequency, in Hz, of the beep to play. The `duration`
/// parameter is the length, in milliseconds, of the beep. Both parameters are clamped to a maximum
/// value of 32,767.
pub fn beep(frequency: u16, duration: u16) {
	// SAFETY: beep is unconditionally safe.
	unsafe { sys::beep(frequency.into(), duration.into()) }
}

/// Plays a series of beeps.
///
/// The `pattern` parameter is a Morse code beep pattern to play.
///
/// # Panics
/// This function panics if the underlying syscall fails, because the only reasons it could fail
/// should be impossible due to the type system.
pub fn beep_pattern(pattern: &str) {
	let result =
		// SAFETY: beep_pattern permits a string pointer/length pair.
		unsafe{call_string(sys::beep_pattern, Some(pattern))};
	// Can’t fail because beep_pattern can only fail due to MemoryFault or StringDecode, and
	// Error::from_i32 already treats those as unreachable.
	result.unwrap_or_else(|_| panic_or_trap!("unreachable"));
}

/// Shuts down the computer.
pub fn shutdown() -> ! {
	// SAFETY: shutdown is unconditionally safe.
	unsafe { sys::shutdown() }
}

/// Reboots the computer.
pub fn reboot() -> ! {
	// SAFETY: reboot is unconditionally safe.
	unsafe { sys::reboot() }
}

/// Halts the computer with an error.
pub fn error(error: &str) -> ! {
	// SAFETY: error accepts a string pointer/length pair; it never returns even if the
	// pointer/length pair are null or invalid.
	unsafe { sys::error(error.as_ptr(), error.len()) }
}

/// Sends a message to the debug log, if enabled.
pub fn debug(message: &str) {
	// SAFETY: debug permits a string pointer/length pair.
	let result = unsafe { call_string(sys::debug, Some(message)) };
	// Can’t fail because beep_pattern can only fail due to MemoryFault or StringDecode, and
	// Error::from_i32 already treats those as unreachable.
	result.unwrap_or_else(|_| panic_or_trap!("unreachable"));
}
