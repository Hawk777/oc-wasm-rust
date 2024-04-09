use super::error::{Error, Result};
use core::ptr;

/// Calls a function and passes an optional string.
///
/// `f` is the function. `s` is the string.
///
/// # Errors
/// Any error returned by `f` (encoded as a negative integer) is returned by this function.
///
/// # Safety
/// `f` must be safe to call with a string pointer/length pair or with a null pointer and zero
/// length.
pub unsafe fn call_string(
	f: unsafe extern "C" fn(*const u8, usize) -> i32,
	s: Option<&str>,
) -> Result<u32> {
	let (ptr, len) = s.map_or((ptr::null(), 0), |s| (s.as_ptr(), s.len()));
	Error::from_i32(f(ptr, len))
}

/// Calls a function that accepts a buffer pointer/length, passes null, and returns the needed
/// buffer length.
///
/// `f` is the buffer-writing function.
///
/// # Errors
/// Any error returned by `f` (encoded as a negative integer) is returned by this function.
///
/// # Safety
/// `f` must be safe to call with a null pointer and zero length.
pub unsafe fn call_buffer_len(f: unsafe extern "C" fn(*mut u8, usize) -> isize) -> Result<usize> {
	Error::from_isize(f(ptr::null_mut(), 0))
}

/// Calls a function that accepts a buffer pointer/length, passes a slice, and returns the
/// written-to portion of the buffer.
///
/// `f` is the buffer-writing function. `buf` is the buffer.
///
/// # Errors
/// Any error returned by `f` (encoded as a negative integer) is returned by this function.
///
/// # Safety
/// `f` must be safe to call with a buffer pointer and length, and must return the number of bytes
/// written into the buffer.
pub unsafe fn call_buffer(
	f: unsafe extern "C" fn(*mut u8, usize) -> isize,
	buf: &mut [u8],
) -> Result<&mut [u8]> {
	let len = buf.len();
	let ptr = buf.as_mut_ptr();
	let bytes_written = Error::from_isize(f(ptr, len))?;
	Ok(buf.get_unchecked_mut(0..bytes_written))
}

/// Calls a function that accepts a buffer pointer/length, passes a slice, and returns the
/// written-to portion of that buffer as a string.
///
/// `f` is the buffer-writing function. `buf` is the buffer.
///
/// # Errors
/// Any error returned by `f` (encoded as a negative integer) is returned by this function.
///
/// # Safety
/// In addition to the requirements specified by [`call_buffer`](call_buffer), the data written
/// into the buffer by `f` must be UTF-8.
pub unsafe fn call_buffer_str(
	f: unsafe extern "C" fn(*mut u8, usize) -> isize,
	buf: &mut [u8],
) -> Result<&mut str> {
	Ok(core::str::from_utf8_unchecked_mut(call_buffer(f, buf)?))
}
