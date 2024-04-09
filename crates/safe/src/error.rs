use crate::panic_or_trap;
use core::fmt::{Display, Formatter};

/// The errors that a system call can return.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Error {
	/// A CBOR data item is invalid CBOR or encodes an unsupported type or value.
	CborDecode,

	/// A buffer provided for the syscall to write into is too short.
	BufferTooShort,

	/// A component UUID refers to a component that does not exist or cannot be accessed.
	NoSuchComponent,

	/// A method invocation refers to a method that does not exist.
	NoSuchMethod,

	/// The parameters are incorrect in a way that does not have a more specific error code.
	BadParameters,

	/// A queue is full.
	QueueFull,

	/// A queue is empty.
	QueueEmpty,

	/// A descriptor is negative or not open.
	BadDescriptor,

	/// There are too many open descriptors.
	TooManyDescriptors,

	/// The operation failed for an otherwise unspecified reason.
	Other,

	/// A system call returned an error code that does not correspond to any known value.
	///
	/// It is likely that OC-Wasm has been updated to a version which adds new error codes, and
	/// OC-Wasm-Safe has not been updated to match.
	Unknown,
}

impl Error {
	/// Returns a string describing the error.
	#[must_use = "This function is only useful for its return value"]
	pub fn as_str(self) -> &'static str {
		match self {
			Self::CborDecode => "CBOR decode error",
			Self::BufferTooShort => "Buffer too short",
			Self::NoSuchComponent => "No such component",
			Self::NoSuchMethod => "No such method",
			Self::BadParameters => "Bad parameters",
			Self::QueueFull => "Queue full",
			Self::QueueEmpty => "Queue empty",
			Self::BadDescriptor => "Bad descriptor",
			Self::TooManyDescriptors => "Too many descriptors",
			Self::Other => "Other error",
			Self::Unknown => "Unknown error",
		}
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
		f.write_str(self.as_str())
	}
}

impl Error {
	/// Checks a system call return value of type `isize` for an error value.
	///
	/// Returns a `Result` containing an `Error` if the value is negative, or the original value if
	/// it was nonnegative.
	///
	/// # Errors
	/// This function fails if the parameter is negative, decoding the represented error code.
	///
	/// # Panics
	/// This function panics if the syscall error code is `MemoryFault` or `StringDecode`. These
	/// syscall errors should be impossible in safe code because the type system prohibits them:
	/// `MemoryFault` should be impossible because all memory regions are taken as slices which are
	/// always valid, and `StringDecode` should be impossible because all strings are taken as
	/// string-slices (`&str`) which are always valid UTF-8.
	pub fn from_isize(value: isize) -> Result<usize> {
		match value {
			-1 => panic_or_trap!("Memory fault"), // Impossible due to memory safety
			-2 => Err(Self::CborDecode),
			-3 => panic_or_trap!("String decode error"), // Impossible due to type safety of &str
			-4 => Err(Self::BufferTooShort),
			-5 => Err(Self::NoSuchComponent),
			-6 => Err(Self::NoSuchMethod),
			-7 => Err(Self::BadParameters),
			-8 => Err(Self::QueueFull),
			-9 => Err(Self::QueueEmpty),
			-10 => Err(Self::BadDescriptor),
			-11 => Err(Self::TooManyDescriptors),
			-12 => Err(Self::Other),
			x if x < 0 => Err(Self::Unknown),
			_ => {
				// Cast from isize to usize is safe because the match arm verifies that x ≥ 0.
				#[allow(clippy::cast_sign_loss)]
				Ok(value as usize)
			}
		}
	}

	/// Checks a system call return value of type `i32` for an error value.
	///
	/// Returns a `Result` containing an `Error` if the value is negative, or the original value if
	/// it was nonnegative.
	///
	/// # Errors
	/// This function fails if the parameter is negative, decoding the represented error code.
	///
	/// # Panics
	/// This function panics if the syscall error code is `MemoryFault` or `StringDecode`. These
	/// syscall errors should be impossible in safe code because the type system prohibits them:
	/// `MemoryFault` should be impossible because all memory regions are taken as slices which are
	/// always valid, and `StringDecode` should be impossible because all strings are taken as
	/// string-slices (`&str`) which are always valid UTF-8.
	pub fn from_i32(value: i32) -> Result<u32> {
		// Cast from i32 to isize is safe because Wasm is a 32-bit target (or more), so isize is at
		// least 32 bits. Cast from usize back to u32 is safe because the value was originally an
		// i32, and from_isize returns an unsigned value.
		#[allow(clippy::cast_possible_truncation)]
		Ok(Self::from_isize(value as isize)? as u32)
	}
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

pub type Result<T> = core::result::Result<T, Error>;
