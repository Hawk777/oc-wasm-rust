//! Provides an error type used for higher level calls.

use alloc::{borrow::ToOwned, string::String};
use core::fmt::{Display, Formatter};

/// The errors that a component call can return.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error {
	/// The call was addressed to a component that does not exist, is not accessible, or is of the
	/// wrong type.
	///
	/// The underlying syscall error is encapsulated.
	BadComponent(oc_wasm_safe::error::Error),

	/// The method was invoked successfully, but failed during execution and provided a detail
	/// message.
	Failed(String),
}

impl Error {
	/// Returns a string describing the error.
	#[must_use = "This function is only useful for its return value"]
	pub fn as_str(&self) -> &str {
		match self {
			Self::BadComponent(_) => "nonexistent or incorrect component",
			Self::Failed(reason) => reason,
		}
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), core::fmt::Error> {
		self.as_str().fmt(f)
	}
}

#[cfg(feature = "std")]
impl std::error::Error for Error {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::BadComponent(underlying) => Some(underlying),
			Self::Failed(_) => None,
		}
	}
}

impl From<oc_wasm_safe::error::Error> for Error {
	fn from(source: oc_wasm_safe::error::Error) -> Self {
		match source {
			oc_wasm_safe::error::Error::BadParameters
			| oc_wasm_safe::error::Error::TooManyDescriptors
			| oc_wasm_safe::error::Error::Other => Self::Failed(source.as_str().to_owned()),
			_ => Self::BadComponent(source),
		}
	}
}
