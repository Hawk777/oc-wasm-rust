//! Provides an error type used for higher level calls.

use core::fmt::{Display, Formatter};

/// The errors that a component call can return.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error {
	/// The call was addressed to a component that does not exist, is not accessible, or is of the
	/// wrong type.
	///
	/// The underlying syscall error is encapsulated.
	BadComponent(oc_wasm_safe::error::Error),

	/// An operation requires a valid recipe, but the configured recipe does not exist or cannot be
	/// used in the machine.
	BadRecipe,

	/// An attempt was made to start or stop a machine that is not under computer control.
	NotComputerControlled,

	/// An attempt was made to perform an operation that takes time to prepare, and it is not ready
	/// yet.
	NotReady,

	/// The operation failed but no more detailed information is available at the type level. The
	/// individual method may document more specific reasons for this error to appear.
	Failed,
}

impl Error {
	/// Returns a string describing the error.
	#[must_use = "This function is only useful for its return value"]
	pub fn as_str(&self) -> &'static str {
		match self {
			Self::BadComponent(_) => "nonexistent or incorrect component",
			Self::BadRecipe => "bad recipe",
			Self::NotComputerControlled => "not computer controlled",
			Self::NotReady => "not ready",
			Self::Failed => "failed",
		}
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), core::fmt::Error> {
		match self {
			Self::BadComponent(e) => write!(f, "nonexistent or incorrect component: {}", e),
			_ => self.as_str().fmt(f),
		}
	}
}

#[cfg(feature = "std")]
impl std::error::Error for Error {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::BadComponent(underlying) => Some(underlying),
			Self::BadRecipe | Self::NotComputerControlled | Self::NotReady | Self::Failed => None,
		}
	}
}

impl From<oc_wasm_safe::error::Error> for Error {
	fn from(source: oc_wasm_safe::error::Error) -> Self {
		match source {
			oc_wasm_safe::error::Error::BadParameters | oc_wasm_safe::error::Error::Other => {
				Self::Failed
			}
			_ => Self::BadComponent(source),
		}
	}
}

impl From<oc_wasm_safe::component::MethodCallError<'_>> for Error {
	fn from(source: oc_wasm_safe::component::MethodCallError<'_>) -> Self {
		source.simplify().into()
	}
}
