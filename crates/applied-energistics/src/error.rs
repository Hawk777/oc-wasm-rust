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

	/// An index number is outside the range of slots for an inventory.
	BadInventorySlot,

	/// A crafting operation cannot be cancelled because it has already finished.
	Completed,

	/// A request was made to examine the result of a crafting operation, but the crafting
	/// operation has not been initiated yet so the information is not available.
	///
	/// This should persist for only a very short time.
	Computing,

	/// A request was made to craft items, but the items cannot be crafted because a resource is
	/// missing.
	///
	/// This usually indicates insufficient stock of a required input item, but it can also occur
	/// in certain other situations, such as if there are free CPUs or if the crafting pattern has
	/// been removed from the ME interface prior to the crafting request being submitted.
	MissingResources,

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
			Self::BadInventorySlot => "invalid slot",
			Self::Completed => "job already completed",
			Self::Computing => "computing",
			Self::MissingResources => "missing resources",
			Self::Failed => "failed",
		}
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), core::fmt::Error> {
		match self {
			Self::BadComponent(e) => write!(f, "nonexistent or incorrect component: {e}"),
			_ => self.as_str().fmt(f),
		}
	}
}

#[cfg(feature = "std")]
impl std::error::Error for Error {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::BadComponent(underlying) => Some(underlying),
			Self::BadInventorySlot
			| Self::Completed
			| Self::Computing
			| Self::MissingResources
			| Self::Failed => None,
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
