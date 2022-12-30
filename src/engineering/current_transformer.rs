//! Provides high-level access to the current transformer APIs.

use crate::error::Error;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{Lockable, OneValue};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for current transformers.
pub const TYPE: &str = "ie_current_transformer";

/// A current transformer component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CurrentTransformer(Address);

impl CurrentTransformer {
	/// Creates a wrapper around a current transformer.
	///
	/// The `address` parameter is the address of the current transformer. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the current transformer.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for CurrentTransformer {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A current transformer component on which methods can be invoked.
///
/// This type combines a current transformer address, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`CurrentTransformer::lock`](CurrentTransformer::lock), and it
/// can be dropped to return the borrow of the invoker and buffer to the caller so they can be
/// reused for other purposes.
///
/// The `'invoker` lifetime is the lifetime of the invoker. The `'buffer` lifetime is the lifetime
/// of the buffer. The `B` type is the type of scratch buffer to use.
pub struct Locked<'invoker, 'buffer, B: Buffer> {
	/// The component address.
	address: Address,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut B,
}

impl<'invoker, 'buffer, B: Buffer> Locked<'invoker, 'buffer, B> {
	/// Returns the power flow through the current transformer.
	///
	/// The returned value is measured in flux per tick and is averaged over the last twenty ticks.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn power(&mut self) -> Result<u32, Error> {
		let ret: OneValue<u32> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getAvgEnergy",
			None,
		)
		.await?;
		Ok(ret.0)
	}
}
