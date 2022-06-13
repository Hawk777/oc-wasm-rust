//! Provides high-level access to the capacitor APIs.

use crate::error::Error;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{Lockable, OneValue};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for low-voltage capacitors.
pub const LV_TYPE: &str = "ie_lv_capacitor";

/// The type name for medium-voltage capacitors.
pub const MV_TYPE: &str = "ie_mv_capacitor";

/// The type name for high-voltage capacitors.
pub const HV_TYPE: &str = "ie_hv_capacitor";

/// The type name for creative-voltage capacitors.
pub const CREATIVE_TYPE: &str = "ie_creative_capacitor";

/// A capacitor component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Capacitor(Address);

impl Capacitor {
	/// Creates a wrapper around a capacitor.
	///
	/// The `address` parameter is the address of the capacitor. It is not checked for correctness
	/// at this time because network topology could change after this function returns; as such,
	/// each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the capacitor.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Capacitor {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A capacitor component on which methods can be invoked.
///
/// This type combines a capacitor address, an [`Invoker`](Invoker) that can be used to make method
/// calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this type
/// can be created by calling [`Capacitor::lock`](Capacitor::lock), and it can be dropped to return
/// the borrow of the invoker and buffer to the caller so they can be reused for other purposes.
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
	/// Returns the amount of energy stored in the capacitor.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn energy_stored(&mut self) -> Result<u32, Error> {
		let ret: OneValue<u32> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getEnergyStored",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the maximum amount of energy the capacitor can hold.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn max_energy_stored(&mut self) -> Result<u32, Error> {
		let ret: OneValue<u32> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getMaxEnergyStored",
			None,
		)
		.await?;
		Ok(ret.0)
	}
}
