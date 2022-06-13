//! Provides high-level access to the bottling machine APIs.

use crate::error::Error;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{fluid::Tank, Lockable, OneValue};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for bottling machine components.
pub const TYPE: &str = "ie_bottling_machine";

/// A bottling machine component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BottlingMachine(Address);

impl BottlingMachine {
	/// Creates a wrapper around a bottling machine.
	///
	/// The `address` parameter is the address of the bottling machine. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the bottling machine.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for BottlingMachine {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A bottling machine component on which methods can be invoked.
///
/// This type combines a bottling machine address, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`BottlingMachine::lock`](BottlingMachine::lock), and it can be
/// dropped to return the borrow of the invoker and buffer to the caller so they can be reused for
/// other purposes.
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
	/// Returns the amount of energy stored in the bottling machine’s internal buffer.
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

	/// Returns the maximum amount of energy the bottling machine’s internal buffer can hold.
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

	/// Returns the bottling machine’s fluid tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn tank(self) -> Result<Tank<'buffer>, Error> {
		let ret: OneValue<Tank<'buffer>> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "getTank", None)
				.await?;
		Ok(ret.0)
	}

	/// Enables or disables computer control of the bottling machine.
	///
	/// If `enable` is `true`, the bottling machine runs or stops based on the most recent call to
	/// [`set_enabled`](#set_enabled). If `enable` is `false`, the bottling machine runs or stops
	/// based on the redstone signal at the control port.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn enable_computer_control(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::enable_computer_control(&self.address, self.invoker, self.buffer, enable)
			.await
	}

	/// Enables or disables the bottling machine.
	///
	/// This can only be called if the bottling machine is under computer control via a preceding
	/// call to [`enable_computer_control`](#enable_computer_control).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotComputerControlled`](Error::NotComputerControlled)
	pub async fn set_enabled(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::set_enabled(&self.address, self.invoker, self.buffer, enable).await
	}
}
