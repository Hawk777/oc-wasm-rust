//! Provides high-level access to the heat exchanger APIs.

use crate::{common::Tank, error::Error};
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::Lockable;
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for heat exchangers.
pub const TYPE: &str = "it_heat_exchanger";

bounded_integer::bounded_integer! {
	/// A tank index.
	pub struct TankNumber { 1..=2 }
}

/// A heat exchanger component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct HeatExchanger(Address);

impl HeatExchanger {
	/// Creates a wrapper around a heat exchanger.
	///
	/// The `address` parameter is the address of the heat exchanger. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the heat exchanger.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for HeatExchanger {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A heat exchanger component on which methods can be invoked.
///
/// This type combines a heat exchanger address, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`HeatExchanger::lock`](HeatExchanger::lock), and it can be
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
	/// Returns an input tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn input_tank(self, number: TankNumber) -> Result<Tank<'buffer>, Error> {
		let method = if number.get() == 1 {
			"getFirstInputTankInfo"
		} else {
			"getSecondInputTankInfo"
		};
		self.tank(method).await
	}

	/// Returns an output tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn output_tank(self, number: TankNumber) -> Result<Tank<'buffer>, Error> {
		let method = if number.get() == 1 {
			"getFirstOutputTankInfo"
		} else {
			"getSecondOutputTankInfo"
		};
		self.tank(method).await
	}

	/// Returns a tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	async fn tank(self, method: &str) -> Result<Tank<'buffer>, Error> {
		let ret: (Tank<'buffer>,) =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, method, None)
				.await?;
		Ok(ret.0)
	}

	/// Enables or disables computer control of the heat exchanger.
	///
	/// If `enable` is `true`, the heat exchanger runs or stops based on the most recent call to
	/// [`set_enabled`](#set_enabled). If `enable` is `false`, the heat exchanger runs or stops
	/// based on the redstone signal at the control port.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn enable_computer_control(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::enable_computer_control(&self.address, self.invoker, self.buffer, enable)
			.await
	}

	/// Enables or disables the heat exchanger.
	///
	/// This can only be called if the heat exchanger is under computer control via a preceding
	/// call to [`enable_computer_control`](#enable_computer_control).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotComputerControlled`](Error::NotComputerControlled)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_enabled(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::set_enabled(&self.address, self.invoker, self.buffer, enable).await
	}
}
