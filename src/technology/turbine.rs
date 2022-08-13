//! Provides high-level access to the turbine APIs.
//!
//! The gas turbine and steam turbine expose very similar APIs, so the same wrapper is used for
//! both with a small amount of logic to switch between the two turbines where needed.

use crate::error::Error;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{fluid::Tank, Lockable, OneValue};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for gas turbines.
pub const GAS_TYPE: &str = "it_gas_turbine";

/// The type name for steam turbines.
pub const STEAM_TYPE: &str = "it_steam_turbine";

/// The two kinds of turbines.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Kind {
	Gas,
	Steam,
}

/// A turbine component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Turbine {
	/// The address of the turbine.
	address: Address,

	/// The kind of turbine.
	kind: Kind,
}

impl Turbine {
	/// Creates a wrapper around a turbine.
	///
	/// The `address` parameter is the address of the turbine. The `type` parameter is the type of
	/// the turbine. They are not checked for correctness at this time because network topology
	/// could change after this function returns; as such, each usage of the value may fail
	/// instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address, kind: Kind) -> Self {
		Self { address, kind }
	}

	/// Returns the address of the turbine.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.address
	}

	/// Returns the kind of turbine.
	#[must_use = "This function is only useful for its return value"]
	pub fn kind(&self) -> Kind {
		self.kind
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Turbine {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.address,
			kind: self.kind,
			invoker,
			buffer,
		}
	}
}

/// A turbine component on which methods can be invoked.
///
/// This type combines a turbine address and kind (gas or steam), an [`Invoker`](Invoker) that can
/// be used to make method calls, and a scratch buffer used to perform CBOR encoding and decoding.
/// A value of this type can be created by calling [`Turbine::lock`](Turbine::lock), and it can be
/// dropped to return the borrow of the invoker and buffer to the caller so they can be reused for
/// other purposes.
///
/// The `'invoker` lifetime is the lifetime of the invoker. The `'buffer` lifetime is the lifetime
/// of the buffer. The `B` type is the type of scratch buffer to use.
pub struct Locked<'invoker, 'buffer, B: Buffer> {
	/// The component address.
	address: Address,

	/// The kind of turbine.
	kind: Kind,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut B,
}

impl<'invoker, 'buffer, B: Buffer> Locked<'invoker, 'buffer, B> {
	/// Returns the current rotation speed of the turbine, in RPM.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn speed(&mut self) -> Result<u32, Error> {
		let ret: OneValue<u32> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getSpeed",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the input tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn input_tank(self) -> Result<Tank<'buffer>, Error> {
		let method = match self.kind {
			Kind::Gas => "getInputTankInfo",
			Kind::Steam => "getTankInfo",
		};
		self.tank(method).await
	}

	/// Returns the output tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn output_tank(self) -> Result<Tank<'buffer>, Error> {
		self.tank("getOutputTankInfo").await
	}

	/// Returns a tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	async fn tank(self, method: &str) -> Result<Tank<'buffer>, Error> {
		let ret: OneValue<Tank<'buffer>> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, method, None)
				.await?;
		Ok(ret.0)
	}

	/// Enables or disables computer control of the turbine.
	///
	/// If `enable` is `true`, the turbine runs or stops based on the most recent call to
	/// [`set_enabled`](#set_enabled). If `enable` is `false`, the turbine runs or stops based on
	/// the redstone signal at the control port.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn enable_computer_control(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::enable_computer_control(&self.address, self.invoker, self.buffer, enable)
			.await
	}

	/// Enables or disables the turbine.
	///
	/// This can only be called if the turbine is under computer control via a preceding call to
	/// [`enable_computer_control`](#enable_computer_control).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotComputerControlled`](Error::NotComputerControlled)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn set_enabled(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::set_enabled(&self.address, self.invoker, self.buffer, enable).await
	}
}
