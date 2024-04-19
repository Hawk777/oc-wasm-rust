//! Provides high-level access to the Tesla coil APIs.

use crate::error::Error;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::Lockable;
use oc_wasm_safe::{
	component::{Invoker, MethodCallError},
	Address,
};

/// The type name for Tesla coils.
pub const TYPE: &str = "ie_tesla_coil";

/// A redstone signal polarity.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SignalLevel {
	/// The Tesla coil activates when it receives a high (nonzero) redstone signal.
	High,

	/// The Tesla coil activates when it receives a low (zero) redstone signal.
	Low,
}

/// A Tesla coil power level.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum PowerLevel {
	/// The Tesla coil charges to its full energy. The configured standard amount of power is
	/// consumed. Entities within six metres are injured and fluorescent tubes within nine metres
	/// glow. Faraday suits melt when hit.
	High,

	/// The Tesla coil charges to a lower energy level. Half of the configured standard amount of
	/// power is consumed. Entities within three metres are injured and fluorescent tubes within 4½
	/// metres glow. Faraday suits protect the wearer from all damage.
	Low,
}

/// A Tesla coil component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TeslaCoil(Address);

impl TeslaCoil {
	/// Creates a wrapper around a Tesla coil.
	///
	/// The `address` parameter is the address of the Tesla coil. It is not checked for correctness
	/// at this time because network topology could change after this function returns; as such,
	/// each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the Tesla coil.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for TeslaCoil {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A Tesla coil component on which methods can be invoked.
///
/// This type combines a Tesla coil address, an [`Invoker`] that can be used to make method calls,
/// and a scratch buffer used to perform CBOR encoding and decoding. A value of this type can be
/// created by calling [`TeslaCoil::lock`], and it can be dropped to return the borrow of the
/// invoker and buffer to the caller so they can be reused for other purposes.
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
	/// Returns the maximum amount of energy the Tesla coil’s internal buffer can hold.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn max_energy_stored(&mut self) -> Result<u32, Error> {
		let ret: (u32,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getMaxEnergyStored",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the amount of energy stored in the Tesla coil’s internal buffer.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn energy_stored(&mut self) -> Result<u32, Error> {
		let ret: (u32,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getEnergyStored",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns whether or not the Tesla coil is currently energized.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn is_active(&mut self) -> Result<bool, Error> {
		let ret: (bool,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"isActive",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Sets which redstone level must be received to energize the Tesla coil.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_redstone_polarity(&mut self, level: SignalLevel) -> Result<(), Error> {
		component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setRSMode",
			Some(&(level == SignalLevel::Low,)),
		)
		.await?;
		Ok(())
	}

	/// Sets which power level the Tesla coil should energize at.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotReady`](Error::NotReady) is returned if the Tesla coil is energized.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_power(&mut self, level: PowerLevel) -> Result<(), Error> {
		let ret: Result<(), MethodCallError<'_>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setPowerMode",
			Some(&(level == PowerLevel::High,)),
		)
		.await;
		match ret {
			Ok(()) => Ok(()),
			Err(e @ MethodCallError::BadParameters(exp)) => {
				const MESSAGE: &str = "Can't switch power mode on an active coil";
				let mut message_buffer = [0_u8; MESSAGE.len()];
				if exp.message(&mut message_buffer) == Ok(MESSAGE) {
					Err(Error::NotReady)
				} else {
					Err(e.into())
				}
			}
			Err(e) => Err(e.into()),
		}
	}
}
