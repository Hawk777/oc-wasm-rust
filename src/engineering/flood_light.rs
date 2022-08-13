//! Provides high-level access to the flood light APIs.

use crate::error::Error;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{Lockable, OneValue};
use oc_wasm_safe::{
	component::{Invoker, MethodCallError},
	Address,
};

/// The type name for flood lights.
pub const TYPE: &str = "ie_floodlight";

/// A flood light component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FloodLight(Address);

impl FloodLight {
	/// Creates a wrapper around a flood light.
	///
	/// The `address` parameter is the address of the flood light. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the flood light.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for FloodLight {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A flood light component on which methods can be invoked.
///
/// This type combines a flood light address, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`FloodLight::lock`](FloodLight::lock), and it can be dropped to
/// return the borrow of the invoker and buffer to the caller so they can be reused for other
/// purposes.
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
	/// Returns the maximum amount of energy the flood light’s internal buffer can hold.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
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

	/// Returns the amount of energy stored in the flood light’s internal buffer.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
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

	/// Pitches the flood light up or down.
	///
	/// The `direction` parameter is `false` to pitch the light up or `true` to pitch the light
	/// down. The elevation axis aims the light towards or away from its base; this may or may not
	/// be in the world’s Y axis depending on how the light is mounted. When initially placed, “up”
	/// is away from the base and “down” is towards the base.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotReady`](Error::NotReady) is returned if the light is shining and was rotated very
	///   recently.
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn turn_elevation(&mut self, direction: bool) -> Result<(), Error> {
		self.turn("turnAroundXZ", direction).await
	}

	/// Yaws the flood light left or right.
	///
	/// The `direction` parameter is `false` to yaw the light negative or `true` to yaw the light
	/// positive. The azimuth axis spins the light on its base; this may or may not be in the
	/// world’s X/Z axis depending on how the light is mounted. When initially placed, negative is
	/// counterclockwise as viewed looking down on the light from opposite the base.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotReady`](Error::NotReady) is returned if the light is shining and was rotated very
	///   recently.
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn turn_azimuth(&mut self, direction: bool) -> Result<(), Error> {
		self.turn("turnAroundY", direction).await
	}

	/// Turns the flood light.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotReady`](Error::NotReady) is returned if the light is shining and was rotated very
	///   recently.
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	async fn turn(&mut self, method: &str, direction: bool) -> Result<(), Error> {
		let ret = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			Some(&OneValue(direction)),
		)
		.await;
		match ret {
			Ok(()) => Ok(()),
			Err(e @ MethodCallError::BadParameters(exp)) => {
				const MESSAGE: &str = "The floodlight can't turn again yet.";
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

	/// Checks whether the flood light is ready to turn.
	///
	/// When the light is shining, it can only be turned slowly; after each step, this method will
	/// return `false` for a little while. When the light is not shining, it can be turned quickly.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn can_turn(&mut self) -> Result<bool, Error> {
		let ret: OneValue<bool> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "canTurn", None)
				.await?;
		Ok(ret.0)
	}

	/// Enables or disables the flood light.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn set_enabled(&mut self, enable: bool) -> Result<(), Error> {
		component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setEnabled",
			Some(&OneValue(enable)),
		)
		.await?;
		Ok(())
	}

	/// Returns whether or not the flood light is currently shining.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn is_active(&mut self) -> Result<bool, Error> {
		let ret: OneValue<bool> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"isActive",
			None,
		)
		.await?;
		Ok(ret.0)
	}
}
