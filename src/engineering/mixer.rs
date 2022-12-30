//! Provides high-level access to the mixer APIs.

use crate::{
	common::{ItemStackWithProgress, OptionItemStackWithProgress},
	error::Error,
};
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{fluid::Tank, Lockable, OneValue};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for mixer components.
pub const TYPE: &str = "ie_mixer";

bounded_integer::bounded_integer! {
	/// An input inventory slot.
	pub struct InputSlot { 1..=8 }
}

/// A mixer component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Mixer(Address);

impl Mixer {
	/// Creates a wrapper around a mixer.
	///
	/// The `address` parameter is the address of the mixer. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the mixer.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Mixer {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A mixer component on which methods can be invoked.
///
/// This type combines a mixer address, an [`Invoker`](Invoker) that can be used to make method
/// calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this type
/// can be created by calling [`Mixer::lock`](Mixer::lock), and it can be dropped to return the
/// borrow of the invoker and buffer to the caller so they can be reused for other purposes.
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
	/// Returns the maximum amount of energy the mixer’s internal buffer can hold.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
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

	/// Returns the amount of energy stored in the mixer’s internal buffer.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
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

	/// Returns whether or not the mixer is currently mixing.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
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

	/// Returns the item stack, if any, in an input slot, plus the progress of mixing that item.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn input_stack(
		self,
		slot: InputSlot,
	) -> Result<Option<ItemStackWithProgress<'buffer>>, Error> {
		let ret: OneValue<OptionItemStackWithProgress<'buffer>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getInputStack",
			Some(&OneValue(slot.get())),
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Returns the bottommost fluid in the mixer.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn tank(self) -> Result<Tank<'buffer>, Error> {
		let ret: OneValue<Tank<'buffer>> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "getTank", None)
				.await?;
		Ok(ret.0)
	}

	/// Returns whether or not the items and fluid in the machine are mixable.
	///
	/// This also returns `false` if the items and fluid are mixable, but the mixing process has
	/// not yet started because the machine is disabled or unpowered.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn has_valid_recipe(&mut self) -> Result<bool, Error> {
		use oc_wasm_safe::component::MethodCallError;
		let ret: Result<OneValue<bool>, MethodCallError<'_>> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"isValidRecipe",
			None,
		)
		.await;
		match ret {
			Ok(OneValue(x)) => Ok(x),
			Err(e @ oc_wasm_safe::component::MethodCallError::Other(exp)) => {
				// Immersive Engineering implements this method by checking whether
				// processQueue.get(0).recipe is null or not. If processQueue is empty, this throws
				// an IndexOutOfBoundsException because that condition is not checked.
				if exp.is_type("java.lang.IndexOutOfBoundsException") {
					Ok(false)
				} else {
					Err(e.into())
				}
			}
			Err(e) => Err(e.into()),
		}
	}

	/// Enables or disables computer control of the furnace.
	///
	/// If `enable` is `true`, the furnace runs or stops based on the most recent call to
	/// [`set_enabled`](#set_enabled). If `enable` is `false`, the furnace runs or stops based on
	/// the redstone signal at the control port.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn enable_computer_control(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::enable_computer_control(&self.address, self.invoker, self.buffer, enable)
			.await
	}

	/// Enables or disables the furnace.
	///
	/// This can only be called if the furnace is under computer control via a preceding call to
	/// [`enable_computer_control`](#enable_computer_control).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotComputerControlled`](Error::NotComputerControlled)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_enabled(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::set_enabled(&self.address, self.invoker, self.buffer, enable).await
	}
}
