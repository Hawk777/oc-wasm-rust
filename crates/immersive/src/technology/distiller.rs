//! Provides high-level access to the distiller APIs.

use crate::{
	common::{ItemStack, Tank},
	error::Error,
};
use minicbor::{Decode, Decoder};
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{inventory::OptionItemStack, map_decoder, Lockable};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for distillers.
pub const TYPE: &str = "it_distiller";

/// A distiller component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Distiller(Address);

impl Distiller {
	/// Creates a wrapper around a distiller.
	///
	/// The `address` parameter is the address of the distiller. It is not checked for correctness
	/// at this time because network topology could change after this function returns; as such,
	/// each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the distiller.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Distiller {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A distiller component on which methods can be invoked.
///
/// This type combines a distiller address, an [`Invoker`] that can be used to make method calls,
/// and a scratch buffer used to perform CBOR encoding and decoding. A value of this type can be
/// created by calling [`Distiller::lock`], and it can be dropped to return the borrow of the
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
	/// Returns the input (water) tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn input_tank(self) -> Result<Tank<'buffer>, Error> {
		self.tank("getInputTankInfo").await
	}

	/// Returns the output (steam) tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn output_tank(self) -> Result<Tank<'buffer>, Error> {
		self.tank("getOutputTankInfo").await
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

	/// Returns the amount of energy stored in the distiller’s internal buffer.
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

	/// Returns the maximum amount of energy the distiller’s internal buffer can hold.
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

	/// Returns the item stacks in the unused canisters slots.
	///
	/// The unused canisters are the input canisters that are full (have not been emptied yet) and
	/// the output canisters that are empty (have not been filled yet).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn unused_canisters(self) -> Result<Canisters<'buffer>, Error> {
		// Yes, the component call is “getEmptyCanisters”. It doesn’t return the empties. It
		// returns the unused ones, which are full for the input tank.
		self.canisters("getEmptyCanisters").await
	}

	/// Returns the item stacks in the used canisters slots.
	///
	/// The used canisters are the input canisters that have been emptied and the output canisters
	/// that have been filled.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn used_canisters(self) -> Result<Canisters<'buffer>, Error> {
		// Yes, the component call is “getFullCanisters”. It doesn’t return the full canisters. It
		// returns the used ones, which are empty for the input tank.
		self.canisters("getFullCanisters").await
	}

	/// Returns the item stacks in canisters slots.
	///
	/// # Errors
	/// * [`BadComponents`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	async fn canisters(self, method: &str) -> Result<Canisters<'buffer>, Error> {
		let ret: (Canisters<'buffer>,) =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, method, None)
				.await?;
		Ok(ret.0)
	}

	/// Enables or disables computer control of the distiller.
	///
	/// If `enable` is `true`, the distiller runs or stops based on the most recent call to
	/// [`set_enabled`](#set_enabled). If `enable` is `false`, the distiller runs or stops based on
	/// the redstone signal at the control port.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn enable_computer_control(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::enable_computer_control(&self.address, self.invoker, self.buffer, enable)
			.await
	}

	/// Enables or disables the distiller.
	///
	/// This can only be called if the distiller is under computer control via a preceding call to
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

/// A collection of canisters in either the unused or used canisters slots.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Canisters<'buffer> {
	/// The canisters for the input tank.
	pub input: Option<ItemStack<'buffer>>,

	/// The canisters for the output tank.
	pub output: Option<ItemStack<'buffer>>,
}

impl<'buffer, Context> Decode<'buffer, Context> for Canisters<'buffer> {
	fn decode(
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
		map_decoder::decode::<CanistersBuilder<'buffer>, _>(d, context)
	}
}

/// A map-decoding builder for a [`Canisters`].
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CanistersBuilder<'buffer> {
	/// The canisters for the input tank.
	input: Option<ItemStack<'buffer>>,

	/// The canisters for the output tank.
	output: Option<ItemStack<'buffer>>,
}

impl<'buffer> map_decoder::Builder<'buffer> for CanistersBuilder<'buffer> {
	type Output = Canisters<'buffer>;

	fn entry<Context>(
		&mut self,
		key: &str,
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<bool, minicbor::decode::Error> {
		if key == "input" {
			self.input = d
				.decode_with::<_, OptionItemStack<'buffer>>(context)?
				.into();
			Ok(true)
		} else if key == "output" {
			self.output = d
				.decode_with::<_, OptionItemStack<'buffer>>(context)?
				.into();
			Ok(true)
		} else {
			Ok(false)
		}
	}

	fn build(self) -> Result<Canisters<'buffer>, minicbor::decode::Error> {
		Ok(Canisters {
			input: self.input,
			output: self.output,
		})
	}
}
