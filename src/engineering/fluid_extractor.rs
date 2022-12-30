//! Provides high-level access to the fermenter and squeezer APIs.
//!
//! The fermenter and squeezer expose identical APIs, so the same wrapper is used for both.

use crate::error::Error;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{
	fluid::{Fluid, OptionFluid, Tank},
	inventory::{ItemStack, OptionItemStack},
	Lockable, OneValue,
};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for fermenters.
pub const FERMENTER_TYPE: &str = "ie_fermenter";

/// The type name for squeezers.
pub const SQUEEZER_TYPE: &str = "ie_squeezer";

bounded_integer::bounded_integer! {
	/// An input slot number.
	pub struct InputSlot { 1..=8 }
}

/// A fluid extractor component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Fermenter(Address);

impl Fermenter {
	/// Creates a wrapper around a fluid extractor.
	///
	/// The `address` parameter is the address of the fluid extractor. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the fluid extractor.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Fermenter {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A fluid extractor component on which methods can be invoked.
///
/// This type combines a fluid extractor address, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`Fermenter::lock`](Fermenter::lock), and it can be dropped to
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
	/// Returns the recipe being executed for a specified input item slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn recipe(self, slot: InputSlot) -> Result<Option<Recipe<'buffer>>, Error> {
		struct Return<'buffer> {
			input: Option<ItemStack<'buffer>>,
			output: Option<ItemStack<'buffer>>,
			fluid: Option<Fluid<'buffer>>,
			process_time: u32,
		}
		impl<'buffer, Context> minicbor::Decode<'buffer, Context> for Return<'buffer> {
			fn decode(
				d: &mut minicbor::Decoder<'buffer>,
				context: &mut Context,
			) -> Result<Self, minicbor::decode::Error> {
				let len = d.array()?.ok_or_else(|| {
					minicbor::decode::Error::message("indefinite-length arrays are not supported")
				})?;
				if len == 0 {
					Ok(Self {
						input: None,
						output: None,
						fluid: None,
						process_time: 0,
					})
				} else if len == 4 {
					Ok(Self {
						input: Some(d.decode_with::<_, ItemStack<'buffer>>(context)?),
						output: d
							.decode_with::<_, OptionItemStack<'buffer>>(context)?
							.into(),
						fluid: d.decode_with::<_, OptionFluid<'buffer>>(context)?.into(),
						process_time: d.u32()?,
					})
				} else {
					Err(minicbor::decode::Error::message("incorrect array length"))
				}
			}
		}

		let ret: Return<'buffer> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getRecipe",
			Some(&OneValue(slot.get())),
		)
		.await?;
		if let Some(input) = ret.input {
			Ok(Some(Recipe {
				input,
				output: ret.output,
				fluid: ret.fluid,
				process_time: ret.process_time,
			}))
		} else {
			Ok(None)
		}
	}

	/// Returns the item stack in a specified input slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn input_stack(self, slot: InputSlot) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getInputStack",
			Some(&OneValue(slot.get())),
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Returns the item stack in the output slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn output_stack(self) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getOutputStack",
			None,
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Returns the fluid output tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn fluid(self) -> Result<Tank<'buffer>, Error> {
		let ret: OneValue<Tank<'buffer>> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getFluid",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the item stack in the empty canisters slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn empty_canisters(self) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getEmptyCannisters",
			None,
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Returns the item stack in the full canisters slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn full_canisters(self) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getFilledCannisters",
			None,
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Returns the maximum amount of energy the fluid extractor’s internal buffer can hold.
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

	/// Returns the amount of energy stored in the fluid extractor’s internal buffer.
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

	/// Returns whether or not the fluid extractor is currently working.
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

	/// Enables or disables computer control of the fluid extractor.
	///
	/// If `enable` is `true`, the fluid extractor runs or stops based on the most recent call to
	/// [`set_enabled`](#set_enabled). If `enable` is `false`, the fluid extractor runs or stops
	/// based on the redstone signal at the control port.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn enable_computer_control(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::enable_computer_control(&self.address, self.invoker, self.buffer, enable)
			.await
	}

	/// Enables or disables the fluid extractor.
	///
	/// This can only be called if the fluid extractor is under computer control via a preceding
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

/// A recipe the fluid extractor can execute.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Recipe<'buffer> {
	/// The item stack in the input slot.
	///
	/// The size of this item stack reflects the number of items physically present.
	pub input: ItemStack<'buffer>,

	/// The item produced as output, if any.
	///
	/// The size of this item stack reflects the number of items produced per input item consumed.
	pub output: Option<ItemStack<'buffer>>,

	/// The fluid produced as output, if any.
	///
	/// The amount of this fluid reflects the amount produced per input item consumed.
	pub fluid: Option<Fluid<'buffer>>,

	/// The number of ticks taken to process one input item.
	pub process_time: u32,
}
