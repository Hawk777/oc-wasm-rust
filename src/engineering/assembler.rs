//! Provides high-level access to the assembler APIs.

use crate::error::Error;
use minicbor::{Decode, Decoder};
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{
	fluid::Tank,
	inventory::{ItemStack, OptionItemStack},
	Lockable, OneValue, TwoValues,
};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for assembler components.
pub const TYPE: &str = "ie_assembler";

bounded_integer::bounded_integer! {
	/// A recipe slot number.
	pub struct RecipeSlot { 1..=3 }
}

bounded_integer::bounded_integer! {
	/// A tank number.
	pub struct TankNumber { 1..=3 }
}

bounded_integer::bounded_integer! {
	/// An item storage slot number.
	pub struct ItemStorageSlot { 1..=18 }
}

/// An assembler component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Assembler(Address);

impl Assembler {
	/// Creates a wrapper around an assembler.
	///
	/// The `address` parameter is the address of the assembler. It is not checked for correctness
	/// at this time because network topology could change after this function returns; as such,
	/// each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the assembler.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Assembler {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// An assembler component on which methods can be invoked.
///
/// This type combines an assembler address, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`Assembler::lock`](Assembler::lock), and it can be dropped to
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
	/// Returns whether all ingredients (both solid and fluid) required to craft the specified
	/// recipe are present in the assembler.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadRecipe`](Error::BadRecipe)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn has_ingredients(&mut self, slot: RecipeSlot) -> Result<bool, Error> {
		let ret: Result<OneValue<bool>, oc_wasm_safe::component::MethodCallError<'_>> =
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"hasIngredients",
				Some(&OneValue(slot.get())),
			)
			.await;
		match ret {
			Ok(OneValue(ret)) => Ok(ret),
			Err(e @ oc_wasm_safe::component::MethodCallError::BadParameters(exp)) => {
				if exp.is_type("java.lang.IllegalArgumentException") {
					const INVALID_1: &str = "The requested recipe is invalid";
					const INVALID_2: &str = "The Assembler cannot craft this recipe";
					const MAX_INVALID_LEN: usize =
						crate::helpers::max_usize(INVALID_1.len(), INVALID_2.len());
					if let Ok(message) = exp.message(&mut [0_u8; MAX_INVALID_LEN]) {
						if message == INVALID_1 || message == INVALID_2 {
							Err(Error::BadRecipe)
						} else {
							Err(e.into())
						}
					} else {
						Err(e.into())
					}
				} else {
					Err(e.into())
				}
			}
			Err(e) => Err(e.into()),
		}
	}

	/// Returns the recipe in a specified slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn recipe(self, slot: RecipeSlot) -> Result<Recipe<'buffer>, Error> {
		let ret: OneValue<Recipe<'buffer>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getRecipe",
			Some(&OneValue(slot.get())),
		)
		.await?;
		Ok(ret.0)
	}

	/// Checks whether a recipe is properly configured.
	///
	/// A recipe is defined as being “properly configured” if it produces any kind of output.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn is_valid_recipe(&mut self, slot: RecipeSlot) -> Result<bool, Error> {
		let ret: OneValue<bool> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"isValidRecipe",
			Some(&OneValue(slot.get())),
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns a tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn tank(self, tank: TankNumber) -> Result<Tank<'buffer>, Error> {
		let ret: OneValue<Tank<'buffer>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getTank",
			Some(&OneValue(tank.get())),
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the maximum amount of energy the assembler’s internal buffer can hold.
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

	/// Returns the amount of energy stored in the assembler’s internal buffer.
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

	/// Returns the item in the specified storage slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn stack_in_slot(
		self,
		slot: ItemStorageSlot,
	) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getStackInSlot",
			Some(&OneValue(slot.get())),
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Returns the item in the output slot of a recipe.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn buffer_stack(self, slot: RecipeSlot) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getBufferStack",
			Some(&OneValue(slot.get())),
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Enables or disables computer control of the assembler.
	///
	/// If `enable` is `true`, the assembler runs or stops each recipe based on the most recent
	/// call to [`set_enabled`](#set_enabled). If `enable` is `false`, the assembler runs or stops
	/// based on the redstone signal at the control port.
	///
	/// Whenever this function is called with `enable` set to `true`, whether or not the assembler
	/// was previously under computer control, all recipes are enabled. It is necessary to
	/// immediately call [`set_enabled`](#set_enabled) to disable recipes if that is not desired.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn enable_computer_control(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::enable_computer_control(&self.address, self.invoker, self.buffer, enable)
			.await
	}

	/// Enables or disables a recipe.
	///
	/// This should only be called if the assembler is under computer control via a preceding call
	/// to [`enable_computer_control`](#enable_computer_control). If that is not the case, the call
	/// will succeed but have no effect.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn set_enabled(&mut self, slot: RecipeSlot, enable: bool) -> Result<(), Error> {
		component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setEnabled",
			Some(&TwoValues(slot.get(), enable)),
		)
		.await?;
		Ok(())
	}
}

/// A recipe.
///
/// The `'buffer` lifetime is the lifetime of the buffer from which strings in the itemstacks are
/// borrowed.
#[derive(Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Recipe<'buffer> {
	/// The nine input items.
	pub inputs: [Option<ItemStack<'buffer>>; 9],

	/// The output item.
	pub output: Option<ItemStack<'buffer>>,
}

impl<'buffer> Decode<'buffer> for Recipe<'buffer> {
	fn decode(d: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		let len = d.map()?.ok_or_else(|| {
			minicbor::decode::Error::message("indefinite-length maps are not supported")
		})?;
		let mut inputs = [None, None, None, None, None, None, None, None, None];
		let mut output = None;
		for _ in 0..len {
			let key = d.str()?;
			if key.starts_with("in") && key.as_bytes().len() == 3 {
				let index = key.as_bytes()[2];
				if index.is_ascii_digit() && index != b'0' {
					let index = (index - b'1') as usize;
					inputs[index] = OptionItemStack::decode(d)?.into();
				} else {
					d.skip()?;
				}
			} else if key == "out" {
				output = OptionItemStack::decode(d)?.into();
			} else {
				d.skip()?;
			}
		}
		Ok(Self { inputs, output })
	}
}
