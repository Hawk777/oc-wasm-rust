//! Provides high-level access to the arc furnace APIs.

use crate::{
	common::{ItemStackWithProgress, OptionItemStackWithProgress},
	error::Error,
};
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{
	inventory::{ItemStack, OptionItemStack},
	Lockable, OneValue,
};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for arc furnace components.
pub const TYPE: &str = "ie_arc_furnace";

bounded_integer::bounded_integer! {
	/// An additive inventory slot.
	pub struct AdditiveSlot { 1..=4 }
}

bounded_integer::bounded_integer! {
	/// An electrode slot.
	pub struct ElectrodeSlot { 1..=3 }
}

bounded_integer::bounded_integer! {
	/// An input inventory slot.
	pub struct InputSlot { 1..=12 }
}

bounded_integer::bounded_integer! {
	/// An output inventory slot.
	pub struct OutputSlot { 1..=6 }
}

/// An arc furnace component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ArcFurnace(Address);

impl ArcFurnace {
	/// Creates a wrapper around an arc furnace.
	///
	/// The `address` parameter is the address of the arc furnace. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the arc furnace.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for ArcFurnace {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// An arc furnace component on which methods can be invoked.
///
/// This type combines an arc furnace address, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`ArcFurnace::lock`](ArcFurnace::lock), and it can be dropped to
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
	/// Returns the maximum amount of energy the arc furnace’s internal buffer can hold.
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

	/// Returns the amount of energy stored in the arc furnace’s internal buffer.
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

	/// Returns whether or not the arc furnace is currently smelting something.
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

	/// Returns the item stack, if any, in an input slot, plus the progress of smelting that item.
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

	/// Returns the item stack, if any, in an output slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn output_stack(self, slot: OutputSlot) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getOutputStack",
			Some(&OneValue(slot.get())),
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Returns the item stack, if any, in an additive slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn additive_stack(
		self,
		slot: AdditiveSlot,
	) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getAdditiveStack",
			Some(&OneValue(slot.get())),
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Returns the item stack, if any, in the slag slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn slag_stack(self) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getSlagStack",
			None,
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Returns whether all three electrodes are present.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn has_electrodes(&mut self) -> Result<bool, Error> {
		let ret: OneValue<bool> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"hasElectrodes",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the item stack, if any, in an electrode slot
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn electrode(self, slot: ElectrodeSlot) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getElectrode",
			Some(&OneValue(slot.get())),
		)
		.await?;
		Ok(ret.0.into())
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
