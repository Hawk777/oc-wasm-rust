//! Provides high-level access to the inventory controller, fluid tank controller, and transposer
//! APIs.
//!
//! # Limitations
//! At this time, database interaction APIs are not supported, and a very few other miscellaneous
//! APIs are also missing. They may be added in a future version.

use crate::common::{RelativeSide, Side};
use crate::error::Error;
use crate::helpers::{max_of_usizes, Ignore};
use alloc::vec::Vec;
use core::num::NonZeroU32;
use minicbor::{Decode, Decoder};
use oc_wasm_futures::invoke::{component_method, value_method, Buffer};
use oc_wasm_helpers::{
	error::NullAndStringOr,
	fluid::{Fluid, Tank},
	inventory::{ItemStack, OptionItemStack},
	FiveValues, FourValues, Lockable, OneValue, ThreeValues, TwoValues,
};
use oc_wasm_safe::{
	component::{Invoker, MethodCallError},
	descriptor, Address,
};

pub use super::robot::ActionSide;

/// The type name for inventory controller components, which can read solid inventory contents and
/// move items around using an internal inventory (for robots and drones only, not adapters), but
/// cannot operate on fluids.
pub const INVENTORY_CONTROLLER_TYPE: &str = "inventory_controller";

/// The type name for tank controller components, which can read fluid tank contents and move
/// fluids around using internal tanks (for robots and drones only, not adapters), but cannot
/// operate on solid items other than moving fluid into and out of containers in the internal
/// inventory (for robots and drones only, not adapters).
pub const TANK_CONTROLLER_TYPE: &str = "tank_controller";

/// The type name for transposer components, which can read both solid inventory and fluid tank
/// contents as well as move both items and fluids between containers.
pub const TRANSPOSER_TYPE: &str = "transposer";

/// An inventory controller component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Controller(Address);

impl Controller {
	/// Creates a wrapper around an inventory controller.
	///
	/// The `address` parameter is the address of the controller. It is not checked for correctness
	/// at this time because network topology could change after this function returns; as such,
	/// each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the controller.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Controller {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// An inventory controller component on which methods can be invoked.
///
/// This type combines an inventory controller address, an [`Invoker`](Invoker) that can be used to
/// make method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of
/// this type can be created by calling [`Controller::lock`](Controller::lock), and it can be
/// dropped to return the borrow of the invoker and buffer to the caller so they can be reused for
/// other purposes.
///
/// Where a function is declared as taking [`impl Side`](Side), an
/// [`AbsoluteSide`](super::common::AbsoluteSide) must be passed if operating on a transposer or an
/// upgrade module installed in an adapter, while a [`RelativeSide`](super::common::RelativeSide)
/// must be passed if operating on an upgrade module installed in a robot or drone.
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
	// WorldInventoryAnalytics

	/// Returns the number of slots in an inventory.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_inventory_size(&mut self, side: impl Side) -> Result<u32, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getInventorySize",
				Some(&OneValue(side)),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns the number of items in an inventory slot.
	///
	/// The `slot` parameter ranges from 1 to the inventory size.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_slot_stack_size(
		&mut self,
		side: impl Side,
		slot: NonZeroU32,
	) -> Result<u32, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getSlotStackSize",
				Some(&TwoValues(side, slot)),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns the maximum size of a stack of items, given the item in an inventory slot.
	///
	/// The `slot` parameter ranges from 1 to the inventory size. If the slot does not contain any
	/// items, `None` is returned because the maximum stack size depends on the item type.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_slot_max_stack_size(
		&mut self,
		side: impl Side,
		slot: NonZeroU32,
	) -> Result<Option<NonZeroU32>, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getSlotMaxStackSize",
				Some(&TwoValues(side, slot)),
			)
			.await,
		)?;
		Ok(NonZeroU32::new(ret.0))
	}

	/// Checks whether two inventory slots contain the same type of item.
	///
	/// The `slot_a` and `slot_b` parameters range from 1 to the inventory size. The `check_nbt`
	/// parameter indicates whether to consider NBT data attached to the item.
	///
	/// Two empty slots are considered equal. An empty slot and a populated slot are considered
	/// unequal. Two of the same item with different damage values are considered equal. Two stacks
	/// of different numbers of the same item are considered equal.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn compare_stacks(
		&mut self,
		side: impl Side,
		slot_a: NonZeroU32,
		slot_b: NonZeroU32,
		check_nbt: bool,
	) -> Result<bool, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"compareStacks",
				Some(&FourValues(side, slot_a, slot_b, check_nbt)),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Checks whether two inventory slots contain ore-dictionary-equivalent items.
	///
	/// The `slot_a` and `slot_b` parameters range from 1 to the inventory size.
	///
	/// Two empty slots are considered equivalent. An empty slot and a populated slot are
	/// considered non-equivalent. A slot is considered equivalent to itself. Otherwise, two slots
	/// are considered equivalent if and only if the items in both slots have at least one ore
	/// dictionary “ore ID” entry in common.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn are_stacks_equivalent(
		&mut self,
		side: impl Side,
		slot_a: NonZeroU32,
		slot_b: NonZeroU32,
	) -> Result<bool, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"areStacksEquivalent",
				Some(&ThreeValues(side, slot_a, slot_b)),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns the item stack in an inventory slot.
	///
	/// The `slot` parameter ranges from 1 to the inventory size. If the slot does not contain any
	/// items, `None` is returned.
	///
	/// The strings in the returned item stack point into, and therefore retain ownership of, the
	/// scratch buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported) if detailed item information is disabled in the
	///   config file.
	pub async fn get_stack_in_slot(
		self,
		side: impl Side,
		slot: NonZeroU32,
	) -> Result<Option<ItemStack<'buffer>>, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getStackInSlot",
				Some(&TwoValues(side, slot)),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns a snapshot of all the item stacks in an inventory.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported) if detailed item information is disabled in the
	///   config file.
	pub async fn get_all_stacks(&mut self, side: impl Side) -> Result<Snapshot, Error> {
		let side: u8 = side.into();
		let ret: OneValue<descriptor::Decoded> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getAllStacks",
				Some(&OneValue(side)),
			)
			.await,
		)?;
		let descriptor = ret.0;
		// SAFETY: This descriptor was just generated by the getAllStacks() method call, so it must
		// be fresh and unique.
		let descriptor = unsafe { descriptor.into_owned() };
		Ok(Snapshot(descriptor))
	}

	/// Returns the internal (Minecraft system) name of an inventory.
	///
	/// For example, this might be `minecraft:chest`.
	///
	/// The returned string points into, and therefore retains ownership of, the scratch buffer.
	/// Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported) if detailed item information is disabled in the
	///   config file.
	pub async fn get_inventory_name(self, side: impl Side) -> Result<&'buffer str, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getInventoryName",
				Some(&OneValue(side)),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	// InventoryTransfer

	/// Moves items between two inventories.
	///
	/// The `count` value indicates the maximum number of items to move. It is clamped to 64; even
	/// if a stack is larger than 64 items, no more than 64 can be moved in a single operation.
	///
	/// The transfer will only ever move items from one slot in the source inventory; the chosen
	/// slot is the first slot from which any items can actually be moved (that is, the first slot
	/// that is nonempty and at least one item of which fits in the sink). The transfer may place
	/// items into multiple slots in the sink; it first merges items into existing stacks of the
	/// same type, then, if any items are left to move, places them into empty slots, in both cases
	/// prioritizing based on the order of slots in the sink inventory. The number of items
	/// actually moved is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn transfer_item<SideType: Side>(
		&mut self,
		source: SideType,
		sink: SideType,
		count: u32,
	) -> Result<u32, Error> {
		let source: u8 = source.into();
		let sink: u8 = sink.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"transferItem",
				Some(&ThreeValues(source, sink, count)),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Moves items between two inventories, taking from only a specific slot in the source.
	///
	/// The `count` value indicates the maximum number of items to move. It is clamped to 64; even
	/// if a stack is larger than 64 items, no more than 64 can be moved in a single operation.
	///
	/// The `source_slot` parameter ranges from 1 to the source inventory size.
	///
	/// The transfer may place items into multiple slots in the sink; it first merges items into
	/// existing stacks of the same type, then, if any items are left to move, places them into
	/// empty slots, in both cases prioritizing based on the order of slots in the sink inventory.
	/// The number of items actually moved is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn transfer_item_from_slot<SideType: Side>(
		&mut self,
		source: SideType,
		sink: SideType,
		count: u32,
		source_slot: NonZeroU32,
	) -> Result<u32, Error> {
		let source: u8 = source.into();
		let sink: u8 = sink.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"transferItem",
				Some(&FourValues(source, sink, count, source_slot.get())),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Moves items between two inventories, taking from only a specific slot in the source and
	/// storing to only a specific slot in the sink.
	///
	/// The `count` value indicates the maximum number of items to move. It is clamped to 64; even
	/// if a stack is larger than 64 items, no more than 64 can be moved in a single operation.
	///
	/// The `source_slot` parameter ranges from 1 to the source inventory size. The `sink_slot`
	/// parameter ranges from 1 to the sink inventory size.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn transfer_item_from_slot_to_slot<SideType: Side>(
		&mut self,
		source: SideType,
		sink: SideType,
		count: u32,
		source_slot: NonZeroU32,
		sink_slot: NonZeroU32,
	) -> Result<u32, Error> {
		let source: u8 = source.into();
		let sink: u8 = sink.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"transferItem",
				Some(&FiveValues(
					source,
					sink,
					count,
					source_slot.get(),
					sink_slot.get(),
				)),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Moves fluids between two tanks.
	///
	/// The `count` value indicates the maximum number of millibuckets to move. The number of
	/// millibuckets moved is returned.
	///
	/// If there is no fluid tank in one of the positions, or if one of the positions is in an
	/// unloaded chunk, `Ok(0)` is returned. OpenComputers does not consider this to be an error.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn transfer_fluid<SideType: Side>(
		&mut self,
		source: SideType,
		sink: SideType,
		count: u32,
	) -> Result<u32, Error> {
		let source: u8 = source.into();
		let sink: u8 = sink.into();
		let ret: TwoValues<bool, u32> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"transferFluid",
				Some(&ThreeValues(source, sink, count)),
			)
			.await,
		)?;
		Ok(ret.1)
	}

	// WorldTankAnalytics

	/// Returns the amount of fluid in a tank, in millibuckets.
	///
	/// The `tank` parameter ranges from 1 to the number of tanks in the target block.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_tank_level(
		&mut self,
		side: impl Side,
		tank: NonZeroU32,
	) -> Result<u32, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getTankLevel",
				Some(&TwoValues(side, tank.get())),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns the amount of fluid a tank can hold, in millibuckets.
	///
	/// The `tank` parameter ranges from 1 to the number of tanks in the target block.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_tank_capacity(
		&mut self,
		side: impl Side,
		tank: NonZeroU32,
	) -> Result<u32, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getTankCapacity",
				Some(&TwoValues(side, tank.get())),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns the fluid in a tank.
	///
	/// The `tank` parameter ranges from 1 to the number of tanks in the target block.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported) if detailed item information is disabled in the
	///   config file.
	pub async fn get_fluid_in_tank(
		self,
		side: impl Side,
		tank: NonZeroU32,
	) -> Result<Tank<'buffer>, Error> {
		let side: u8 = side.into();
		let ret: OneValue<Tank<'buffer>> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getFluidInTank",
				Some(&TwoValues(side, tank.get())),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns the fluids in all tanks in a block.
	///
	/// Although tanks are indexed starting from 1 in most situations, for the purpose of this
	/// method, the tanks are returned in a vector which is obviously 0-indexed.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported) if detailed item information is disabled in the
	///   config file.
	pub async fn get_fluids_in_tanks(self, side: impl Side) -> Result<Vec<Tank<'buffer>>, Error> {
		let side: u8 = side.into();
		let ret: OneValue<Vec<Tank<'buffer>>> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getFluidInTank",
				Some(&OneValue(side)),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	// TankInventoryControl

	/// Returns the amount of fluid in a fluid container in the robot or drone’s internal
	/// inventory.
	///
	/// The `slot` parameter ranges from 1 to the internal inventory size.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`BadItem`](Error::BadItem)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_tank_level_in_slot(&mut self, slot: NonZeroU32) -> Result<u32, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getTankLevelInSlot",
				Some(&OneValue(slot.get())),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns the amount of fluid in the fluid container in the currently selected slot of the
	/// robot or drone’s internal inventory.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadItem`](Error::BadItem)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_tank_level_in_selected_slot(&mut self) -> Result<u32, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method::<(), _, _>(
				self.invoker,
				self.buffer,
				&self.address,
				"getTankLevelInSlot",
				None,
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns the total size of a fluid container in the robot or drone’s internal inventory.
	///
	/// The `slot` parameter ranges from 1 to the internal inventory size.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`BadItem`](Error::BadItem)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_tank_capacity_in_slot(&mut self, slot: NonZeroU32) -> Result<u32, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getTankCapacityInSlot",
				Some(&OneValue(slot.get())),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns the total size of the fluid container in the currently selected slot of the robot
	/// or drone’s internal inventory.
	///
	/// If the slot does not contain a fluid container (either because it contains a
	/// non-fluid-container item or because it does not contain anything), `None` is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadItem`](Error::BadItem)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_tank_capacity_in_selected_slot(&mut self) -> Result<u32, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method::<(), _, _>(
				self.invoker,
				self.buffer,
				&self.address,
				"getTankCapacityInSlot",
				None,
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns information about the fluid in a fluid container in the robot or drone’s internal
	/// inventory.
	///
	/// The `slot` parameter ranges from 1 to the internal inventory size.
	///
	/// If the slot contains an empty fluid container, `None` is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned for any unrecognized error.
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`BadItem`](Error::BadItem)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported)
	pub async fn get_fluid_in_tank_in_slot(
		self,
		slot: NonZeroU32,
	) -> Result<Option<Fluid<'buffer>>, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getFluidInTankInSlot",
				Some(&OneValue(slot.get())),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns information about the fluid in the fluid container in the selected slot of the
	/// robot or drone’s internal inventory.
	///
	/// If the slot contains an empty fluid container, `None` is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned for any unrecognized error.
	/// * [`BadItem`](Error::BadItem)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported)
	pub async fn get_fluid_in_tank_in_selected_slot(self) -> Result<Option<Fluid<'buffer>>, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method::<(), _, _>(
				self.invoker,
				self.buffer,
				&self.address,
				"getFluidInTankInSlot",
				None,
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns information about the fluid in the robot or drone’s specified internal tank.
	///
	/// If the tank is empty, `None` is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned for any unrecognized error.
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported)
	pub async fn get_fluid_in_internal_tank(
		self,
		tank: NonZeroU32,
	) -> Result<Option<Fluid<'buffer>>, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getFluidInInternalTank",
				Some(&OneValue(tank.get())),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns information about the fluid in the robot or drone’s currently selected internal
	/// tank.
	///
	/// If the tank is empty, `None` is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned for any unrecognized error.
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported)
	pub async fn get_fluid_in_selected_internal_tank(
		self,
	) -> Result<Option<Fluid<'buffer>>, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method::<(), _, _>(
				self.invoker,
				self.buffer,
				&self.address,
				"getFluidInInternalTank",
				None,
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Moves fluid from a fluid container in the robot or drone’s currently selected inventory
	/// slot into the robot or drone’s currently selected internal tank.
	///
	/// On success, the amount of fluid moved is returned. For certain types of source containers,
	/// this may be larger than `amount`.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadItem`](Error::BadItem) is returned if the item is not a fluid container, if there is
	///   no item in the selected slot, if there is no inventory, or in some cases if the item is
	///   empty.
	/// * [`Failed`](Error::Failed) is returned in some cases if the item is empty.
	/// * [`InventoryFull`](Error::InventoryFull)
	/// * [`NoInventory`](Error::NoInventory) is returned if there is no tank.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn drain(&mut self, amount: NonZeroU32) -> Result<u32, Error> {
		self.drain_or_fill(amount, "drain").await
	}

	/// Moves fluid from the robot or drone’s currently selected internal tank into a fluid
	/// container in the robot or drone’s currently selected inventory slot.
	///
	/// On success, the amount of fluid moved is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadItem`](Error::BadItem) is returned if the item is not a fluid container, if there is
	///   no item in the selected slot, if there is no inventory, or in some cases if the item is
	///   full.
	/// * [`Failed`](Error::Failed) is returned if the tank is empty, if the item contains a fluid
	///   that cannot be mixed with the fluid in the tank, or in some cases if the item is full.
	/// * [`NoInventory`](Error::NoInventory) is returned if there is no tank.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn fill(&mut self, amount: NonZeroU32) -> Result<u32, Error> {
		self.drain_or_fill(amount, "fill").await
	}

	// InventoryAnalytics

	/// Returns the item stack in a robot or drone’s internal inventory slot.
	///
	/// The `slot` parameter ranges from 1 to the inventory size. If the slot does not contain any
	/// items, `None` is returned.
	///
	/// The strings in the returned item stack point into, and therefore retain ownership of, the
	/// scratch buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported)
	pub async fn get_stack_in_internal_slot(
		self,
		slot: NonZeroU32,
	) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getStackInInternalSlot",
				Some(&OneValue(slot)),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Returns the item stack in the robot or drone’s currently selected internal inventory slot.
	///
	/// The strings in the returned item stack point into, and therefore retain ownership of, the
	/// scratch buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported)
	pub async fn get_stack_in_selected_internal_slot(
		self,
	) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method::<(), _, _>(
				self.invoker,
				self.buffer,
				&self.address,
				"getStackInInternalSlot",
				None,
			)
			.await,
		)?;
		Ok(ret.0)
	}

	/// Checks whether a robot or drone’s internal inventory slot contains an
	/// ore-dictionary-equivalent item to the currently selected internal inventory slot.
	///
	/// The `slot` parameter ranges from 1 to the inventory size.
	///
	/// Two empty slots are considered equivalent. An empty slot and a populated slot are
	/// considered non-equivalent. A slot is considered equivalent to itself. Otherwise, two slots
	/// are considered equivalent if and only if the items in both slots have at least one ore
	/// dictionary “ore ID” entry in common.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn is_equivalent_to(&mut self, slot: NonZeroU32) -> Result<bool, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"isEquivalentTo",
				Some(&OneValue(slot.get())),
			)
			.await,
		)?;
		Ok(ret.0)
	}

	// InventoryWorldControlMk2

	/// Drops items from the robot’s selected slot into a specific slot of an adjacent inventory.
	///
	/// Up to `count` items from the currently selected slot in the robot’s inventory are moved
	/// into slot `slot` of the inventory on side `side`. The `face` parameter indicates which face
	/// of the destination location to look for inventory slots.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`InventoryFull`](Error::InventoryFull)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if there is no inventory on side `side` (or on face
	///   `face` of the block on side `side`) or if there are no items in the currently selected
	///   slot.
	pub async fn drop_into_slot(
		&mut self,
		side: ActionSide,
		slot: NonZeroU32,
		count: u32,
		face: Option<RelativeSide>,
	) -> Result<(), Error> {
		let side = u8::from(side);
		let slot = slot.get();
		let ret: TwoValues<bool, Option<&str>> = Self::map_errors(if let Some(f) = face {
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"dropIntoSlot",
				Some(&FourValues(side, slot, count, u8::from(f))),
			)
			.await
		} else {
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"dropIntoSlot",
				Some(&ThreeValues(side, slot, count)),
			)
			.await
		})?;
		match ret {
			TwoValues(true, _) => Ok(()),
			TwoValues(false, Some("inventory full/invalid slot")) => Err(Error::InventoryFull),
			TwoValues(false, _) => Err(Error::Failed),
		}
	}

	/// Sucks up items from a specific slot in an adjacent inventory block into the robot’s
	/// internal inventory.
	///
	/// Up to `count` items from the stack in slot `slot` in the inventory on side `side` are
	/// inserted into the robot’s inventory. The `face` parameter indicates on which face of the
	/// source location to look for inventory slots.
	///
	/// The sucked items are placed into the robot’s inventory, initially into the currently
	/// selected slot, then into slots after it, then wrapping around to slots before it, as
	/// necessary to hold all the sucked items. If there is not enough space to hold the items,
	/// then the items that cannot be held are left behind in their original location.
	///
	/// On success, the number of items actually moved is returned, which may be less than `count`
	/// if the source stack does not have that many items or if that many items do not fit into the
	/// robot’s inventory, including zero if the source stack is empty or there is no space at all
	/// in the robot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if there is no inventory on side `side` (or on face
	///   `face` of the block on side `side`), or if `slot` is greater than the number of slots in
	///   the external inventory.
	pub async fn suck_from_slot(
		&mut self,
		side: ActionSide,
		slot: NonZeroU32,
		count: u32,
		face: Option<RelativeSide>,
	) -> Result<u32, Error> {
		struct FalseOrU32(u32);
		impl<Context> Decode<'_, Context> for FalseOrU32 {
			fn decode(
				d: &mut Decoder<'_>,
				_: &mut Context,
			) -> Result<Self, minicbor::decode::Error> {
				if d.datatype()? == minicbor::data::Type::Bool {
					if d.bool()? {
						Err(minicbor::decode::Error::message(
							"expected only false, not true",
						))
					} else {
						Ok(Self(0))
					}
				} else {
					Ok(Self(d.u32()?))
				}
			}
		}
		let side = u8::from(side);
		let slot = slot.get();
		let ret: OneValue<FalseOrU32> = Self::map_errors(if let Some(f) = face {
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"suckFromSlot",
				Some(&FourValues(side, slot, count, u8::from(f))),
			)
			.await
		} else {
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"suckFromSlot",
				Some(&ThreeValues(side, slot, count)),
			)
			.await
		})?;
		Ok(ret.0 .0)
	}

	// UpgradeInventoryController

	/// Swaps the equipped tool with the currently selected inventory slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NoInventory`](Error::NoInventory) is returned if the robot does not have an inventory
	///   and therefore has no item to equip.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn equip(&mut self) -> Result<(), Error> {
		let ret: OneValue<bool> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "equip", None)
				.await?;
		if ret.0 {
			Ok(())
		} else {
			Err(Error::NoInventory)
		}
	}

	/// Implements the `drain` and `fill` functions.
	///
	/// On success, the amount of fluid moved is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadItem`](Error::BadItem)
	/// * [`Failed`](Error::Failed) is returned if the tank is empty (for a fill operation) or full
	///   (for a drain operation), or if the destination contains a fluid that cannot be mixed with
	///   the fluid being moved.
	/// * [`InventoryFull`](Error::InventoryFull)
	/// * [`NoInventory`](Error::NoInventory) is returned if there is no tank.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	async fn drain_or_fill(&mut self, amount: NonZeroU32, method: &str) -> Result<u32, Error> {
		let ret: TwoValues<bool, u32> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				method,
				Some(&OneValue(amount.get())),
			)
			.await,
		)?;
		Ok(ret.1)
	}

	/// Given a `Result<NullAndStringOr<T>, MethodCallError>`, maps the errors (both exceptions and
	/// null-and-string-style errors) to appropriate error constants, returning any success object
	/// unmodified.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned for any unrecognized error.
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`BadItem`](Error::BadItem)
	/// * [`Failed`](Error::Failed)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported)
	fn map_errors<T>(x: Result<NullAndStringOr<'_, T>, MethodCallError<'_>>) -> Result<T, Error> {
		const INCOMPATIBLE_FLUID: &str = "incompatible fluid";
		const INCOMPATIBLE_OR_NO_FLUID: &str = "incompatible or no fluid";
		const INVALID_SLOT: &str = "invalid slot";
		const INVALID_TANK_INDEX: &str = "invalid tank index";
		const ITEM_IS_EMPTY_OR_NOT_A_FLUID_CONTAINER: &str =
			"item is empty or not a fluid container";
		const ITEM_IS_FULL_OR_NOT_A_FLUID_CONTAINER: &str = "item is full or not a fluid container";
		const ITEM_IS_NOT_A_FLUID_CONTAINER: &str = "item is not a fluid container";
		const NO_INVENTORY: &str = "no inventory";
		const NO_TANK: &str = "no tank";
		const NOT_ENOUGH_ENERGY: &str = "not enough energy";
		const NOT_ENABLED_IN_CONFIG: &str = "not enabled in config";
		const TANK_IS_EMPTY: &str = "tank is empty";
		const TANK_IS_FULL: &str = "tank is full";
		const UNKNOWN: &str = "Unknown";
		const ERROR_MESSAGE_BUFFER_SIZE: usize = max_of_usizes(&[
			INCOMPATIBLE_FLUID.len(),
			INCOMPATIBLE_OR_NO_FLUID.len(),
			INVALID_SLOT.len(),
			INVALID_TANK_INDEX.len(),
			ITEM_IS_EMPTY_OR_NOT_A_FLUID_CONTAINER.len(),
			ITEM_IS_FULL_OR_NOT_A_FLUID_CONTAINER.len(),
			ITEM_IS_NOT_A_FLUID_CONTAINER.len(),
			NO_INVENTORY.len(),
			NO_TANK.len(),
			NOT_ENOUGH_ENERGY.len(),
			NOT_ENABLED_IN_CONFIG.len(),
			TANK_IS_FULL.len(),
			UNKNOWN.len(),
		]);
		match x {
			Ok(NullAndStringOr::Ok(x)) => Ok(x),
			Ok(NullAndStringOr::Err(
				INCOMPATIBLE_FLUID | INCOMPATIBLE_OR_NO_FLUID | TANK_IS_EMPTY,
			)) => Err(Error::Failed),
			Ok(NullAndStringOr::Err(INVALID_SLOT | INVALID_TANK_INDEX)) => {
				Err(Error::BadInventorySlot)
			}
			Ok(NullAndStringOr::Err(
				ITEM_IS_EMPTY_OR_NOT_A_FLUID_CONTAINER
				| ITEM_IS_FULL_OR_NOT_A_FLUID_CONTAINER
				| ITEM_IS_NOT_A_FLUID_CONTAINER,
			)) => Err(Error::BadItem),
			Ok(NullAndStringOr::Err(NO_INVENTORY | NO_TANK | UNKNOWN)) => Err(Error::NoInventory),
			Ok(NullAndStringOr::Err(NOT_ENOUGH_ENERGY)) => Err(Error::NotEnoughEnergy),
			Ok(NullAndStringOr::Err(NOT_ENABLED_IN_CONFIG)) => Err(Error::Unsupported),
			Ok(NullAndStringOr::Err(TANK_IS_FULL)) => Err(Error::InventoryFull),
			Ok(NullAndStringOr::Err(_)) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
			Err(MethodCallError::BadParameters(exception)) => {
				let mut buffer = [0_u8; ERROR_MESSAGE_BUFFER_SIZE];
				match exception.message(&mut buffer) {
					Ok(INVALID_SLOT | INVALID_TANK_INDEX) => Err(Error::BadInventorySlot),
					Ok(NO_INVENTORY | NO_TANK | UNKNOWN) => Err(Error::NoInventory),
					Ok(NOT_ENOUGH_ENERGY) => Err(Error::NotEnoughEnergy),
					Ok(NOT_ENABLED_IN_CONFIG) => Err(Error::Unsupported),
					_ => Err(Error::BadComponent(
						oc_wasm_safe::error::Error::BadParameters,
					)),
				}
			}
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}
}

/// A snapshot of the contents of an inventory.
#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Snapshot(pub descriptor::Owned);

impl<'handle, 'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B>
	for &'handle Snapshot
{
	type Locked = LockedSnapshot<'handle, 'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		use oc_wasm_safe::descriptor::AsDescriptor;
		LockedSnapshot {
			descriptor: self.0.as_descriptor(),
			invoker,
			buffer,
		}
	}
}

/// A snapshot of the contents of an inventory on which methods can be invoked.
///
/// This type combines an inventory snapshot, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`Snapshot::lock`](Lockable::lock), and it can be dropped to
/// return the borrow of the invoker and buffer to the caller so they can be reused for other
/// purposes.
///
/// The `'snapshot` lifetime is the lifetime of the original snapshot. The `'invoker` lifetime is
/// the lifetime of the invoker. The `'buffer` lifetime is the lifetime of the buffer. The `B` type
/// is the type of scratch buffer to use.
pub struct LockedSnapshot<'snapshot, 'invoker, 'buffer, B: Buffer> {
	/// The descriptor.
	descriptor: descriptor::Borrowed<'snapshot>,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut B,
}

impl<'snapshot, 'invoker, 'buffer, B: Buffer> LockedSnapshot<'snapshot, 'invoker, 'buffer, B> {
	/// Returns the next item stack in the snapshot.
	///
	/// If the next slot is empty, `None` is returned.
	///
	/// The strings in the returned item stack point into, and therefore retain ownership of, the
	/// scratch buffer. Consequently, the `LockedSnapshot` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot) is returned if iteration has reached the
	///   end of the slots.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn next(self) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: Vec<OptionItemStack<'buffer>> = oc_wasm_futures::invoke::value::<(), _, _, _>(
			self.invoker,
			self.buffer,
			&self.descriptor,
			None,
		)
		.await?;
		if let Some(elt) = ret.into_iter().next() {
			// OptionItemStack was returned, whether empty or nonempty → slot exists but might be
			// empty.
			Ok(elt.0)
		} else {
			// Return value list was of length zero → slot does not exist.
			Err(Error::BadInventorySlot)
		}
	}

	/// Returns a specific item stack in the snapshot.
	///
	/// The `slot` parameter ranges from 1 to the inventory size. If the `slot` parameter is out of
	/// range, `None` is returned.
	///
	/// This method does not have any effect on the “current position” used by [`next`](Self::next)
	/// and [`reset`](Self::reset).
	///
	/// The strings in the returned item stack point into, and therefore retain ownership of, the
	/// scratch buffer. Consequently, the `LockedSnapshot` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get(self, slot: NonZeroU32) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: OneValue<OptionItemStack<'buffer>> = oc_wasm_futures::invoke::value_indexed_read(
			self.invoker,
			self.buffer,
			&self.descriptor,
			Some(&OneValue(slot)),
		)
		.await?;
		Ok(ret.0.into())
	}

	/// Rewinds the iterator over slots used by [`next`](Self::next).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn reset(&mut self) -> Result<(), Error> {
		value_method::<(), Ignore, _, _>(
			self.invoker,
			self.buffer,
			&self.descriptor,
			"reset",
			None,
		)
		.await?;
		Ok(())
	}

	/// Returns the number of slots in the inventory.
	///
	/// This method does not have any effect on the “current position” used by [`next`](Self::next)
	/// and [`reset`](Self::reset).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn count(&mut self) -> Result<u32, Error> {
		let ret: OneValue<_> =
			value_method::<(), _, _, _>(self.invoker, self.buffer, &self.descriptor, "count", None)
				.await?;
		Ok(ret.0)
	}

	/// Returns all items in the inventory.
	///
	/// This method does not have any effect on the “current position” used by [`next`](Self::next)
	/// and [`reset`](Self::reset).
	///
	/// The strings in the returned item stacks point into, and therefore retain ownership of, the
	/// scratch buffer. Consequently, the `LockedSnapshot` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_all(self) -> Result<Vec<Option<ItemStack<'buffer>>>, Error> {
		fn do_decode_one_based_map_as_vector<'buffer, Context>(
			d: &mut Decoder<'buffer>,
			context: &mut Context,
		) -> Result<Vec<Option<ItemStack<'buffer>>>, minicbor::decode::Error> {
			// Turbofish can’t be used with #[cbor(decode_with)] due to unnamable Context type
			// parameter.
			oc_wasm_helpers::decode_one_based_map_as_vector::<
				Context,
				OptionItemStack<'_>,
				Option<ItemStack<'_>>,
			>(d, context)
		}

		#[derive(Decode)]
		struct Return<'buffer> {
			#[b(0)]
			#[cbor(decode_with = "do_decode_one_based_map_as_vector")]
			x: Vec<Option<ItemStack<'buffer>>>,
		}

		let ret: Return<'buffer> = value_method::<(), _, _, _>(
			self.invoker,
			self.buffer,
			&self.descriptor,
			"getAll",
			None,
		)
		.await?;
		Ok(ret.x)
	}
}
