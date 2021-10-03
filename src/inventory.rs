//! Provides high-level access to the inventory controller, fluid tank controller, and transposer
//! APIs.
//!
//! # Limitations
//! At this time, only APIs available to computers are supported (APIs specific to drone or robots
//! are not supported), and database interaction APIs are not supported. They may be added in a
//! future version.

use crate::common::AbsoluteSide;
use crate::error::Error;
use crate::helpers::{
	FiveValues, FourValues, Ignore, NullAndStringOr, OneValue, ThreeValues, TwoValues,
};
use alloc::borrow::ToOwned;
use alloc::vec::Vec;
use core::num::NonZeroU32;
use minicbor::{Decode, Decoder, Encode};
use oc_wasm_futures::invoke::{component_method, value_method};
use oc_wasm_safe::{component::Invoker, descriptor, Address};

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

	/// Locks the controller so methods can be invoked on it.
	///
	/// The [`Invoker`](Invoker) and a scratch buffer must be provided. They are released and can
	/// be reused once the [`Locked`](Locked) is dropped.
	#[must_use = "This function is only useful for its return value"]
	pub fn lock<'invoker, 'buffer>(
		&self,
		invoker: &'invoker mut Invoker,
		buffer: &'buffer mut Vec<u8>,
	) -> Locked<'invoker, 'buffer> {
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
/// The `'invoker` lifetime is the lifetime of the invoker. The `'buffer` lifetime is the lifetime
/// of the buffer.
pub struct Locked<'invoker, 'buffer> {
	/// The component address.
	address: Address,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut Vec<u8>,
}

impl<'invoker, 'buffer> Locked<'invoker, 'buffer> {
	// WorldInventoryAnalytics

	/// Returns the number of slots in an inventory.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on the
	///   specified side.
	pub async fn get_inventory_size(&mut self, side: AbsoluteSide) -> Result<u32, Error> {
		let ret: NullAndStringOr<'_, OneValue<_>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getInventorySize",
			Some(&OneValue(u8::from(side))),
		)
		.await?;
		Ok(ret.into_result()?.0)
	}

	/// Returns the number of items in an inventory slot.
	///
	/// The `slot` parameter ranges from 1 to the inventory size.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on the
	///   specified side, or if the requested slot number is greater than the inventory size.
	pub async fn get_slot_stack_size(
		&mut self,
		side: AbsoluteSide,
		slot: NonZeroU32,
	) -> Result<u32, Error> {
		let ret: OneValue<_> = self
			.call_check_invalid_slots("getSlotStackSize", &TwoValues(u8::from(side), slot))
			.await?;
		Ok(ret.0)
	}

	/// Returns the maximum size of a stack of items, given the item in an inventory slot.
	///
	/// The `slot` parameter ranges from 1 to the inventory size. If the slot does not contain any
	/// items, `None` is returned because the maximum stack size depends on the item type.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on the
	///   specified side, or if the requested slot number is greater than the inventory size.
	pub async fn get_slot_max_stack_size(
		&mut self,
		side: AbsoluteSide,
		slot: NonZeroU32,
	) -> Result<Option<NonZeroU32>, Error> {
		let ret: OneValue<_> = self
			.call_check_invalid_slots("getSlotMaxStackSize", &TwoValues(u8::from(side), slot))
			.await?;
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
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on the
	///   specified side, or if either of the requested slot numbers is greater than the inventory
	///   size.
	pub async fn compare_stacks(
		&mut self,
		side: AbsoluteSide,
		slot_a: NonZeroU32,
		slot_b: NonZeroU32,
		check_nbt: bool,
	) -> Result<bool, Error> {
		let ret: OneValue<_> = self
			.call_check_invalid_slots(
				"compareStacks",
				&FourValues(u8::from(side), slot_a, slot_b, check_nbt),
			)
			.await?;
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
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on the
	///   specified side, or if either of the requested slot numbers is greater than the inventory
	///   size.
	pub async fn are_stacks_equivalent(
		&mut self,
		side: AbsoluteSide,
		slot_a: NonZeroU32,
		slot_b: NonZeroU32,
	) -> Result<bool, Error> {
		let ret: OneValue<_> = self
			.call_check_invalid_slots(
				"areStacksEquivalent",
				&ThreeValues(u8::from(side), slot_a, slot_b),
			)
			.await?;
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
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on the
	///   specified side, if the requested slot number is greater than the inventory size, or if
	///   reading full item information is disabled in the configuration file.
	pub async fn get_stack_in_slot(
		self,
		side: AbsoluteSide,
		slot: NonZeroU32,
	) -> Result<ItemStack<'buffer>, Error> {
		let ret: Result<NullAndStringOr<'_, OneValue<_>>, oc_wasm_safe::error::Error> =
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getStackInSlot",
				Some(&TwoValues(u8::from(side), slot)),
			)
			.await;
		Ok(Self::unpack_bad_parameters_with_message(ret, "invalid slot")?.0)
	}

	/// Returns a snapshot of all the item stacks in an inventory.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on the
	///   specified side, or if reading full item information is disabled in the configuration
	///   file.
	pub async fn get_all_stacks(&mut self, side: AbsoluteSide) -> Result<Snapshot, Error> {
		let ret: NullAndStringOr<'_, OneValue<descriptor::Decoded>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getAllStacks",
			Some(&OneValue(u8::from(side))),
		)
		.await?;
		let descriptor = ret.into_result()?.0;
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
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on the
	///   specified side, or if reading full item information is disabled in the configuration
	///   file.
	pub async fn get_inventory_name(self, side: AbsoluteSide) -> Result<&'buffer str, Error> {
		let ret: NullAndStringOr<'_, OneValue<_>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getInventoryName",
			Some(&OneValue(u8::from(side))),
		)
		.await?;
		Ok(ret.into_result()?.0)
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
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   not a transposer.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on one of
	///   the specified sides, or if there is not enough energy to perform the operation.
	pub async fn transfer_item(
		&mut self,
		source: AbsoluteSide,
		sink: AbsoluteSide,
		count: u32,
	) -> Result<u32, Error> {
		let ret: Result<NullAndStringOr<'_, OneValue<_>>, oc_wasm_safe::error::Error> =
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"transferItem",
				Some(&ThreeValues(u8::from(source), u8::from(sink), count)),
			)
			.await;
		Ok(Locked::unpack_bad_parameters_with_message(ret, "invalid slot")?.0)
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
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   not a transposer.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on one of
	///   the specified sides, if the requested slot number is greater than the inventory size, or
	///   if there is not enough energy to perform the operation.
	pub async fn transfer_item_from_slot(
		&mut self,
		source: AbsoluteSide,
		sink: AbsoluteSide,
		count: u32,
		source_slot: NonZeroU32,
	) -> Result<u32, Error> {
		let ret: Result<NullAndStringOr<'_, OneValue<_>>, oc_wasm_safe::error::Error> =
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"transferItem",
				Some(&FourValues(
					u8::from(source),
					u8::from(sink),
					count,
					source_slot.get(),
				)),
			)
			.await;
		Ok(Locked::unpack_bad_parameters_with_message(ret, "invalid slot")?.0)
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
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   not a transposer.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on one of
	///   the specified sides, if a requested slot number is greater than the corresponding
	///   inventory size, or if there is not enough energy to perform the operation.
	pub async fn transfer_item_from_slot_to_slot(
		&mut self,
		source: AbsoluteSide,
		sink: AbsoluteSide,
		count: u32,
		source_slot: NonZeroU32,
		sink_slot: NonZeroU32,
	) -> Result<u32, Error> {
		let ret: Result<NullAndStringOr<'_, OneValue<_>>, oc_wasm_safe::error::Error> =
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"transferItem",
				Some(&FiveValues(
					u8::from(source),
					u8::from(sink),
					count,
					source_slot.get(),
					sink_slot.get(),
				)),
			)
			.await;
		Ok(Locked::unpack_bad_parameters_with_message(ret, "invalid slot")?.0)
	}

	/// Moves fluids between two tanks.
	///
	/// The `count` value indicates the maximum number of millibuckets to move. The number of
	/// millibuckets moved is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   not a transposer.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible fluid tank on one of
	///   the specified sides, or if there is not enough energy to perform the operation.
	pub async fn transfer_fluid(
		&mut self,
		source: AbsoluteSide,
		sink: AbsoluteSide,
		count: u32,
	) -> Result<u32, Error> {
		let ret: Result<NullAndStringOr<'_, TwoValues<bool, u32>>, oc_wasm_safe::error::Error> =
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"transferFluid",
				Some(&ThreeValues(u8::from(source), u8::from(sink), count)),
			)
			.await;
		Ok(Locked::unpack_bad_parameters_with_message(ret, "invalid tank")?.1)
	}

	// WorldTankAnalytics

	/// Returns the amount of fluid in a tank, in millibuckets.
	///
	/// The `tank` parameter ranges from 1 to the number of tanks in the target block.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor a tank controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible tank on the specified
	///   side or if the requested tank number is greater than the number of tanks.
	pub async fn get_tank_level(
		&mut self,
		side: AbsoluteSide,
		tank: NonZeroU32,
	) -> Result<u32, Error> {
		let ret: Result<NullAndStringOr<'_, OneValue<_>>, oc_wasm_safe::error::Error> =
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getTankLevel",
				Some(&TwoValues(u8::from(side), tank.get())),
			)
			.await;
		Ok(Locked::unpack_bad_parameters_with_message(ret, "invalid tank")?.0)
	}

	/// Returns the amount of fluid a tank can hold, in millibuckets.
	///
	/// The `tank` parameter ranges from 1 to the number of tanks in the target block.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor a tank controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible tank on the specified
	///   side or if the requested tank number is greater than the number of tanks.
	pub async fn get_tank_capacity(
		&mut self,
		side: AbsoluteSide,
		tank: NonZeroU32,
	) -> Result<u32, Error> {
		let ret: Result<NullAndStringOr<'_, OneValue<_>>, oc_wasm_safe::error::Error> =
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"getTankCapacity",
				Some(&TwoValues(u8::from(side), tank.get())),
			)
			.await;
		Ok(Locked::unpack_bad_parameters_with_message(ret, "invalid tank")?.0)
	}

	/// Returns the fluid in a tank.
	///
	/// The `tank` parameter ranges from 1 to the number of tanks in the target block.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor a tank controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible tank on the specified
	///   side, if the requested tank number is greater than the number of tanks, or if reading
	///   full item information is disabled in the configuration file.
	pub async fn get_fluid_in_tank(
		self,
		side: AbsoluteSide,
		tank: NonZeroU32,
	) -> Result<Option<FluidInTank<'buffer>>, Error> {
		let ret: Result<
			NullAndStringOr<'_, OneValue<OptionFluidInTank<'buffer>>>,
			oc_wasm_safe::error::Error,
		> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getFluidInTank",
			Some(&TwoValues(u8::from(side), tank.get())),
		)
		.await;
		let ret = Self::unpack_bad_parameters_with_message(ret, "invalid tank")?;
		Ok(ret.0 .0)
	}

	/// Returns the fluids in all tanks in a block.
	///
	/// Although tanks are indexed starting from 1 in most situations, for the purpose of this
	/// method, the tanks are returned in a vector which is obviously 0-indexed.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor a tank controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible tank on the specified
	///   side or if reading full item information is disabled in the configuration file.
	pub async fn get_fluids_in_tanks(
		self,
		side: AbsoluteSide,
	) -> Result<Vec<Option<FluidInTank<'buffer>>>, Error> {
		let ret: NullAndStringOr<'_, OneValue<GetFluidsInTanksResult<'buffer>>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getFluidInTank",
			Some(&OneValue(u8::from(side))),
		)
		.await?;
		Ok(ret.into_result()?.0 .0)
	}

	/// Makes a method call that accepts one or more slot parameters and converts a
	/// [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) error into a failure due to
	/// invalid slot number.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if there is not an accessible inventory on the
	///   specified side, or if either of the requested slot numbers is greater than the inventory
	///   size.
	async fn call_check_invalid_slots<'retval, Params: Encode, Return: Decode<'retval>>(
		&'retval mut self,
		method: &str,
		params: &Params,
	) -> Result<Return, Error> {
		let ret: Result<NullAndStringOr<'_, Return>, oc_wasm_safe::error::Error> =
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				method,
				Some(&params),
			)
			.await;
		Locked::unpack_bad_parameters_with_message(ret, "invalid slot")
	}

	/// Converts a [`BadParameters`](oc_wasm_safe::error::Error::BadParameters) into
	/// [`Failed`](Error::Failed) with a specified message, converts all other syscall errors into
	/// [`BadComponent`](Error::BadComponent), and unwraps a [`NullAndStringOr`](NullAndStringOr)
	/// into a [`Failed`](Error::Failed) or its contents.
	fn unpack_bad_parameters_with_message<T: Decode<'buffer>>(
		value: Result<NullAndStringOr<'buffer, T>, oc_wasm_safe::error::Error>,
		bad_parameters_message: &str,
	) -> Result<T, Error> {
		match value {
			Ok(NullAndStringOr::Ok(v)) => Ok(v),
			Ok(NullAndStringOr::Err(e)) => Err(Error::Failed(e.to_owned())),
			Err(oc_wasm_safe::error::Error::BadParameters) => {
				Err(Error::Failed(bad_parameters_message.to_owned()))
			}
			Err(e) => Err(e.into()),
		}
	}
}

/// Information about an item stack.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ItemStack<'buffer> {
	/// The internal (Minecraft system) name of the item.
	///
	/// For example, this might be `minecraft:cobblestone`.
	pub name: &'buffer str,

	/// The human-readable name of the item.
	///
	/// For example, this might be `Cobblestone`.
	pub label: &'buffer str,

	/// The number of items in the stack.
	pub size: u32,

	/// The maximum number of items that can be held in the stack.
	pub max_size: u32,

	/// The damage value of the item, if it is a tool, or zero if not.
	pub damage: u32,

	/// The damage value at which the item breaks, if it is a tool, or zero if not.
	pub max_damage: u32,

	/// Whether the item has extra NBT data attached.
	pub has_tag: bool,
}

impl<'buffer> Decode<'buffer> for ItemStack<'buffer> {
	fn decode(d: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		// The CBOR fits in memory, so it must be <2³² elements.
		#[allow(clippy::cast_possible_truncation)]
		let len = d.map()?.ok_or(minicbor::decode::Error::Message(""))? as usize;
		let mut name: Option<&'buffer str> = None;
		let mut label: Option<&'buffer str> = None;
		let mut size: Option<u32> = None;
		let mut max_size: Option<u32> = None;
		let mut damage: Option<u32> = None;
		let mut max_damage: Option<u32> = None;
		let mut has_tag: Option<bool> = None;
		for _ in 0..len {
			let key = d.str()?;
			match key {
				"name" => name = Some(d.str()?),
				"label" => label = Some(d.str()?),
				"size" => size = Some(d.u32()?),
				"maxSize" => max_size = Some(d.u32()?),
				"damage" => damage = Some(d.u32()?),
				"maxDamage" => max_damage = Some(d.u32()?),
				"hasTag" => has_tag = Some(d.bool()?),
				_ => return Err(minicbor::decode::Error::Message("")),
			}
		}
		if let Some(name) = name {
			if let Some(label) = label {
				if let Some(size) = size {
					if let Some(max_size) = max_size {
						if let Some(damage) = damage {
							if let Some(max_damage) = max_damage {
								if let Some(has_tag) = has_tag {
									return Ok(Self {
										name,
										label,
										size,
										max_size,
										damage,
										max_damage,
										has_tag,
									});
								}
							}
						}
					}
				}
			}
		}
		Err(minicbor::decode::Error::Message(""))
	}
}

/// Information about an item stack which may or may not exist.
///
/// This type exists, rather than just using `Option<ItemStack>` directly, because `Option` has a
/// blanket `Decode` implementation, and we need a different implementation which also maps a
/// non-null empty map to `None`.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct OptionItemStack<'buffer>(Option<ItemStack<'buffer>>);

impl<'buffer> Decode<'buffer> for OptionItemStack<'buffer> {
	fn decode(d: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		if d.datatype()? == minicbor::data::Type::Null {
			// Null → no itemstack.
			Ok(Self(None))
		} else {
			let mut p = d.probe();
			if let Ok(Some(0)) = p.map() {
				// Map with zero elements → no itemstack.
				d.map()?;
				Ok(Self(None))
			} else {
				// Something else → decode an itemstack.
				Ok(Self(Some(d.decode()?)))
			}
		}
	}
}

/// A vector of `ItemStack` objects that is decoded from an index-value map in ascending index
/// order rather than a CBOR array.
///
/// Because of how `getAll` works in OpenComputers, the map indices are one-based instead of
/// zero-based.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the objects
/// refer.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct GetAllResult<'buffer>(Vec<ItemStack<'buffer>>);

impl<'buffer> Decode<'buffer> for GetAllResult<'buffer> {
	fn decode(d: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		let len = d.map()?;
		// The CBOR fits in memory, so it must be <2³² elements.
		#[allow(clippy::cast_possible_truncation)]
		let len = len.ok_or(minicbor::decode::Error::Message(""))? as usize;
		let mut ret = Vec::with_capacity(len);
		for _ in 0..len {
			let index = d.u32()?;
			if index as usize != ret.len() + 1 {
				return Err(minicbor::decode::Error::Message(""));
			}
			ret.push(d.decode::<ItemStack<'buffer>>()?);
		}
		Ok(Self(ret))
	}
}

/// A snapshot of the contents of an inventory.
#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Snapshot(pub descriptor::Owned);

impl Snapshot {
	/// Locks the snapshot so methods can be invoked on it.
	///
	/// The [`Invoker`](Invoker) and a scratch buffer must be provided. They are released and can
	/// be reused once the [`LockedSnapshot`](LockedSnapshot) is dropped.
	#[must_use = "This function is only useful for its return value"]
	pub fn lock<'handle, 'invoker, 'buffer>(
		&'handle self,
		invoker: &'invoker mut Invoker,
		buffer: &'buffer mut Vec<u8>,
	) -> LockedSnapshot<'handle, 'invoker, 'buffer> {
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
/// type can be created by calling [`Snapshot::lock`](Snapshot::lock), and it can be dropped to
/// return the borrow of the invoker and buffer to the caller so they can be reused for other
/// purposes.
///
/// The `'snapshot` lifetime is the lifetime of the original snapshot. The `'invoker` lifetime is
/// the lifetime of the invoker. The `'buffer` lifetime is the lifetime of the buffer.
pub struct LockedSnapshot<'snapshot, 'invoker, 'buffer> {
	/// The descriptor.
	descriptor: descriptor::Borrowed<'snapshot>,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut Vec<u8>,
}

impl<'snapshot, 'invoker, 'buffer> LockedSnapshot<'snapshot, 'invoker, 'buffer> {
	/// Returns the next item stack in the snapshot.
	///
	/// If the next slot is empty, `None` is returned.
	///
	/// The strings in the returned item stack point into, and therefore retain ownership of, the
	/// scratch buffer. Consequently, the `LockedSnapshot` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if iteration has reached the end of the slots.
	pub async fn next(self) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: Vec<OptionItemStack<'buffer>> = oc_wasm_futures::invoke::value::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.descriptor,
			None,
		)
		.await?;
		if let Some(elt) = ret.get(0) {
			// OptionItemStack was returned, whether empty or nonempty → slot exists but might be
			// empty.
			Ok(elt.0)
		} else {
			// Return value list was of length zero → slot does not exist.
			Err(Error::Failed("invalid slot".to_owned()))
		}
	}

	/// Returns a specific item stack in the snapshot.
	///
	/// The `slot` parameter ranges from 1 to the inventory size. If the slot does not contain any
	/// items, a record for `minecraft:air` is returned.
	///
	/// This method does not have any effect on the “current position” used by [`next`](Self::next)
	/// and [`reset`](Self::reset).
	///
	/// The strings in the returned item stack point into, and therefore retain ownership of, the
	/// scratch buffer. Consequently, the `LockedSnapshot` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	/// * [`Failed`](Error::Failed) is returned if the requested slot number is greater than the
	///   inventory size.
	pub async fn get(self, slot: NonZeroU32) -> Result<ItemStack<'buffer>, Error> {
		let ret: OneValue<Option<_>> = oc_wasm_futures::invoke::value_indexed_read(
			self.invoker,
			self.buffer,
			&self.descriptor,
			Some(&OneValue(slot)),
		)
		.await?;
		if let Some(stack) = ret.0 {
			Ok(stack)
		} else {
			Err(Error::Failed("invalid slot".to_owned()))
		}
	}

	/// Rewinds the iterator over slots used by [`next`](Self::next).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	pub async fn reset(&mut self) -> Result<(), Error> {
		value_method::<(), Ignore, _>(self.invoker, self.buffer, &self.descriptor, "reset", None)
			.await?;
		Ok(())
	}

	/// Returns the number of slots in the inventory.
	///
	/// This method does not have any effect on the “current position” used by [`next`](Self::next)
	/// and [`reset`](Self::reset).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	pub async fn count(&mut self) -> Result<u32, Error> {
		let ret: OneValue<_> =
			value_method::<(), _, _>(self.invoker, self.buffer, &self.descriptor, "count", None)
				.await?;
		Ok(ret.0)
	}

	/// Returns all items in the inventory.
	///
	/// If a slot does not contain any items, a record for `minecraft:air` is returned.
	///
	/// This method does not have any effect on the “current position” used by [`next`](Self::next)
	/// and [`reset`](Self::reset).
	///
	/// The strings in the returned item stacks point into, and therefore retain ownership of, the
	/// scratch buffer. Consequently, the `LockedSnapshot` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component does not exist or is
	///   neither a transposer nor an inventory controller upgrade.
	pub async fn get_all(self) -> Result<Vec<ItemStack<'buffer>>, Error> {
		let ret: OneValue<GetAllResult<'buffer>> =
			value_method::<(), _, _>(self.invoker, self.buffer, &self.descriptor, "getAll", None)
				.await?;
		Ok(ret.0 .0)
	}
}

/// Information about a fluid in a tank.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FluidInTank<'buffer> {
	/// The internal (Minecraft system) name of the item.
	///
	/// For example, this might be `water`.
	pub name: &'buffer str,

	/// The human-readable name of the item.
	///
	/// For example, this might be `Water`.
	pub label: &'buffer str,

	/// The number of millibuckets of fluid in the tank.
	pub amount: u32,

	/// The maximum number of millibuckets the tank can hold.
	pub capacity: u32,

	/// Whether the fluid has extra NBT data attached.
	pub has_tag: bool,
}

impl<'buffer> Decode<'buffer> for FluidInTank<'buffer> {
	fn decode(d: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		// The CBOR fits in memory, so it must be <2³² elements.
		#[allow(clippy::cast_possible_truncation)]
		let len = d.map()?.ok_or(minicbor::decode::Error::Message(""))? as usize;
		let mut name: Option<_> = None;
		let mut label: Option<_> = None;
		let mut amount: Option<_> = None;
		let mut capacity: Option<_> = None;
		let mut has_tag: Option<_> = None;
		for _ in 0..len {
			let key = d.str()?;
			match key {
				"name" => name = Some(d.str()?),
				"label" => label = Some(d.str()?),
				"amount" => amount = Some(d.u32()?),
				"capacity" => capacity = Some(d.u32()?),
				"hasTag" => has_tag = Some(d.bool()?),
				_ => return Err(minicbor::decode::Error::Message("")),
			}
		}
		if let Some(name) = name {
			if let Some(label) = label {
				if let Some(amount) = amount {
					if let Some(capacity) = capacity {
						if let Some(has_tag) = has_tag {
							return Ok(Self {
								name,
								label,
								amount,
								capacity,
								has_tag,
							});
						}
					}
				}
			}
		}
		Err(minicbor::decode::Error::Message(""))
	}
}

/// Information about a fluid in a tank which may or may not exist.
///
/// This type exists, rather than just using `Option<FluidInTank>` directly, because `Option` has a
/// blanket `Decode` implementation, and we need a different implementation which also maps a
/// non-null map containing a skeleton subset of keys to `None`.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct OptionFluidInTank<'buffer>(Option<FluidInTank<'buffer>>);

impl<'buffer> Decode<'buffer> for OptionFluidInTank<'buffer> {
	fn decode(d: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		if d.datatype()? == minicbor::data::Type::Null {
			// Null is not used by OpenComputers AFAIK, but for future-proofing should probably map
			// to None.
			d.skip()?;
			Ok(Self(None))
		} else {
			let mut p = d.probe();
			if let Some(n) = p.map()? {
				// The CBOR fits in memory, so it must be <2³² elements.
				#[allow(clippy::cast_possible_truncation)]
				let n = n as usize;
				// Map with n elements. Look for an “amount” key.
				let mut amount_key_seen = false;
				for _ in 0..n {
					let key = p.str()?;
					if key == "amount" {
						amount_key_seen = true;
						let value = p.u32()?;
						if value == 0 {
							// This is an empty tank.
							d.skip()?;
							return Ok(Self(None));
						}
					} else {
						p.skip()?;
					}
				}
				if amount_key_seen {
					// Map contains an “amount” key and the value is nonzero.
					Ok(Self(Some(d.decode()?)))
				} else {
					// Map that does not contain an “amount” key. Consider this an empty tank.
					d.skip()?;
					Ok(Self(None))
				}
			} else {
				// Map of indefinite length. OC-Wasm never writes indefinite-length items.
				Err(minicbor::decode::Error::Message(""))
			}
		}
	}
}

/// A vector of `Option<FluidInTank>` objects that is decoded from a CBOR array.
///
/// Each element is decoded as an `OptionFluidInTank` rather than an `Option<FluidInTank>`.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the objects
/// refer.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct GetFluidsInTanksResult<'buffer>(Vec<Option<FluidInTank<'buffer>>>);

impl<'buffer> Decode<'buffer> for GetFluidsInTanksResult<'buffer> {
	fn decode(d: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		let len = d.array()?;
		// The CBOR fits in memory, so it must be <2³² elements.
		#[allow(clippy::cast_possible_truncation)]
		let len = len.ok_or(minicbor::decode::Error::Message(""))? as usize;
		let mut ret = Vec::with_capacity(len);
		for _ in 0..len {
			ret.push(d.decode::<OptionFluidInTank<'buffer>>()?.0);
		}
		Ok(Self(ret))
	}
}
