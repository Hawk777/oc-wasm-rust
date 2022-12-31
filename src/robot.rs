//! Provides high-level access to the robot component APIs.

use crate::common::{RelativeSide, Rgb, Side, TryFromIntError};
use crate::error::Error;
use core::convert::TryFrom;
use core::fmt::{Display, Formatter};
use core::num::NonZeroU32;
use core::str::FromStr;
use minicbor::Decode;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{error::NullAndStringOr, Lockable};
use oc_wasm_safe::{
	component::{Invoker, MethodCallError},
	Address,
};

/// The type name for robot components.
pub const TYPE: &str = "robot";

/// An error returned when converting a [`RelativeSide`](RelativeSide) into an
/// [`ActionSide`](ActionSide) if the value does not map.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TryFromRelativeSideError(());

impl Display for TryFromRelativeSideError {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), core::fmt::Error> {
		"invalid value".fmt(f)
	}
}

#[cfg(feature = "std")]
impl std::error::Error for TryFromRelativeSideError {}

/// The directions a robot can move.
///
/// A robot cannot strafe to the left or right. In order to move in such directions, it must first
/// turn to face towards or away from that direction instead.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum MoveDirection {
	Down,
	Up,
	Back,
	Front,
}

impl From<MoveDirection> for u8 {
	fn from(x: MoveDirection) -> Self {
		match x {
			MoveDirection::Down => 0,
			MoveDirection::Up => 1,
			MoveDirection::Back => 2,
			MoveDirection::Front => 3,
		}
	}
}

impl From<MoveDirection> for usize {
	fn from(x: MoveDirection) -> Self {
		u8::from(x) as usize
	}
}

impl From<MoveDirection> for RelativeSide {
	fn from(x: MoveDirection) -> Self {
		match x {
			MoveDirection::Down => Self::Bottom,
			MoveDirection::Up => Self::Top,
			MoveDirection::Back => Self::Back,
			MoveDirection::Front => Self::Front,
		}
	}
}

impl TryFrom<u8> for MoveDirection {
	type Error = TryFromIntError;

	fn try_from(x: u8) -> Result<Self, Self::Error> {
		match x {
			0 => Ok(Self::Down),
			1 => Ok(Self::Up),
			2 => Ok(Self::Back),
			3 => Ok(Self::Front),
			_ => Err(TryFromIntError(())),
		}
	}
}

impl TryFrom<RelativeSide> for MoveDirection {
	type Error = TryFromRelativeSideError;

	fn try_from(x: RelativeSide) -> Result<Self, Self::Error> {
		match x {
			RelativeSide::Bottom => Ok(Self::Down),
			RelativeSide::Top => Ok(Self::Up),
			RelativeSide::Back => Ok(Self::Back),
			RelativeSide::Front => Ok(Self::Front),
			_ => Err(TryFromRelativeSideError(())),
		}
	}
}

impl From<ActionSide> for MoveDirection {
	fn from(x: ActionSide) -> Self {
		match x {
			ActionSide::Bottom => Self::Down,
			ActionSide::Top => Self::Up,
			ActionSide::Front => Self::Front,
		}
	}
}

impl Side for MoveDirection {}

/// The sides on which a robot can manipulate blocks.
///
/// A robot cannot manipulate blocks to its left or right sides or behind it. In order to
/// manipulate such blocks, it must first turn to face that direction instead.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ActionSide {
	Bottom,
	Top,
	Front,
}

impl From<ActionSide> for u8 {
	fn from(x: ActionSide) -> Self {
		match x {
			ActionSide::Bottom => 0,
			ActionSide::Top => 1,
			ActionSide::Front => 3,
		}
	}
}

impl From<ActionSide> for usize {
	fn from(x: ActionSide) -> Self {
		u8::from(x) as usize
	}
}

impl From<ActionSide> for RelativeSide {
	fn from(x: ActionSide) -> Self {
		match x {
			ActionSide::Bottom => Self::Bottom,
			ActionSide::Top => Self::Top,
			ActionSide::Front => Self::Front,
		}
	}
}

impl TryFrom<u8> for ActionSide {
	type Error = TryFromIntError;

	fn try_from(x: u8) -> Result<Self, Self::Error> {
		match x {
			0 => Ok(Self::Bottom),
			1 => Ok(Self::Top),
			3 => Ok(Self::Front),
			_ => Err(TryFromIntError(())),
		}
	}
}

impl TryFrom<RelativeSide> for ActionSide {
	type Error = TryFromRelativeSideError;

	fn try_from(x: RelativeSide) -> Result<Self, Self::Error> {
		match x {
			RelativeSide::Bottom => Ok(Self::Bottom),
			RelativeSide::Top => Ok(Self::Top),
			RelativeSide::Front => Ok(Self::Front),
			_ => Err(TryFromRelativeSideError(())),
		}
	}
}

impl Side for ActionSide {}

/// The directions in which a robot can rotate.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Rotation {
	Clockwise,
	Counterclockwise,
}

/// The things that can be hit when swinging a tool.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ToolHit {
	/// The tool hit an entity, which was attacked.
	Entity,

	/// The tool hit a block, which was broken.
	Block,

	/// The tool hit a fire, which was extinguished.
	Fire,

	/// The tool hit air, which did nothing.
	Air,
}

/// The possible results of successfully right-clicking on a location.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ActivateResult {
	/// A usable block, such as a lever or button, was activated.
	BlockActivated,

	/// The equipped inventory item, such as a block of dirt, a torch, a spawn egg, or a fire
	/// charge, was placed into the world.
	ItemPlaced,

	/// The equipped inventory item, such as a water bucket or an empty bucket when facing a fluid,
	/// was used in a way other than placing a block into the world.
	ItemUsed,

	/// The equipped inventory item, such as shears, was used on an adjacent entity.
	ItemInteracted,
}

/// The things that can exist in the space of a block.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum BlockContent {
	/// There is a creature (player, friendly, neutral, or hostile) or minecart in the space.
	///
	/// Despite the name, note that certain other entities, such as drones, do not return this
	/// value. The name is based on the name used within OpenComputers itself.
	Entity,

	/// There is nothing in the space.
	Air,

	/// There is a liquid in the space.
	Liquid,

	/// There is a block in the space that the robot can move into, which would be destroyed if it
	/// did so.
	///
	/// An example of this kind of content is tall grass.
	Replaceable,

	/// There is a block in the space that a player could walk through, but the robot cannot.
	///
	/// An example of this kind of content is a flower.
	Passable,

	/// There is a normal block in the space.
	Solid,
}

impl BlockContent {
	/// Returns a string describing the content.
	#[must_use = "This function is only useful for its return value"]
	pub fn as_str(&self) -> &'static str {
		match self {
			Self::Entity => "entity",
			Self::Air => "air",
			Self::Liquid => "liquid",
			Self::Replaceable => "replaceable",
			Self::Passable => "passable",
			Self::Solid => "solid",
		}
	}
}

impl Display for BlockContent {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), core::fmt::Error> {
		self.as_str().fmt(f)
	}
}

impl FromStr for BlockContent {
	type Err = Error;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"entity" => Ok(Self::Entity),
			"air" => Ok(Self::Air),
			"liquid" => Ok(Self::Liquid),
			"replaceable" => Ok(Self::Replaceable),
			"passable" => Ok(Self::Passable),
			"solid" => Ok(Self::Solid),
			_ => Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown)),
		}
	}
}

impl TryFrom<&str> for BlockContent {
	type Error = <Self as FromStr>::Err;

	fn try_from(s: &str) -> Result<Self, Self::Error> {
		Self::from_str(s)
	}
}

/// A robot component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Robot(Address);

impl Robot {
	/// Creates a wrapper around a robot.
	///
	/// The `address` parameter is the address of the robot. It is not checked for correctness at
	/// this time because network topology could change after this function returns; as such, each
	/// usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the robot.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Robot {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A robot component on which methods can be invoked.
///
/// This type combines a robot address, an [`Invoker`](Invoker) that can be used to make method
/// calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this type
/// can be created by calling [`Robot::lock`](Robot::lock), and it can be dropped to return the
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
	/// Returns the colour of the robot’s side body light.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_light_colour(&mut self) -> Result<Rgb, Error> {
		let ret: (u32,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getLightColor",
			None,
		)
		.await?;
		Ok(Rgb(ret.0))
	}

	/// Sets the colour of the robot’s side body light.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_light_colour(&mut self, colour: Rgb) -> Result<(), Error> {
		component_method::<_, (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"setLightColor",
			Some(&(colour.0,)),
		)
		.await?;
		Ok(())
	}

	/// Returns the durability of the equipped tool, or `None` if no tool is equipped or the
	/// equipped tool does not have a concept of durability.
	///
	/// The durability value, if available, is a number between 0 and 1.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn durability(&mut self) -> Result<Option<f64>, Error> {
		let ret: NullAndStringOr<'_, (f64,)> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"durability",
			None,
		)
		.await?;
		Ok(match ret {
			NullAndStringOr::Ok(v) => Some(v.0),
			NullAndStringOr::Err(_) => None,
		})
	}

	/// Moves the robot.
	///
	/// The `direction` parameter indicates in which direction to try to move.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`Blocked`](Error::Blocked)
	/// * [`ImpossibleMove`](Error::ImpossibleMove)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn move_robot(&mut self, direction: MoveDirection) -> Result<(), Error> {
		let ret: NullAndStringOr<'_, (bool,)> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"move",
			Some(&(u8::from(direction),)),
		)
		.await?;
		match ret {
			NullAndStringOr::Ok(_) => Ok(()),
			NullAndStringOr::Err("not enough energy") => Err(Error::NotEnoughEnergy),
			NullAndStringOr::Err("impossible move") => Err(Error::ImpossibleMove),
			NullAndStringOr::Err(s) => Err(Error::Blocked(s.parse()?)),
		}
	}

	/// Turns the robot.
	///
	/// The `direction` parameter indicates in which direction to turn.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn turn(&mut self, direction: Rotation) -> Result<(), Error> {
		let ret: NullAndStringOr<'_, (bool,)> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"turn",
			Some(&(direction == Rotation::Clockwise,)),
		)
		.await?;
		match ret {
			NullAndStringOr::Ok(_) => Ok(()),
			NullAndStringOr::Err("not enough energy") => Err(Error::NotEnoughEnergy),
			NullAndStringOr::Err(_) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
		}
	}

	/// Returns the robot’s name.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn name(self) -> Result<&'buffer str, Error> {
		Ok(component_method::<(), (&'buffer str,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"name",
			None,
		)
		.await?
		.0)
	}

	/// Left-clicks the currently equipped tool on an adjacent block or space.
	///
	/// The `side` parameter indicates where to swing the tool, relative the robot’s current facing
	/// direction. The `face` parameter indicates which face of the target location to aim at. The
	/// `sneak` parameter indicates whether or not to sneak while operating the tool.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadItem`](Error::BadItem) is returned if the block in the target location is too hard
	///   for the equipped tool to break, or the tool is of the wrong kind (for example, a shovel
	///   swinging at cobblestone).
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn swing(
		&mut self,
		side: ActionSide,
		face: Option<RelativeSide>,
		sneak: bool,
	) -> Result<ToolHit, Error> {
		let ret: (bool, &str) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"swing",
			Some(&(u8::from(side), face.map(u8::from), sneak)),
		)
		.await?;
		if ret.1 == "entity" {
			Ok(ToolHit::Entity)
		} else if ret.1 == "block" {
			if ret.0 {
				Ok(ToolHit::Block)
			} else {
				Err(Error::BadItem)
			}
		} else if ret.1 == "fire" {
			Ok(ToolHit::Fire)
		} else if ret.1 == "air" {
			Ok(ToolHit::Air)
		} else {
			// The OpenComputers robot component’s swing method does not return any other values.
			// Therefore, if we see another value, we must be addressing a different component that
			// also has a method named swing.
			Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
		}
	}

	/// Right-clicks the currently equipped item on an adjacent block or space.
	///
	/// The `side` parameter indicates where to use the item, relative the robot’s current facing
	/// direction. The `face` parameter indicates which face of the target location to aim at. The
	/// `sneak` parameter indicates whether or not to sneak while operating the tool. The
	/// `duration` parameter indicates how long to hold down the right mouse button.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if the item is not usable (for example, if it is a
	///   piece of coal), is usable on certain entities but such entities are not present (for
	///   example, if it is a shears and there is nothing in front of the robot or the entity in
	///   front of the robot is a zombie), or is a placeable block but there is no adjacent block
	///   on which to mount it and the robot is not equipped with an angel upgrade.
	pub async fn use_item(
		&mut self,
		side: ActionSide,
		face: Option<RelativeSide>,
		sneak: bool,
		duration: f64,
	) -> Result<ActivateResult, Error> {
		let ret: (bool, Option<&str>) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"use",
			Some(&(u8::from(side), face.map(u8::from), sneak, duration)),
		)
		.await?;
		if ret.1 == Some("block_activated") {
			Ok(ActivateResult::BlockActivated)
		} else if ret.1 == Some("item_placed") {
			Ok(ActivateResult::ItemPlaced)
		} else if ret.1 == Some("item_used") {
			Ok(ActivateResult::ItemUsed)
		} else if ret.1 == Some("item_interacted") {
			Ok(ActivateResult::ItemInteracted)
		} else if ret.0 {
			// The OpenComputers robot component’s swing method does not return any other success
			// reasons. Therefore, if we see another value, we must be addressing a different
			// component that also has a method named use.
			Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
		} else {
			Err(Error::Failed)
		}
	}

	/// Places one of the currently selected item into the world as a block.
	///
	/// The `side` parameter indicates in which location, relative to the robot’s current facing
	/// direction, the item should be placed. The `face` parameter indicates on which face of the
	/// target location the item should be placed (for example, for a lever or torch, on which
	/// adjacent surface the item should be mounted), again relative to the robot’s current facing
	/// direction; or is set to `None` to try all possible faces. The `sneak` parameter indicates
	/// whether or not to sneak while placing the item.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadItem`](Error::BadItem) is returned if there is no item selected.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if the selected item does not have a placeable
	///   block form (e.g. it is a tool instead); the target location is obstructed by an existing
	///   block; there is no existing block adjacent to the target placement location and the robot
	///   is not equipped with an angel upgrade; or `face` is provided, there is no existing block
	///   adjacent to the target placement location on the specified side, and the robot is not
	///   equipped with an angel upgrade; `face` is provided and points back towards the robot; or
	///   there is not enough energy.
	pub async fn place(
		&mut self,
		side: ActionSide,
		face: Option<RelativeSide>,
		sneak: bool,
	) -> Result<(), Error> {
		let ret: NullAndStringOr<'_, (bool,)> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"place",
			Some(&(u8::from(side), face.map(u8::from), sneak)),
		)
		.await?;
		match ret {
			NullAndStringOr::Ok((true,)) => Ok(()),
			NullAndStringOr::Ok((false,)) => Err(Error::Failed),
			NullAndStringOr::Err("nothing selected") => Err(Error::BadItem),
			NullAndStringOr::Err(_) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
		}
	}

	/// Checks what’s present in a specified direction.
	///
	/// The `side` parameter indicates which space, relative to the robot’s current facing
	/// direction, to scan.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn detect(&mut self, side: ActionSide) -> Result<BlockContent, Error> {
		let ret: (bool, &str) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"detect",
			Some(&(u8::from(side),)),
		)
		.await?;
		ret.1.parse()
	}

	/// Returns the size of the robot’s inventory, in slots.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn inventory_size(&mut self) -> Result<u32, Error> {
		Ok(component_method::<(), (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"inventorySize",
			None,
		)
		.await?
		.0)
	}

	/// Returns the currently selected inventory slot number.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NoInventory`](Error::NoInventory)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn selected(&mut self) -> Result<NonZeroU32, Error> {
		let ret: (u32,) =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "select", None)
				.await?;
		match NonZeroU32::new(ret.0) {
			Some(n) => Ok(n),
			None => Err(Error::NoInventory),
		}
	}

	/// Selects an inventory slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn select(&mut self, slot: NonZeroU32) -> Result<(), Error> {
		let ret = component_method::<_, (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"select",
			Some(&(slot,)),
		)
		.await;
		match ret {
			Ok(_) => Ok(()),
			Err(MethodCallError::BadParameters(_)) => Err(Error::BadInventorySlot),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Returns the number of items in an inventory slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn count(&mut self, slot: NonZeroU32) -> Result<u32, Error> {
		let ret = component_method::<_, (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"count",
			Some(&(slot,)),
		)
		.await;
		match ret {
			Ok((n,)) => Ok(n),
			Err(MethodCallError::BadParameters(_)) => Err(Error::BadInventorySlot),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Returns the number of items in the currently selected inventory slot.
	///
	/// If the robot does not have an inventory, this function returns 0.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn count_selected(&mut self) -> Result<u32, Error> {
		Ok(component_method::<(), (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"count",
			None,
		)
		.await?
		.0)
	}

	/// Returns the number of items that can be added to an inventory slot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn space(&mut self, slot: NonZeroU32) -> Result<u32, Error> {
		let ret = component_method::<_, (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"space",
			Some(&(slot,)),
		)
		.await;
		match ret {
			Ok((n,)) => Ok(n),
			Err(MethodCallError::BadParameters(_)) => Err(Error::BadInventorySlot),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Returns the number of items that can be added to the currently selected inventory slot.
	///
	/// If the robot does not have an inventory, this function returns 64.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn space_selected(&mut self) -> Result<u32, Error> {
		Ok(component_method::<(), (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"space",
			None,
		)
		.await?
		.0)
	}

	/// Returns whether or not the currently selected inventory slot contains the same item as the
	/// specified inventory slot.
	///
	/// The `nbt` parameter indicates whether to consider NBT data during the comparison. Two empty
	/// slots are considered equal to each other but not to any nonempty slot. Between two nonempty
	/// slots, the sizes of the stacks do not matter; a slot with one torch and a slot with two
	/// torches are considered equal.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn compare_to(&mut self, other_slot: NonZeroU32, nbt: bool) -> Result<bool, Error> {
		let ret = component_method::<_, (bool,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"compareTo",
			Some(&(other_slot, nbt)),
		)
		.await;
		match ret {
			Ok((f,)) => Ok(f),
			Err(MethodCallError::BadParameters(_)) => Err(Error::BadInventorySlot),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Moves items from the currently selected inventory slot to the specified inventory slot.
	///
	/// The `amount` parameter indicates the maximum number of items to transfer. If the target
	/// slot is empty, then the lesser of `amount` and the number of items available is moved. If
	/// the selected and target slots contain the same type of item, then the lesser of `amount`,
	/// the number of items available, and the remaining space in the target is moved. If the
	/// target slot contains a different type of item and `amount` is greater than or equal to the
	/// number of items in the selected slot, then the two stacks are swapped.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if `amount` was nonzero but no items could be moved
	///   (because the source stack is empty, the target stack is of the same type and full, or the
	///   target stack is of a different type and `amount` is less than the size of the source
	///   stack and therefore the stacks cannot be swapped).
	pub async fn transfer_to(&mut self, target_slot: NonZeroU32, amount: u32) -> Result<(), Error> {
		let ret = component_method::<_, (bool,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"transferTo",
			Some(&(target_slot, amount)),
		)
		.await;
		match ret {
			Ok((true,)) => Ok(()),
			Ok((false,)) => Err(Error::Failed),
			Err(MethodCallError::BadParameters(_)) => Err(Error::BadInventorySlot),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Compares a block in the world with the selected inventory slot.
	///
	/// The `fuzzy` parameter, when `true`, indicates that the comparison should ignore item
	/// subtypes (for example, dirt and coarse dirt are considered equal under fuzzy comparison
	/// rules). An empty itemstack is considered unequal to any block, and an air block is
	/// considered unequal to any itemstack.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn compare(&mut self, side: ActionSide, fuzzy: bool) -> Result<bool, Error> {
		Ok(component_method::<_, (bool,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"compare",
			Some(&(u8::from(side), fuzzy)),
		)
		.await?
		.0)
	}

	/// Drops items from the robot’s selected slot into the world as an itemstack entity or into an
	/// adjacent inventory.
	///
	/// If a block with an inventory is present on the specified side, up to `count` items from the
	/// currently selected slot are inserted into the inventory following the usual item insertion
	/// rules. If there are fewer than `count` items available or the inventory does not have space
	/// to hold `count` items, then fewer items are moved.
	///
	/// If no inventory is present on the specified side, up to `count` items are dropped into the
	/// world as an itemstack entity. If there are fewer than `count` items available, then a
	/// smaller stack is dropped.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`InventoryFull`](Error::InventoryFull)
	/// * [`NoItem`](Error::NoItem)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn drop(&mut self, side: ActionSide, count: u32) -> Result<(), Error> {
		let ret = component_method::<_, (bool, Option<&str>), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"drop",
			Some(&(u8::from(side), count)),
		)
		.await;
		match ret {
			Ok((true, _)) => Ok(()),
			Ok((false, None)) => Err(Error::NoItem),
			Ok((false, Some("inventory full"))) => Err(Error::InventoryFull),
			Ok((false, Some(_))) => Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown)),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Sucks up items from an itemstack entity or an adjacent inventory block.
	///
	/// If a block with an inventory is present on the specified side, up to `count` items from a
	/// single stack in that inventory are inserted into the robot’s inventory.
	///
	/// If no inventory is present on the specified side, one itemstack entity (of any size,
	/// ignoring `count`) is picked up from the world.
	///
	/// The sucked items are placed into the robot’s inventory, initially into the currently
	/// selected slot, then into slots after it, then wrapping around to slots before it, as
	/// necessary to hold all the sucked items. If there is not enough space to hold the items,
	/// then the items that cannot be held are left behind in their original location.
	///
	/// If all the robot’s inventory slots contain items, preventing a new type of item from being
	/// added, external inventory slots or itemstack entities are selected to match an item type
	/// already present in the robot. Otherwise, the first populated inventory slot, or an
	/// arbitrary itemstack entity, is selected.
	///
	/// On success, the number of items actually moved is returned, which may be less than `count`
	/// if the source stack does not have that many items or if that many items do not fit into the
	/// robot’s inventory, or may be more than `count` if the source is an itemstack entity because
	/// `count` is ignored.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if there are no items in the specified direction,
	///   there is no space to move the items into (due to either number or type), or the adjacent
	///   inventory does not allow items to be removed.
	pub async fn suck(&mut self, side: ActionSide, count: u32) -> Result<u32, Error> {
		enum BoolOrU32 {
			Bool(bool),
			U32(u32),
		}
		impl<Context> Decode<'_, Context> for BoolOrU32 {
			fn decode(
				d: &mut minicbor::Decoder<'_>,
				_: &mut Context,
			) -> Result<Self, minicbor::decode::Error> {
				if d.datatype()? == minicbor::data::Type::Bool {
					Ok(Self::Bool(d.bool()?))
				} else {
					Ok(Self::U32(d.u32()?))
				}
			}
		}
		let ret: (BoolOrU32,) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"suck",
			Some(&(u8::from(side), count)),
		)
		.await?;
		match ret.0 {
			BoolOrU32::Bool(false) => Err(Error::Failed),
			BoolOrU32::Bool(true) => {
				// The OpenComputers robot component’s suck method never returns true. Therefore,
				// if we see true, we must be addressing a different component that also has a
				// method named suck.
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
			BoolOrU32::U32(count) => Ok(count),
		}
	}

	/// Returns the number of internal fluid tanks.
	///
	/// This is typically equal to the number of tank upgrades (not tank controller upgrades)
	/// installed in the robot.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn tank_count(&mut self) -> Result<u32, Error> {
		Ok(component_method::<(), (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"tankCount",
			None,
		)
		.await?
		.0)
	}

	/// Returns the currently selected internal fluid tank number.
	///
	/// If the robot has no tanks at all, this function returns 1.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn selected_tank(&mut self) -> Result<NonZeroU32, Error> {
		Ok(component_method::<(), (NonZeroU32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"selectTank",
			None,
		)
		.await?
		.0)
	}

	/// Selects an internal fluid tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn select_tank(&mut self, tank: NonZeroU32) -> Result<(), Error> {
		let ret = component_method::<_, (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"selectTank",
			Some(&(tank,)),
		)
		.await;
		match ret {
			Ok(_) => Ok(()),
			Err(MethodCallError::BadParameters(_)) => Err(Error::BadInventorySlot),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Returns the number of millibuckets of fluid in an internal fluid tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn tank_level(&mut self, tank: NonZeroU32) -> Result<u32, Error> {
		let ret = component_method::<_, (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"tankLevel",
			Some(&(tank,)),
		)
		.await;
		match ret {
			Ok((n,)) => Ok(n),
			Err(MethodCallError::BadParameters(_)) => Err(Error::BadInventorySlot),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Returns the number of millibuckets of fluid in the currently selected internal fluid tank.
	///
	/// If the robot does not have any tanks, this function returns 0.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn tank_level_selected(&mut self) -> Result<u32, Error> {
		Ok(component_method::<(), (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"tankLevel",
			None,
		)
		.await?
		.0)
	}

	/// Returns the number of millibuckets of fluid that can be added to an internal fluid tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn tank_space(&mut self, tank: NonZeroU32) -> Result<u32, Error> {
		let ret = component_method::<_, (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"tankSpace",
			Some(&(tank,)),
		)
		.await;
		match ret {
			Ok((n,)) => Ok(n),
			Err(MethodCallError::BadParameters(_)) => Err(Error::BadInventorySlot),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Returns the number of millibuckets of fluid that can be added to the currently selected
	/// internal fluid tank.
	///
	/// If the robot does not have any tanks, this function returns 0.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn tank_space_selected(&mut self) -> Result<u32, Error> {
		Ok(component_method::<(), (u32,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"tankSpace",
			None,
		)
		.await?
		.0)
	}

	/// Returns whether or not the currently selected internal fluid tank contains the same type of
	/// fluid as the specified internal fluid tank.
	///
	/// Two empty tanks are considered equal. An empty tank is not equal to any nonempty tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn compare_fluid_to(&mut self, other_tank: NonZeroU32) -> Result<bool, Error> {
		let ret = component_method::<_, (bool,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"compareFluidTo",
			Some(&(other_tank,)),
		)
		.await;
		match ret {
			Ok((f,)) => Ok(f),
			Err(MethodCallError::BadParameters(_)) => Err(Error::BadInventorySlot),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Moves fluid from the currently selected internal fluid tank to the specified internal fluid
	/// tank.
	///
	/// The `amount` parameter indicates the maximum number of millibuckets to transfer. If the
	/// target tank is able to hold any fluid of the type held in the source tank, then the minimum
	/// of `amount`, the amount of fluid in the source tank, and the amount of space in the target
	/// tank is moved. If the target tank is not able to hold any of the fluid type held in the
	/// source tank (either because it is full or because the fluids are of different types), and
	/// if `amount` is greater than or equal to the amount of fluid in the source tank, then the
	/// two tanks’ contents are swapped. If the source tank is empty, the entire destination tank
	/// is moved to the source tank (i.e. the tanks’s contents are swapped).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if the requested tank number is greater than the
	///   tank count, or `amount` was nonzero but no fluid could be moved (because the source tank
	///   is empty, the target tank is of the same type and full, or the target tank is of a
	///   different type and `amount` is less than the size of the source tank and therefore the
	///   fluids cannot be swapped).
	pub async fn transfer_fluid_to(
		&mut self,
		target_tank: NonZeroU32,
		amount: u32,
	) -> Result<(), Error> {
		let ret = component_method::<_, NullAndStringOr<'_, bool>, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"transferFluidTo",
			Some(&(target_tank, amount)),
		)
		.await;
		match ret {
			Ok(NullAndStringOr::Ok(true)) => Ok(()),
			Ok(NullAndStringOr::Err("incompatible or no fluid")) => Err(Error::Failed),
			Ok(NullAndStringOr::Err("invalid index")) | Err(MethodCallError::BadParameters(_)) => {
				Err(Error::BadInventorySlot)
			}
			Ok(NullAndStringOr::Ok(false) | NullAndStringOr::Err(_)) => {
				// The OpenComputers robot component’s transferFluidTo method never returns false.
				// Therefore, if we see true, we must be addressing a different component that also
				// has a method named transferFluidTo. Also do that for any other null-and-string
				// returns we don’t recognize.
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
			Err(e) => Err(e.into()),
		}
	}

	/// Returns whether or not the currently selected internal fluid tank contains the same type of
	/// fluid as a tank in an external block.
	///
	/// The `tank` parameter selects the tank within the external block. An empty tank (or
	/// nonexistent tank, in the case of a robot without any internal tanks) is considered unequal
	/// to everything, including another empty tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot) is returned if there is no fluid-containing
	///   block on the specified side or if the `tank` parameter is greater than the number of
	///   tanks in the block and the selected internal tank is nonempty, but only if the selected
	///   tank is non-empty.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn compare_fluid(
		&mut self,
		side: ActionSide,
		tank: NonZeroU32,
	) -> Result<bool, Error> {
		let ret = component_method::<_, (bool,), _>(
			self.invoker,
			self.buffer,
			&self.address,
			"compareFluid",
			Some(&(u8::from(side), tank)),
		)
		.await;
		match ret {
			Ok((b,)) => Ok(b),
			Err(MethodCallError::BadParameters(_)) => Err(Error::BadInventorySlot),
			Err(e) => Err(e.into()),
		}
	}

	/// Moves fluid from an external block’s fluid tank to the currently selected internal fluid
	/// tank.
	///
	/// On success, the amount of fluid moved is the minimum of `amount`, the amount of fluid in
	/// the source tank, and the amount of space in the target tank, and this number is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadItem`](Error::BadItem) is returned if the destination already contains an
	///   incompatible fluid.
	/// * [`InventoryFull`](Error::InventoryFull)
	/// * [`NoInventory`](Error::NoInventory) is returned if the robot does not contain any tanks.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn drain(&mut self, side: ActionSide, amount: u32) -> Result<u32, Error> {
		self.drain_or_fill(side, amount, "drain").await
	}

	/// Moves fluid from the currently selected internal fluid tank to an external block.
	///
	/// On success, the amount of fluid moved is the minimum of `amount`, the amount of fluid in
	/// the source tank, and the amount of space in the target tank, and this number is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadItem`](Error::BadItem) is returned if the destination already contains an
	///   incompatible fluid.
	/// * [`InventoryFull`](Error::InventoryFull)
	/// * [`NoInventory`](Error::NoInventory) is returned if the robot does not contain any tanks.
	/// * [`NoItem`](Error::NoItem) is returned if the source tank is empty and `amount` is
	///   nonzero.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn fill(&mut self, side: ActionSide, amount: u32) -> Result<u32, Error> {
		self.drain_or_fill(side, amount, "fill").await
	}

	/// Implements the [`drain`](drain) or [`fill`](fill) function.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadItem`](Error::BadItem) is returned if the destination already contains an
	///   incompatible fluid.
	/// * [`InventoryFull`](Error::InventoryFull)
	/// * [`NoInventory`](Error::NoInventory) is returned if the robot does not contain any tanks.
	/// * [`NoItem`](Error::NoItem) is returned if the source tank is empty and `amount` is
	///   nonzero.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	async fn drain_or_fill(
		&mut self,
		side: ActionSide,
		amount: u32,
		method: &str,
	) -> Result<u32, Error> {
		let ret: NullAndStringOr<'_, (bool, u32)> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			Some(&(u8::from(side), amount)),
		)
		.await?;
		match ret {
			NullAndStringOr::Ok((_, n)) => Ok(n),
			NullAndStringOr::Err("incompatible or no fluid") => Err(Error::BadItem),
			NullAndStringOr::Err("no space" | "tank is full") => Err(Error::InventoryFull),
			NullAndStringOr::Err("no tank selected") => Err(Error::NoInventory),
			NullAndStringOr::Err("tank is empty") => Err(Error::NoItem),
			NullAndStringOr::Err(_) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
		}
	}
}
