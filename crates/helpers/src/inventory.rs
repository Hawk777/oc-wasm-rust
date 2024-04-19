use crate::map_decoder;
use minicbor::decode::{Decode, Decoder, Error};

/// Information about an item stack.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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

impl<'buffer, Context> Decode<'buffer, Context> for ItemStack<'buffer> {
	fn decode(d: &mut Decoder<'buffer>, context: &mut Context) -> Result<Self, Error> {
		match OptionItemStack::decode(d, context)?.into() {
			Some(s) => Ok(s),
			None => Err(Error::message("missing item stack")),
		}
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
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct OptionItemStack<'buffer>(pub Option<ItemStack<'buffer>>);

impl<'buffer, Context> Decode<'buffer, Context> for OptionItemStack<'buffer> {
	fn decode(d: &mut Decoder<'buffer>, context: &mut Context) -> Result<Self, Error> {
		map_decoder::decode_nullable::<OptionItemStackBuilder<'buffer>, Context>(d, context)
	}
}

impl<'buffer> From<OptionItemStack<'buffer>> for Option<ItemStack<'buffer>> {
	fn from(x: OptionItemStack<'buffer>) -> Option<ItemStack<'buffer>> {
		x.0
	}
}

/// A map-decoding builder for an [`OptionItemStack`].
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct OptionItemStackBuilder<'buffer> {
	/// The internal (Minecraft system) name of the item.
	///
	/// For example, this might be `minecraft:cobblestone`.
	name: Option<&'buffer str>,

	/// The human-readable name of the item.
	///
	/// For example, this might be `Cobblestone`.
	label: Option<&'buffer str>,

	/// The number of items in the stack.
	size: Option<u32>,

	/// The maximum number of items that can be held in the stack.
	max_size: Option<u32>,

	/// The damage value of the item, if it is a tool, or zero if not.
	damage: Option<u32>,

	/// The damage value at which the item breaks, if it is a tool, or zero if not.
	max_damage: Option<u32>,

	/// Whether the item has extra NBT data attached.
	has_tag: Option<bool>,
}

impl<'buffer> map_decoder::Builder<'buffer> for OptionItemStackBuilder<'buffer> {
	type Output = OptionItemStack<'buffer>;

	fn entry<Context>(
		&mut self,
		key: &str,
		d: &mut Decoder<'buffer>,
		_: &mut Context,
	) -> Result<bool, Error> {
		match key {
			"name" => {
				self.name = Some(d.str()?);
				Ok(true)
			}
			"label" => {
				self.label = Some(d.str()?);
				Ok(true)
			}
			"size" => {
				self.size = Some(d.u32()?);
				Ok(true)
			}
			"maxSize" => {
				self.max_size = Some(d.u32()?);
				Ok(true)
			}
			"damage" => {
				self.damage = Some(d.u32()?);
				Ok(true)
			}
			"maxDamage" => {
				self.max_damage = Some(d.u32()?);
				Ok(true)
			}
			"hasTag" => {
				self.has_tag = Some(d.bool()?);
				Ok(true)
			}
			_ => Ok(false),
		}
	}

	fn build(self) -> Result<Self::Output, Error> {
		// If all the required keys are present, return an item stack.
		if let Some(name) = self.name {
			if let Some(label) = self.label {
				if let Some(size) = self.size {
					if let Some(max_size) = self.max_size {
						if let Some(damage) = self.damage {
							if let Some(max_damage) = self.max_damage {
								if let Some(has_tag) = self.has_tag {
									// Some APIs map an empty slot to “zero air” instead of an
									// empty table.
									return Ok(OptionItemStack(
										if name == "minecraft:air" && size == 0 {
											None
										} else {
											Some(ItemStack {
												name,
												label,
												size,
												max_size,
												damage,
												max_damage,
												has_tag,
											})
										},
									));
								}
							}
						}
					}
				}
			}
		}

		// If all the required keys are absent, return None.
		if (
			self.name,
			self.label,
			self.size,
			self.max_size,
			self.damage,
			self.max_damage,
			self.has_tag,
		) == (None, None, None, None, None, None, None)
		{
			return Ok(OptionItemStack(None));
		}

		// If some but not all of the required keys are present, fail.
		Err(Error::message("missing key in item stack"))
	}
}

impl<'buffer> map_decoder::NullableBuilder<'buffer> for OptionItemStackBuilder<'buffer> {
	fn build_null() -> OptionItemStack<'buffer> {
		OptionItemStack(None)
	}
}
