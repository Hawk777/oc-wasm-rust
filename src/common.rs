//! Data types that are used by more than one Immersive machine.

use minicbor::{Decode, Decoder};
use oc_wasm_helpers::{
	inventory::{ItemStack, OptionItemStack, OptionItemStackBuilder},
	map_decoder,
};

/// An item stack with progress information.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ItemStackWithProgress<'buffer> {
	/// The item stack.
	pub item_stack: ItemStack<'buffer>,

	/// How many ticks the top item of the stack has been processing.
	pub progress: u32,

	/// How many ticks an item of the stack needs to be fully processed.
	pub max_progress: u32,
}

impl<'buffer> Decode<'buffer> for ItemStackWithProgress<'buffer> {
	fn decode(d: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		match d.decode::<OptionItemStackWithProgress<'buffer>>()? {
			OptionItemStackWithProgress(Some(s)) => Ok(s),
			OptionItemStackWithProgress(None) => {
				Err(minicbor::decode::Error::message("missing input stack"))
			}
		}
	}
}

/// Information about an item stack with progress information which may or may not exist.
///
/// This type exists, rather than just using `Option<ItemStackWithProgress>` directly, because
/// `Option` has a blanket `Decode` implementation, and we need a different implementation which
/// also maps a non-null empty map to `None`.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct OptionItemStackWithProgress<'buffer>(pub Option<ItemStackWithProgress<'buffer>>);

impl<'buffer> Decode<'buffer> for OptionItemStackWithProgress<'buffer> {
	fn decode(d: &mut Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		map_decoder::decode_nullable::<OptionItemStackWithProgressBuilder<'buffer>>(d)
	}
}

impl<'buffer> From<OptionItemStackWithProgress<'buffer>>
	for Option<ItemStackWithProgress<'buffer>>
{
	fn from(x: OptionItemStackWithProgress<'buffer>) -> Option<ItemStackWithProgress<'buffer>> {
		x.0
	}
}

/// A map-decoding builder for an [`OptionItemStackWithProgress`](OptionItemStackWithProgress).
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct OptionItemStackWithProgressBuilder<'buffer> {
	/// The item stack.
	pub item_stack: OptionItemStackBuilder<'buffer>,

	/// How many ticks the top item of the stack has been processing.
	pub progress: Option<u32>,

	/// How many ticks an item of the stack needs to be fully processed.
	///
	/// If the stack is not being processed, this may be zero.
	pub max_progress: u32,
}

impl<'buffer> map_decoder::Builder<'buffer> for OptionItemStackWithProgressBuilder<'buffer> {
	type Output = OptionItemStackWithProgress<'buffer>;

	fn entry(
		&mut self,
		key: &'buffer str,
		d: &mut Decoder<'buffer>,
	) -> Result<bool, minicbor::decode::Error> {
		match key {
			"progress" => {
				self.progress = Some(d.u32()?);
				Ok(true)
			}
			"maxProgress" => {
				self.max_progress = d.u32()?;
				Ok(true)
			}
			_ => self.item_stack.entry(key, d),
		}
	}

	fn build(self) -> Result<Self::Output, minicbor::decode::Error> {
		if let OptionItemStack(Some(item_stack)) = self.item_stack.build()? {
			// There’s an item stack here. Grab the progress and max progress values and build the
			// input stack.
			if let Some(progress) = self.progress {
				Ok(OptionItemStackWithProgress(Some(ItemStackWithProgress {
					item_stack,
					progress,
					max_progress: self.max_progress,
				})))
			} else {
				Err(minicbor::decode::Error::message(
					"missing key in input stack",
				))
			}
		} else {
			// There’s no item stack here, which is fine.
			Ok(OptionItemStackWithProgress(None))
		}
	}
}

impl<'buffer> map_decoder::NullableBuilder<'buffer>
	for OptionItemStackWithProgressBuilder<'buffer>
{
	fn build_null() -> OptionItemStackWithProgress<'buffer> {
		OptionItemStackWithProgress(None)
	}
}
