use crate::map_decoder;
use core::num::NonZeroU32;
use minicbor::decode::{Decode, Decoder, Error};

/// Information about a fluid.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Fluid<'buffer> {
	/// The internal (Minecraft system) name of the item.
	///
	/// For example, this might be `water`.
	pub name: &'buffer str,

	/// The human-readable name of the item.
	///
	/// For example, this might be `Water`.
	pub label: &'buffer str,

	/// The number of millibuckets of fluid in the container.
	pub amount: NonZeroU32,

	/// Whether the fluid has extra NBT data attached.
	pub has_tag: bool,
}

impl<'buffer, Context> Decode<'buffer, Context> for Fluid<'buffer> {
	fn decode(d: &mut Decoder<'buffer>, context: &mut Context) -> Result<Self, Error> {
		match OptionFluid::decode(d, context)?.into() {
			Some(f) => Ok(f),
			None => Err(Error::message("missing fluid")),
		}
	}
}

/// Information about a fluid which may or may not exist.
///
/// This type exists, rather than just using `Option<Fluid>` directly, because `Option` has a
/// blanket `Decode` implementation, and we need a different implementation which also maps a
/// non-null empty map to `None`.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[allow(clippy::module_name_repetitions)]
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct OptionFluid<'buffer>(pub Option<Fluid<'buffer>>);

impl<'buffer, Context> Decode<'buffer, Context> for OptionFluid<'buffer> {
	fn decode(d: &mut Decoder<'buffer>, context: &mut Context) -> Result<Self, Error> {
		map_decoder::decode_nullable::<OptionFluidBuilder<'buffer>, Context>(d, context)
	}
}

impl<'buffer> From<OptionFluid<'buffer>> for Option<Fluid<'buffer>> {
	fn from(x: OptionFluid<'buffer>) -> Option<Fluid<'buffer>> {
		x.0
	}
}

/// A map-decoding builder for an [`OptionFluid`](OptionFluid).
#[derive(Clone, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct OptionFluidBuilder<'buffer> {
	/// The internal (Minecraft system) name of the item.
	///
	/// For example, this might be `water`.
	name: Option<&'buffer str>,

	/// The human-readable name of the item.
	///
	/// For example, this might be `Water`.
	label: Option<&'buffer str>,

	/// The number of millibuckets of fluid in the container.
	amount: Option<u32>,

	/// Whether the fluid has extra NBT data attached.
	has_tag: Option<bool>,
}

impl<'buffer> map_decoder::Builder<'buffer> for OptionFluidBuilder<'buffer> {
	type Output = OptionFluid<'buffer>;

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
			"amount" => {
				self.amount = Some(d.u32()?);
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
		// If all the required keys are present, and the amount is nonzero, return a fluid.
		if let Some(name) = self.name {
			if let Some(label) = self.label {
				if let Some(amount) = self.amount {
					if let Some(amount) = NonZeroU32::new(amount) {
						if let Some(has_tag) = self.has_tag {
							return Ok(OptionFluid(Some(Fluid {
								name,
								label,
								amount,
								has_tag,
							})));
						}
					}
				}
			}
		}

		// If all the required keys are absent, return None.
		if (self.name, self.label, self.amount, self.has_tag) == (None, None, None, None) {
			return Ok(OptionFluid(None));
		}

		// If the amount is present but zero, return None.
		if self.amount == Some(0) {
			return Ok(OptionFluid(None));
		}

		// If some but not all of the required keys are present, fail.
		Err(minicbor::decode::Error::message("missing key in fluid"))
	}
}

impl<'buffer> map_decoder::NullableBuilder<'buffer> for OptionFluidBuilder<'buffer> {
	fn build_null() -> OptionFluid<'buffer> {
		OptionFluid(None)
	}
}

/// Information about a fluid tank.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Tank<'buffer> {
	/// The fluid contained in the tank, if any.
	pub fluid: Option<Fluid<'buffer>>,

	/// The maximum number of millibuckets the tank can hold.
	pub capacity: u32,
}

impl<'buffer, Context> Decode<'buffer, Context> for Tank<'buffer> {
	fn decode(d: &mut Decoder<'buffer>, context: &mut Context) -> Result<Self, Error> {
		map_decoder::decode::<TankBuilder<'buffer>, Context>(d, context)
	}
}

/// A map-decoding builder for a [`Tank`](Tank).
#[derive(Clone, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TankBuilder<'buffer> {
	/// The fluid.
	fluid: OptionFluidBuilder<'buffer>,

	/// The maximum number of millibuckets the tank can hold.
	capacity: Option<u32>,
}

impl<'buffer> map_decoder::Builder<'buffer> for TankBuilder<'buffer> {
	type Output = Tank<'buffer>;

	fn entry<Context>(
		&mut self,
		key: &'buffer str,
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<bool, Error> {
		match key {
			"capacity" => {
				self.capacity = Some(d.u32()?);
				Ok(true)
			}
			_ => self.fluid.entry(key, d, context),
		}
	}

	fn build(self) -> Result<Self::Output, Error> {
		if let Some(capacity) = self.capacity {
			// The capacity is present. The fluid may or may not be; that’s fine, because the tank
			// might be empty.
			Ok(Tank {
				fluid: self.fluid.build()?.into(),
				capacity,
			})
		} else {
			// The capacity is absent, which should not happen.
			Err(Error::message("missing key in tank"))
		}
	}
}
