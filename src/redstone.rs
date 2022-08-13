//! Provides high-level access to the redstone APIs.

use crate::common::{Colour, Side, BLOCK_SIDES, COLOURS};
use crate::error::Error;
use crate::helpers::Ignore;
use minicbor::{Decode, Encode};
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{Lockable, OneValue, ThreeValues, TwoValues};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for redstone components.
pub const TYPE: &str = "redstone";

/// A redstone component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Redstone(Address);

impl Redstone {
	/// Creates a wrapper around a redstone block or card.
	///
	/// The `address` parameter is the address of the component. It is not checked for correctness
	/// at this time because network topology could change after this function returns; as such,
	/// each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the component.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Redstone {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A redstone component on which methods can be invoked.
///
/// This type combines a redstone block or card address, an [`Invoker`](Invoker) that can be used
/// to make method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value
/// of this type can be created by calling [`Redstone::lock`](Redstone::lock), and it can be
/// dropped to return the borrow of the invoker and buffer to the caller so they can be reused for
/// other purposes.
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
	/// Returns the signal strengths received on all six sides.
	///
	/// The returned array is indexed by side. For a redstone block, the indices should be
	/// [absolute sides](crate::common::AbsoluteSide). For a redstone card in a computer, the
	/// indices should be [relative sides](crate::common::RelativeSide).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_input(&mut self) -> Result<[u8; BLOCK_SIDES], Error> {
		self.get_vanilla("getInput").await
	}

	/// Returns the signal strengths received on a side.
	///
	/// For a redstone block, the `side` parameter must be an [absolute
	/// side](crate::common::AbsoluteSide). For a redstone card in a computer, the `side` parameter
	/// must be a [relative side](crate::common::RelativeSide).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_side_input(&mut self, side: impl Side) -> Result<u8, Error> {
		self.get_side_vanilla("getInput", side.into()).await
	}

	/// Returns the bundled signal strengths received on all six sides.
	///
	/// The returned array is indexed by side. For a redstone block, the indices should be
	/// [absolute sides](crate::common::AbsoluteSide). For a redstone card in a computer, the
	/// indices should be [relative sides](crate::common::RelativeSide). Each element of the outer
	/// array is itself an array indexed by colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_bundled_input(&mut self) -> Result<[[u8; COLOURS]; BLOCK_SIDES], Error> {
		self.get_bundled("getBundledInput").await
	}

	/// Returns the bundled signal strengths received on a side.
	///
	/// For a redstone block, the `side` parameter must be an [absolute
	/// side](crate::common::AbsoluteSide). For a redstone card in a computer, the `side` parameter
	/// must be a [relative side](crate::common::RelativeSide).
	///
	/// The returned array is indexed by colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_side_bundled_input(
		&mut self,
		side: impl Side,
	) -> Result<[u8; COLOURS], Error> {
		self.get_side_bundled("getBundledInput", side.into()).await
	}

	/// Returns the bundled signal strength received on a side on a single colour of wire.
	///
	/// For a redstone block, the `side` parameter must be an [absolute
	/// side](crate::common::AbsoluteSide). For a redstone card in a computer, the `side` parameter
	/// must be a [relative side](crate::common::RelativeSide).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_side_colour_bundled_input(
		&mut self,
		side: impl Side,
		colour: Colour,
	) -> Result<u8, Error> {
		self.get_side_colour_bundled("getBundledInput", side.into(), colour.into())
			.await
	}

	/// Returns the signal strengths emitted on all six sides.
	///
	/// The returned array is indexed by side. For a redstone block, the indices should be
	/// [absolute sides](crate::common::AbsoluteSide). For a redstone card in a computer, the
	/// indices should be [relative sides](crate::common::RelativeSide).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_output(&mut self) -> Result<[u8; BLOCK_SIDES], Error> {
		self.get_vanilla("getOutput").await
	}

	/// Returns the signal strength emitted on a side.
	///
	/// For a redstone block, the `side` parameter must be an [absolute
	/// side](crate::common::AbsoluteSide). For a redstone card in a computer, the `side` parameter
	/// must be a [relative side](crate::common::RelativeSide).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_side_output(&mut self, side: impl Side) -> Result<u8, Error> {
		self.get_side_vanilla("getOutput", side.into()).await
	}

	/// Returns the bundled signal strengths emitted on all six sides.
	///
	/// The returned array is indexed by side. For a redstone block, the indices should be
	/// [absolute sides](crate::common::AbsoluteSide). For a redstone card in a computer, the
	/// indices should be [relative sides](crate::common::RelativeSide). Each element of the outer
	/// array is itself an array indexed by colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_bundled_output(&mut self) -> Result<[[u8; COLOURS]; BLOCK_SIDES], Error> {
		self.get_bundled("getBundledOutput").await
	}

	/// Returns the bundled signal strengths emitted on a side.
	///
	/// For a redstone block, the `side` parameter must be an [absolute
	/// side](crate::common::AbsoluteSide). For a redstone card in a computer, the `side` parameter
	/// must be a [relative side](crate::common::RelativeSide).
	///
	/// The returned array is indexed by colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_side_bundled_output(
		&mut self,
		side: impl Side,
	) -> Result<[u8; COLOURS], Error> {
		self.get_side_bundled("getBundledOutput", side.into()).await
	}

	/// Returns the bundled signal strength emitted on a side on a single colour of wire.
	///
	/// For a redstone block, the `side` parameter must be an [absolute
	/// side](crate::common::AbsoluteSide). For a redstone card in a computer, the `side` parameter
	/// must be a [relative side](crate::common::RelativeSide).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_side_colour_bundled_output(
		&mut self,
		side: impl Side,
		colour: Colour,
	) -> Result<u8, Error> {
		self.get_side_colour_bundled("getBundledOutput", side.into(), colour.into())
			.await
	}

	/// Sets the signal strengths to emit on any subset of the sides.
	///
	/// The `levels` parameter contains an element for each side. For a redstone block, the indices
	/// into this array are [absolute sides](crate::common::AbsoluteSide). For a redstone card in a
	/// computer, the indices are [relative sides](crate::common::RelativeSide). Each element of
	/// the array can be `Some` with the new signal strength to emit, or `None` to leave that side
	/// unmodified.
	///
	/// The old signal levels, prior to modification, are returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_output(
		&mut self,
		levels: &[Option<u8>; BLOCK_SIDES],
	) -> Result<[u8; BLOCK_SIDES], Error> {
		let ret: OneValue<ArrayAsMap<u8, BLOCK_SIDES>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setOutput",
			Some(&OneValue(ArrayAsMap(*levels))),
		)
		.await?;
		Ok(ret.0 .0)
	}

	/// Sets the signal strength to emit on a single side.
	///
	/// The `side` parameter selects the side to modify. For a redstone block, the value must be an
	/// [absolute side](crate::common::AbsoluteSide). For a redstone card in a computer, the value
	/// must be a [relative side](crate::common::RelativeSide).
	///
	/// The old signal level, prior to modification, is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_side_output(&mut self, side: impl Side, level: u8) -> Result<u8, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setOutput",
			Some(&TwoValues(side, level)),
		)
		.await?;
		Ok(ret.0)
	}

	/// Sets the bundled signal strengths to emit on any subset of wires on any subset of sides.
	///
	/// The `levels` parameter contains an element for each side. For a redstone block, the indices
	/// into this array are [absolute sides](crate::common::AbsoluteSide). For a redstone card in a
	/// computer, the indices are [relative sides](crate::common::RelativeSide). Each element of
	/// the array is itself another array. The inner arrays are indexed by colour. Each element of
	/// the inner array can be `Some` with the new signal strength to emit, or `None` to leave that
	/// colour unmodified.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_bundled_output(
		&mut self,
		levels: &[[Option<u8>; COLOURS]; BLOCK_SIDES],
	) -> Result<(), Error> {
		let param: OneValue<ArrayAsMap<ArrayAsMap<Option<u8>, COLOURS>, BLOCK_SIDES>> =
			OneValue(ArrayAsMap([
				ArrayAsMap(levels[0]),
				ArrayAsMap(levels[1]),
				ArrayAsMap(levels[2]),
				ArrayAsMap(levels[3]),
				ArrayAsMap(levels[4]),
				ArrayAsMap(levels[5]),
			]));
		component_method::<_, Ignore, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"setBundledOutput",
			Some(&param),
		)
		.await?;
		Ok(())
	}

	/// Sets the bundled signal strengths to emit on any subset of wires on a single side.
	///
	/// The `side` parameter selects the side to modify. For a redstone block, the value must be an
	/// [absolute side](crate::common::AbsoluteSide). For a redstone card in a computer, the value
	/// must be a [relative side](crate::common::RelativeSide).
	///
	/// The `levels` parameter contains an element for each colour. Each element of `levels` can be
	/// `Some` with the new signal strength to emit, or `None` to leave that colour unmodified.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_side_bundled_output(
		&mut self,
		side: impl Side,
		levels: &[Option<u8>; COLOURS],
	) -> Result<(), Error> {
		let side: u8 = side.into();
		component_method::<_, Ignore, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"setBundledOutput",
			Some(&TwoValues(side, ArrayAsMap(*levels))),
		)
		.await?;
		Ok(())
	}

	/// Sets the bundled signal strength to emit on a single wire on a single side.
	///
	/// The `side` parameter selects the side to modify. For a redstone block, the value must be an
	/// [absolute side](crate::common::AbsoluteSide). For a redstone card in a computer, the value
	/// must be a [relative side](crate::common::RelativeSide).
	///
	/// The old signal level, prior to modification, is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_side_colour_bundled_output(
		&mut self,
		side: impl Side,
		colour: Colour,
		level: u8,
	) -> Result<u8, Error> {
		let side: u8 = side.into();
		let colour: u8 = colour.into();
		let ret: OneValue<_> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setBundledOutput",
			Some(&ThreeValues(side, colour, level)),
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the comparator value on a given side.
	///
	/// For a redstone block, the `side` parameter must be an [absolute
	/// side](crate::common::AbsoluteSide). For a redstone card in a computer, the `side` parameter
	/// must be a [relative side](crate::common::RelativeSide).
	///
	/// The returned value is the signal level that would be emitted by a comparator sensing the
	/// adjacent block. If the target block is not readable by a comparator, zero is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_comparator_input(&mut self, side: impl Side) -> Result<u8, Error> {
		let side: u8 = side.into();
		let ret: OneValue<_> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getComparatorInput",
			Some(&OneValue(side)),
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the wake threshold.
	///
	/// When any redstone input changes from being strictly less than the threshold to greater than
	/// or equal to the threshold, the containing computer (in the case of a redstone card) or all
	/// connected computers (in the case of a redstone block) are powered on if they are off.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_wake_threshold(&mut self) -> Result<u32, Error> {
		let ret: OneValue<_> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getWakeThreshold",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Sets the wake threshold.
	///
	/// When any redstone input changes from being strictly less than the threshold to greater than
	/// or equal to the threshold, the containing computer (in the case of a redstone card) or all
	/// connected computers (in the case of a redstone block) are powered on if they are off.
	///
	/// The old wake threshold, prior to modification, is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_wake_threshold(&mut self, threshold: u32) -> Result<u32, Error> {
		let ret: OneValue<_> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setWakeThreshold",
			Some(&OneValue(threshold)),
		)
		.await?;
		Ok(ret.0)
	}

	async fn get_vanilla(&mut self, method: &str) -> Result<[u8; BLOCK_SIDES], Error> {
		let ret: OneValue<ArrayAsMap<u8, BLOCK_SIDES>> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, method, None)
				.await?;
		Ok(ret.0 .0)
	}

	async fn get_side_vanilla(&mut self, method: &str, side: u8) -> Result<u8, Error> {
		let ret: OneValue<_> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			Some(&OneValue(side)),
		)
		.await?;
		Ok(ret.0)
	}

	async fn get_bundled(&mut self, method: &str) -> Result<[[u8; COLOURS]; BLOCK_SIDES], Error> {
		let ret: OneValue<ArrayAsMap<ArrayAsMap<u8, COLOURS>, BLOCK_SIDES>> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, method, None)
				.await?;
		Ok([
			ret.0 .0[0].0,
			ret.0 .0[1].0,
			ret.0 .0[2].0,
			ret.0 .0[3].0,
			ret.0 .0[4].0,
			ret.0 .0[5].0,
		])
	}

	async fn get_side_bundled(&mut self, method: &str, side: u8) -> Result<[u8; COLOURS], Error> {
		let ret: OneValue<ArrayAsMap<u8, COLOURS>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			Some(&OneValue(side)),
		)
		.await?;
		Ok(ret.0 .0)
	}

	async fn get_side_colour_bundled(
		&mut self,
		method: &str,
		side: u8,
		colour: u8,
	) -> Result<u8, Error> {
		let ret: OneValue<_> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			Some(&TwoValues(side, colour)),
		)
		.await?;
		Ok(ret.0)
	}
}

/// An array that is CBOR-encoded as a map keyed by array index, rather than a CBOR array.
///
/// To be decoded, the element type must implement [`Copy`](Copy), [`Decode`](minicbor::Decode) and
/// [`Default`](Default), and any elements not included in the CBOR-encoded map will be returned at
/// their default values.
///
/// To be encoded, the element type must be an [`Option`](Option) and the type contained therein
/// must implement [`Encode`](minicbor::Encode). `None` values are omitted from the map entirely,
/// while `Some` values are encoded as their contents, keyed by array position.
#[derive(Clone, Copy)]
struct ArrayAsMap<T, const LENGTH: usize>(pub [T; LENGTH]);

impl<'buffer, T: Copy + Decode<'buffer> + Default, const LENGTH: usize> Decode<'buffer>
	for ArrayAsMap<T, LENGTH>
{
	fn decode(d: &mut minicbor::Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		let mut ret = [T::default(); LENGTH];
		// The CBOR fits in memory, so it must be <2³² elements.
		#[allow(clippy::cast_possible_truncation)]
		let length = d
			.map()?
			.ok_or_else(|| minicbor::decode::Error::message(""))? as usize;
		for _ in 0..length {
			let key = d.u32()?;
			ret[key as usize] = d.decode::<T>()?;
		}
		Ok(Self(ret))
	}
}

impl<T: Encode, const LENGTH: usize> Encode for ArrayAsMap<Option<T>, LENGTH> {
	fn encode<W: minicbor::encode::Write>(
		&self,
		e: &mut minicbor::Encoder<W>,
	) -> Result<(), minicbor::encode::Error<W::Error>> {
		let count = self.0.iter().filter(|i| i.is_some()).count();
		e.map(count as u64)?;
		for i in 0..LENGTH {
			if let Some(elt) = &self.0[i] {
				e.u64(i as u64)?;
				e.encode(elt)?;
			}
		}
		Ok(())
	}
}

impl<T: Encode, const INNER_LENGTH: usize, const OUTER_LENGTH: usize> Encode
	for ArrayAsMap<ArrayAsMap<Option<T>, INNER_LENGTH>, OUTER_LENGTH>
{
	fn encode<W: minicbor::encode::Write>(
		&self,
		e: &mut minicbor::Encoder<W>,
	) -> Result<(), minicbor::encode::Error<W::Error>> {
		e.map(OUTER_LENGTH as u64)?;
		for i in 0..OUTER_LENGTH {
			e.u64(i as u64)?;
			e.encode(&self.0[i])?;
		}
		Ok(())
	}
}

impl<T, const LENGTH: usize> Default for ArrayAsMap<T, LENGTH>
where
	[T; LENGTH]: Default,
{
	fn default() -> Self {
		Self(Default::default())
	}
}
