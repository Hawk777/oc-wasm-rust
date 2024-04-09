//! Provides high-level access to the crusher APIs.

use crate::{common::ItemStack, error::Error};
#[cfg(feature = "alloc")]
use alloc::vec::Vec;
use minicbor::Decoder;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{inventory::OptionItemStack, map_decoder, Lockable};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for crushers.
pub const TYPE: &str = "ie_crusher";

/// A crusher component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Crusher(Address);

impl Crusher {
	/// Creates a wrapper around a crusher.
	///
	/// The `address` parameter is the address of the crusher. It is not checked for correctness at
	/// this time because network topology could change after this function returns; as such, each
	/// usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the crusher.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Crusher {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A crusher component on which methods can be invoked.
///
/// This type combines a crusher address, an [`Invoker`](Invoker) that can be used to make method
/// calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this type
/// can be created by calling [`Crusher::lock`](Crusher::lock), and it can be dropped to return the
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
	/// Returns the amount of energy stored in the crusher’s internal buffer.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn energy_stored(&mut self) -> Result<u32, Error> {
		let ret: (u32,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getEnergyStored",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the maximum amount of energy the crusher’s internal buffer can hold.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn max_energy_stored(&mut self) -> Result<u32, Error> {
		let ret: (u32,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getMaxEnergyStored",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns whether or not the crusher is currently crushing something.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn is_active(&mut self) -> Result<bool, Error> {
		let ret: (bool,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"isActive",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the items that have been dropped into the crusher and not fully crushed yet.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	#[cfg(feature = "alloc")]
	pub async fn input_queue(self) -> Result<Vec<Job<'buffer>>, Error> {
		#[derive(minicbor::Decode)]
		struct Return<'buffer> {
			#[b(0)]
			#[cbor(
				decode_with = "oc_wasm_helpers::decode_one_based_map_as_vector::<Ctx, Job<'_>, Job<'_>>"
			)]
			x: Vec<Job<'buffer>>,
		}

		let ret: Return<'buffer> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getInputQueue",
			None,
		)
		.await?;
		Ok(ret.x)
	}

	/// Enables or disables computer control of the crusher.
	///
	/// If `enable` is `true`, the crusher runs or stops based on the most recent call to
	/// [`set_enabled`](#set_enabled). If `enable` is `false`, the crusher runs or stops based on
	/// the redstone signal at the control port.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn enable_computer_control(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::enable_computer_control(&self.address, self.invoker, self.buffer, enable)
			.await
	}

	/// Enables or disables the crusher.
	///
	/// This can only be called if the crusher is under computer control via a preceding call to
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

/// One or more items of the same type being crushed.
#[cfg(feature = "alloc")]
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Job<'buffer> {
	/// The input item.
	///
	/// The size of the stack is the number of items in this job that have been loaded into the
	/// crusher.
	pub input: ItemStack<'buffer>,

	/// The output item.
	///
	/// The size of the stack is the number of items produced per single input item.
	pub output: ItemStack<'buffer>,

	/// How many ticks the first item of the job has been processing.
	pub progress: u32,

	/// How many ticks are needed to crush one item of the job.
	pub max_progress: u32,
}

#[cfg(feature = "alloc")]
impl<'buffer, Context> minicbor::Decode<'buffer, Context> for Job<'buffer> {
	fn decode(
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
		map_decoder::decode::<JobBuilder<'buffer>, _>(d, context)
	}
}

/// A map-decoding builder for a [`Job`](Job).
#[cfg(feature = "alloc")]
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct JobBuilder<'buffer> {
	/// The input item.
	///
	/// The size of the stack is the number of items in this job that have been loaded into the
	/// crusher.
	input: Option<ItemStack<'buffer>>,

	/// The output item.
	///
	/// The size of the stack is the number of items produced per single input item.
	output: Option<ItemStack<'buffer>>,

	/// How many ticks the first item of the job has been processing.
	progress: Option<u32>,

	/// How many ticks are needed to crush one item of the job.
	max_progress: Option<u32>,
}

#[cfg(feature = "alloc")]
impl<'buffer> map_decoder::Builder<'buffer> for JobBuilder<'buffer> {
	type Output = Job<'buffer>;

	fn entry<Context>(
		&mut self,
		key: &'buffer str,
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<bool, minicbor::decode::Error> {
		if key == "input" {
			let input: Vec<OptionItemStack<'buffer>> = d.decode_with(context).expect("input fail");
			self.input = input
				.into_iter()
				.next()
				.and_then(Option::<ItemStack<'buffer>>::from);
			Ok(true)
		} else if key == "output" {
			self.output = d
				.decode_with::<_, OptionItemStack<'buffer>>(context)
				.expect("output fail")
				.into();
			Ok(true)
		} else if key == "progress" {
			self.progress = Some(d.u32().expect("progress fail"));
			Ok(true)
		} else if key == "maxProgress" {
			self.max_progress = Some(d.u32().expect("maxProgress fail"));
			Ok(true)
		} else {
			Ok(false)
		}
	}

	fn build(self) -> Result<Job<'buffer>, minicbor::decode::Error> {
		if let Some(input) = self.input {
			if let Some(output) = self.output {
				if let Some(progress) = self.progress {
					if let Some(max_progress) = self.max_progress {
						return Ok(Job {
							input,
							output,
							progress,
							max_progress,
						});
					}
				}
			}
		}

		Err(minicbor::decode::Error::message(
			"missing key in input queue job",
		))
	}
}
