//! Provides high-level access to the refinery APIs.

use crate::error::Error;
use minicbor::{Decode, Decoder};
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{
	fluid::{Fluid, Tank},
	inventory::{ItemStack, OptionItemStack},
	map_decoder, Lockable,
};
use oc_wasm_safe::{
	component::{Invoker, MethodCallError},
	Address,
};

/// The type name for refineries.
pub const TYPE: &str = "ie_refinery";

/// A refinery component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Refinery(Address);

impl Refinery {
	/// Creates a wrapper around a refinery.
	///
	/// The `address` parameter is the address of the refinery. It is not checked for correctness
	/// at this time because network topology could change after this function returns; as such,
	/// each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the refinery.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Refinery {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A refinery component on which methods can be invoked.
///
/// This type combines a refinery address, an [`Invoker`](Invoker) that can be used to make method
/// calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this type
/// can be created by calling [`Refinery::lock`](Refinery::lock), and it can be dropped to return
/// the borrow of the invoker and buffer to the caller so they can be reused for other purposes.
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
	/// Returns the amount of energy stored in the refinery’s internal buffer.
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

	/// Returns the maximum amount of energy the refinery’s internal buffer can hold.
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

	/// Returns the two input tanks.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn input_tanks(self) -> Result<[Tank<'buffer>; 2], Error> {
		struct Return<'buffer> {
			inputs: [Tank<'buffer>; 2],
		}
		impl<'buffer, Context> Decode<'buffer, Context> for Return<'buffer> {
			fn decode(
				d: &mut Decoder<'buffer>,
				context: &mut Context,
			) -> Result<Self, minicbor::decode::Error> {
				map_decoder::decode::<ReturnBuilder<'buffer>, _>(d, context)
			}
		}
		#[derive(Default)]
		struct ReturnBuilder<'buffer> {
			input1: Option<Tank<'buffer>>,
			input2: Option<Tank<'buffer>>,
		}
		impl<'buffer> map_decoder::Builder<'buffer> for ReturnBuilder<'buffer> {
			type Output = Return<'buffer>;

			fn entry<Context>(
				&mut self,
				key: &str,
				d: &mut Decoder<'buffer>,
				context: &mut Context,
			) -> Result<bool, minicbor::decode::Error> {
				if key == "input1" {
					self.input1 = Some(d.decode_with(context)?);
					Ok(true)
				} else if key == "input2" {
					self.input2 = Some(d.decode_with(context)?);
					Ok(true)
				} else {
					Ok(false)
				}
			}

			fn build(self) -> Result<Self::Output, minicbor::decode::Error> {
				if let (Some(input1), Some(input2)) = (self.input1, self.input2) {
					Ok(Return {
						inputs: [input1, input2],
					})
				} else {
					Err(minicbor::decode::Error::message(
						"missing key in input tanks",
					))
				}
			}
		}

		let ret: (Return<'buffer>,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getInputFluidTanks",
			None,
		)
		.await?;
		Ok(ret.0.inputs)
	}

	/// Returns the output tank.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn output_tank(self) -> Result<Tank<'buffer>, Error> {
		let ret: (Tank<'buffer>,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getOutputTank",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the recipe currently being refined.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn recipe(self) -> Result<Option<Recipe<'buffer>>, Error> {
		let ret: Result<(Recipe<'buffer>,), MethodCallError<'_>> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getRecipe",
			None,
		)
		.await;
		match ret {
			Ok((r,)) => Ok(Some(r)),
			Err(e @ MethodCallError::BadParameters(exp)) => {
				// If processQueue.get(0).recipe is null, this exception is thrown.
				const MESSAGE: &str = "The recipe of the refinery is invalid";
				let mut message_buffer = [0_u8; MESSAGE.len()];
				if exp.message(&mut message_buffer) == Ok(MESSAGE) {
					Ok(None)
				} else {
					Err(e.into())
				}
			}
			Err(e @ MethodCallError::Other(exp)) => {
				// Immersive Engineering implements this method by returning
				// processQueue.get(0).recipe. If processQueue is empty, this throws an
				// IndexOutOfBoundsException because that condition is not checked.
				if exp.is_type("java.lang.IndexOutOfBoundsException") {
					Ok(None)
				} else {
					Err(e.into())
				}
			}
			Err(e) => Err(e.into()),
		}
	}

	/// Returns whether or not the fluids in the refinery are refinable.
	///
	/// This also returns `false` if the fluids are refinable, but the refining process has not yet
	/// started because the machine is disabled or unpowered.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn has_valid_recipe(&mut self) -> Result<bool, Error> {
		let ret: Result<(bool,), MethodCallError<'_>> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"isValidRecipe",
			None,
		)
		.await;
		match ret {
			Ok((x,)) => Ok(x),
			Err(e @ MethodCallError::Other(exp)) => {
				// Immersive Engineering implements this method by checking whether
				// processQueue.get(0).recipe is null or not. If processQueue is empty, this throws
				// an IndexOutOfBoundsException because that condition is not checked.
				if exp.is_type("java.lang.IndexOutOfBoundsException") {
					Ok(false)
				} else {
					Err(e.into())
				}
			}
			Err(e) => Err(e.into()),
		}
	}

	/// Returns the empty canisters.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn empty_canisters(self) -> Result<Canisters<'buffer>, Error> {
		self.canisters("getEmptyCannisters").await
	}

	/// Returns the full canisters.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn full_canisters(self) -> Result<Canisters<'buffer>, Error> {
		self.canisters("getFullCannisters").await
	}

	/// Returns the empty or filled canisters.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	async fn canisters(self, method: &str) -> Result<Canisters<'buffer>, Error> {
		let ret: (Canisters<'buffer>,) =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, method, None)
				.await?;
		Ok(ret.0)
	}

	/// Enables or disables computer control of the refinery.
	///
	/// If `enable` is `true`, the refinery runs or stops based on the most recent call to
	/// [`set_enabled`](#set_enabled). If `enable` is `false`, the refinery runs or stops based on
	/// the redstone signal at the control port.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn enable_computer_control(&mut self, enable: bool) -> Result<(), Error> {
		crate::helpers::enable_computer_control(&self.address, self.invoker, self.buffer, enable)
			.await
	}

	/// Enables or disables the refinery.
	///
	/// This can only be called if the refinery is under computer control via a preceding call to
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

/// A collection of canisters in either the empty or filled canisters slots.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Canisters<'buffer> {
	/// The canisters for the input tanks.
	pub inputs: [Option<ItemStack<'buffer>>; 2],

	/// The canisters for the output tank.
	pub output: Option<ItemStack<'buffer>>,
}

impl<'buffer, Context> Decode<'buffer, Context> for Canisters<'buffer> {
	fn decode(
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
		map_decoder::decode::<CanistersBuilder<'buffer>, _>(d, context)
	}
}

/// A map-decoding builder for a [`Canisters`](Canisters).
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CanistersBuilder<'buffer> {
	/// The canisters for the two input fluid tanks.
	inputs: [Option<ItemStack<'buffer>>; 2],

	/// The canisters for the output fluid tank.
	output: Option<ItemStack<'buffer>>,
}

impl<'buffer> map_decoder::Builder<'buffer> for CanistersBuilder<'buffer> {
	type Output = Canisters<'buffer>;

	fn entry<Context>(
		&mut self,
		key: &str,
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<bool, minicbor::decode::Error> {
		if key == "input1" {
			self.inputs[0] = d
				.decode_with::<_, OptionItemStack<'buffer>>(context)?
				.into();
			Ok(true)
		} else if key == "input2" {
			self.inputs[1] = d
				.decode_with::<_, OptionItemStack<'buffer>>(context)?
				.into();
			Ok(true)
		} else if key == "output" {
			self.output = d
				.decode_with::<_, OptionItemStack<'buffer>>(context)?
				.into();
			Ok(true)
		} else {
			Ok(false)
		}
	}

	fn build(self) -> Result<Canisters<'buffer>, minicbor::decode::Error> {
		Ok(Canisters {
			inputs: self.inputs,
			output: self.output,
		})
	}
}

/// A recipe the refinery can execute.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding strings to which the object
/// refers.
#[derive(Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct Recipe<'buffer> {
	/// The two input fluids.
	pub inputs: [Fluid<'buffer>; 2],

	/// The output fluid.
	pub output: Fluid<'buffer>,
}

impl<'buffer, Context> Decode<'buffer, Context> for Recipe<'buffer> {
	fn decode(
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
		map_decoder::decode::<RecipeBuilder<'buffer>, _>(d, context)
	}
}

/// A map-decoding builder for a [`Recipe`](Recipe).
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RecipeBuilder<'buffer> {
	/// The two input fluids.
	inputs: [Option<Fluid<'buffer>>; 2],

	/// The output fluid.
	output: Option<Fluid<'buffer>>,
}

impl<'buffer> map_decoder::Builder<'buffer> for RecipeBuilder<'buffer> {
	type Output = Recipe<'buffer>;

	fn entry<Context>(
		&mut self,
		key: &str,
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<bool, minicbor::decode::Error> {
		if key == "input1" {
			self.inputs[0] = Some(d.decode_with(context)?);
			Ok(true)
		} else if key == "input2" {
			self.inputs[1] = Some(d.decode_with(context)?);
			Ok(true)
		} else if key == "output" {
			self.output = Some(d.decode_with(context)?);
			Ok(true)
		} else {
			Ok(false)
		}
	}

	fn build(self) -> Result<Recipe<'buffer>, minicbor::decode::Error> {
		if let ([Some(input1), Some(input2)], Some(output)) = (self.inputs, self.output) {
			Ok(Recipe {
				inputs: [input1, input2],
				output,
			})
		} else {
			Err(minicbor::decode::Error::message("missing key in recipe"))
		}
	}
}
