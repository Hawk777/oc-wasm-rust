//! Provides high-level access to the core sample drill APIs.

use crate::error::Error;
use minicbor::{Decode, Decoder};
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{Lockable, OneValue};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for core sample drill components.
pub const TYPE: &str = "ie_sample_drill";

/// A core sample drill component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Drill(Address);

impl Drill {
	/// Creates a wrapper around a core sample drill.
	///
	/// The `address` parameter is the address of the core sample drill. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the core sample drill.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Drill {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A core sample drill component on which methods can be invoked.
///
/// This type combines a core sample drill address, an [`Invoker`](Invoker) that can be used to
/// make method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of
/// this type can be created by calling [`Drill::lock`](Drill::lock), and it can be dropped to
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
	/// Returns how far through the drilling process the drill is.
	///
	/// The return value is a number between 0 and 1 inclusive.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn progress(&mut self) -> Result<f32, Error> {
		let ret: OneValue<f32> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getSampleProgress",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns whether or not the drilling process is complete.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn is_finished(&mut self) -> Result<bool, Error> {
		let ret: OneValue<bool> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"isSamplingFinished",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the unlocalized name of the discovered mineral.
	///
	/// `None` is returned if there is no vein here or if the core sample has been removed from the
	/// drill.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotReady`](Error::NotReady) is returned if the drilling process is not complete.
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn vein_unlocalized_name(self) -> Result<Option<&'buffer str>, Error> {
		struct Return<'buffer>(Result<Option<&'buffer str>, Error>);
		impl<'buffer, Context> Decode<'buffer, Context> for Return<'buffer> {
			fn decode(
				d: &mut Decoder<'buffer>,
				_: &mut Context,
			) -> Result<Self, minicbor::decode::Error> {
				let len = d.array()?.ok_or_else(|| {
					minicbor::decode::Error::message("indefinite-length arrays are not supported")
				})?;
				if len == 0 {
					// This means sampling isn’t finished yet.
					Ok(Self(Err(Error::NotReady)))
				} else if len == 1 {
					// This means sampling is finished. An empty string means there is no vein.
					let s = d.str()?;
					Ok(Self(Ok(if s.is_empty() { None } else { Some(s) })))
				} else {
					Err(minicbor::decode::Error::message("incorrect array length"))
				}
			}
		}
		let ret: Return<'buffer> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getVeinUnlocalizedName",
			None,
		)
		.await?;
		ret.0
	}

	/// Returns the localized name of the discovered mineral.
	///
	/// `None` is returned if there is no vein here or if the core sample has been removed from the
	/// drill.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotReady`](Error::NotReady) is returned if the drilling process is not complete.
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn vein_localized_name(self) -> Result<Option<&'buffer str>, Error> {
		struct Return<'buffer>(Result<Option<&'buffer str>, Error>);
		impl<'buffer, Context> Decode<'buffer, Context> for Return<'buffer> {
			fn decode(
				d: &mut Decoder<'buffer>,
				_: &mut Context,
			) -> Result<Self, minicbor::decode::Error> {
				let len = d.array()?.ok_or_else(|| {
					minicbor::decode::Error::message("indefinite-length arrays are not supported")
				})?;
				if len == 0 {
					// This means sampling isn’t finished yet.
					Ok(Self(Err(Error::NotReady)))
				} else if len == 1 {
					// This means sampling is finished. A null means there is no vein.
					if d.datatype()? == minicbor::data::Type::Null {
						d.null()?;
						Ok(Self(Ok(None)))
					} else {
						let s = d.str()?;
						Ok(Self(Ok(Some(s))))
					}
				} else {
					Err(minicbor::decode::Error::message("incorrect array length"))
				}
			}
		}
		let ret: Return<'buffer> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getVeinLocalizedName",
			None,
		)
		.await?;
		ret.0
	}

	/// Returns the integrity level of the vein.
	///
	/// The vein integrity level is normally a number between 0 and 1 indicating what fraction of
	/// the vein’s deposit is still present. 0 is returned if there is no vein here or if the core
	/// sample has been removed from the drill. −1 is returned if mineral deposits are configured
	/// to be infinite.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotReady`](Error::NotReady) is returned if the drilling process is not complete.
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn vein_integrity(&mut self) -> Result<f32, Error> {
		struct Return(Result<f32, Error>);
		impl<Context> Decode<'_, Context> for Return {
			fn decode(
				d: &mut Decoder<'_>,
				_: &mut Context,
			) -> Result<Self, minicbor::decode::Error> {
				let len = d.array()?.ok_or_else(|| {
					minicbor::decode::Error::message("indefinite-length arrays are not supported")
				})?;
				if len == 0 {
					// This means sampling isn’t finished yet.
					Ok(Self(Err(Error::NotReady)))
				} else if len == 1 {
					// This means sampling is finished.
					Ok(Self(Ok(d.f32()?)))
				} else {
					Err(minicbor::decode::Error::message("incorrect array length"))
				}
			}
		}
		let ret: Return = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getVeinIntegrity",
			None,
		)
		.await?;
		ret.0
	}

	/// Returns the amount of ore left in the vein.
	///
	/// `None` is returned if the drilling process is not complete, if the core sample has been
	/// removed from the drill, or if mineral deposits are configured to be infinite. If there is
	/// no vein here, the return value is as if for a full, untouched vein (i.e. the configured
	/// deposit size is returned).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn vein_expected_yield(&mut self) -> Result<Option<u32>, Error> {
		let ret: OneValue<i32> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getVeinExpectedYield",
			None,
		)
		.await?;
		Ok(ret.0.try_into().ok())
	}

	/// Returns the amount of energy stored in the core sample drill’s internal buffer.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
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

	/// Returns the maximum amount of energy the core sample drill’s internal buffer can hold.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
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

	/// Starts over the drilling process.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors](Error::TooManyDescriptors)
	pub async fn reset(&mut self) -> Result<(), Error> {
		component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "reset", None)
			.await?;
		Ok(())
	}
}
