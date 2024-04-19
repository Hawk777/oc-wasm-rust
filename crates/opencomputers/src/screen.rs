//! Provides high-level access to the screen APIs.

use crate::common::Dimension;
use crate::error::Error;
use alloc::vec::Vec;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{error::NullAndStringOr, Lockable};
use oc_wasm_safe::{component::Invoker, Address};

/// The type name for screen components.
pub const TYPE: &str = "screen";

/// A screen component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Screen(Address);

impl Screen {
	/// Creates a wrapper around a screen.
	///
	/// The `address` parameter is the address of the screen. It is not checked for correctness at
	/// this time because network topology could change after this function returns; as such, each
	/// usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the screen.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'a, B: 'a + Buffer> Lockable<'a, 'a, B> for Screen {
	type Locked = Locked<'a, B>;

	fn lock(&self, invoker: &'a mut Invoker, buffer: &'a mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A screen component on which methods can be invoked.
///
/// This type combines a screen address, an [`Invoker`] that can be used to make method calls, and
/// a scratch buffer used to perform CBOR encoding and decoding. A value of this type can be
/// created by calling [`Screen::lock`], and it can be dropped to return the borrow of the invoker
/// and buffer to the caller so they can be reused for other purposes.
///
/// The `'a` lifetime is the lifetime of the invoker and the buffer. The `B` type is the type of
/// scratch buffer to use.
pub struct Locked<'a, B: Buffer> {
	/// The component address.
	address: Address,

	/// The invoker.
	invoker: &'a mut Invoker,

	/// The buffer.
	buffer: &'a mut B,
}

impl<'a, B: Buffer> Locked<'a, B> {
	/// Checks whether the screen is powered on or off.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn is_on(&mut self) -> Result<bool, Error> {
		let ret: (bool,) =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "isOn", None)
				.await?;
		Ok(ret.0)
	}

	/// Powers on the screen, returning whether the power was previously off.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn turn_on(&mut self) -> Result<bool, Error> {
		let ret: (bool, bool) =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "turnOn", None)
				.await?;
		Ok(ret.0)
	}

	/// Powers off the screen, returning whether the power was previously on.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn turn_off(&mut self) -> Result<bool, Error> {
		let ret: (bool, bool) =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "turnOff", None)
				.await?;
		Ok(ret.0)
	}

	/// Returns the screen’s aspect ratio. The aspect ratio is measured in metres.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_aspect_ratio(&mut self) -> Result<Dimension, Error> {
		let ret: (f64, f64) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getAspectRatio",
			None,
		)
		.await?;
		// For some reason the method call’s return value is a pair of f64s, but in reality the
		// numbers are always counts of Minecraft blocks so they are small nonnegative integers.
		#[allow(clippy::cast_possible_truncation)]
		#[allow(clippy::cast_sign_loss)]
		Ok(Dimension {
			width: ret.0 as u32,
			height: ret.1 as u32,
		})
	}

	/// Returns the addresses of the keyboards connected to the screen.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_keyboards(&mut self) -> Result<Vec<Address>, Error> {
		let ret: (Vec<Address>,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getKeyboards",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Sets whether mouse positions are reported at subpixel granularity and returns the old
	/// setting.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported) is returned if the screen is not advanced enough to
	///   return subpixel-granularity touch data.
	pub async fn set_precise(&mut self, precise: bool) -> Result<bool, Error> {
		let ret: NullAndStringOr<'_, (bool,)> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setPrecise",
			Some(&(precise,)),
		)
		.await?;
		match ret {
			NullAndStringOr::Ok((f,)) => Ok(f),
			NullAndStringOr::Err("unsupported operation") => Err(Error::Unsupported),
			NullAndStringOr::Err(_) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
		}
	}

	/// Returns whether mouse positions are reported at subpixel granularity.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn is_precise(&mut self) -> Result<bool, Error> {
		let ret: (bool,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"isPrecise",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Sets whether the touch-screen and open-GUI gestures are inverted from their normal
	/// configuration and returns the old setting.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_touch_mode_inverted(&mut self, inverted: bool) -> Result<bool, Error> {
		let ret: (bool,) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setTouchModeInverted",
			Some(&(inverted,)),
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns whether the touch-screen and open-GUI gestures are inverted from their normal
	/// configuration.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn is_touch_mode_inverted(&mut self) -> Result<bool, Error> {
		let ret: (bool,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"isTouchModeInverted",
			None,
		)
		.await?;
		Ok(ret.0)
	}
}
