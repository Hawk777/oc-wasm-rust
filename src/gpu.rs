//! Provides high-level access to the GPU APIs.

use crate::common::{Dimension, Lockable, Point, Rgb, Vector2};
use crate::error::Error;
use crate::helpers::{FiveValues, Ignore, NullAndStringOr, OneValue, TwoValues};
use alloc::vec::Vec;
use core::fmt::{Debug, Formatter};
use minicbor::Encode;
use oc_wasm_futures::invoke::component_method;
use oc_wasm_safe::{
	component::{Invoker, MethodCallError},
	Address,
};

/// The type name for GPU components.
pub const TYPE: &str = "gpu";

/// A GPU component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Gpu(Address);

impl Gpu {
	/// Creates a wrapper around a GPU.
	///
	/// The `address` parameter is the address of the GPU. It is not checked for correctness at
	/// this time because network topology could change after this function returns; as such, each
	/// usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the GPU.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'a> Lockable<'a, 'a> for Gpu {
	type Locked = Locked<'a>;

	fn lock(&self, invoker: &'a mut Invoker, buffer: &'a mut Vec<u8>) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A GPU component on which methods can be invoked.
///
/// This type combines a GPU address, an [`Invoker`](Invoker) that can be used to make method
/// calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this type
/// can be created by calling [`Gpu::lock`](Gpu::lock), and it can be dropped to return the borrow
/// of the invoker and buffer to the caller so they can be reused for other purposes.
///
/// The `'a` lifetime is the lifetime of the invoker and the buffer.
pub struct Locked<'a> {
	/// The component address.
	address: Address,

	/// The invoker.
	invoker: &'a mut Invoker,

	/// The buffer.
	buffer: &'a mut Vec<u8>,
}

impl<'a> Locked<'a> {
	/// Binds the GPU to a screen.
	///
	/// If `reset` is `true`, then the screen is reset to its maximum resolution and colour depth,
	/// and its colours are set to white on black. The text content, however, is not cleared.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn bind(&mut self, screen: Address, reset: bool) -> Result<(), Error> {
		let ret: NullAndStringOr<'_, Ignore> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"bind",
			Some(&TwoValues(screen, reset)),
		)
		.await?;
		match ret {
			NullAndStringOr::Ok(_) => Ok(()),
			NullAndStringOr::Err(_) => Err(Error::BadScreen),
		}
	}

	/// Returns the address of the screen the GPU is bound to, or `None` if it is unbound.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn get_screen(&mut self) -> Result<Option<Address>, Error> {
		let ret: NullAndStringOr<'_, OneValue<_>> =
			component_method::<(), _>(self.invoker, self.buffer, &self.address, "getScreen", None)
				.await?;
		Ok(match ret {
			NullAndStringOr::Ok(ret) => ret.0,
			NullAndStringOr::Err(_) => None,
		})
	}

	/// Returns the background colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn get_background(&mut self) -> Result<Colour, Error> {
		self.get_colour("getBackground").await
	}

	/// Sets the background colour, returning the old colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadPaletteIndex`](Error::BadPaletteIndex)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn set_background(
		&mut self,
		colour: Colour,
	) -> Result<(Rgb, Option<PaletteIndex>), Error> {
		self.set_colour(colour, "setBackground").await
	}

	/// Returns the foreground colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn get_foreground(&mut self) -> Result<Colour, Error> {
		self.get_colour("getForeground").await
	}

	/// Sets the foreground colour, returning the old colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadPaletteIndex`](Error::BadPaletteIndex)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn set_foreground(
		&mut self,
		colour: Colour,
	) -> Result<(Rgb, Option<PaletteIndex>), Error> {
		self.set_colour(colour, "setForeground").await
	}

	/// Returns the colour at a palette index.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadPaletteIndex`](Error::BadPaletteIndex)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn get_palette_colour(&mut self, index: PaletteIndex) -> Result<Rgb, Error> {
		let ret: Result<NullAndStringOr<'_, OneValue<u32>>, _> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getPaletteColor",
			Some(&OneValue(index.0)),
		)
		.await;
		let ret = Self::map_bad_parameters(ret, Error::BadPaletteIndex)?;
		let ret = Self::map_no_screen(ret)?;
		Ok(Rgb(ret.0))
	}

	/// Sets the colour at a palette index, returning the old colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadPaletteIndex`](Error::BadPaletteIndex)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn set_palette_colour(
		&mut self,
		index: PaletteIndex,
		colour: Rgb,
	) -> Result<Rgb, Error> {
		let ret: Result<NullAndStringOr<'_, OneValue<u32>>, _> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setPaletteColor",
			Some(&TwoValues(index.0, colour.0)),
		)
		.await;
		let ret = Self::map_bad_parameters(ret, Error::BadPaletteIndex)?;
		let ret = Self::map_no_screen(ret)?;
		Ok(Rgb(ret.0))
	}

	/// Returns the maximum supported colour depth based on the tiers of the GPU and the bound
	/// screen.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn max_depth(&mut self) -> Result<u8, Error> {
		let ret: NullAndStringOr<'_, OneValue<_>> =
			component_method::<(), _>(self.invoker, self.buffer, &self.address, "maxDepth", None)
				.await?;
		let ret = Self::map_no_screen(ret)?;
		Ok(ret.0)
	}

	/// Returns the colour depth currently in use.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn get_depth(&mut self) -> Result<u8, Error> {
		let ret: NullAndStringOr<'_, OneValue<_>> =
			component_method::<(), _>(self.invoker, self.buffer, &self.address, "getDepth", None)
				.await?;
		let ret = Self::map_no_screen(ret)?;
		Ok(ret.0)
	}

	/// Sets the colour depth.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadDepth`](Error::BadDepth)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn set_depth(&mut self, depth: u8) -> Result<(), Error> {
		let ret: Result<NullAndStringOr<'_, Ignore>, _> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setDepth",
			Some(&OneValue(depth)),
		)
		.await;
		let ret = Self::map_bad_parameters(ret, Error::BadDepth)?;
		Self::map_no_screen(ret)?;
		Ok(())
	}

	/// Returns the maximum supported resolution based on the tiers of the GPU and the bound
	/// screen.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn max_resolution(&mut self) -> Result<Dimension, Error> {
		self.get_dimension("maxResolution").await
	}

	/// Returns the resolution currently in use.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn get_resolution(&mut self) -> Result<Dimension, Error> {
		self.get_dimension("getResolution").await
	}

	/// Sets the screen resolution, returning whether or not it was changed from its previous
	/// value.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadCoordinate`](Error::BadCoordinate)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn set_resolution(&mut self, resolution: Dimension) -> Result<bool, Error> {
		self.set_dimension("setResolution", resolution).await
	}

	/// Returns the viewport currently in use.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn get_viewport(&mut self) -> Result<Dimension, Error> {
		self.get_dimension("getViewport").await
	}

	/// Sets the viewport, returning whether or not it was changed from its previous value.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadCoordinate`](Error::BadCoordinate)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn set_viewport(&mut self, resolution: Dimension) -> Result<bool, Error> {
		self.set_dimension("setViewport", resolution).await
	}

	/// Returns the character and colours at a specific character cell.
	///
	/// It is possible for a Java string to contain an unpaired UTF-16 surrogate half. It is
	/// possible (in Lua, at least) to place such a byte sequence into a character cell in a
	/// screen. Should this method be called on such a character cell, a replacement character is
	/// silently returned instead.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadCoordinate`](Error::BadCoordinate)
	/// * [`BadScreen`](Error::BadScreen)
	pub async fn get(&mut self, point: Point) -> Result<CharacterCellContents, Error> {
		type Return<'character> = FiveValues<&'character str, u32, u32, Option<u32>, Option<u32>>;
		let ret: Result<NullAndStringOr<'_, Return<'_>>, _> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"get",
			Some(&TwoValues(point.x, point.y)),
		)
		.await;
		let ret = Self::map_bad_parameters(ret, Error::BadCoordinate)?;
		let ret = Self::map_no_screen(ret)?;
		if let Some(character) = ret.0.chars().next() {
			Ok(CharacterCellContents {
				character,
				foreground: (Rgb(ret.1), ret.3.map(PaletteIndex)),
				background: (Rgb(ret.2), ret.4.map(PaletteIndex)),
			})
		} else {
			// A GPU’s get method never returns an empty string. Therefore, if we see one,
			// we must not have been talking to a GPU.
			Err(Error::BadComponent(oc_wasm_safe::error::Error::CborDecode))
		}
	}

	/// Writes text to the screen.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	pub async fn set(
		&mut self,
		position: Point,
		text: &str,
		direction: TextDirection,
	) -> Result<(), Error> {
		#[derive(Encode)]
		#[cbor(array)]
		struct Params<'a> {
			#[n(0)]
			x: u32,
			#[n(1)]
			y: u32,
			#[n(2)]
			value: &'a str,
			#[n(3)]
			direction: TextDirection,
		}
		let ret: NullAndStringOr<'_, Ignore> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"set",
			Some(&Params {
				x: position.x,
				y: position.y,
				value: text,
				direction,
			}),
		)
		.await?;
		match ret {
			NullAndStringOr::Ok(_) => Ok(()),
			NullAndStringOr::Err("no screen") => Err(Error::BadScreen),
			NullAndStringOr::Err("not enough energy") => Err(Error::NotEnoughEnergy),
			NullAndStringOr::Err(_) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
		}
	}

	/// Copies data from one rectangle to another.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	pub async fn copy(
		&mut self,
		source: Point,
		dimension: Dimension,
		translation: Vector2,
	) -> Result<(), Error> {
		#[derive(Encode)]
		#[cbor(array)]
		struct Params {
			#[n(0)]
			x: u32,
			#[n(1)]
			y: u32,
			#[n(2)]
			width: u32,
			#[n(3)]
			height: u32,
			#[n(4)]
			tx: i32,
			#[n(5)]
			ty: i32,
		}
		let ret: NullAndStringOr<'_, Ignore> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"copy",
			Some(&Params {
				x: source.x,
				y: source.y,
				width: dimension.width,
				height: dimension.height,
				tx: translation.x,
				ty: translation.y,
			}),
		)
		.await?;
		match ret {
			NullAndStringOr::Ok(_) => Ok(()),
			NullAndStringOr::Err("no screen") => Err(Error::BadScreen),
			NullAndStringOr::Err("not enough energy") => Err(Error::NotEnoughEnergy),
			NullAndStringOr::Err(_) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
		}
	}

	/// Fills a rectangle with a character.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	pub async fn fill(
		&mut self,
		target: Point,
		dimension: Dimension,
		character: char,
	) -> Result<(), Error> {
		#[derive(Encode)]
		#[cbor(array)]
		struct Params<'a> {
			#[n(0)]
			x: u32,
			#[n(1)]
			y: u32,
			#[n(2)]
			width: u32,
			#[n(3)]
			height: u32,
			#[n(4)]
			value: &'a str,
		}
		let mut character_buffer = [0_u8; 4];
		let character = character.encode_utf8(&mut character_buffer);
		let ret: Result<NullAndStringOr<'_, Ignore>, _> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"fill",
			Some(&Params {
				x: target.x,
				y: target.y,
				width: dimension.width,
				height: dimension.height,
				value: character,
			}),
		)
		.await;
		match ret {
			Ok(NullAndStringOr::Ok(_)) => Ok(()),
			Ok(NullAndStringOr::Err("no screen")) => Err(Error::BadScreen),
			Ok(NullAndStringOr::Err("not enough energy")) => Err(Error::NotEnoughEnergy),
			Ok(NullAndStringOr::Err(_)) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
			Err(
				e @ (MethodCallError::Other(exception) | MethodCallError::BadParameters(exception)),
			) => {
				// BadParameters would be the sensible error if the character involves more than
				// one UTF-16 code unit. However, OpenComputers throws unqualified Exception, not
				// IllegalArgumentException, in this case, which maps to Other. Just in case they
				// make it more sane later, check both! And for more certainty, check the message
				// too.
				let len = exception.message_length();
				self.buffer.resize(len, 0);
				let message = exception.message(self.buffer).unwrap();
				if message == "invalid fill value" {
					Err(Error::BadFillCharacter)
				} else {
					Err(Error::BadComponent(e.into()))
				}
			}
			Err(e) => {
				// Any other errors convert to BadComponent as usual.
				Err(Error::BadComponent(e.into()))
			}
		}
	}

	/// Returns the foreground or background colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	#[allow(clippy::missing_panics_doc)] // Only encode() calls to a Vec which cannot fail.
	async fn get_colour(&mut self, method: &str) -> Result<Colour, Error> {
		let ret: NullAndStringOr<'_, TwoValues<u32, bool>> =
			component_method::<(), _>(self.invoker, self.buffer, &self.address, method, None)
				.await?;
		let ret = Self::map_no_screen(ret)?;
		Ok(if ret.1 {
			Colour::PaletteIndex(PaletteIndex(ret.0))
		} else {
			Colour::Rgb(Rgb(ret.0))
		})
	}

	/// Sets the foreground or background colour, returning the old colour.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadPaletteIndex`](Error::BadPaletteIndex)
	/// * [`BadScreen`](Error::BadScreen)
	#[allow(clippy::missing_panics_doc)] // Only encode() calls to a Vec which cannot fail.
	async fn set_colour(
		&mut self,
		colour: Colour,
		method: &str,
	) -> Result<(Rgb, Option<PaletteIndex>), Error> {
		let params: TwoValues<u32, bool> = match colour {
			Colour::Rgb(rgb) => TwoValues(rgb.0, false),
			Colour::PaletteIndex(pi) => TwoValues(pi.0, true),
		};
		let ret: Result<NullAndStringOr<'_, TwoValues<u32, Option<u32>>>, _> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			Some(&params),
		)
		.await;
		let ret = Self::map_bad_parameters(ret, Error::BadPaletteIndex)?;
		let ret = Self::map_no_screen(ret)?;
		Ok((Rgb(ret.0), ret.1.map(PaletteIndex)))
	}

	/// Returns the current or maximum resolution or the current viewport size.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadScreen`](Error::BadScreen)
	async fn get_dimension(&mut self, method: &str) -> Result<Dimension, Error> {
		let ret: NullAndStringOr<'_, Dimension> =
			component_method::<(), _>(self.invoker, self.buffer, &self.address, method, None)
				.await?;
		let ret = Self::map_no_screen(ret)?;
		Ok(ret)
	}

	/// Sets the resolution or viewport size, returning whether or not it changed.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadCoordinate`](Error::BadCoordinate)
	/// * [`BadScreen`](Error::BadScreen)
	async fn set_dimension(&mut self, method: &str, parameter: Dimension) -> Result<bool, Error> {
		let ret: Result<NullAndStringOr<'_, OneValue<_>>, _> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			Some(&parameter),
		)
		.await;
		let ret = Self::map_bad_parameters(ret, Error::BadCoordinate)?;
		let ret = Self::map_no_screen(ret)?;
		Ok(ret.0)
	}

	/// Given a “raw” `Result` whose
	/// [`MethodCallError::BadParameters`](MethodCallError::BadParameters) needs to map to one
	/// specific [`Error`](Error) value, with all others mapping to
	/// [`Error::BadComponent`](Error::BadComponent), returns the “cooked” `Result` with the errors
	/// mapped accordingly.
	fn map_bad_parameters<T>(
		x: Result<T, MethodCallError<'_>>,
		bad_parameters: Error,
	) -> Result<T, Error> {
		x.map_err(|e| match e {
			MethodCallError::BadParameters(_) => bad_parameters,
			e => Error::BadComponent(e.into()),
		})
	}

	/// Given a `NullAndStringOr` from a function whose only expected null-and-string return is “no
	/// screen”, maps that error to [`Error::BadScreen`](Error::BadScreen), all other
	/// null-and-string errors to [`Error::BadComponent`](Error::BadComponent), and returns any
	/// success object unmodified.
	fn map_no_screen<T>(x: NullAndStringOr<'_, T>) -> Result<T, Error> {
		match x {
			NullAndStringOr::Ok(x) => Ok(x),
			NullAndStringOr::Err("no screen") => Err(Error::BadScreen),
			NullAndStringOr::Err(_) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
		}
	}
}

impl Debug for Locked<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), core::fmt::Error> {
		Gpu::new(self.address).fmt(f)
	}
}

/// A palette index.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PaletteIndex(pub u32);

/// The types of colours available on a GPU.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Colour {
	/// A 24-bit RGB value.
	Rgb(Rgb),

	/// A palette index.
	PaletteIndex(PaletteIndex),
}

/// The full contents of a character cell.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CharacterCellContents {
	/// The character displayed at this position.
	pub character: char,
	/// The foreground colour.
	pub foreground: (Rgb, Option<PaletteIndex>),
	/// The background colour.
	pub background: (Rgb, Option<PaletteIndex>),
}

/// The possible directions in which text can be written.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum TextDirection {
	/// The text is written from left to write.
	Horizontal,
	/// The text is written from top to bottom.
	Vertical,
}

impl Encode for TextDirection {
	fn encode<W: minicbor::encode::Write>(
		&self,
		e: &mut minicbor::Encoder<W>,
	) -> Result<(), minicbor::encode::Error<W::Error>> {
		e.bool(*self == Self::Vertical)?;
		Ok(())
	}
}
