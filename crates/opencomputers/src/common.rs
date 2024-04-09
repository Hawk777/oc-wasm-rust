use core::convert::TryFrom;
use minicbor::{Decode, Encode};

pub use oc_wasm_helpers::{
	error::TryFromInt as TryFromIntError,
	fluid::{Fluid, Tank},
	inventory::ItemStack,
	sides::{Absolute as AbsoluteSide, Relative as RelativeSide, Side, BLOCK_SIDES},
	Lockable,
};

/// A rectangular dimension
#[derive(Clone, Copy, Debug, Decode, Encode, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cbor(array)]
pub struct Dimension {
	/// The width of the rectangle.
	#[n(0)]
	pub width: u32,

	/// The height of the rectangle.
	#[n(1)]
	pub height: u32,
}

/// A nonnegative position coordinate.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Point {
	/// The X coordinate.
	pub x: u32,

	/// The Y coordinate.
	pub y: u32,
}

/// A 2D integer vector.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Vector2 {
	/// The vector’s X component.
	pub x: i32,

	/// The vector’s Y component.
	pub y: i32,
}

/// The number of colours in the standard Minecraft colour table.
pub const COLOURS: usize = 16;

/// A colour.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Colour {
	White,
	Orange,
	Magenta,
	LightBlue,
	Yellow,
	Lime,
	Pink,
	Grey,
	LightGrey,
	Cyan,
	Purple,
	Blue,
	Brown,
	Green,
	Red,
	Black,
}

impl From<Colour> for u8 {
	fn from(x: Colour) -> u8 {
		match x {
			Colour::White => 0,
			Colour::Orange => 1,
			Colour::Magenta => 2,
			Colour::LightBlue => 3,
			Colour::Yellow => 4,
			Colour::Lime => 5,
			Colour::Pink => 6,
			Colour::Grey => 7,
			Colour::LightGrey => 8,
			Colour::Cyan => 9,
			Colour::Purple => 10,
			Colour::Blue => 11,
			Colour::Brown => 12,
			Colour::Green => 13,
			Colour::Red => 14,
			Colour::Black => 15,
		}
	}
}

impl From<Colour> for usize {
	fn from(x: Colour) -> usize {
		u8::from(x) as usize
	}
}

impl TryFrom<u8> for Colour {
	type Error = oc_wasm_helpers::error::TryFromInt;

	fn try_from(x: u8) -> Result<Self, Self::Error> {
		match x {
			0 => Ok(Colour::White),
			1 => Ok(Colour::Orange),
			2 => Ok(Colour::Magenta),
			3 => Ok(Colour::LightBlue),
			4 => Ok(Colour::Yellow),
			5 => Ok(Colour::Lime),
			6 => Ok(Colour::Pink),
			7 => Ok(Colour::Grey),
			8 => Ok(Colour::LightGrey),
			9 => Ok(Colour::Cyan),
			10 => Ok(Colour::Purple),
			11 => Ok(Colour::Blue),
			12 => Ok(Colour::Brown),
			13 => Ok(Colour::Green),
			14 => Ok(Colour::Red),
			15 => Ok(Colour::Black),
			_ => Err(oc_wasm_helpers::error::TryFromInt),
		}
	}
}

/// A 24-bit RGB colour.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Rgb(pub u32);
