use core::convert::TryFrom;
use minicbor::{Decode, Encode};

pub use oc_wasm_helpers::{
	fluid::{Fluid, Tank},
	inventory::ItemStack,
	Lockable,
};

/// An error returned when converting an invalid value to an enumeration.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TryFromIntError(pub(crate) ());

impl core::fmt::Display for TryFromIntError {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
		"invalid value".fmt(f)
	}
}

#[cfg(feature = "std")]
impl std::error::Error for TryFromIntError {}

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

/// The number of sides on a block.
pub const BLOCK_SIDES: usize = 6;

/// A trait implemented by both absolute and relative block side enumerations.
pub trait Side: Into<u8> + Into<usize> {}

/// An absolute side of a block.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AbsoluteSide {
	Down,
	Up,
	North,
	South,
	West,
	East,
}

impl From<AbsoluteSide> for u8 {
	fn from(x: AbsoluteSide) -> Self {
		match x {
			AbsoluteSide::Down => 0,
			AbsoluteSide::Up => 1,
			AbsoluteSide::North => 2,
			AbsoluteSide::South => 3,
			AbsoluteSide::West => 4,
			AbsoluteSide::East => 5,
		}
	}
}

impl From<AbsoluteSide> for usize {
	fn from(x: AbsoluteSide) -> Self {
		u8::from(x) as usize
	}
}

impl TryFrom<u8> for AbsoluteSide {
	type Error = TryFromIntError;

	fn try_from(x: u8) -> Result<Self, Self::Error> {
		match x {
			0 => Ok(AbsoluteSide::Down),
			1 => Ok(AbsoluteSide::Up),
			2 => Ok(AbsoluteSide::North),
			3 => Ok(AbsoluteSide::South),
			4 => Ok(AbsoluteSide::West),
			5 => Ok(AbsoluteSide::East),
			_ => Err(TryFromIntError(())),
		}
	}
}

impl Side for AbsoluteSide {}

/// A relative side of a block.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum RelativeSide {
	Bottom,
	Top,
	Back,
	Front,
	Right,
	Left,
}

impl From<RelativeSide> for u8 {
	fn from(x: RelativeSide) -> Self {
		match x {
			RelativeSide::Bottom => 0,
			RelativeSide::Top => 1,
			RelativeSide::Back => 2,
			RelativeSide::Front => 3,
			RelativeSide::Right => 4,
			RelativeSide::Left => 5,
		}
	}
}

impl From<RelativeSide> for usize {
	fn from(x: RelativeSide) -> Self {
		u8::from(x) as usize
	}
}

impl TryFrom<u8> for RelativeSide {
	type Error = TryFromIntError;

	fn try_from(x: u8) -> Result<Self, Self::Error> {
		match x {
			0 => Ok(RelativeSide::Bottom),
			1 => Ok(RelativeSide::Top),
			2 => Ok(RelativeSide::Back),
			3 => Ok(RelativeSide::Front),
			4 => Ok(RelativeSide::Right),
			5 => Ok(RelativeSide::Left),
			_ => Err(TryFromIntError(())),
		}
	}
}

impl Side for RelativeSide {}

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
	type Error = TryFromIntError;

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
			_ => Err(TryFromIntError(())),
		}
	}
}

/// A 24-bit RGB colour.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Rgb(pub u32);
