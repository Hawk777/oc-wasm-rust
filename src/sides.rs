//! APIs related to sides of blocks.

use crate::error;

/// The number of sides on a block.
pub const BLOCK_SIDES: usize = 6;

/// A trait implemented by both absolute and relative block side enumerations.
pub trait Side: Into<u8> + Into<usize> {}

/// An absolute side of a block.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Absolute {
	Down,
	Up,
	North,
	South,
	West,
	East,
}

impl From<Absolute> for u8 {
	fn from(x: Absolute) -> Self {
		match x {
			Absolute::Down => 0,
			Absolute::Up => 1,
			Absolute::North => 2,
			Absolute::South => 3,
			Absolute::West => 4,
			Absolute::East => 5,
		}
	}
}

impl From<Absolute> for usize {
	fn from(x: Absolute) -> Self {
		u8::from(x) as usize
	}
}

impl TryFrom<u8> for Absolute {
	type Error = error::TryFromInt;

	fn try_from(x: u8) -> Result<Self, Self::Error> {
		match x {
			0 => Ok(Absolute::Down),
			1 => Ok(Absolute::Up),
			2 => Ok(Absolute::North),
			3 => Ok(Absolute::South),
			4 => Ok(Absolute::West),
			5 => Ok(Absolute::East),
			_ => Err(error::TryFromInt),
		}
	}
}

impl Side for Absolute {}

/// A relative side of a block.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Relative {
	Bottom,
	Top,
	Back,
	Front,
	Right,
	Left,
}

impl From<Relative> for u8 {
	fn from(x: Relative) -> Self {
		match x {
			Relative::Bottom => 0,
			Relative::Top => 1,
			Relative::Back => 2,
			Relative::Front => 3,
			Relative::Right => 4,
			Relative::Left => 5,
		}
	}
}

impl From<Relative> for usize {
	fn from(x: Relative) -> Self {
		u8::from(x) as usize
	}
}

impl TryFrom<u8> for Relative {
	type Error = error::TryFromInt;

	fn try_from(x: u8) -> Result<Self, Self::Error> {
		match x {
			0 => Ok(Relative::Bottom),
			1 => Ok(Relative::Top),
			2 => Ok(Relative::Back),
			3 => Ok(Relative::Front),
			4 => Ok(Relative::Right),
			5 => Ok(Relative::Left),
			_ => Err(error::TryFromInt),
		}
	}
}

impl Side for Relative {}
