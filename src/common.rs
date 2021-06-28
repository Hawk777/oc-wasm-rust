use minicbor::{Decode, Encode};

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
