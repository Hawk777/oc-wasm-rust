//! Provides an error type used for higher level calls.

use core::fmt::{Display, Formatter};

/// The errors that a component call can return.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Error {
	/// The call was addressed to a component that does not exist, is not accessible, or is of the
	/// wrong type.
	///
	/// The underlying syscall error is encapsulated.
	BadComponent(oc_wasm_safe::error::Error),

	/// An unsupported screen resolution, viewport size, or screen coordinate was requested.
	BadCoordinate,

	/// An unsupported colour depth was requested.
	BadDepth,

	/// The filename passed to a function is structurally invalid to be a filename.
	BadFilename,

	/// The character to use to fill the screen lies outside the basic multilingual plane and
	/// therefore cannot be used for filling.
	BadFillCharacter,

	/// An index number is outside the range of slots for an inventory or tanks for a
	/// fluid-containing block.
	///
	/// In the case of item inventories, this is only returned if the block is in fact an
	/// inventory; [`NoInventory`](Self::NoInventory) is returned for non-inventories. In the case
	/// of fluid containers, this is also returned if the block is not a fluid container at all (it
	/// is treated as a fluid container with zero tanks), but not if the block is in an unloaded
	/// chunk.
	BadInventorySlot,

	/// An item is not usable for the requested purpose.
	///
	/// For example, an item that does not hold fluid cannot have its fluid level or capacity
	/// queried.
	BadItem,

	/// A set-colour request tried to set a colour by palette index, and the palette index was
	/// unacceptable. This could be because the palette index was outside the range of the palette
	/// for the current bit depth, or it could be because the current bit depth does not support a
	/// palette at all.
	BadPaletteIndex,

	/// The address specified when binding a GPU does not exist, is inaccessible, or is not a
	/// screen; or, a GPU operation which requires a screen was executed when the GPU is unbound or
	/// the screen to which it was bound has been disconnected.
	BadScreen,

	/// The robot cannot move because there is something in the way.
	Blocked(super::robot::BlockContent),

	/// The checksum passed when trying to make an EEPROM read-only does not match the checksum of
	/// the contents of the EEPROM.
	ChecksumMismatch,

	/// The data (e.g. to write to a file or EEPROM or send over a communication interface) is too
	/// large.
	DataTooLarge,

	/// The named file or directory does not exist.
	FileNotFound,

	/// The robot tried to move to a position that is too far away from support (and does not have
	/// a suitable hover upgrade to allow such movement), tried to move into an unloaded chunk, or
	/// the destination is otherwise unsuitable for a reason other than there being something in
	/// the way.
	ImpossibleMove,

	/// An attempt was made to move an item or fluid into an inventory slot or tank, but it could
	/// not be inserted because the slot or tank is full, or the existing contents are not
	/// compatible with the new addition.
	InventoryFull,

	/// A seek in a file would place the file pointer at a negative position.
	NegativeSeek,

	/// There is no item in an inventory slot when one is required.
	NoItem,

	/// There is no inventory in the specified location. This includes if the inventory is in an
	/// unloaded chunk.
	///
	/// This is also returned when attempting to access a fluid tank in an unloaded chunk, but not
	/// when attempting to access a fluid tank in a block that does not contain any tanks; in the
	/// latter case, [`BadInventorySlot`](Self::BadInventorySlot) is used instead.
	NoInventory,

	/// The computer does not have enough energy to perform the operation.
	NotEnoughEnergy,

	/// The storage (e.g. EEPROM or filesystem) cannot be written to because it is read-only.
	StorageReadOnly,

	/// There are too many open descriptors.
	TooManyDescriptors,

	/// There are too many listening network ports.
	TooManyOpenPorts,

	/// There are too many parts in a packet.
	TooManyParts,

	/// The operation is not supported for this component.
	///
	/// For example:
	/// * An attempt was made to set a filesystem’s label, but this particular filesystem does not
	///   support labels.
	/// * An attempt was made to access item stack information in an inventory, but detailed item
	///   information access is disabled in the configuration file.
	Unsupported,

	/// The operation failed but no more detailed information is available at the type level. The
	/// individual method may document more specific reasons for this error to appear.
	Failed,
}

impl Error {
	/// Returns a string describing the error.
	#[must_use = "This function is only useful for its return value"]
	pub fn as_str(&self) -> &'static str {
		match self {
			Self::BadComponent(_) => "nonexistent or incorrect component",
			Self::BadCoordinate => "bad coordinate",
			Self::BadDepth => "bad depth",
			Self::BadFilename => "bad filename",
			Self::BadFillCharacter => "bad fill character",
			Self::BadInventorySlot => "bad inventory slot",
			Self::BadItem => "bad item",
			Self::BadPaletteIndex => "bad palette index",
			Self::BadScreen => "bad screen",
			Self::Blocked(super::robot::BlockContent::Entity) => "blocked by entity",
			Self::Blocked(super::robot::BlockContent::Air) => "blocked by air",
			Self::Blocked(super::robot::BlockContent::Liquid) => "blocked by liquid",
			Self::Blocked(super::robot::BlockContent::Replaceable) => "blocked by replaceable",
			Self::Blocked(super::robot::BlockContent::Passable) => "blocked by passable",
			Self::Blocked(super::robot::BlockContent::Solid) => "blocked by solid",
			Self::ChecksumMismatch => "checksum mismatch",
			Self::DataTooLarge => "data too large",
			Self::FileNotFound => "file not found",
			Self::ImpossibleMove => "impossible move",
			Self::InventoryFull => "inventory full",
			Self::NegativeSeek => "seek to negative position",
			Self::NoInventory => "no inventory",
			Self::NoItem => "no item",
			Self::NotEnoughEnergy => "not enough energy",
			Self::StorageReadOnly => "storage is read-only",
			Self::TooManyDescriptors => "too many descriptors",
			Self::TooManyOpenPorts => "too many open ports",
			Self::TooManyParts => "too many parts",
			Self::Unsupported => "unsupported",
			Self::Failed => "failed",
		}
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), core::fmt::Error> {
		match self {
			Self::BadComponent(e) => write!(f, "nonexistent or incorrect component: {e}"),
			_ => self.as_str().fmt(f),
		}
	}
}

#[cfg(feature = "std")]
impl std::error::Error for Error {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::BadComponent(underlying) => Some(underlying),
			Self::BadCoordinate
			| Self::BadDepth
			| Self::BadFilename
			| Self::BadFillCharacter
			| Self::BadInventorySlot
			| Self::BadItem
			| Self::BadPaletteIndex
			| Self::BadScreen
			| Self::Blocked(_)
			| Self::ChecksumMismatch
			| Self::DataTooLarge
			| Self::FileNotFound
			| Self::ImpossibleMove
			| Self::InventoryFull
			| Self::NegativeSeek
			| Self::NoInventory
			| Self::NoItem
			| Self::NotEnoughEnergy
			| Self::StorageReadOnly
			| Self::TooManyDescriptors
			| Self::TooManyOpenPorts
			| Self::TooManyParts
			| Self::Unsupported
			| Self::Failed => None,
		}
	}
}

impl From<oc_wasm_safe::error::Error> for Error {
	fn from(source: oc_wasm_safe::error::Error) -> Self {
		match source {
			oc_wasm_safe::error::Error::BadParameters | oc_wasm_safe::error::Error::Other => {
				Self::Failed
			}
			oc_wasm_safe::error::Error::TooManyDescriptors => Self::TooManyDescriptors,
			_ => Self::BadComponent(source),
		}
	}
}

impl From<oc_wasm_safe::component::MethodCallError<'_>> for Error {
	fn from(source: oc_wasm_safe::component::MethodCallError<'_>) -> Self {
		source.simplify().into()
	}
}
