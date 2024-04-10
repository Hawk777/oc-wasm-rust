//! Provides high-level access to the ME Interface APIs, which are available in different
//! variations on ME Interfaces in part and block form.

use crate::{error::Error, network_control::NetworkControl};
use core::num::NonZeroU32;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{error::NullAndStringOr, inventory::ItemStack, sides, Lockable};
use oc_wasm_safe::{component::Invoker, Address};

/// An ME Interface block.
///
/// This must not be used for an ME Interface part, but only the full-block form.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Block(Address);

impl Block {
	/// Creates a wrapper around an ME Interface block.
	///
	/// The `address` parameter is the address of the ME Interface. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the component.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}

	/// Returns the common “network control” API.
	#[must_use = "This function is only useful for its return value"]
	pub fn network_control(&self) -> NetworkControl {
		NetworkControl::new(self.0)
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Block {
	type Locked = LockedBlock<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Self::Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// An ME Interface block on which methods can be invoked.
///
/// This type combines an ME Interface block address, an [`Invoker`](Invoker) that can be used to
/// make method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of
/// this type can be created by calling [`Block::lock`](Block::lock), and it can be dropped to
/// return the borrow of the invoker and buffer to the caller so they can be reused for other
/// purposes.
///
/// The `'invoker` lifetime is the lifetime of the invoker. The `'buffer` lifetime is the lifetime
/// of the buffer. The `B` type is the type of scratch buffer to use.
pub struct LockedBlock<'invoker, 'buffer, B: Buffer> {
	/// The component address.
	address: Address,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut B,
}

impl<'invoker, 'buffer, B: Buffer> LockedBlock<'invoker, 'buffer, B> {
	/// Returns the item stack in one of the interface’s configuration (not pattern) slots.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	pub async fn get_configuration(
		self,
		slot: NonZeroU32,
	) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: NullAndStringOr<'buffer, (_,)> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getInterfaceConfiguration",
			Some(&(slot.get(),)),
		)
		.await?;
		match ret {
			NullAndStringOr::Ok(x) => Ok(x.0),
			NullAndStringOr::Err("invalid slot") => Err(Error::BadInventorySlot),
			NullAndStringOr::Err(_) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
		}
	}
}

/// An ME Interface part.
///
/// This must not be used for an ME Interface block, but only the small-part-mounted-on-cable form.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Part(Address);

impl Part {
	/// Creates a wrapper around an ME Interface part.
	///
	/// The `address` parameter is the address of the ME Interface. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the component.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}

	/// Returns the common “network control” API.
	#[must_use = "This function is only useful for its return value"]
	pub fn network_control(&self) -> NetworkControl {
		NetworkControl::new(self.0)
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Part {
	type Locked = LockedPart<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Self::Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// An ME Interface part on which methods can be invoked.
///
/// This type combines an ME Interface part address, an [`Invoker`](Invoker) that can be used to
/// make method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of
/// this type can be created by calling [`Part::lock`](Part::lock), and it can be dropped to return
/// the borrow of the invoker and buffer to the caller so they can be reused for other purposes.
///
/// The `'invoker` lifetime is the lifetime of the invoker. The `'buffer` lifetime is the lifetime
/// of the buffer. The `B` type is the type of scratch buffer to use.
pub struct LockedPart<'invoker, 'buffer, B: Buffer> {
	/// The component address.
	address: Address,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut B,
}

impl<'invoker, 'buffer, B: Buffer> LockedPart<'invoker, 'buffer, B> {
	/// Returns the item stack in one of the interface’s configuration (not pattern) slots.
	///
	/// The `side` parameter indicates which out of six possible ME Interface parts within the
	/// block to address.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadInventorySlot`](Error::BadInventorySlot)
	pub async fn get_configuration(
		self,
		side: sides::Absolute,
		slot: NonZeroU32,
	) -> Result<Option<ItemStack<'buffer>>, Error> {
		let ret: NullAndStringOr<'buffer, (_,)> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getInterfaceConfiguration",
			Some(&(u8::from(side), slot.get())),
		)
		.await?;
		match ret {
			NullAndStringOr::Ok(x) => Ok(x.0),
			NullAndStringOr::Err("invalid slot") => Err(Error::BadInventorySlot),
			NullAndStringOr::Err(_) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
		}
	}
}
