//! Provides high-level access to the EEPROM APIs.

use crate::common::Lockable;
use crate::error::Error;
use crate::helpers::{Ignore, NullAndStringOr, OneValue};
use minicbor::bytes::ByteSlice;
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_safe::{
	component::{Invoker, MethodCallError},
	Address,
};

/// The type name for EEPROM components.
pub const TYPE: &str = "eeprom";

/// An EEPROM component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Eeprom(Address);

impl Eeprom {
	/// Creates a wrapper around an EEPROM.
	///
	/// The `address` parameter is the address of the EEPROM. It is not checked for correctness at
	/// this time because network topology could change after this function returns; as such, each
	/// usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the EEPROM.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Eeprom {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// An EEPROM component on which methods can be invoked.
///
/// This type combines an EEPROM address, an [`Invoker`](Invoker) that can be used to make method
/// calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this type
/// can be created by calling [`Eeprom::lock`](Eeprom::lock), and it can be dropped to return the
/// borrow of the invoker and buffer to the caller so they can be reused for other purposes.
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
	/// Returns the contents of the main storage area.
	///
	/// In an EEPROM used for booting, the main storage area contains the BIOS code.
	///
	/// The returned byte slice points into, and therefore retains ownership of, the scratch
	/// buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn get(self) -> Result<&'buffer [u8], Error> {
		let ret: OneValue<&ByteSlice> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "get", None)
				.await?;
		Ok(ret.0)
	}

	/// Writes to the main storage area.
	///
	/// In an EEPROM used for booting, the main storage area contains the BIOS code.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`DataTooLarge`](Error::DataTooLarge)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`StorageReadOnly`](Error::StorageReadOnly)
	pub async fn set(&mut self, data: &[u8]) -> Result<(), Error> {
		let data: &ByteSlice = data.into();
		Self::map_errors::<Ignore>(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"set",
				Some(&OneValue(data)),
			)
			.await,
			Error::DataTooLarge,
		)?;
		Ok(())
	}

	/// Returns the label, if it has one.
	///
	/// The label is displayed in the item’s tooltip.
	///
	/// The returned string slice points into, and therefore retains ownership of, the scratch
	/// buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn get_label(self) -> Result<&'buffer str, Error> {
		let ret: OneValue<_> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getLabel",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Sets the label and returns the new label, which may be truncated.
	///
	/// The label is displayed in the item’s tooltip.
	///
	/// The returned string slice points into, and therefore retains ownership of, the scratch
	/// buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`StorageReadOnly`](Error::StorageReadOnly)
	pub async fn set_label(self, label: &str) -> Result<&'buffer str, Error> {
		let ret: OneValue<_> = Self::map_errors(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"setLabel",
				Some(&OneValue(label)),
			)
			.await,
			Error::BadComponent(oc_wasm_safe::error::Error::Unknown),
		)?;
		Ok(ret.0)
	}

	/// Returns the capacity, in bytes, of the main storage area.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn get_size(&mut self) -> Result<usize, Error> {
		let ret: OneValue<_> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "getSize", None)
				.await?;
		Ok(ret.0)
	}

	/// Returns the CRC32 of the contents of the main storage area.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn get_checksum(&mut self) -> Result<u32, Error> {
		let ret: OneValue<&'_ str> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getChecksum",
			None,
		)
		.await?;
		let ret = ret.0;
		if let Ok(ret) = u32::from_str_radix(ret, 16) {
			Ok(ret)
		} else {
			Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
		}
	}

	/// Makes the EEPROM read-only.
	///
	/// A read-only EEPROM’s main storage area and label cannot be modified. Its volatile data area
	/// can still be modified. A read-only EEPROM cannot be made read-write again later.
	///
	/// For safety, the checksum value must be passed as a parameter.
	///
	/// If the EEPROM is already read-only, this method successfully does nothing (assuming the
	/// checksum is correct).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`ChecksumMismatch`](Error::ChecksumMismatch)
	pub async fn make_read_only(&mut self, checksum: u32) -> Result<(), Error> {
		Self::map_errors::<Ignore>(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"makeReadonly",
				Some(&OneValue(&alloc::format!("{:08x}", checksum))),
			)
			.await,
			Error::BadComponent(oc_wasm_safe::error::Error::Unknown),
		)?;
		Ok(())
	}

	/// Returns the capacity, in bytes, of the volatile data area.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn get_data_size(&mut self) -> Result<usize, Error> {
		let ret: OneValue<_> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getDataSize",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the contents of the volatile data area.
	///
	/// In an EEPROM used for booting, the volatile data area contains the UUID of the filesystem
	/// to prefer booting from.
	///
	/// The returned byte slice points into, and therefore retains ownership of, the scratch
	/// buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn get_data(self) -> Result<&'buffer [u8], Error> {
		let ret: OneValue<&ByteSlice> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "getData", None)
				.await?;
		Ok(ret.0)
	}

	/// Writes to the volatile data area.
	///
	/// In an EEPROM used for booting, the volatile data area contains the UUID of the filesystem
	/// to prefer booting from.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`DataTooLarge`](Error::DataTooLarge)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	pub async fn set_data(&mut self, data: &[u8]) -> Result<(), Error> {
		let data: &ByteSlice = data.into();
		Self::map_errors::<Ignore>(
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"setData",
				Some(&OneValue(data)),
			)
			.await,
			Error::DataTooLarge,
		)?;
		Ok(())
	}

	/// Given a `Result<NullAndStringOr<T>, MethodCallError>`, maps the errors (both exceptions and
	/// null-and-string-style errors) to appropriate error constants, returning any success object
	/// unmodified. [`BadParameters`](MethodCallError::BadParameters) is mapped to a specified
	/// value.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned for any unrecognized error.
	/// * [`ChecksumMismatch`](Error::ChecksumMismatch)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`StorageReadOnly`](Error::StorageReadOnly)
	fn map_errors<T>(
		x: Result<NullAndStringOr<'_, T>, MethodCallError<'_>>,
		bad_parameters: Error,
	) -> Result<T, Error> {
		match x {
			Ok(NullAndStringOr::Ok(x)) => Ok(x),
			Ok(NullAndStringOr::Err("incorrect checksum")) => Err(Error::ChecksumMismatch),
			Ok(NullAndStringOr::Err("not enough energy")) => Err(Error::NotEnoughEnergy),
			Ok(NullAndStringOr::Err("storage is readonly")) => Err(Error::StorageReadOnly),
			Ok(NullAndStringOr::Err(_)) => {
				Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
			}
			Err(MethodCallError::BadParameters(_)) => Err(bad_parameters),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}
}
