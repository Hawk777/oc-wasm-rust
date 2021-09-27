//! Provides high-level access to the filesystem APIs.

use crate::error::Error;
use crate::helpers::{Ignore, OneOptionalValue, OneValue, TwoValues};
use alloc::borrow::ToOwned;
use alloc::vec::Vec;
use minicbor::{Decode, Encode};
use oc_wasm_futures::invoke::component_method;
use oc_wasm_safe::{component::Invoker, descriptor, Address};

/// The type name for filesystem components.
pub const TYPE: &str = "filesystem";

/// A filesystem component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Filesystem(Address);

impl Filesystem {
	/// Creates a wrapper around a filesystem.
	///
	/// The `address` parameter is the address of the filesystem. It is not checked for correctness
	/// at this time because network topology could change after this function returns; as such,
	/// each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the filesystem.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}

	/// Locks the filesystem so methods can be invoked on it.
	///
	/// The [`Invoker`](Invoker) and a scratch buffer must be provided. They are released and can
	/// be reused once the [`Locked`](Locked) is dropped.
	#[must_use = "This function is only useful for its return value"]
	pub fn lock<'invoker, 'buffer>(
		&self,
		invoker: &'invoker mut Invoker,
		buffer: &'buffer mut Vec<u8>,
	) -> Locked<'invoker, 'buffer> {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A filesystem component on which methods can be invoked.
///
/// This type combines a filesystem address, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`Filesystem::lock`](Filesystem::lock), and it can be dropped to
/// return the borrow of the invoker and buffer to the caller so they can be reused for other
/// purposes.
///
/// The `'invoker` lifetime is the lifetime of the invoker. The `'buffer` lifetime is the lifetime
/// of the buffer.
pub struct Locked<'invoker, 'buffer> {
	/// The component address.
	address: Address,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut Vec<u8>,
}

impl<'invoker, 'buffer> Locked<'invoker, 'buffer> {
	/// Returns the filesystem’s label, if it has one.
	///
	/// The returned string slice points into, and therefore retains ownership of, the scratch
	/// buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	pub async fn get_label(self) -> Result<Option<&'buffer str>, Error> {
		let ret: OneValue<_> =
			component_method::<(), _>(self.invoker, self.buffer, &self.address, "getLabel", None)
				.await?;
		Ok(ret.0)
	}

	/// Sets or clears the filesystem’s label and returns the new label, which may be truncated.
	///
	/// The returned string slice points into, and therefore retains ownership of, the scratch
	/// buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the drive does not support labels or if the
	///   label cannot be changed.
	pub async fn set_label(self, label: Option<&str>) -> Result<Option<&'buffer str>, Error> {
		let ret: Result<OneValue<_>, oc_wasm_safe::error::Error> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setLabel",
			Some(&OneValue(label)),
		)
		.await;
		if let Err(oc_wasm_safe::error::Error::BadParameters) = ret {
			// This is returned if the filesystem has a label but the label cannot be modified, as
			// happens for a tmpfs.
			Err(Error::Failed("label read only".to_owned()))
		} else {
			Ok(ret?.0)
		}
	}

	/// Returns whether the filesystem is read-only.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	pub async fn is_read_only(&mut self) -> Result<bool, Error> {
		let ret: OneValue<_> =
			component_method::<(), _>(self.invoker, self.buffer, &self.address, "isReadOnly", None)
				.await?;
		Ok(ret.0)
	}

	/// Returns the total capacity of the filesystem, in bytes.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	pub async fn get_space_total(&mut self) -> Result<u64, Error> {
		// The component call returns a u64 if the filesystem has a capacity limit, or an f64 equal
		// to infinity if it doesn’t.
		enum Return {
			Finite(u64),
			Infinite,
		}
		use minicbor::{data::Type, Decoder};
		impl Decode<'_> for Return {
			fn decode(d: &mut Decoder<'_>) -> Result<Self, minicbor::decode::Error> {
				match d.datatype()? {
					Type::F16 | Type::F32 | Type::F64 => {
						d.skip()?;
						Ok(Return::Infinite)
					}
					_ => Ok(Return::Finite(d.u64()?)),
				}
			}
		}
		let ret: OneValue<Return> =
			component_method::<(), _>(self.invoker, self.buffer, &self.address, "spaceTotal", None)
				.await?;
		Ok(match ret.0 {
			Return::Finite(x) => x,
			Return::Infinite => u64::MAX,
		})
	}

	/// Returns the size of all files the filesystem, in bytes.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	pub async fn get_space_used(&mut self) -> Result<u64, Error> {
		let ret: OneValue<_> =
			component_method::<(), _>(self.invoker, self.buffer, &self.address, "spaceUsed", None)
				.await?;
		Ok(ret.0)
	}

	/// Returns whether a file or directory of the given name exists.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	pub async fn exists(&mut self, path: &str) -> Result<bool, Error> {
		self.call_path_to_value("exists", path).await
	}

	/// Returns the size of a file.
	///
	/// If the path does not exist or is a directory, zero is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	pub async fn size(&mut self, path: &str) -> Result<u64, Error> {
		self.call_path_to_value("size", path).await
	}

	/// Returns whether a path refers to an existing directory.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	pub async fn is_directory(&mut self, path: &str) -> Result<bool, Error> {
		self.call_path_to_value("isDirectory", path).await
	}

	/// Returns the last modification time of a file, or the creation time of a directory, in
	/// milliseconds since the UNIX epoch.
	///
	/// If the path does not exist, zero is returned. Zero may also be returned on certain
	/// filesystems which do not track modification times, such as virtual filesystems.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	pub async fn last_modified(&mut self, path: &str) -> Result<u64, Error> {
		self.call_path_to_value("lastModified", path).await
	}

	/// Returns the objects contained within a directory.
	///
	/// The returned string slices point into, and therefore retain ownership of, the scratch
	/// buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the path does not exist or is a file.
	pub async fn list(self, path: &str) -> Result<Vec<DirectoryEntry<'buffer>>, Error> {
		let ret: OneOptionalValue<Vec<DirectoryEntry<'buffer>>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"list",
			Some(&OneValue(path)),
		)
		.await?;
		if let Some(ret) = ret.0 {
			Ok(ret)
		} else {
			Err(Error::Failed("no such directory".to_owned()))
		}
	}

	/// Creates a directory and, if missing, its parents.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the target directory already exists, or if a
	///   directory on the path cannot be created (typically due to lack of space or due to an
	///   intermediate path component being an existing file).
	pub async fn make_directory(&mut self, path: &str) -> Result<(), Error> {
		let created: bool = self.call_path_to_value("makeDirectory", path).await?;
		if created {
			Ok(())
		} else {
			Err(Error::Failed("could not create directory".to_owned()))
		}
	}

	/// Removes a file or directory and its contents.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the removal fails (typically due to the path not
	///   existing).
	pub async fn remove(&mut self, path: &str) -> Result<(), Error> {
		let removed: bool = self.call_path_to_value("remove", path).await?;
		if removed {
			Ok(())
		} else {
			Err(Error::Failed("delete failed".to_owned()))
		}
	}

	/// Renames a file or directory.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the rename fails (typically due to the source
	///   path not existing, the destination path existing and being of a different type than the
	///   source, or the destination being a non-empty directory).
	pub async fn rename(&mut self, source: &str, destination: &str) -> Result<(), Error> {
		let renamed: OneValue<bool> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"rename",
			Some(&TwoValues(source, destination)),
		)
		.await?;
		if renamed.0 {
			Ok(())
		} else {
			Err(Error::Failed("rename failed".to_owned()))
		}
	}

	/// Opens a file in read mode.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the path does not exist or is a directory or if
	///   there are too many open files.
	pub async fn open_read(&mut self, path: &str) -> Result<ReadHandle, Error> {
		Ok(ReadHandle {
			address: self.address,
			descriptor: self.call_open(path, "r").await?,
		})
	}

	/// Opens a file in write mode.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the path is a directory, if the parent of the
	///   path is not a directory, or if there are too many open files.
	pub async fn open_write(&mut self, path: &str, mode: WriteMode) -> Result<WriteHandle, Error> {
		Ok(WriteHandle {
			address: self.address,
			descriptor: self.call_open(path, mode.as_str()).await?,
		})
	}

	/// Performs a method call that takes a path as its only parameter and returns a single value
	/// that does not borrow from the buffer.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	async fn call_path_to_value<T>(&mut self, method: &str, path: &str) -> Result<T, Error>
	where
		for<'a> T: Decode<'a>,
	{
		let ret: OneValue<T> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			Some(&OneValue(path)),
		)
		.await?;
		Ok(ret.0)
	}

	/// Performs an `open` method call.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the method fails.
	async fn call_open(&mut self, path: &str, mode: &str) -> Result<descriptor::Owned, Error> {
		let descriptor: Result<OneValue<descriptor::Decoded>, oc_wasm_safe::error::Error> =
			component_method(
				self.invoker,
				self.buffer,
				&self.address,
				"open",
				Some(&TwoValues(path, mode)),
			)
			.await;
		match descriptor {
			Ok(OneValue(descriptor)) => Ok(
				// SAFETY: This descriptor was just generated by the open() method call, so it must
				// be fresh and unique.
				unsafe { descriptor.into_owned() },
			),
			Err(oc_wasm_safe::error::Error::Other) => Err(Error::Failed("open failed".to_owned())),
			Err(e) => Err(e.into()),
		}
	}
}

/// The type of a directory entry.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum DirectoryEntryType {
	/// The object is a file.
	File,

	/// The object is a directory.
	Directory,
}

/// A directory entry.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DirectoryEntry<'buffer> {
	/// The name of the object.
	pub name: &'buffer str,

	/// The type of the object.
	pub object_type: DirectoryEntryType,
}

impl<'buffer> Decode<'buffer> for DirectoryEntry<'buffer> {
	fn decode(d: &mut minicbor::Decoder<'buffer>) -> Result<Self, minicbor::decode::Error> {
		let name = d.str()?;
		Ok(match name.strip_suffix('/') {
			Some(name) => DirectoryEntry {
				name,
				object_type: DirectoryEntryType::Directory,
			},
			None => DirectoryEntry {
				name,
				object_type: DirectoryEntryType::File,
			},
		})
	}
}

/// The possible ways to open a file for writing.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum WriteMode {
	/// If the file does not exist, creates it as an empty file. If it does exist, deletes its
	/// contents. In either case, returns a handle with initial position zero.
	Truncate,

	/// If the file does not exist, creates it as an empty file. If it does exist, leaves its
	/// contents intact. In either case, returns a handle initially positioned at the end of the
	/// file.
	Append,
}

impl WriteMode {
	fn as_str(self) -> &'static str {
		match self {
			Self::Truncate => "w",
			Self::Append => "a",
		}
	}
}

/// The possible ways to seek within a file.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Seek {
	/// Seeks to an absolute position.
	Set,

	/// Seeks to a position relative to the current position.
	Current,

	/// Seeks to a position relative to the end of the file.
	End,
}

impl Encode for Seek {
	fn encode<W: minicbor::encode::Write>(
		&self,
		e: &mut minicbor::Encoder<W>,
	) -> Result<(), minicbor::encode::Error<W::Error>> {
		e.str(match self {
			Self::Set => "set",
			Self::Current => "cur",
			Self::End => "end",
		})?;
		Ok(())
	}
}

/// A handle to a file open for reading.
///
/// The file is closed when the handle is dropped.
#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ReadHandle {
	/// The filesystem component address.
	address: Address,

	/// The opaque value descriptor.
	descriptor: descriptor::Owned,
}

impl ReadHandle {
	/// Locks the handle so methods can be invoked on it.
	///
	/// The [`Invoker`](Invoker) and a scratch buffer must be provided. They are released and can
	/// be reused once the [`LockedReadHandle`](LockedReadHandle) is dropped.
	#[must_use = "This function is only useful for its return value"]
	pub fn lock<'handle, 'invoker, 'buffer>(
		&'handle self,
		invoker: &'invoker mut Invoker,
		buffer: &'buffer mut Vec<u8>,
	) -> LockedReadHandle<'handle, 'invoker, 'buffer> {
		LockedReadHandle {
			handle: self,
			invoker,
			buffer,
		}
	}
}

/// A readable file handle on which methods can be invoked.
///
/// This type combines a readable file handle, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`ReadHandle::lock`](ReadHandle::lock), and it can be dropped to
/// return the borrow of the invoker and buffer to the caller so they can be reused for other
/// purposes.
///
/// The `'handle` lifetime is the lifetime of the original file handle. The `'invoker` lifetime is
/// the lifetime of the invoker. The `'buffer` lifetime is the lifetime of the buffer.
pub struct LockedReadHandle<'handle, 'invoker, 'buffer> {
	/// The file handle.
	handle: &'handle ReadHandle,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut Vec<u8>,
}

impl<'handle, 'invoker, 'buffer> LockedReadHandle<'handle, 'invoker, 'buffer> {
	/// Seeks to a position in the file and returns the resulting absolute byte position.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the effective seek position is negative.
	pub async fn seek(&mut self, basis: Seek, offset: i64) -> Result<u64, Error> {
		seek_impl(
			self.invoker,
			self.buffer,
			&self.handle.address,
			&self.handle.descriptor,
			basis,
			offset,
		)
		.await
	}

	/// Reads bytes from the file.
	///
	/// `None` is returned if no bytes were read because the handle’s initial position was at or
	/// beyond EOF.
	///
	/// The returned byte slice points into, and therefore retains ownership of, the scratch
	/// buffer. Consequently, the `LockedReadHandle` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the read operation failed, typically because the
	///   computer does not have enough energy stored.
	pub async fn read(self, length: usize) -> Result<Option<&'buffer [u8]>, Error> {
		use minicbor::bytes::ByteSlice;
		#[derive(Encode)]
		#[cbor(array)]
		struct Params<'descriptor>(#[n(0)] &'descriptor descriptor::Owned, #[n(1)] usize);
		let ret: Result<OneValue<Option<&'buffer ByteSlice>>, oc_wasm_safe::error::Error> =
			component_method(
				self.invoker,
				self.buffer,
				&self.handle.address,
				"read",
				Some(&Params(&self.handle.descriptor, length)),
			)
			.await;
		match ret {
			Ok(OneValue(Some(bytes))) => Ok(Some(bytes)),
			Ok(OneValue(None)) => Ok(None),
			Err(oc_wasm_safe::error::Error::Other) => Err(Error::Failed("read failed".to_owned())),
			Err(e) => Err(e.into()),
		}
	}
}

/// A handle to a file open for reading.
///
/// The file is closed when the handle is dropped.
#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct WriteHandle {
	/// The filesystem component address.
	address: Address,

	/// The opaque value descriptor.
	descriptor: descriptor::Owned,
}

impl WriteHandle {
	/// Locks the handle so methods can be invoked on it.
	///
	/// The [`Invoker`](Invoker) and a scratch buffer must be provided. They are released and can
	/// be reused once the [`LockedWriteHandle`](LockedWriteHandle) is dropped.
	#[must_use = "This function is only useful for its return value"]
	pub fn lock<'handle, 'invoker, 'buffer>(
		&'handle self,
		invoker: &'invoker mut Invoker,
		buffer: &'buffer mut Vec<u8>,
	) -> LockedWriteHandle<'handle, 'invoker, 'buffer> {
		LockedWriteHandle {
			handle: self,
			invoker,
			buffer,
		}
	}
}

/// A readable file handle on which methods can be invoked.
///
/// This type combines a readable file handle, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`WriteHandle::lock`](WriteHandle::lock), and it can be dropped
/// to return the borrow of the invoker and buffer to the caller so they can be reused for other
/// purposes.
///
/// The `'handle` lifetime is the lifetime of the original file handle. The `'invoker` lifetime is
/// the lifetime of the invoker. The `'buffer` lifetime is the lifetime of the buffer.
pub struct LockedWriteHandle<'handle, 'invoker, 'buffer> {
	/// The file handle.
	handle: &'handle WriteHandle,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut Vec<u8>,
}

impl<'handle, 'invoker, 'buffer> LockedWriteHandle<'handle, 'invoker, 'buffer> {
	/// Seeks to a position in the file and returns the resulting absolute byte position.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the effective seek position is negative.
	pub async fn seek(&mut self, basis: Seek, offset: i64) -> Result<u64, Error> {
		seek_impl(
			self.invoker,
			self.buffer,
			&self.handle.address,
			&self.handle.descriptor,
			basis,
			offset,
		)
		.await
	}

	/// Writes bytes to the file.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`Failed`](Error::Failed) is returned if the write operation failed, typically because
	///   the computer does not have enough energy stored.
	pub async fn write(&mut self, bytes: &[u8]) -> Result<(), Error> {
		use minicbor::bytes::ByteSlice;
		#[derive(Encode)]
		#[cbor(array)]
		struct Params<'descriptor, 'bytes>(
			#[n(0)] &'descriptor descriptor::Owned,
			#[n(1)] &'bytes ByteSlice,
		);
		let ret: Result<Ignore, oc_wasm_safe::error::Error> = component_method(
			self.invoker,
			self.buffer,
			&self.handle.address,
			"write",
			Some(&Params(&self.handle.descriptor, bytes.into())),
		)
		.await;
		match ret {
			Ok(_) => Ok(()),
			Err(oc_wasm_safe::error::Error::Other) => Err(Error::Failed("write failed".to_owned())),
			Err(e) => Err(e.into()),
		}
	}
}

/// Seeks to a position in a file and returns the resulting absolute byte position.
///
/// # Errors
/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
///   inaccessible, or is not a filesystem.
/// * [`Failed`](Error::Failed) is returned if the effective seek position is negative.
async fn seek_impl(
	invoker: &mut Invoker,
	buffer: &mut Vec<u8>,
	address: &Address,
	descriptor: &descriptor::Owned,
	basis: Seek,
	offset: i64,
) -> Result<u64, Error> {
	#[derive(Encode)]
	#[cbor(array)]
	struct Params<'descriptor>(
		#[n(0)] &'descriptor descriptor::Owned,
		#[n(1)] Seek,
		#[n(2)] i64,
	);
	let ret: Result<OneValue<_>, oc_wasm_safe::error::Error> = component_method(
		invoker,
		buffer,
		address,
		"seek",
		Some(&Params(descriptor, basis, offset)),
	)
	.await;
	match ret {
		Ok(OneValue(position)) => Ok(position),
		Err(oc_wasm_safe::error::Error::BadParameters) => {
			Err(Error::Failed("bad seek position".to_owned()))
		}
		Err(e) => Err(e.into()),
	}
}