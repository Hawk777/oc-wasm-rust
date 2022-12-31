//! Provides high-level access to the filesystem APIs.

use crate::error::Error;
use crate::helpers::{max_usize, Ignore};
use alloc::vec::Vec;
use minicbor::{Decode, Encode};
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::Lockable;
use oc_wasm_safe::{
	component::{Invoker, MethodCallError},
	descriptor, extref, Address,
};

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
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Filesystem {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
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
	/// Returns the filesystem’s label, if it has one.
	///
	/// The returned string slice points into, and therefore retains ownership of, the scratch
	/// buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_label(self) -> Result<Option<&'buffer str>, Error> {
		let ret: (Option<&'buffer str>,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getLabel",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Sets or clears the filesystem’s label and returns the new label, which may be truncated.
	///
	/// The returned string slice points into, and therefore retains ownership of, the scratch
	/// buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`StorageReadOnly`](Error::StorageReadOnly) is returned if this filesystem has a label
	///   that cannot be changed.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Unsupported`](Error::Unsupported) is returned if this filesystem does not support
	///   labels.
	pub async fn set_label(self, label: Option<&str>) -> Result<Option<&'buffer str>, Error> {
		// SAFETY: component_method() both encodes and submits the CBOR in one go.
		let label = label.map(|l| unsafe { extref::String::new(l) });
		let ret: Result<(Option<&'buffer str>,), MethodCallError<'_>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setLabel",
			Some(&(label,)),
		)
		.await;
		match ret {
			Ok((label,)) => Ok(label),
			Err(MethodCallError::BadParameters(_)) => Err(Error::StorageReadOnly),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(MethodCallError::Other(_)) => Err(Error::Unsupported),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Returns whether the filesystem is read-only.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn is_read_only(&mut self) -> Result<bool, Error> {
		let ret: (bool,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"isReadOnly",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the total capacity of the filesystem, in bytes.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_space_total(&mut self) -> Result<u64, Error> {
		// The component call returns a u64 if the filesystem has a capacity limit, or an f64 equal
		// to infinity if it doesn’t.
		enum Return {
			Finite(u64),
			Infinite,
		}
		use minicbor::{data::Type, Decoder};
		impl<Context> Decode<'_, Context> for Return {
			fn decode(
				d: &mut Decoder<'_>,
				_: &mut Context,
			) -> Result<Self, minicbor::decode::Error> {
				match d.datatype()? {
					Type::F16 | Type::F32 | Type::F64 => {
						d.skip()?;
						Ok(Return::Infinite)
					}
					_ => Ok(Return::Finite(d.u64()?)),
				}
			}
		}
		let ret: (Return,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"spaceTotal",
			None,
		)
		.await?;
		Ok(match ret.0 {
			Return::Finite(x) => x,
			Return::Infinite => u64::MAX,
		})
	}

	/// Returns the size of all files the filesystem, in bytes.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_space_used(&mut self) -> Result<u64, Error> {
		let ret: (u64,) = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"spaceUsed",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns whether a file or directory of the given name exists.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadFilename`](Error::BadFilename)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn exists(&mut self, path: &str) -> Result<bool, Error> {
		self.call_path_to_value("exists", path).await
	}

	/// Returns the size of a file.
	///
	/// If the path does not exist or is a directory, zero is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadFilename`](Error::BadFilename)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn size(&mut self, path: &str) -> Result<u64, Error> {
		self.call_path_to_value("size", path).await
	}

	/// Returns whether a path refers to an existing directory.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadFilename`](Error::BadFilename)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
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
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadFilename`](Error::BadFilename)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn last_modified(&mut self, path: &str) -> Result<u64, Error> {
		self.call_path_to_value("lastModified", path).await
	}

	/// Returns the objects contained within a directory.
	///
	/// If the name refers to a file rather than a directory, a single entry is returned specifying
	/// the file itself.
	///
	/// The returned string slices point into, and therefore retain ownership of, the scratch
	/// buffer. Consequently, the `Locked` is consumed and cannot be reused.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`FileNotFound`](Error::FileNotFound)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn list(self, path: &str) -> Result<Vec<DirectoryEntry<'buffer>>, Error> {
		// SAFETY: component_method() both encodes and submits the CBOR in one go.
		let path = unsafe { extref::String::new(path) };
		let ret: Result<(Option<Vec<DirectoryEntry<'buffer>>>,), _> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"list",
			Some(&(path,)),
		)
		.await;
		match ret {
			Ok((Some(listing),)) => Ok(listing),
			Ok((None,)) | Err(MethodCallError::Other(_)) => Err(Error::FileNotFound),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(e) => Err(Error::BadComponent(e.into())),
		}
	}

	/// Creates a directory and, if missing, its parents.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadFilename`](Error::BadFilename)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if the target directory already exists, or if a
	///   directory on the path cannot be created (typically due to lack of space, an intermediate
	///   path component being an existing file, or the filesystem being read-only).
	pub async fn make_directory(&mut self, path: &str) -> Result<(), Error> {
		let ret: bool = self.call_path_to_value("makeDirectory", path).await?;
		if ret {
			Ok(())
		} else {
			Err(Error::Failed)
		}
	}

	/// Removes a file or directory and its contents.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadFilename`](Error::BadFilename)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if the removal fails (typically due to the path not
	///   existing or the filesystem being read-only).
	pub async fn remove(&mut self, path: &str) -> Result<(), Error> {
		let removed: bool = self.call_path_to_value("remove", path).await?;
		if removed {
			Ok(())
		} else {
			Err(Error::Failed)
		}
	}

	/// Renames a file or directory.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadFilename`](Error::BadFilename)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if the rename fails (typically due to the source
	///   path not existing, the destination path existing and being of a different type than the
	///   source, the destination being a non-empty directory, or the filesystem being read-only).
	pub async fn rename(&mut self, source: &str, destination: &str) -> Result<(), Error> {
		// SAFETY: component_method() both encodes and submits the CBOR in one go.
		let source = unsafe { extref::String::new(source) };
		let destination = unsafe { extref::String::new(destination) };
		let ret: Result<(bool,), _> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"rename",
			Some(&(source, destination)),
		)
		.await;
		match ret {
			Ok((true,)) => Ok(()),
			Ok((false,)) => Err(Error::Failed),
			Err(MethodCallError::TooManyDescriptors) => Err(Error::TooManyDescriptors),
			Err(MethodCallError::Other(_)) => Err(Error::BadFilename),
			Err(e) => Err(e.into()),
		}
	}

	/// Opens a file in read mode.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`FileNotFound`](Error::FileNotFound)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn open_read(&mut self, path: &str) -> Result<ReadHandle, Error> {
		Ok(ReadHandle {
			address: self.address,
			descriptor: self.call_open(path, "r", Error::FileNotFound).await?,
		})
	}

	/// Opens a file in write mode.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the filesystem does not exist, is
	///   inaccessible, or is not a filesystem.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`Failed`](Error::Failed) is returned if the path is a directory, the parent of the path
	///   is not a directory, or the filesystem is read-only.
	pub async fn open_write(&mut self, path: &str, mode: WriteMode) -> Result<WriteHandle, Error> {
		Ok(WriteHandle {
			address: self.address,
			descriptor: self.call_open(path, mode.as_str(), Error::Failed).await?,
		})
	}

	/// Performs a method call that takes a path as its only parameter and returns a single value
	/// that does not borrow from the buffer.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`BadFilename`](Error::BadFilename)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	async fn call_path_to_value<T>(&mut self, method: &str, path: &str) -> Result<T, Error>
	where
		for<'a> T: Decode<'a, ()>,
	{
		// SAFETY: component_method() both encodes and submits the CBOR in one go.
		let path = unsafe { extref::String::new(path) };
		let ret: Result<(T,), _> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			Some(&(path,)),
		)
		.await;
		match ret {
			Ok((v,)) => Ok(v),
			Err(MethodCallError::Other(_)) => Err(Error::BadFilename),
			Err(e) => Err(e.into()),
		}
	}

	/// Performs an `open` method call.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * The error passed in `file_not_found_error`
	async fn call_open(
		&mut self,
		path: &str,
		mode: &str,
		file_not_found_error: Error,
	) -> Result<descriptor::Owned, Error> {
		// SAFETY: component_method() both encodes and submits the CBOR in one go.
		let path = unsafe { extref::String::new(path) };
		let descriptor: Result<(descriptor::Decoded,), MethodCallError<'_>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"open",
			Some(&(path, mode)),
		)
		.await;
		match descriptor {
			Ok((descriptor,)) => Ok(
				// SAFETY: This descriptor was just generated by the open() method call, so it must
				// be fresh and unique.
				unsafe { descriptor.into_owned() },
			),
			Err(MethodCallError::Other(exception)) => {
				if exception.is_type("java.io.FileNotFoundException") {
					// FileNotFoundException is raised for almost everything.
					Err(file_not_found_error)
				} else if exception.is_type("java.io.IOException") {
					// There appears to be only one non-FileNotFoundException IOException, which is
					// too many open handles. Just in case, check the message.
					const TOO_MANY_OPEN_HANDLES: &str = "too many open handles";
					const ERROR_MESSAGE_BUFFER_SIZE: usize = TOO_MANY_OPEN_HANDLES.len();
					let mut message_buffer = [0_u8; ERROR_MESSAGE_BUFFER_SIZE];
					match exception.message(&mut message_buffer) {
						Ok(TOO_MANY_OPEN_HANDLES) => Err(Error::TooManyDescriptors),
						_ => Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown)),
					}
				} else {
					// No other exceptions appear to be thrown, so if one is, it means we’re
					// talking to something that’s not a normal filesystem.
					Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
				}
			}
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

impl<'buffer, Context> Decode<'buffer, Context> for DirectoryEntry<'buffer> {
	fn decode(
		d: &mut minicbor::Decoder<'buffer>,
		_: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
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

impl<Context> Encode<Context> for Seek {
	fn encode<W: minicbor::encode::Write>(
		&self,
		e: &mut minicbor::Encoder<W>,
		_: &mut Context,
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

impl<'handle, 'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B>
	for &'handle ReadHandle
{
	type Locked = LockedReadHandle<'handle, 'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
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
/// type can be created by calling [`ReadHandle::lock`](Lockable::lock), and it can be dropped to
/// return the borrow of the invoker and buffer to the caller so they can be reused for other
/// purposes.
///
/// The `'handle` lifetime is the lifetime of the original file handle. The `'invoker` lifetime is
/// the lifetime of the invoker. The `'buffer` lifetime is the lifetime of the buffer. The `B` type
/// is the type of scratch buffer to use.
pub struct LockedReadHandle<'handle, 'invoker, 'buffer, B: Buffer> {
	/// The file handle.
	handle: &'handle ReadHandle,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut B,
}

impl<'handle, 'invoker, 'buffer, B: Buffer> LockedReadHandle<'handle, 'invoker, 'buffer, B> {
	/// Seeks to a position in the file and returns the resulting absolute byte position.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NegativeSeek`](Error::NegativeSeek)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
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
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn read(self, length: usize) -> Result<Option<&'buffer [u8]>, Error> {
		use minicbor::bytes::ByteSlice;
		#[derive(Encode)]
		#[cbor(array)]
		struct Params<'descriptor>(#[n(0)] &'descriptor descriptor::Owned, #[n(1)] usize);
		let ret: Result<(Option<&'buffer ByteSlice>,), MethodCallError<'_>> = component_method(
			self.invoker,
			self.buffer,
			&self.handle.address,
			"read",
			Some(&Params(&self.handle.descriptor, length)),
		)
		.await;
		match ret {
			Ok((Some(bytes),)) => Ok(Some(bytes)),
			Ok((None,)) => Ok(None),
			Err(MethodCallError::Other(exception)) => {
				if exception.is_type("java.io.IOException") {
					// The borrow checker isn’t quite smart enough to notice that we can drop the
					// borrow of “buffer” and reuse it in this branch because we’re not going to
					// return anything that depends on it. Fortunately, the error string we’re
					// looking for is small and of fixed size, so just use a fixed-size buffer
					// instead.
					const NOT_ENOUGH_ENERGY: &str = "not enough energy";
					let mut message_buffer = [0_u8; NOT_ENOUGH_ENERGY.len()];
					match exception.message(&mut message_buffer) {
						Ok(m) if m == NOT_ENOUGH_ENERGY => Err(Error::NotEnoughEnergy),
						_ => Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown)),
					}
				} else {
					Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
				}
			}
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

impl<'handle, 'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B>
	for &'handle WriteHandle
{
	type Locked = LockedWriteHandle<'handle, 'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		LockedWriteHandle {
			handle: self,
			invoker,
			buffer,
		}
	}
}

/// A writeable file handle on which methods can be invoked.
///
/// This type combines a writeable file handle, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`WriteHandle::lock`](Lockable::lock), and it can be dropped to
/// return the borrow of the invoker and buffer to the caller so they can be reused for other
/// purposes.
///
/// The `'handle` lifetime is the lifetime of the original file handle. The `'invoker` lifetime is
/// the lifetime of the invoker. The `'buffer` lifetime is the lifetime of the buffer. The `B` type
/// is the type of scratch buffer to use.
pub struct LockedWriteHandle<'handle, 'invoker, 'buffer, B: Buffer> {
	/// The file handle.
	handle: &'handle WriteHandle,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut B,
}

impl<'handle, 'invoker, 'buffer, B: Buffer> LockedWriteHandle<'handle, 'invoker, 'buffer, B> {
	/// Seeks to a position in the file and returns the resulting absolute byte position.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`NegativeSeek`](Error::NegativeSeek)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
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
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`DataTooLarge`](Error::DataTooLarge) if the disk is out of space
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn write(&mut self, bytes: &[u8]) -> Result<(), Error> {
		#[derive(Encode)]
		#[cbor(array)]
		struct Params<'descriptor, 'bytes>(
			#[n(0)] &'descriptor descriptor::Owned,
			#[n(1)] extref::Bytes<'bytes>,
		);
		// SAFETY: component_method() both encodes and submits the CBOR in one go.
		let bytes = unsafe { extref::Bytes::new(bytes) };
		let ret: Result<Ignore, MethodCallError<'_>> = component_method(
			self.invoker,
			self.buffer,
			&self.handle.address,
			"write",
			Some(&Params(&self.handle.descriptor, bytes)),
		)
		.await;
		match ret {
			Ok(_) => Ok(()),
			Err(MethodCallError::Other(exception)) => {
				if exception.is_type("java.io.IOException") {
					// The borrow checker isn’t quite smart enough to notice that we can drop the
					// borrow of “buffer” and reuse it in this branch because we’re not going to
					// return anything that depends on it. Fortunately, the error string we’re
					// looking for is small and of fixed size, so just use a fixed-size buffer
					// instead.
					const NOT_ENOUGH_ENERGY: &str = "not enough energy";
					const NOT_ENOUGH_SPACE: &str = "not enough space";
					let mut message_buffer =
						[0_u8; max_usize(NOT_ENOUGH_ENERGY.len(), NOT_ENOUGH_SPACE.len())];
					match exception.message(&mut message_buffer) {
						Ok(m) if m == NOT_ENOUGH_ENERGY => Err(Error::NotEnoughEnergy),
						Ok(m) if m == NOT_ENOUGH_SPACE => Err(Error::DataTooLarge),
						_ => Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown)),
					}
				} else {
					Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
				}
			}
			Err(e) => Err(e.into()),
		}
	}
}

/// Seeks to a position in a file and returns the resulting absolute byte position.
///
/// # Errors
/// * [`BadComponent`](Error::BadComponent)
/// * [`NegativeSeek`](Error::NegativeSeek)
/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
async fn seek_impl<B: Buffer>(
	invoker: &mut Invoker,
	buffer: &mut B,
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
	let ret: Result<(u64,), MethodCallError<'_>> = component_method(
		invoker,
		buffer,
		address,
		"seek",
		Some(&Params(descriptor, basis, offset)),
	)
	.await;
	match ret {
		Ok((position,)) => Ok(position),
		Err(MethodCallError::BadParameters(_)) => Err(Error::NegativeSeek),
		Err(e) => Err(e.into()),
	}
}
