//! Provides high-level access to the modem API, as implemented on network cards and wireless
//! network cards.

use crate::error::Error;
use crate::helpers::max_usize;
use core::num::NonZeroU16;
use minicbor::{Decode, Encode};
use oc_wasm_futures::invoke::{component_method, Buffer};
use oc_wasm_helpers::{Lockable, OneValue, TwoValues};
use oc_wasm_safe::{
	component::{Invoker, MethodCallError},
	extref, Address,
};

/// The type name for network cards.
pub const TYPE: &str = "modem";

/// A data type that can be sent as part of a network packet.
///
/// The `'a` lifetime is the lifetime of the data, if the part contains a string or byte array.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum PacketPart<'a> {
	/// The packet part is a null.
	///
	/// A null costs six bytes of space in the packet and is preserved end-to-end.
	Null,

	/// The packet part is a boolean.
	///
	/// A boolean costs six bytes of space in the packet and is preserved end-to-end.
	Bool(bool),

	/// The packet part is a 32-bit integer.
	///
	/// An `i32` costs six bytes of space in the packet. It is converted to an [`F64`](#F64) on the
	/// receiver; therefore, this enumeration element can never appear in a received message.
	/// However, it is still useful as it is cheaper to send an `i32` (six bytes) than an `f64`
	/// (ten bytes).
	I32(i32),

	/// The packet part is a 64-bit floating-point number.
	///
	/// An `f64` costs ten bytes of space in the packet and is preserved end-to-end.
	F64(f64),

	/// The packet part is a text string.
	///
	/// An empty `str` costs three byte of space in the packet. A non-empty `str` costs two plus
	/// the length of its UTF-8-encoded form bytes of space in the packet. It is preserved
	/// end-to-end.
	Str(&'a str),

	/// The packet part is an array of bytes.
	///
	/// An empty byte array costs three bytes of space in the packet. A non-empty byte array costs
	/// two plus its length bytes of space in the packet. It is preserved end-to-end.
	Bytes(&'a [u8]),
}

impl From<()> for PacketPart<'_> {
	fn from(_: ()) -> Self {
		Self::Null
	}
}

impl From<bool> for PacketPart<'_> {
	fn from(value: bool) -> Self {
		Self::Bool(value)
	}
}

impl From<u8> for PacketPart<'_> {
	fn from(value: u8) -> Self {
		Self::I32(value.into())
	}
}

impl From<u16> for PacketPart<'_> {
	fn from(value: u16) -> Self {
		Self::I32(value.into())
	}
}

impl From<u32> for PacketPart<'_> {
	fn from(value: u32) -> Self {
		match TryInto::<i32>::try_into(value) {
			Ok(n) => Self::I32(n),
			Err(_) => Self::F64(value.into()),
		}
	}
}

impl From<i8> for PacketPart<'_> {
	fn from(value: i8) -> Self {
		Self::I32(value.into())
	}
}

impl From<i16> for PacketPart<'_> {
	fn from(value: i16) -> Self {
		Self::I32(value.into())
	}
}

impl From<i32> for PacketPart<'_> {
	fn from(value: i32) -> Self {
		Self::I32(value)
	}
}

impl From<f32> for PacketPart<'_> {
	fn from(value: f32) -> Self {
		Self::F64(value.into())
	}
}

impl From<f64> for PacketPart<'_> {
	fn from(value: f64) -> Self {
		Self::F64(value)
	}
}

impl<'a> From<&'a str> for PacketPart<'a> {
	fn from(value: &'a str) -> Self {
		Self::Str(value)
	}
}

impl<'a> From<&'a [u8]> for PacketPart<'a> {
	fn from(value: &'a [u8]) -> Self {
		Self::Bytes(value)
	}
}

impl<'a, const N: usize> From<&'a [u8; N]> for PacketPart<'a> {
	fn from(value: &'a [u8; N]) -> Self {
		Self::Bytes(value)
	}
}

impl<'a, const N: usize> From<&'a minicbor::bytes::ByteArray<N>> for PacketPart<'a> {
	fn from(value: &'a minicbor::bytes::ByteArray<N>) -> Self {
		Self::Bytes(&**value)
	}
}

impl<'a> From<&'a minicbor::bytes::ByteSlice> for PacketPart<'a> {
	fn from(value: &'a minicbor::bytes::ByteSlice) -> Self {
		Self::Bytes(value)
	}
}

impl<'a> From<&'a minicbor::bytes::ByteVec> for PacketPart<'a> {
	fn from(value: &'a minicbor::bytes::ByteVec) -> Self {
		Self::Bytes(value)
	}
}

impl<'buffer, Context> Decode<'buffer, Context> for PacketPart<'buffer> {
	fn decode(
		d: &mut minicbor::decode::Decoder<'buffer>,
		_: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
		use minicbor::data::Type;
		match d.datatype()? {
			Type::Bool => Ok(Self::Bool(d.bool()?)),
			Type::Null => {
				d.null()?;
				Ok(Self::Null)
			}
			Type::Undefined => {
				d.undefined()?;
				Ok(Self::Null)
			}
			// I32 can’t happen according to the OpenComputers source code; it is converted to F64
			// before signal delivery.
			Type::F16 | Type::F32 | Type::F64 => Ok(Self::F64(d.f64()?)),
			Type::Bytes => Ok(Self::Bytes(d.bytes()?)),
			Type::String => Ok(Self::Str(d.str()?)),
			t => Err(minicbor::decode::Error::type_mismatch(t)),
		}
	}

	fn nil() -> Option<Self> {
		Some(Self::Null)
	}
}

impl<Context> Encode<Context> for PacketPart<'_> {
	fn encode<W: minicbor::encode::Write>(
		&self,
		e: &mut minicbor::Encoder<W>,
		c: &mut Context,
	) -> Result<(), minicbor::encode::Error<W::Error>> {
		match *self {
			Self::Null => e.null(),
			Self::Bool(v) => e.bool(v),
			Self::I32(v) => e.i32(v),
			Self::F64(v) => e.f64(v),
			// SAFETY: We are sweeping things under the carpet a bit. In theory a consumer could
			// create a PacketPart, then CBOR-encode it themselves, then drop the PacketPart and
			// the string or byte array to which it points, then submit the CBOR via
			// oc_wasm_safe::component::Invoker (which takes the already-encoded CBOR). But the
			// ergonomics of making something in PacketPart itself unsafe would be really bad.
			// Unfortunately we do not have the ability to say “impl Encode but only allow it to be
			// used within this module”, which would solve this problem (because nobody else could
			// CBOR-encode the PacketPart). So it seems most useful to just do this, even though
			// it’s not *technically* completely safe—it is safe when the PacketPart is passed to
			// the send or broadcast methods in this module.
			Self::Str(v) => e.encode_with(unsafe { extref::String::new(v) }, c),
			Self::Bytes(v) => e.encode_with(unsafe { extref::Bytes::new(v) }, c),
		}?;
		Ok(())
	}

	fn is_nil(&self) -> bool {
		*self == Self::Null
	}
}

/// A remote wakeup configuration.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum WakeMessage<'a> {
	/// Remote wakeup is disabled.
	Disabled,

	/// Remote wakeup is enabled with an exact-match requirement. The computer wakes up if it
	/// receives a network packet containing exactly one part, where that part is either the
	/// specified string or a byte array containing that string’s UTF-8 encoding.
	Exact(&'a str),

	/// Remote wakeup is enabled with a fuzzy-match requirement. The computer wakes up if it
	/// receives a network packet containing at least one part, where the first part is either the
	/// specified string or a byte array containing that string’s UTF-8 encoding.
	Fuzzy(&'a str),
}

impl<'buffer, Context> Decode<'buffer, Context> for WakeMessage<'buffer> {
	fn decode(
		d: &mut minicbor::Decoder<'buffer>,
		context: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
		let inner = TwoValues::<Option<&'buffer str>, bool>::decode(d, context)?;
		match inner {
			TwoValues(None, _) => Ok(Self::Disabled),
			TwoValues(Some(s), false) => Ok(Self::Exact(s)),
			TwoValues(Some(s), true) => Ok(Self::Fuzzy(s)),
		}
	}
}

impl<Context> Encode<Context> for WakeMessage<'_> {
	fn encode<W: minicbor::encode::Write>(
		&self,
		e: &mut minicbor::Encoder<W>,
		context: &mut Context,
	) -> Result<(), minicbor::encode::Error<W::Error>> {
		let inner: TwoValues<Option<extref::String<'_>>, bool> = match self {
			Self::Disabled => TwoValues(None, false),
			// SAFETY: We are sweeping things under the carpet a bit. In theory a consumer could
			// create a WakeMessage, then CBOR-encode it themselves, then drop the WakeMessage and
			// the string or byte array to which it points, then submit the CBOR via
			// oc_wasm_safe::component::Invoker (which takes the already-encoded CBOR). But the
			// ergonomics of making something in WakeMessage itself unsafe would be really bad.
			// Unfortunately we do not have the ability to say “impl Encode but only allow it to be
			// used within this module”, which would solve this problem (because nobody else could
			// CBOR-encode the WakeMessage). So it seems most useful to just do this, even though
			// it’s not *technically* completely safe—it is safe when the WakeMessage is passed to
			// the set_wake_message method in this module.
			Self::Exact(s) => TwoValues(Some(unsafe { extref::String::new(s) }), false),
			Self::Fuzzy(s) => TwoValues(Some(unsafe { extref::String::new(s) }), true),
		};
		inner.encode(e, context)
	}
}

/// A network card.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Modem(Address);

impl Modem {
	/// Creates a wrapper around a network card.
	///
	/// The `address` parameter is the address of the network card. It is not checked for
	/// correctness at this time because network topology could change after this function returns;
	/// as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the network card.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for Modem {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A network card on which methods can be invoked.
///
/// This type combined a network card address, and [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`Modem::lock`](Modem::lock), and it can be dropped to return
/// the borrow of the invoker and buffer to the caller so they can be reused for other purposes.
///
/// The `'invoker` lifetime is the lifetime of the invoker. The `'buffer` lifetime is the lifetime
/// of the buffer. The `B` type is the type of scratch buffer to use.
pub struct Locked<'invoker, 'buffer, B> {
	/// The component address.
	address: Address,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut B,
}

impl<'invoker, 'buffer, B: Buffer> Locked<'invoker, 'buffer, B> {
	/// Checks whether this network card is wired or wireless.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn is_wireless(&mut self) -> Result<bool, Error> {
		let ret: OneValue<_> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"isWireless",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Checks whether a specified port number accepts packets.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn is_open(&mut self, port: NonZeroU16) -> Result<bool, Error> {
		let ret: OneValue<_> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"isOpen",
			Some(&OneValue(port)),
		)
		.await?;
		Ok(ret.0)
	}

	/// Starts accepting packets on a port number.
	///
	/// Returns `true` if the port was previously closed and is now open, or `false` if it was
	/// already open.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`TooManyOpenPorts`](Error::TooManyOpenPorts)
	pub async fn open(&mut self, port: NonZeroU16) -> Result<bool, Error> {
		let ret: Result<OneValue<_>, MethodCallError<'_>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"open",
			Some(&OneValue(port)),
		)
		.await;
		match ret {
			Ok(OneValue(ret)) => Ok(ret),
			Err(MethodCallError::Other(exception)) => {
				if exception.is_type("java.io.IOException") {
					const TOO_MANY_OPEN_PORTS: &str = "too many open ports";
					let mut message_buffer = [0_u8; TOO_MANY_OPEN_PORTS.len()];
					match exception.message(&mut message_buffer) {
						Ok(TOO_MANY_OPEN_PORTS) => Err(Error::TooManyOpenPorts),
						_ => Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown)),
					}
				} else {
					Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown))
				}
			}
			Err(e) => Err(e.into()),
		}
	}

	/// Stops accepting packets on a port number.
	///
	/// Returns `true` if the port was previously open and is now closed, or `false` if it was
	/// previously closed.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn close(&mut self, port: NonZeroU16) -> Result<bool, Error> {
		let ret: OneValue<_> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"close",
			Some(&OneValue(port)),
		)
		.await?;
		Ok(ret.0)
	}

	/// Stops accepting packets on all ports.
	///
	/// Returns `true` if any ports were previously open and are now closed, or `false` if all
	/// ports were previously closed.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn close_all(&mut self) -> Result<bool, Error> {
		let ret: OneValue<_> =
			component_method::<(), _, _>(self.invoker, self.buffer, &self.address, "close", None)
				.await?;
		Ok(ret.0)
	}

	/// Sends a unicast packet.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`DataTooLarge`](Error::DataTooLarge)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`TooManyParts`](Error::TooManyParts)
	pub async fn send(
		&mut self,
		destination: Address,
		port: NonZeroU16,
		parts: &[PacketPart<'_>],
	) -> Result<(), Error> {
		struct Params<'parts> {
			destination: Address,
			port: NonZeroU16,
			parts: &'parts [PacketPart<'parts>],
		}
		impl<Context> Encode<Context> for Params<'_> {
			fn encode<W: minicbor::encode::Write>(
				&self,
				e: &mut minicbor::Encoder<W>,
				context: &mut Context,
			) -> Result<(), minicbor::encode::Error<W::Error>> {
				e.array(2 + self.parts.len() as u64)?;
				self.destination.encode(e, context)?;
				self.port.encode(e, context)?;
				for part in self.parts {
					part.encode(e, context)?;
				}
				Ok(())
			}
		}
		self.do_send(
			"send",
			&Params {
				destination,
				port,
				parts,
			},
		)
		.await
	}

	/// Sends a broadcast packet.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`DataTooLarge`](Error::DataTooLarge)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`TooManyParts`](Error::TooManyParts)
	pub async fn broadcast(
		&mut self,
		port: NonZeroU16,
		parts: &[PacketPart<'_>],
	) -> Result<(), Error> {
		struct Params<'parts> {
			port: NonZeroU16,
			parts: &'parts [PacketPart<'parts>],
		}
		impl<Context> Encode<Context> for Params<'_> {
			fn encode<W: minicbor::encode::Write>(
				&self,
				e: &mut minicbor::Encoder<W>,
				context: &mut Context,
			) -> Result<(), minicbor::encode::Error<W::Error>> {
				e.array(1 + self.parts.len() as u64)?;
				self.port.encode(e, context)?;
				for part in self.parts {
					part.encode(e, context)?;
				}
				Ok(())
			}
		}
		self.do_send("broadcast", &Params { port, parts }).await
	}

	/// Implements the `send` and `broadcast` methods.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`DataTooLarge`](Error::DataTooLarge)
	/// * [`NotEnoughEnergy`](Error::NotEnoughEnergy)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	/// * [`TooManyParts`](Error::TooManyParts)
	async fn do_send<P: Encode<()>>(&mut self, method: &str, params: &P) -> Result<(), Error> {
		let ret: Result<OneValue<bool>, MethodCallError<'_>> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			Some(params),
		)
		.await;
		match ret {
			Ok(_) => Ok(()),
			Err(MethodCallError::BadParameters(exception)) => {
				const PACKET_HAS_TOO_MANY_PARTS: &str = "packet has too many parts";
				const PACKET_TOO_BIG_PREFIX: &str = "packet too big";
				const MESSAGE_BUFFER_SIZE: usize = max_usize(
					PACKET_HAS_TOO_MANY_PARTS.len(),
					PACKET_TOO_BIG_PREFIX.len() + 20, /* for the “ (max NNNNN)” part */
				);
				let mut message_buffer = [0_u8; MESSAGE_BUFFER_SIZE];
				match exception.message(&mut message_buffer) {
					Ok(PACKET_HAS_TOO_MANY_PARTS) => Err(Error::TooManyParts),
					Ok(m) if m.starts_with(PACKET_TOO_BIG_PREFIX) => Err(Error::DataTooLarge),
					_ => Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown)),
				}
			}
			Err(e @ MethodCallError::Other(exception)) => {
				if exception.is_type("java.io.IOException") {
					const NOT_ENOUGH_ENERGY: &str = "not enough energy";
					let mut message_buffer = [0_u8; NOT_ENOUGH_ENERGY.len()];
					match exception.message(&mut message_buffer) {
						Ok(NOT_ENOUGH_ENERGY) => Err(Error::NotEnoughEnergy),
						_ => Err(Error::BadComponent(oc_wasm_safe::error::Error::Unknown)),
					}
				} else {
					Err(e.into())
				}
			}
			Err(e) => Err(e.into()),
		}
	}

	/// Returns the current wireless transmit strength.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component is not a wireless
	///   network card, including if it is an ordinary wired network card.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_strength(&mut self) -> Result<f64, Error> {
		let ret: OneValue<_> = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getStrength",
			None,
		)
		.await?;
		Ok(ret.0)
	}

	/// Sets the wireless transmit strength.
	///
	/// Returns the new transmit strength, after clamping to the range permitted by the card.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent) is returned if the component is not a wireless
	///   network card, including if it is an ordinary wired network card.
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_strength(&mut self, strength: f64) -> Result<f64, Error> {
		let ret: OneValue<_> = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setStrength",
			Some(&OneValue(strength)),
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the wakeup message configuration.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn get_wake_message(self) -> Result<WakeMessage<'buffer>, Error> {
		let ret = component_method::<(), _, _>(
			self.invoker,
			self.buffer,
			&self.address,
			"getWakeMessage",
			None,
		)
		.await?;
		Ok(ret)
	}

	/// Sets the wakeup message configuration.
	///
	/// The old configuration is returned.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`TooManyDescriptors`](Error::TooManyDescriptors)
	pub async fn set_wake_message(
		self,
		config: WakeMessage<'_>,
	) -> Result<WakeMessage<'buffer>, Error> {
		let ret = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"setWakeMessage",
			Some(&config),
		)
		.await?;
		Ok(ret)
	}
}

/// The name of the signal sent when a network packet is received.
pub const MESSAGE_NAME: &str = "modem_message";

/// Information about a received network packet.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding the backing data for the packet
/// parts. The `N` constant parameter is the maximum number of parts the packet can contain.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Message<'buffer, const N: usize> {
	/// The address of the network card that received the packet.
	pub receiver: Address,

	/// The address of the network card that sent the packet.
	///
	/// It is unclear whether this is always the original sender, or whether it may change as the
	/// packet is forwarded through relays. The documentation claims it will change, but the
	/// JavaDocs in the source code claim it will not.
	pub sender: Address,

	/// The port number to which the packet is addressed.
	///
	/// This value is zero if the message was received on a linked card.
	pub port: u16,

	/// The distance the packet travelled through the air, if received by a wireless network card.
	pub distance: f64,

	/// The parts of the packet.
	///
	/// Any parts beyond the end of the packet are set to [`PacketPart::Null`](PacketPart::Null).
	pub parts: [PacketPart<'buffer>; N],
}

impl<'buffer, Context, const N: usize> Decode<'buffer, Context> for Message<'buffer, N> {
	fn decode(
		d: &mut minicbor::Decoder<'buffer>,
		context: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
		// If the CBOR fits in RAM, the array length must be ≤ maximum usize.
		#[allow(clippy::cast_possible_truncation)]
		let len = d.array()?.unwrap() as usize;
		if len >= 4 {
			let parts_count = len - 4;
			if parts_count <= N {
				let receiver = Address::decode(d, context)?;
				let sender = Address::decode(d, context)?;
				// The port number is always a u16; it is only delivered as an f64 because
				// OpenComputers is like that.
				#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
				let port = d.f64()? as u16;
				let distance = d.f64()?;
				let mut parts = [PacketPart::Null; N];
				for i in parts[..parts_count].iter_mut() {
					*i = PacketPart::decode(d, context)?;
				}
				Ok(Self {
					receiver,
					sender,
					port,
					distance,
					parts,
				})
			} else {
				Err(minicbor::decode::Error::message("too many parts"))
			}
		} else {
			Err(minicbor::decode::Error::message("array too short"))
		}
	}
}
