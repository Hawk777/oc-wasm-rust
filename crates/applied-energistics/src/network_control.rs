//! Provides high-level access to the “network control” APIs, which are available on both ME
//! controllers and ME interfaces.

use crate::error::Error;
use alloc::vec::Vec;
use minicbor::{Decode, Decoder, Encode, Encoder};
use oc_wasm_futures::invoke::{component_method, value_method, Buffer};
use oc_wasm_helpers::{
	fluid::Fluid,
	inventory::{ItemStack, OptionItemStackBuilder},
	map_decoder, Lockable,
};
use oc_wasm_safe::{component::Invoker, descriptor, descriptor::AsDescriptor, extref, Address};

/// A network control component.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct NetworkControl(Address);

impl NetworkControl {
	/// Creates a wrapper around a network control component.
	///
	/// The `address` parameter is the address of the network control component. It is not checked
	/// for correctness at this time because network topology could change after this function
	/// returns; as such, each usage of the value may fail instead.
	#[must_use = "This function is only useful for its return value"]
	pub fn new(address: Address) -> Self {
		Self(address)
	}

	/// Returns the address of the component.
	#[must_use = "This function is only useful for its return value"]
	pub fn address(&self) -> &Address {
		&self.0
	}
}

impl<'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B> for NetworkControl {
	type Locked = Locked<'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		Locked {
			address: self.0,
			invoker,
			buffer,
		}
	}
}

/// A network control component on which methods can be invoked.
///
/// This type combines a network control component address, an [`Invoker`](Invoker) that can be
/// used to make method calls, and a scratch buffer used to perform CBOR encoding and decoding. A
/// value of this type can be created by calling [`NetworkControl::lock`](NetworkControl::lock),
/// and it can be dropped to return the borrow of the invoker and buffer to the caller so they can
/// be reused for other purposes.
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
	/// Returns information about the crafting CPUs in the network.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn cpus(self) -> Result<Vec<Cpu<'buffer>>, Error> {
		let ret: (Vec<Cpu<'buffer>>,) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getCpus",
			None::<&()>,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns information about all craftable items known to the network that match a given
	/// filter.
	///
	/// For this method, the size of an item stack in the filter refers to the number of items
	/// produced for one invocation of the crafting process.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn craftables(self, filter: &ItemStackFilter<'_>) -> Result<Vec<Craftable>, Error> {
		// A wrapper around a Craftable that can be CBOR-decoded. Craftable itself doesn’t
		// implement Decode, because it is dangerous to decode a Craftable (which contains a
		// descriptor) from arbitrary CBOR: it is only safe if the CBOR is known to come from the
		// return of a component method call which has allocated fresh, valid descriptors. So
		// CraftableWrapper implements Decode (but that is safe because it only exists within this
		// function and cannot be called with anything other than component method call return
		// CBOR), and Craftable itself does not (which makes it safe to expose as a public API).
		//
		// It needs to be `repr(transparent)` so that a Vec<CraftableWrapper> can be smuggled
		// through raw pointers into a Vec<Craftable> without doing a second allocation.
		#[repr(transparent)]
		struct CraftableWrapper(Craftable);
		impl<Context> Decode<'_, Context> for CraftableWrapper {
			fn decode(
				d: &mut Decoder<'_>,
				context: &mut Context,
			) -> Result<Self, minicbor::decode::Error> {
				let desc = descriptor::Decoded::decode(d, context)?;
				// SAFETY: We just decoded the Decoded from a CBOR array element that is part of a
				// component method call return value and that has not previously been used for
				// anything else. Therefore the descriptor number must be valid and uniquely held
				// by the Decoded object.
				Ok(Self(Craftable(unsafe { desc.into_owned() })))
			}
		}

		let ret: (Vec<CraftableWrapper>,) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getCraftables",
			Some(&(filter,)),
		)
		.await?;
		let ret = ret.0;

		// Prevent the Vec<CraftableWrapper> from deallocating the memory whose ownership is going
		// to be migrated into the Vec<Craftable>.
		let mut ret = core::mem::ManuallyDrop::new(ret);
		// Reinterpret the Vec’s contents as Craftables. SAFETY: CraftableWrapper is
		// repr(transparent) of Craftable.
		let ret = unsafe {
			Vec::from_raw_parts(
				ret.as_mut_ptr().cast::<Craftable>(),
				ret.len(),
				ret.capacity(),
			)
		};

		Ok(ret)
	}

	/// Returns information about all stored items in the network that match a given filter.
	///
	/// If an item is absent but the network knows how to craft it, a stack is still returned for
	/// that item with stack size zero.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn items_in_network(
		self,
		filter: &ItemStackFilter<'_>,
	) -> Result<Vec<ItemStackInNetwork<'buffer>>, Error> {
		let ret: (Vec<ItemStackInNetwork<'buffer>>,) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getItemsInNetwork",
			Some(&(filter,)),
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns information about all stored fluids in the network.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn fluids_in_network(self) -> Result<Vec<Fluid<'buffer>>, Error> {
		let ret: (Vec<Fluid<'buffer>>,) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"getFluidsInNetwork",
			None::<&()>,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the average power injected into the network over the last ten ticks.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn average_power_injection(&mut self) -> Result<f64, Error> {
		self.nullary_f64("getAvgPowerInjection").await
	}

	/// Returns the average power consumption in the network, including idle draw, over the last
	/// ten ticks.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn average_power_usage(&mut self) -> Result<f64, Error> {
		self.nullary_f64("getAvgPowerUsage").await
	}

	/// Returns the idle power draw of the network.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn idle_power_usage(&mut self) -> Result<f64, Error> {
		self.nullary_f64("getIdlePowerUsage").await
	}

	/// Returns the maximum amount of energy the network can store.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn max_stored_energy(&mut self) -> Result<f64, Error> {
		self.nullary_f64("getMaxStoredPower").await
	}

	/// Returns the amount of energy stored in the network.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn stored_energy(&mut self) -> Result<f64, Error> {
		self.nullary_f64("getStoredPower").await
	}

	/// Checks whether the network is powered.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn powered(&mut self) -> Result<bool, Error> {
		let ret: (bool,) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			"isNetworkPowered",
			None::<&()>,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the energy demand on the network.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn energy_demand(&mut self) -> Result<f64, Error> {
		self.nullary_f64("getEnergyDemand").await
	}

	/// Calls a method that takes no parameters and returns an `f64`.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	async fn nullary_f64(&mut self, method: &str) -> Result<f64, Error> {
		let ret: (f64,) = component_method(
			self.invoker,
			self.buffer,
			&self.address,
			method,
			None::<&()>,
		)
		.await?;
		Ok(ret.0)
	}
}

/// Information about a crafting CPU.
///
/// The `'buffer` lifetime is the lifetime of the buffer holding the name string.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Cpu<'buffer> {
	/// The name of the CPU.
	///
	/// This can be set by naming any of the blocks making up the CPU in an anvil or inscriber. If
	/// no blocks are thus named, an empty string is provided.
	pub name: &'buffer str,

	/// Whether the CPU is currently in use running a crafting process.
	pub busy: bool,

	/// The amount of crafting storage in the CPU, in bytes.
	pub storage: u64,

	/// The number of coprocessors in the CPU.
	pub coprocessors: u32,
}

impl<'buffer, Context> Decode<'buffer, Context> for Cpu<'buffer> {
	fn decode(
		d: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
		map_decoder::decode::<CpuBuilder<'buffer>, Context>(d, context)
	}
}

/// A map-decoding builder for a [Cpu](Cpu).
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CpuBuilder<'buffer> {
	name: Option<&'buffer str>,
	busy: Option<bool>,
	storage: Option<u64>,
	coprocessors: Option<u32>,
}

impl<'buffer> map_decoder::Builder<'buffer> for CpuBuilder<'buffer> {
	type Output = Cpu<'buffer>;

	fn entry<Context>(
		&mut self,
		key: &str,
		decoder: &mut Decoder<'buffer>,
		_: &mut Context,
	) -> Result<bool, minicbor::decode::Error> {
		match key {
			"name" => {
				self.name = Some(decoder.str()?);
				Ok(true)
			}
			"busy" => {
				self.busy = Some(decoder.bool()?);
				Ok(true)
			}
			"storage" => {
				self.storage = Some(decoder.u64()?);
				Ok(true)
			}
			"coprocessors" => {
				self.coprocessors = Some(decoder.u32()?);
				Ok(true)
			}
			_ => Ok(false),
		}
	}

	fn build(self) -> Result<Cpu<'buffer>, minicbor::decode::Error> {
		if let Some(name) = self.name {
			if let Some(busy) = self.busy {
				if let Some(storage) = self.storage {
					if let Some(coprocessors) = self.coprocessors {
						return Ok(Cpu {
							name,
							busy,
							storage,
							coprocessors,
						});
					}
				}
			}
		}

		Err(minicbor::decode::Error::message("missing key in CPU"))
	}
}

/// A filter that can be applied to item stacks to limit which items are returned.
///
/// The `'s` lifetime is the lifetime of the strings in the filter.
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ItemStackFilter<'s> {
	/// The internal (Minecraft system) name of the item, or `None` to not filter by internal name.
	pub name: Option<&'s str>,

	/// The human-readable name of the item, or `None` to not filter by human-readable name.
	pub label: Option<&'s str>,

	/// The number of items, or `None` to not filter by stack size.
	pub size: Option<u32>,

	/// The maximum size of a stack of a given item type, or `None` to not filter by maximum stack
	/// size.
	pub max_size: Option<u32>,

	/// The damage value of the item, or `None` to not filter by damage value.
	pub damage: Option<u32>,

	/// The damage at which the item breaks, or `None` to not filter by maximum damage.
	pub max_damage: Option<u32>,

	/// Whether the item must have extra NBT data attached, or `None` to accept items both with and
	/// without extra NBT data.
	pub has_tag: Option<bool>,
}

impl<Context> Encode<Context> for ItemStackFilter<'_> {
	fn encode<W: minicbor::encode::Write>(
		&self,
		e: &mut Encoder<W>,
		context: &mut Context,
	) -> Result<(), minicbor::encode::Error<W::Error>> {
		// SAFETY: We are sweeping things under the carpet a bit. In theory a consumer could create
		// an ItemStackFilter, then CBOR-encode it themselves, then drop the ItemStackFilter and
		// the string or byte array to which it points, then submit the CBOR via
		// oc_wasm_safe::component::Invoker (which takes the already-encoded CBOR). But the
		// ergonomics of making something in ItemStackFilter itself unsafe would be really bad.
		// Unfortunately we do not have the ability to say “impl Encode but only allow it to be
		// used within this module”, which would solve this problem (because nobody else could
		// CBOR-encode the ItemStackFilter). So it seems most useful to just do this, even though
		// it’s not *technically* completely safe—it is safe when the ItemStackFilter is passed to
		// the methods in this module.
		let name = self.name.map(|i| unsafe { extref::String::new(i) });
		let label = self.label.map(|i| unsafe { extref::String::new(i) });
		e.begin_map()?;
		if let Some(name) = name {
			e.str("name")?;
			name.encode(e, context)?;
		}
		if let Some(label) = label {
			e.str("label")?;
			label.encode(e, context)?;
		}
		if let Some(size) = self.size {
			e.str("size")?;
			size.encode(e, context)?;
		}
		if let Some(max_size) = self.max_size {
			e.str("maxSize")?;
			max_size.encode(e, context)?;
		}
		if let Some(damage) = self.damage {
			e.str("damage")?;
			damage.encode(e, context)?;
		}
		if let Some(max_damage) = self.max_damage {
			e.str("maxDamage")?;
			max_damage.encode(e, context)?;
		}
		if let Some(has_tag) = self.has_tag {
			e.str("hasTag")?;
			has_tag.encode(e, context)?;
		}
		e.end()?;
		Ok(())
	}
}

/// An item stack for an item type stored in the network.
#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ItemStackInNetwork<'buffer> {
	/// The item stack.
	pub item_stack: ItemStack<'buffer>,

	/// Whether the network knows how to craft this item.
	///
	/// This value only indicates whether a recipe exists. The required input resources may or may
	/// not be available.
	pub is_craftable: bool,
}

impl<'buffer, Context> Decode<'buffer, Context> for ItemStackInNetwork<'buffer> {
	fn decode(
		decoder: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<Self, minicbor::decode::Error> {
		map_decoder::decode::<ItemStackInNetworkBuilder<'buffer>, Context>(decoder, context)
	}
}

/// A builder for [`ItemStackInNetwork`](ItemStackInNetwork).
#[derive(Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct ItemStackInNetworkBuilder<'buffer> {
	/// The item stack.
	item_stack: OptionItemStackBuilder<'buffer>,

	/// Whether the network knows how to craft this item.
	///
	/// This value only indicates whether a recipe exists. The required input resources may or may
	/// not be available.
	is_craftable: bool,
}

impl<'buffer> map_decoder::Builder<'buffer> for ItemStackInNetworkBuilder<'buffer> {
	type Output = ItemStackInNetwork<'buffer>;

	fn entry<Context>(
		&mut self,
		key: &'buffer str,
		decoder: &mut Decoder<'buffer>,
		context: &mut Context,
	) -> Result<bool, minicbor::decode::Error> {
		if key == "isCraftable" {
			self.is_craftable = decoder.bool()?;
			Ok(true)
		} else {
			self.item_stack.entry(key, decoder, context)
		}
	}

	fn build(self) -> Result<Self::Output, minicbor::decode::Error> {
		match self.item_stack.build()?.0 {
			Some(item_stack) => Ok(Self::Output {
				item_stack,
				is_craftable: self.is_craftable,
			}),
			None => Err(minicbor::decode::Error::message("missing item stack")),
		}
	}
}

/// A crafting recipe that can be executed to produce one or more of an item.
#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Craftable(descriptor::Owned);

impl<'handle, 'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B>
	for &'handle Craftable
{
	type Locked = LockedCraftable<'handle, 'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		LockedCraftable {
			descriptor: self.0.as_descriptor(),
			invoker,
			buffer,
		}
	}
}

/// A crafting recipe on which methods can be invoked.
///
/// This type combines a crafting recipe, an [`Invoker`](Invoker) that can be used to make method
/// calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this type
/// can be created by calling [`Craftable::lock`](Lockable::lock), and it can be dropped to return
/// the borrow of the invoker and buffer to the caller so they can be reused for other purposes.
///
/// The `'handle` lifetime is the lifetime of the original [`Craftable`](Craftable). The `'invoker`
/// lifetime is the lifetime of the invoker. The `'buffer` lifetime is the lifetime of the buffer.
/// The `B` type is the type of scratch buffer to use.
pub struct LockedCraftable<'handle, 'invoker, 'buffer, B: Buffer> {
	/// The descriptor.
	descriptor: descriptor::Borrowed<'handle>,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut B,
}

impl<'handle, 'invoker, 'buffer, B: Buffer> LockedCraftable<'handle, 'invoker, 'buffer, B> {
	/// Returns the item(s) created per execution of the crafting recipe.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn item_stack(self) -> Result<ItemStack<'buffer>, Error> {
		let ret: (ItemStack<'buffer>,) = value_method(
			self.invoker,
			self.buffer,
			&self.descriptor,
			"getItemStack",
			None::<&()>,
		)
		.await?;
		Ok(ret.0)
	}

	/// Returns the number of items requested for this crafting operation that are currently in
	/// progress.
	///
	/// This appears to mostly correspond to the “Crafting” line in the CPU status window. It has
	/// the following properties:
	/// * It is capped at 64. Even if enough resources are immediately available to craft more,
	///   only 64 items are actually requested at a time, with the remainder being scheduled as
	///   in-progress items complete. However, it may momentarily read 65 at certain times. It is
	///   unknown at this time whether the limit is actually 64 or the item stack size limit of the
	///   output item.
	/// * Only as many items are shown as can be crafted at this moment; if a CPU is crafting
	///   prerequisites, the items that will be crafted once the prerequisites are finished are not
	///   included.
	/// * At the beginning of a crafting operation, the number ramps up from zero; it is not
	///   immediately set to the initial count of craftable items.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn requesting(&mut self) -> Result<u32, Error> {
		let ret: (u32,) = value_method(
			self.invoker,
			self.buffer,
			&self.descriptor,
			"requesting",
			None::<&()>,
		)
		.await?;
		Ok(ret.0)
	}

	/// Initiates a crafting operation.
	///
	/// `amount` indicates the number of items to craft, which, if the recipe produces more than
	/// one output, is rounded up to the next multiple of the output size. `priority` indicates how
	/// to choose between available crafting CPUs. `cpu`, if not an empty string, indicates the
	/// name of the crafting CPU on which the recipe will execute (if multiple CPUs have the same
	/// name and `cpu` is provided, one so named is selected arbitrarily, ignoring `priority`).
	///
	/// # Warning
	/// This method does *not* return [`MissingResources`](Error::MissingResources). Instead, if
	/// resources are missing, this method will return a valid
	/// [`CraftingOperation`](CraftingOperation), and method calls on that object will return
	/// [`MissingResources`](Error::MissingResources).
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	pub async fn request(
		&mut self,
		amount: u32,
		priority: CpuPriority,
		cpu: &str,
	) -> Result<CraftingOperation, Error> {
		// SAFETY: component_method() both encodes and submits the CBOR in one go.
		let cpu = unsafe { extref::String::new(cpu) };
		let ret: (descriptor::Decoded,) = value_method(
			self.invoker,
			self.buffer,
			&self.descriptor,
			"request",
			Some(&(amount, priority == CpuPriority::Fastest, cpu)),
		)
		.await?;
		// SAFETY: We just got the descriptor::Decoded from a value method’s return CBOR, so it is
		// guaranteed to be valid, fresh, and unique.
		Ok(CraftingOperation(unsafe { ret.0.into_owned() }))
	}
}

/// A criterion for selecting a crafting CPU to execute a recipe.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum CpuPriority {
	/// The recipe should execute on the cheapest CPU that can hold it.
	///
	/// This may minimize power consumption and will also leave the more powerful CPUs available
	/// for more complicated recipes that require them.
	Cheapest,

	/// The recipe should execute on the most powerful CPU that is not currently in use.
	///
	/// This may speed up execution of the recipe, especially if coprocessors are installed.
	Fastest,
}

/// An ongoing crafting operation.
#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CraftingOperation(descriptor::Owned);

impl<'handle, 'invoker, 'buffer, B: 'buffer + Buffer> Lockable<'invoker, 'buffer, B>
	for &'handle CraftingOperation
{
	type Locked = LockedCraftingOperation<'handle, 'invoker, 'buffer, B>;

	fn lock(&self, invoker: &'invoker mut Invoker, buffer: &'buffer mut B) -> Self::Locked {
		LockedCraftingOperation {
			descriptor: self.0.as_descriptor(),
			invoker,
			buffer,
		}
	}
}

/// An ongoing crafting operation on which methods can be invoked.
///
/// This type combines a crafting operation, an [`Invoker`](Invoker) that can be used to make
/// method calls, and a scratch buffer used to perform CBOR encoding and decoding. A value of this
/// type can be created by calling [`CraftingOperation::lock`](Lockable::lock), and it can be
/// dropped to return the borrow of the invoker and buffer to the caller so they can be reused for
/// other purposes.
///
/// The `'handle` lifetime is the lifetime of the original
/// [`CraftingOperation`](CraftingOperation). The `'invoker` lifetime is the lifetime of the
/// invoker. The `'buffer` lifetime is the lifetime of the buffer. The `B` type is the type of
/// scratch buffer to use.
pub struct LockedCraftingOperation<'handle, 'invoker, 'buffer, B: Buffer> {
	/// The descriptor.
	descriptor: descriptor::Borrowed<'handle>,

	/// The invoker.
	invoker: &'invoker mut Invoker,

	/// The buffer.
	buffer: &'buffer mut B,
}

impl<'handle, 'invoker, 'buffer, B: Buffer> LockedCraftingOperation<'handle, 'invoker, 'buffer, B> {
	/// Checks whether the crafting operation has been cancelled.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`Computing`](Error::Computing)
	/// * [`MissingResources`](Error::MissingResources)
	pub async fn cancelled(&mut self) -> Result<bool, Error> {
		self.call_bool("isCanceled").await
	}

	/// Checks whether the crafting operation has finished successfully.
	///
	/// A cancelled operation is not considered done.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`Computing`](Error::Computing)
	/// * [`MissingResources`](Error::MissingResources)
	pub async fn done(&mut self) -> Result<bool, Error> {
		self.call_bool("isDone").await
	}

	/// Cancels the crafting operation.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`Completed`](Error::Completed)
	/// * [`Computing`](Error::Computing)
	/// * [`MissingResources`](Error::MissingResources)
	pub async fn cancel(&mut self) -> Result<(), Error> {
		self.call_bool("cancel").await?;
		Ok(())
	}

	/// Calls a method that returns a `bool` on success.
	///
	/// # Errors
	/// * [`BadComponent`](Error::BadComponent)
	/// * [`Completed`](Error::Completed)
	/// * [`Computing`](Error::Computing)
	/// * [`MissingResources`](Error::MissingResources)
	async fn call_bool(&mut self, method: &str) -> Result<bool, Error> {
		// These methods may return:
		// * A null followed by the string “computing”,
		// * A boolean false followed by an error message, or
		// * A boolean value.
		//
		// For compatibility should this change in future, if there is a string message, we accept
		// either a boolean or null and do not require a fixed correlation between specific
		// messages and whether the first element is null or false.
		fn decode_nullable_boolean<Context>(
			decoder: &mut Decoder<'_>,
			_: &mut Context,
		) -> Result<bool, minicbor::decode::Error> {
			if decoder.datatype()? == minicbor::data::Type::Null {
				decoder.null()?;
				Ok(false)
			} else {
				Ok(decoder.bool()?)
			}
		}
		#[derive(Decode)]
		struct Return<'buffer> {
			#[n(0)]
			#[cbor(decode_with = "decode_nullable_boolean")]
			value: bool,

			#[b(1)]
			error_message: Option<&'buffer str>,
		}
		let ret: (Return<'_>,) = value_method(
			self.invoker,
			self.buffer,
			&self.descriptor,
			method,
			None::<&()>,
		)
		.await?;
		let ret = ret.0;
		match ret.error_message {
			Some("job already completed") => Err(Error::Completed),
			Some("computing") => Err(Error::Computing),
			Some("missing resources?") => Err(Error::MissingResources),
			// This might not be entirely accurate, but hopefully the main reason why this might
			// happen is that the AE network parts have been removed or are in an unloaded chunk or
			// something like that.
			Some(_) => Err(Error::BadComponent(
				oc_wasm_safe::error::Error::NoSuchComponent,
			)),
			None => Ok(ret.value),
		}
	}
}
