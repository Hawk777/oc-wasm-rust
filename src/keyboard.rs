//! Provides decoders for the keyboard-related signals.
//!
//! Keyboards, while components, do not expose any methods, so no component wrapper is provided in
//! this module.

use core::num::NonZeroU32;
use minicbor::{Decode, Decoder};
use oc_wasm_safe::Address;

/// Information about a key press or release event, excluding the name of the player who pressed or
/// released the key.
///
/// This structure, unlike [`KeySignal`](KeySignal), is fully owned. Thus, a typical application
/// which does not care about the player name can convert a [`KeySignal`](KeySignal) into a
/// `BasicKeySignal` in order to regain use of the decode buffer.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BasicKeySignal {
	/// The address of the keyboard component.
	pub keyboard: Address,

	/// The character that was pressed or released, if any.
	///
	/// This is `None` if the event does not produce a character. Examples of
	/// non-character-producing key events are events related to lock keys, arrow keys, modifier
	/// keys, navigation keys, or other special keys.
	pub character: Option<char>,

	/// The keycode that was pressed or released, if any.
	///
	/// This is `None` if the event does not have a keycode. One example of a non-keycode-producing
	/// key event is a composed character; in this case, the individual keystrokes are not
	/// reported, but the composed character is reported as a character without associated keycode.
	/// Another example is a Windows key (aka super key) or Menu key, both of which (under some
	/// operating systems at least) send key signals with neither [`character`](character) nor
	/// `keycode` filled in.
	pub keycode: Option<NonZeroU32>,
}

impl<'buffer> From<&KeySignal<'buffer>> for BasicKeySignal {
	fn from(source: &KeySignal<'buffer>) -> Self {
		Self {
			keyboard: source.keyboard,
			character: source.character,
			keycode: source.keycode,
		}
	}
}

/// Information about a key press or release signal.
#[derive(Clone, Debug, Decode, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cbor(array)]
pub struct KeySignal<'buffer> {
	/// The address of the keyboard component.
	#[b(0)]
	pub keyboard: Address,

	/// The character that was pressed or released, if any.
	///
	/// This is `None` if the signal does not produce a character. Examples of
	/// non-character-producing key signals are signals related to lock keys, arrow keys, modifier
	/// keys, navigation keys, or other special keys.
	#[b(1)]
	#[cbor(decode_with = "KeySignal::decode_character")]
	pub character: Option<char>,

	/// The keycode that was pressed or released, if any.
	///
	/// This is `None` if the signal does not have a keycode. One example of a
	/// non-keycode-producing key signal is a composed character; in this case, the individual
	/// keystrokes are not reported, but the composed character is reported as a character without
	/// associated keycode. Another example is a Windows key (aka super key) or Menu key, both of
	/// which (under some operating systems at least) send key signals with neither
	/// [`character`](character) nor `keycode` filled in.
	#[b(2)]
	#[cbor(decode_with = "KeySignal::decode_keycode")]
	pub keycode: Option<NonZeroU32>,

	/// The name of the player who pressed or released the key.
	///
	/// This is `None` if reporting of player names is disabled in the mod configuration.
	#[b(3)]
	pub player: Option<&'buffer str>,
}

impl<'buffer> KeySignal<'buffer> {
	/// The name of the signal sent when a key is pressed.
	///
	/// This includes typematic repeats of held keys.
	pub const KEY_DOWN: &'static str = "key_down";

	/// The name of the signal sent when a key is released.
	pub const KEY_UP: &'static str = "key_up";

	/// Decodes the `character` field of a `KeySignal`.
	///
	/// This field is encoded as a double-precision floating point number in CBOR and needs to be
	/// converted to its proper type.
	fn decode_character(d: &mut Decoder<'buffer>) -> Result<Option<char>, minicbor::decode::Error> {
		// Unicode code points are 0≤N≤0x10FFFF.
		#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
		let character = d.f64()? as u32;
		if character == 0 {
			Ok(None)
		} else {
			Ok(char::from_u32(character))
		}
	}

	/// Decodes the `keycode` field of a `KeySignal`.
	///
	/// This field is encoded as a double-precision floating point number in CBOR and needs to be
	/// converted to its proper type.
	fn decode_keycode(
		d: &mut Decoder<'buffer>,
	) -> Result<Option<NonZeroU32>, minicbor::decode::Error> {
		// Keycodes are small nonnegative numbers.
		#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
		let keycode = d.f64()? as u32;
		Ok(NonZeroU32::new(keycode))
	}

	/// Discards the player name information from this value.
	#[must_use]
	pub fn to_basic(&self) -> BasicKeySignal {
		BasicKeySignal::from(self)
	}
}

/// Information about a clipboard paste signal.
#[derive(Clone, Debug, Decode, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cbor(array)]
pub struct ClipboardSignal<'buffer> {
	/// The address of the keyboard component.
	#[b(0)]
	pub keyboard: Address,

	/// The text that was pasted.
	#[b(1)]
	pub text: &'buffer str,

	/// The name of the player who pasted the text.
	///
	/// This is `None` if reporting of player names is disabled in the mod configuration.
	#[b(2)]
	pub player: Option<&'buffer str>,
}

impl<'buffer> ClipboardSignal<'buffer> {
	/// The name of the signal sent when text is pasted from the clipboard.
	pub const SIGNAL: &'static str = "clipboard";
}
