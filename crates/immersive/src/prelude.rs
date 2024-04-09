//! Provides symbols that virtually all consumers will want to import all of.
//!
//! Importing `*` from the prelude should be reasonably safe. The prelude largely exports traits as
//! `_` so that methods can be called on them. Any names in the prelude are chosen to be extremely
//! unlikely to collide.

pub use oc_wasm_helpers::Lockable as _;
