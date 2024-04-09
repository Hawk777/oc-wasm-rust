OC-Wasm-safe provides a memory-safe but low-level API for Rust code running on
[OpenComputers](https://oc.cil.li/) computers running the
[OC-Wasm](https://gitlab.com/Hawk777/oc-wasm) architecture. This crate provides
access to the full capabilities of OpenComputers as well as any other mods that
add OpenComputers interoperability by means of Components or Signals. It is
generally not meant to be used alone, but rather to provide some useful APIs
directly while also serving as a building block for more ergonomic APIs where
appropriate.
