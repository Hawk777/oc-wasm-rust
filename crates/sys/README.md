OC-Wasm-sys is a collection of raw FFI bindings for Rust code running on
[OpenComputers](https://oc.cil.li/) computers running the
[OC-Wasm](https://gitlab.com/Hawk777/oc-wasm) architecture. You probably don’t
want to use this crate directly, since your code will be full of `unsafe`
blocks if you access raw FFI bindings. You probably want
[OC-Wasm-safe](https://gitlab.com/Hawk777/oc-wasm-safe), and potentially some
of the additional crates mentioned there, instead.
