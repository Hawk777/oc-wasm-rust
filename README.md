OC-Wasm-Cassette provides a convenient wrapper that makes it easy to use an
`async fn` as a top-level function in an OC-Wasm application.

Usage is as simple as:
```rust
async fn main() -> Infallible {
	// Your code here
}

#[no_mangle]
pub extern "C" fn run(arg: i32) -> i32 {
	oc_wasm_cassette::run(arg, main)
}
```
