#[link(wasm_import_module = "descriptor")]
extern "C" {
	pub fn close(descriptor: u32) -> i32;

	pub fn dup(descriptor: u32) -> i32;
}
