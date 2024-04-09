#[link(wasm_import_module = "execute")]
extern "C" {
	pub fn clear();

	pub fn add(pointer: *const u8, length: usize) -> i32;

	pub fn execute() -> !;
}
