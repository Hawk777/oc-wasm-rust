#[link(wasm_import_module = "component")]
extern "C" {
	#[link_name = "listStart"]
	pub fn list_start(type_pointer: *const u8, length: usize) -> i32;

	#[link_name = "listNext"]
	pub fn list_next(buffer: *mut u8) -> i32;

	#[link_name = "listType"]
	pub fn list_type(buffer: *mut u8, length: usize) -> isize;

	#[link_name = "type"]
	pub fn component_type(address: *const u8, buffer: *mut u8, buffer_length: usize) -> isize;

	pub fn slot(address: *const u8, length: usize) -> i32;

	#[link_name = "methodsStartComponent"]
	pub fn methods_start_component(address: *const u8) -> i32;

	#[link_name = "methodsStartValue"]
	pub fn methods_start_value(descriptor: u32) -> i32;

	#[link_name = "methodsNext"]
	pub fn methods_next(buffer: *mut u8, length: usize, attributes: *mut u32) -> isize;

	#[link_name = "documentationComponent"]
	pub fn documentation_component(
		address: *const u8,
		method: *const u8,
		method_length: usize,
		buffer: *mut u8,
		buffer_length: usize,
	) -> isize;

	#[link_name = "documentationValue"]
	pub fn documentation_value(
		descriptor: u32,
		method: *const u8,
		method_length: usize,
		buffer: *mut u8,
		buffer_length: usize,
	) -> isize;

	#[link_name = "invokeComponentMethod"]
	pub fn invoke_component_method(
		address: *const u8,
		method: *const u8,
		method_length: usize,
		params: *const u8,
	) -> i32;

	#[link_name = "invokeValue"]
	pub fn invoke_value(descriptor: u32, params: *const u8) -> i32;

	#[link_name = "invokeValueIndexedRead"]
	pub fn invoke_value_indexed_read(descriptor: u32, params: *const u8) -> i32;

	#[link_name = "invokeValueIndexedWrite"]
	pub fn invoke_value_indexed_write(descriptor: u32, params: *const u8) -> i32;

	#[link_name = "invokeValueMethod"]
	pub fn invoke_value_method(
		descriptor: u32,
		method: *const u8,
		method_length: usize,
		params: *const u8,
	) -> i32;

	#[link_name = "invokeEnd"]
	pub fn invoke_end(buffer: *mut u8, length: usize) -> isize;

	#[link_name = "invokeCancel"]
	pub fn invoke_cancel();

	#[link_name = "lastExceptionMessage"]
	pub fn last_exception_message(buffer: *mut u8, buffer_length: usize) -> isize;

	#[link_name = "lastExceptionIsType"]
	pub fn last_exception_is_type(class: *const u8, class_length: usize) -> i32;
}
