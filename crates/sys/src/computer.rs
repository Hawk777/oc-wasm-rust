use ordered_float::NotNan;

#[link(wasm_import_module = "computer")]
extern "C" {
	pub fn uptime() -> NotNan<f64>;

	#[link_name = "cpuTime"]
	pub fn cpu_time() -> NotNan<f64>;

	#[link_name = "worldTime"]
	pub fn world_time() -> u64;

	pub fn address(buffer: *mut u8) -> i32;

	#[link_name = "tmpfsAddress"]
	pub fn tmpfs_address(buffer: *mut u8) -> i32;

	#[link_name = "installedRAM"]
	pub fn installed_ram() -> u32;

	#[link_name = "freeRAM"]
	pub fn free_ram() -> u32;

	#[link_name = "pushSignal"]
	pub fn push_signal(signal: *const u8) -> i32;

	#[link_name = "pullSignal"]
	pub fn pull_signal(buffer: *mut u8, length: usize) -> isize;

	#[link_name = "aclStart"]
	pub fn acl_start();

	#[link_name = "aclNext"]
	pub fn acl_next(buffer: *mut u8, length: usize) -> isize;

	#[link_name = "addUser"]
	pub fn add_user(name: *const u8, length: usize) -> i32;

	#[link_name = "removeUser"]
	pub fn remove_user(name: *const u8, length: usize) -> i32;

	pub fn energy() -> NotNan<f64>;

	#[link_name = "maxEnergy"]
	pub fn max_energy() -> NotNan<f64>;

	#[link_name = "charWidth"]
	pub fn char_width(ch: u32) -> u32;

	pub fn beep(frequency: u32, duration: u32);

	#[link_name = "beepPattern"]
	pub fn beep_pattern(pattern: *const u8, length: usize) -> i32;

	pub fn shutdown() -> !;

	pub fn reboot() -> !;

	pub fn error(address: *const u8, length: usize) -> !;

	pub fn debug(address: *const u8, length: usize) -> i32;
}
