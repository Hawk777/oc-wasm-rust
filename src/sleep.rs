//! Allows the caller to wait for an amount of time, measured either in computer uptime or in-game
//! real time.
//!
//! Sleep functions can be distinguished based on which measurement of time they use. In this axis,
//! two measurements are available: uptime and world time. Uptime measures the time the computer
//! has been in a loaded chunk since it was last booted; it passes only while the game is running
//! (i.e. not paused or shut down) and the computer is in a loaded chunk. World time, in contrast,
//! still only passes while the game is running (i.e. not paused or shut down), but it still passes
//! even if the chunk containing the computer is unloaded; world time is therefore synchronized to
//! the world’s day/night cycle.
//!
//! Sleep functions can also be distinguished based on whether they accept a duration (a time
//! period relative to the moment when they are called) or a deadline (a point in time at which the
//! sleep will end).
//!
//! Sleeping based on uptime is more efficient than sleeping based on world time. Therefore, if
//! either is acceptable for your application, prefer uptime.

use core::cmp::min;
use core::future::Future;
use core::pin::Pin;
use core::task::{Context, Poll, Waker};
use core::time::Duration;
use oc_wasm_safe::computer;
use ordered_float::NotNan;

/// A type that can be interpreted as a number measured in 20 Hz ticks.
trait AsMinecraftTicks {
	/// Returns the number of ticks.
	fn as_minecraft_ticks(&self) -> u64;
}

impl AsMinecraftTicks for Duration {
	fn as_minecraft_ticks(&self) -> u64 {
		match self.checked_mul(20) {
			Some(n) => n.as_secs(),
			None => u64::MAX,
		}
	}
}

/// Allocates a unique 64-bit number.
///
/// Each call to this function returns a different 64-bit integer than any other call.
fn alloc_id() -> u64 {
	static mut COUNTER: u64 = 0;
	// OC-Wasm is single-threaded, so a second thread cannot touch COUNTER at the same time. This
	// function is a leaf (it does not call any other functions), so the same thread also cannot
	// touch COUNTER at the same time via reentrancy.
	let ret = unsafe { COUNTER };
	unsafe { COUNTER = ret + 1 };
	ret
}

/// Registers a waker to be called at the next timeslice.
pub fn register_next_timeslice_wakeup(waker: &Waker) {
	imp::register_next_timeslice(waker);
}

/// A future that completes on the next timeslice.
#[must_use = "A future does nothing unless awaited."]
pub struct UntilNextTimeslice(bool);

impl Future for UntilNextTimeslice {
	type Output = ();

	fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
		if self.0 {
			Poll::Ready(())
		} else {
			self.0 = true;
			register_next_timeslice_wakeup(context.waker());
			Poll::Pending
		}
	}
}

/// A future that completes at a particular computer uptime.
#[must_use = "A future does nothing unless awaited."]
pub struct UntilUptime {
	deadline: NotNan<f64>,
	id: u64,
}

impl Future for UntilUptime {
	type Output = ();

	fn poll(self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
		if computer::uptime() >= self.deadline {
			Poll::Ready(())
		} else {
			imp::register_uptime(self.deadline, self.id, context.waker());
			Poll::Pending
		}
	}
}

impl Drop for UntilUptime {
	fn drop(&mut self) {
		imp::unregister_uptime(self.deadline, self.id);
	}
}

/// Waits until the next timeslice.
///
/// This future completes once the current timeslice has been relinquished and the next timeslice
/// has started. This is useful during long-running computations to avoid reaching the timeslice
/// limit and being forcefully terminated.
pub fn until_next_timeslice() -> UntilNextTimeslice {
	UntilNextTimeslice(false)
}

/// Sleeps for a number of seconds of computer uptime.
///
/// This future completes once the computer has been present in a loaded chunk for `duration`,
/// starting from now.
pub fn for_uptime(duration: Duration) -> UntilUptime {
	until_uptime(computer::uptime() + duration.as_secs_f64())
}

/// Sleeps until a deadline measured in seconds of computer uptime.
///
/// This future completes once the computer has been present in a loaded chunk for at least
/// `deadline` seconds since last boot.
pub fn until_uptime(deadline: NotNan<f64>) -> UntilUptime {
	UntilUptime {
		deadline,
		id: alloc_id(),
	}
}

/// Returns the shortest sleep time currently requested, in ticks of computer uptime.
///
/// This function’s return value should be returned from the top-level `run` function to specify
/// the system sleep time.
#[must_use = "This function is only useful for its return value"]
pub fn shortest_requested() -> i32 {
	#[allow(clippy::cast_possible_truncation)] // The value is clamped to i32::MAX.
	{
		min(
			imp::shortest_requested().as_minecraft_ticks(),
			i32::MAX as u64,
		) as i32
	}
}

/// Wakes up any sleeps that are now finished.
///
/// This function should be called on entry to the `run` function to wake up any tasks whose sleep
/// times have now expired.
pub fn check_for_wakeups() {
	imp::check_for_wakeups()
}

/// Converts a floating-point time delta in seconds into a duration.
///
/// This function handles out-of-range and negative values by clamping them to lie within the valid
/// range. For negative values, a duration of zero is appropriate as it will cause immediate
/// wakeup. For large values, a duration of 2⁶⁴ is appropriate as that is a sufficiently long time
/// as to be irrelevant.
fn delta_to_duration(delta: NotNan<f64>) -> Duration {
	// We can’t clamp to exactly u64::MAX. That’s because, while all u32s are exactly representable
	// as f64s, not all u64s are; converting a large u64 into an f64 involves rounding. In the case
	// of u64::MAX, it unfortunately involves rounding *up*, which means the clamp can then produce
	// an f64 greater than u64::MAX. Calling from_secs_f64 on such a value then panics.
	//
	// Clamping to 4096 less than u64::MAX does the job. An f64 has 53 significand bits, so to
	// affect the LSb of the f64’s significand, we need to subtract an integer that is at least
	// (64−53=11) bits wide; 4096 is 2¹². This has no notable impact on the actual value of the
	// sleep, as 4096 is less than ¼ of a part per quadrillion of u64::MAX, and in any case the
	// maximum sleep time is half a trillion years.
	#[allow(clippy::cast_precision_loss)] // Handled by subtracting 4096 as described.
	Duration::from_secs_f64(delta.into_inner().clamp(0.0, (u64::MAX - 4096) as f64))
}

/// The internal implementation details of the `sleep` module.
///
/// There are two variants of this `imp` module. This variant is used when the `proper-waker`
/// feature flag is enabled.
#[cfg(feature = "proper-waker")]
mod imp {
	use super::delta_to_duration;
	use alloc::collections::BTreeMap;
	use alloc::vec::Vec;
	use core::task::Waker;
	use core::time::Duration;
	use oc_wasm_safe::computer;
	use ordered_float::NotNan;

	/// Returns the vector used to hold [`Wakers`](Waker)s that are waiting for the next timeslice.
	///
	/// # Safety
	/// This function returns a mutable reference to the same object on every call. The caller must
	/// not call `next_timeslice_queue` a second time before it has dropped the first reference.
	unsafe fn next_timeslice_queue() -> &'static mut Vec<Waker> {
		static mut QUEUE: Vec<Waker> = Vec::new();
		&mut QUEUE
	}

	/// Returns the map used to hold [`Wakers`](Waker)s that are waiting for specific deadlines in
	/// wakeup order.
	///
	/// # Safety
	/// This function returns a mutable reference to the same object on every call. The caller must
	/// not call `deadline_map` a second time before it has dropped the first reference.
	unsafe fn deadline_map() -> &'static mut BTreeMap<(NotNan<f64>, u64), Waker> {
		static mut MAP: Option<BTreeMap<(NotNan<f64>, u64), Waker>> = None;
		// SAFETY: BTreeMap::new() does not call sleepers(), so a second mutable reference to MAP
		// cannot be created via reentrancy. OC-Wasm is single-threaded, so a second reference to
		// MAP cannot be created by another thread. The MAP variable is local to this function, so
		// a second mutable reference cannot be created by other code accessing it directly. The
		// documentation requires that the caller maintain only one reference at a time, so a
		// second mutable reference cannot be created by additional calls to this function.
		MAP.get_or_insert_with(BTreeMap::new)
	}

	/// Registers the executing task to wake up at the next timeslice.
	pub fn register_next_timeslice(waker: &Waker) {
		let waker = waker.clone();
		// SAFETY: Vec::push() does not call next_timeslice_queue(), so a second reference is not
		// created within this thread while the first reference exists.
		unsafe { next_timeslice_queue() }.push(waker);
	}

	/// Registers the executing task to wake up at a particular deadline.
	pub fn register_uptime(deadline: NotNan<f64>, id: u64, waker: &Waker) {
		let waker = waker.clone();
		// SAFETY: BTreeMap::insert() does not call deadline_map(), so a second reference is not
		// created within this thread while the first reference exists.
		unsafe { deadline_map() }.insert((deadline, id), waker);
	}

	/// Removes a registered wakeup.
	pub fn unregister_uptime(deadline: NotNan<f64>, id: u64) {
		// SAFETY: BTreeMap::remove() does not call deadline_map(), so a second reference is not
		// created within this thread while the first reference exists.
		let elt = unsafe { deadline_map() }.remove(&(deadline, id));
		// Extend the lifetime of the waker until here, to ensure that the deadline_map() reference
		// is gone before Waker::drop() (if any) runs, just in case Waker::drop() calls this
		// method.
		drop(elt);
	}

	/// Returns the first key in the deadline map, if any.
	fn first_deadline_key() -> Option<(NotNan<f64>, u64)> {
		// SAFETY: Neither BTreeMap::iter() nor btree_map::Iter::next() nor Option::map() calls
		// deadline_map(), so a second reference is not created within this thread while the first
		// reference exists.
		unsafe { deadline_map() }.iter().next().map(|elt| *elt.0)
	}

	/// Returns how long to sleep until the earliest deadline.
	pub fn shortest_requested() -> Duration {
		// SAFETY: Vec::is_empty() does not call next_timeslice_queue(), so a second reference is
		// not created within this thread while the first reference exists.
		let any_next_timeslice = !unsafe { next_timeslice_queue() }.is_empty();
		if any_next_timeslice {
			Duration::from_secs(0)
		} else if let Some(first_key) = first_deadline_key() {
			delta_to_duration(first_key.0 - computer::uptime())
		} else {
			Duration::from_secs(u64::MAX)
		}
	}

	/// Pops and returns a [`Waker`](Waker) whose deadline has passed.
	///
	/// Returns `None` if there is no such [`Waker`](Waker).
	fn pop_deadline_passed(now: NotNan<f64>) -> Option<Waker> {
		if let Some(first_key) = first_deadline_key() {
			if first_key.0 <= now {
				// SAFETY: BTreeMap::remove() does not call deadline_map(), so a second reference
				// is not created within this thread while the first reference exists.
				unsafe { deadline_map() }.remove(&first_key)
			} else {
				None
			}
		} else {
			None
		}
	}

	/// Wakes up all sleepers whose deadline has been reached.
	pub fn check_for_wakeups() {
		// Wake those waiting for the next timeslice. SAFETY: Vec::pop() does not call
		// next_timeslice_queue(), so a second reference is not created within this thread while
		// the first reference exists.
		while let Some(waker) = unsafe { next_timeslice_queue() }.pop() {
			waker.wake();
		}

		// Wake those whose wakeup deadline has passed.
		let now = computer::uptime();
		while let Some(sleeper) = pop_deadline_passed(now) {
			sleeper.wake();
		}
	}
}

#[cfg(not(feature = "proper-waker"))]
mod imp {
	use super::delta_to_duration;
	use core::cmp::min;
	use core::task::Waker;
	use core::time::Duration;
	use oc_wasm_safe::computer;
	use ordered_float::NotNan;

	/// Zero, as a `NotNan<f64>`.
	const NOTNAN_ZERO: NotNan<f64> = unsafe {
		// SAFETY: Zero is not NaN.
		NotNan::new_unchecked(0.0)
	};

	/// Positive infinity, as a `NotNan<f64>`.
	const NOTNAN_INFINITY: NotNan<f64> = unsafe {
		// SAFETY: Infinity is not NaN.
		NotNan::new_unchecked(f64::INFINITY)
	};

	static mut EARLIEST_DEADLINE: NotNan<f64> = NOTNAN_INFINITY;

	pub fn register_next_timeslice(_: &Waker) {
		// SAFETY: OC-Wasm is single-threaded so only one thread can be accessing
		// EARLIEST_DEADLINE.
		unsafe { EARLIEST_DEADLINE = NOTNAN_ZERO };
	}

	pub fn register_uptime(deadline: NotNan<f64>, _: u64, _: &Waker) {
		// SAFETY: OC-Wasm is single-threaded so only one thread can be accessing
		// EARLIEST_DEADLINE.
		unsafe { EARLIEST_DEADLINE = min(EARLIEST_DEADLINE, deadline) };
	}

	pub fn unregister_uptime(_: NotNan<f64>, _: u64) {
		// We do not have enough information to unregister. Doing nothing is harmless; it may just
		// result in tasks being woken before they strictly need to be.
	}

	pub fn shortest_requested() -> Duration {
		// SAFETY: OC-Wasm is single-threaded so only one thread can be accessing
		// EARLIEST_DEADLINE.
		let delta = unsafe { EARLIEST_DEADLINE } - computer::uptime();
		delta_to_duration(delta)
	}

	pub fn check_for_wakeups() {
		// SAFETY: OC-Wasm is single-threaded so only one thread can be accessing
		// EARLIEST_DEADLINE.
		unsafe { EARLIEST_DEADLINE = NOTNAN_INFINITY };
	}
}
