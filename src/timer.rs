use cpu::Interrupt;
use cpu::TrapHandler;

const CTRL_FREQ : u8 = 0b11;
const CTRL_ENABLE : u8 = 0b1 << 2;

#[derive(Copy, Clone, Debug)]
pub struct Timer {
	timer: u16,
	counter : u8,
	modulo : u8,
	control : u8,
}

impl Timer {
	pub fn new() -> Timer {
		Timer {
			timer: 0,
			counter : 0,
			modulo : 0,
			control : 0,
		}
	}

	fn freqency_bitmask(&self) -> u16 {
		let freq = self.control & 0b11;
		if freq == 0 {
			1 << 9
		} else {
			1 << ((freq * 2) + 1)
		}
	}

	fn tick_clock(&mut self) -> bool {
		let old = self.timer;
		let new = old.wrapping_add(1);
		self.timer = new;

		if self.control & CTRL_ENABLE == 0 {
			return false;
		}

		// Increment the counter if the timer is enabled. Just copy the hardware
		// and increment on a falling edge of the bit specified in control.
		let bitmask = self.freqency_bitmask();
		if bitmask & old > 0 && bitmask & new == 0 {
			let (new_counter, interrupt) = self.counter.overflowing_add(1);
			self.counter = new_counter;
			return interrupt;
		}
		return false;
	}
}

impl TrapHandler for Timer {
    fn read(&self, addr: u16) -> Result<u8, String> {
    	if addr == 0xff04 {
    		Ok((self.timer >> 8) as u8)
    	} else if addr == 0xff05 {
    		Ok(self.counter)
    	} else if addr == 0xff06 {
    		Ok(self.modulo)
    	} else if addr == 0xff07 {
    		Ok(self.control)
    	} else {
    		Err(format!("Read from {:x} incorrectly trapped to Timer", addr))
    	}
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), String> {
    	if addr == 0xff04 {
    		self.timer = 0;
    	} else if addr == 0xff05 {
    		// TODO Double check behaviour here
    		self.counter = val;
    	} else if addr == 0xff06 {
    		self.modulo = val;
    	} else if addr == 0xff07 {
    		self.control = val & (CTRL_FREQ | CTRL_ENABLE);
    	}	else {
    		return Err(format!("Write to {:x} incorrectly trapped to Timer", addr))
    	}
    	Ok(())
    }

    fn tick(&mut self, cycles: u16) -> Result<Option<Interrupt>, String> {
    	let mut interrupt = false;
    	for _ in 0..(cycles*4) {
	    	if self.tick_clock() {
	    		// TODO The interrupt should actually be delayed by 1 cycle (4 clocks).
	    		interrupt = true;
	    	}
	    }
	    if interrupt {
	    	self.counter = self.modulo;
	    	Ok(Some(Interrupt::Timer))
	    } else {
	    	Ok(None)
	    }
    }
}
