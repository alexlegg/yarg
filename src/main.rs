mod cpu;
mod emulator;
mod joypad;
mod opcode;
mod ppu;
mod sdl;
mod timer;

use std::env;

fn main() {
		let args: Vec<String> = env::args().collect();
		let mut show_vram : bool = false;
		for arg in args {
			if arg == "-v" {
				show_vram = true
			}
		}
    let emu = emulator::Emulator::new();
    sdl::init(emu, show_vram);
}
