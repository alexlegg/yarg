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
		let mut use_bootrom : bool = false;
		let mut args_processed = 1;
		for arg in &args {
			if arg == "-v" {
				show_vram = true;
				args_processed += 1;
			}
			if arg == "-b" {
				use_bootrom = true;
				args_processed += 1;
			}
		}
		if args_processed == args.len() {
			println!("Must specify ROM");
			return;
		}
		let bootrom_fn = if use_bootrom { Some("roms/bootrom.gb") } else { None };
		if let Some(rom_fn) = args.last() {
			let emu = emulator::Emulator::new(bootrom_fn, rom_fn);
	    sdl::init(emu, show_vram);
		}
}
