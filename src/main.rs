mod cartridge;
mod cpu;
mod emulator;
mod joypad;
mod opcode;
mod ppu;
mod sdl;
mod timer;

use emulator::Emulator;
use std::env;
use std::io;
use std::io::Write;

fn debugger_cli(bootrom_fn: Option<&str>, rom_fn: &str) -> Result<(), String> {
    let mut emu = Emulator::new(bootrom_fn, rom_fn)?;
    loop {
        print!("> ");
        let _ = io::stdout().flush();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let mut iter = input.split_whitespace();
                match iter.next() {
                    Some("break") | Some("b") => {
                        if let Some(val) = iter.next() {
                            if let Ok(bp) = u16::from_str_radix(val, 16) {
                                println!("Set breakpoint at {:#06x}", bp);
                                emu.add_breakpoint(bp);
                                continue;
                            }
                        }
                        println!("Couldn't parse breakpoint");
                    }
                    Some("run") | Some("r") => {
                        sdl::init(&mut emu, false);
                    }
                    Some("reset") | Some("e") => {
                        emu = emulator::Emulator::new(bootrom_fn, rom_fn)?;
                    }
                    Some("quit") | Some("q") => break,
                    _ => println!("Unrecognised command"),
                }
            }
            Err(error) => {
                println!("error: {}", error);
                break;
            }
        }
    }
    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut show_vram: bool = false;
    let mut use_bootrom: bool = false;
    let mut args_processed = 1;
    let mut debugger = false;
    for arg in &args {
        if arg == "-v" {
            show_vram = true;
            args_processed += 1;
        }
        if arg == "-b" {
            use_bootrom = true;
            args_processed += 1;
        }
        if arg == "-d" {
            debugger = true;
            args_processed += 1;
        }
    }
    if args_processed == args.len() {
        println!("Must specify ROM");
        return;
    }
    let bootrom_fn = if use_bootrom {
        Some("roms/bootrom.gb")
    } else {
        None
    };
    if let Some(rom_fn) = args.last() {
        if debugger {
            debugger_cli(bootrom_fn, rom_fn);
        } else if let Ok(mut emu) = emulator::Emulator::new(bootrom_fn, rom_fn) {
            sdl::init(&mut emu, show_vram);
        }
    }
}
