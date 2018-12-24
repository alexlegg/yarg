pub mod editor;
pub mod windows;

use std::env;
use std::fs;
use yarg::decode;
use yarg::emulator::Emulator;

fn disassemble_rom(rom: &[u8]) -> Result<Vec<(u16, String)>, String> {
  let mut disassembled_rom = Vec::new();
  let mut addr: usize = 0;
  while addr < 0x7fff {
    let get_operand = |operand| Ok(rom[addr + operand as usize]);
    match decode::decode(rom[addr], get_operand) {
      Ok((size, instr)) => {
        disassembled_rom.push((addr as u16, format!("{:?}", instr)));
        addr += size as usize;
      }
      _ => {
        disassembled_rom.push((addr as u16, format!("{:x}", rom[addr])));
        addr += 1;
      }
    }
  }
  Ok(disassembled_rom)
}

fn main() -> Result<(), String> {
  let args: Vec<String> = env::args().collect();
  let mut use_bootrom: bool = false;
  let mut args_processed = 1;
  let mut debugger = false;
  for arg in &args {
    if arg == "-v" {
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
    return Err("Must specify ROM file".to_string());
  }

  let bootrom = if use_bootrom {
    Some(fs::read("rom/bootrom.gb").map_err(|_| "Could not read bootrom file".to_string())?)
  } else {
    None
  };
  let rom_fn = args
    .last()
    .ok_or_else(|| "Must specify ROM file".to_string())?;

  let rom = fs::read(rom_fn).map_err(|_| "Could not read ROM file".to_string())?;
  let disassembled_rom = if debugger {
    Some(disassemble_rom(&rom)?)
  } else {
    None
  };

  let mut emu = Emulator::new(bootrom, rom);
  windows::init(&mut emu, disassembled_rom);
  Ok(())
}
