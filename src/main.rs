use std::env;
use std::fs;
use std::io;
use std::io::Write;
use yarg::emulator::Emulator;
use yarg::sdl;

fn debugger_cli(bootrom: &Option<Vec<u8>>, rom: &[u8]) -> Result<(), String> {
  // TODO: Add reset logic to Emulator so I don't need to clone here.
  let mut emu = Emulator::new(bootrom.clone(), rom.to_vec());
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
            emu = Emulator::new(bootrom.clone(), rom.to_vec());
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

fn main() -> Result<(), String> {
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
    return Err("Must specify ROM file".to_string());
  }

  let bootrom = if use_bootrom {
    Some(fs::read("rom/bootrom.gb").map_err(|_| "Could not read bootrom file".to_string())?)
  } else {
    None
  };
  let rom_fn = args.last().ok_or_else(|| "Must specify ROM file".to_string())?;

  let rom = fs::read(rom_fn).map_err(|_| "Could not read ROM file".to_string())?;

  if debugger {
    return debugger_cli(&bootrom, &rom);
  }

  let mut emu = Emulator::new(bootrom, rom);
  sdl::init(&mut emu, show_vram);
  Ok(())
}
