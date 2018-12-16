use yarg::asm::assembler;
use yarg::asm::parser::Parser;
use yarg::emulator::Emulator;
use yarg::joypad::JoypadInput;

const FAIL_ADDR: u16 = 0x300;
const PASS_ADDR: u16 = 0x400;

fn run_rom(asm: &str) -> Result<String, String> {
  let parser = Parser::new(asm.chars());
  let rom = assembler::assemble(parser.parse()?)?;
  let mut emu = Emulator::new(None, rom);
  emu.add_breakpoint(FAIL_ADDR);
  emu.add_breakpoint(PASS_ADDR);
  loop {
    if let Err(s) = emu.emu_loop(JoypadInput::new()) {
      break Ok(s);
    }
  }
}

#[test]
fn adc() -> Result<(), String> {
  let result = run_rom(include_str!("asm/adc.s"))?;
  assert_eq!(result, format!("Breakpoint {:#06x}", PASS_ADDR));
  Ok(())
}

#[test]
fn add() -> Result<(), String> {
  let result = run_rom(include_str!("asm/add.s"))?;
  assert_eq!(result, format!("Breakpoint {:#06x}", PASS_ADDR));
  Ok(())
}
