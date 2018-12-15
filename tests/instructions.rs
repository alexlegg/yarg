use yarg::asm::assembler;
use yarg::asm::parser::Parser;
use yarg::emulator::Emulator;
use yarg::joypad::JoypadInput;

const FAIL_ADDR: u16 = 0x300;
const PASS_ADDR: u16 = 0x400;

#[test]
fn adc() -> Result<(), String> {
  let test_asm = include_str!("asm/adc.s");
  let parser = Parser::new(test_asm.chars());
  let rom = assembler::assemble(parser.parse()?)?;
  let mut emu = Emulator::new(None, rom);
  emu.add_breakpoint(FAIL_ADDR);
  emu.add_breakpoint(PASS_ADDR);
  let r = loop {
    if let Err(s) = emu.emu_loop(JoypadInput::new()) {
      break s;
    }
  };
  assert_eq!(r, format!("Breakpoint {:#06x}", PASS_ADDR));
  emu.debug_dump();
  Ok(())
}
