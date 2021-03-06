#![allow(clippy::identity_op)]
use crate::asm::operation::{Address, Condition, Operation, Reg};
use crate::cpu::Cpu;
use crate::cpu::Flag;
use crate::decode;
use crate::joypad::JoypadInput;
use bincode;
use std::collections::VecDeque;

const DEBUG_STREAM_SIZE: usize = 15;

pub struct Emulator {
  instruction_stream: VecDeque<(u16, Operation<Address>)>,
  cpu: Cpu,
  breakpoints: Vec<u16>,
  at_breakpoint: bool,
}

impl Emulator {
  pub fn new(bootrom: Option<Vec<u8>>, rom: Vec<u8>) -> Emulator {
    let mut emu = Emulator {
      instruction_stream: VecDeque::with_capacity(DEBUG_STREAM_SIZE),
      cpu: Cpu::new(bootrom, rom),
      breakpoints: Vec::new(),
      at_breakpoint: false,
    };
    for _ in 0..DEBUG_STREAM_SIZE {
      emu.instruction_stream.push_back((0, Operation::Nop));
    }
    emu
  }

  pub fn emu_loop(&mut self, joypad: JoypadInput) -> Result<(), String> {
    self.cpu.joypad.set_state(joypad);
    let ret = cpu_loop(self);
    if ret.is_err() {
      self.debug_dump();
    }
    ret
  }

  pub fn add_breakpoint(&mut self, bp: u16) {
    self.breakpoints.push(bp);
  }

  pub fn screen_buffer(&self) -> &[u8] {
    &*self.cpu.ppu.screen_buffer
  }

  pub fn should_draw(&mut self) -> bool {
    self.cpu.ppu.should_draw()
  }

  pub fn get_tile_data(&mut self) -> Option<Box<[u8; 128 * 192 * 4]>> {
    self.cpu.ppu.get_tile_data()
  }

  pub fn debug_dump(&self) {
    println!();
    self.cpu.dump_stack();
    self.cpu.dump_regs();
    println!();
    println!("Dumping instruction stream");
    for (pc, inst) in &self.instruction_stream {
      println!("{:#06x}: {:?}", pc, inst);
    }
  }

  pub fn save_state(&self) -> Result<Vec<u8>, String> {
    bincode::serialize(&self.cpu).map_err(|_| "Failed to save state".to_string())
  }

  pub fn restore_state(&mut self, state: &[u8]) -> Result<(), String> {
    self.cpu = bincode::deserialize(state).map_err(|_| "Failed to restore state".to_string())?;
    Ok(())
  }
}

fn half_carried(curr: u8, val: u8) -> bool {
  (curr & 0x0f) + (val & 0x0f) > 0x0f
}

fn half_borrowed(curr: u8, val: u8) -> bool {
  (curr & 0x0f) < (val & 0x0f)
}

#[allow(clippy::verbose_bit_mask)]
#[allow(clippy::cyclomatic_complexity)]
fn cpu_loop(emu: &mut Emulator) -> Result<(), String> {
  let cpu = &mut emu.cpu;

  cpu.check_for_interrupt()?;

  // Tick once here. Every instruction will take at least one cycle for
  // reading the value at PC.
  cpu.tick(1)?;

  if cpu.is_halted {
    return Ok(());
  }

  let pc = cpu.get_reg16(Reg::PC)?;
  let (inst_size, inst) = get_inst(&cpu)?;

  if emu.breakpoints.contains(&pc) {
    if emu.at_breakpoint {
      emu.at_breakpoint = false;
    } else {
      emu.at_breakpoint = true;
      return Err(format!("Breakpoint {:#06x}", pc));
    }
  }

  cpu.set_pc(pc.wrapping_add(inst_size))?;

  emu.instruction_stream.pop_front();
  emu.instruction_stream.push_back((pc, inst));

  match inst {
    Operation::Nop => Ok(()),
    Operation::Stop => Ok(()),
    Operation::Halt => {
      cpu.is_halted = true;
      Ok(())
    }
    Operation::DecimalAdjustAccumulator => {
      let mut correction = 0;
      let mut c = false;
      if cpu.get_flag(Flag::H) || (!cpu.get_flag(Flag::N) && (cpu.a & 0xf) > 0x9) {
        correction |= 0x6;
      }
      if cpu.get_flag(Flag::C) || (!cpu.get_flag(Flag::N) && cpu.a > 0x99) {
        correction |= 0x60;
        c = true;
      }
      let next_a = if cpu.get_flag(Flag::N) {
        cpu.a.wrapping_sub(correction)
      } else {
        cpu.a.wrapping_add(correction)
      };
      cpu.set_flag(Flag::Z, next_a == 0);
      cpu.set_flag(Flag::C, c);
      cpu.set_flag(Flag::H, false);
      cpu.a = next_a;
      Ok(())
    }
    Operation::Load8(destination, source) => {
      let src = cpu.get_address8(source)?;
      cpu.set_address8(destination, src)
    }
    Operation::Load16(destination, source) => {
      // TODO The timing here is most likely wrong.
      let src = match source {
        Address::Register(r) => {
          cpu.tick(1)?;
          cpu.get_reg16(r)
        }
        Address::Data16(imm) => {
          cpu.tick(2)?;
          Ok(imm)
        }
        _ => Err("Load16 source".to_string()),
      }?;
      match destination {
        Address::Register(r) => cpu.set_reg16(r, src),
        Address::Immediate(addr) => {
          cpu.tick(3)?;
          cpu.write_mem16(addr, src)
        }
        _ => Err("Load16 destination".to_string()),
      }
    }
    Operation::LoadIncrement(destination, source) => {
      let src = cpu.get_address8(source)?;
      cpu.set_address8(destination, src)?;
      let hl = cpu.get_reg16(Reg::HL)?.wrapping_add(1);
      cpu.set_reg16(Reg::HL, hl)?;
      Ok(())
    }
    Operation::LoadDecrement(destination, source) => {
      let src = cpu.get_address8(source)?;
      cpu.set_address8(destination, src)?;
      let hl = cpu.get_reg16(Reg::HL)?.wrapping_sub(1);
      cpu.set_reg16(Reg::HL, hl)?;
      Ok(())
    }
    Operation::Increment(destination) => {
      let val0 = cpu.get_address8(destination)?;
      let val1 = val0.wrapping_add(1);
      cpu.set_flag(Flag::Z, val1 == 0);
      cpu.set_flag(Flag::N, false);
      cpu.set_flag(Flag::H, (val0 & 0x0f) + 1 > 0x0f);
      cpu.set_address8(destination, val1)
    }
    Operation::Increment16(destination) => {
      cpu.tick(1)?;
      let reg = match destination {
        Address::Register(r) => Ok(r),
        _ => Err("Increment has bad destination".to_string()),
      }?;
      let v = cpu.get_reg16(reg)?.wrapping_add(1);
      cpu.set_reg16(reg, v)?;
      Ok(())
    }
    Operation::Decrement(destination) => {
      let val0 = cpu.get_address8(destination)?;
      let val1 = val0.wrapping_sub(1);
      cpu.set_flag(Flag::Z, val1 == 0);
      cpu.set_flag(Flag::N, true);
      cpu.set_flag(Flag::H, val0 & 0x0f == 0);
      cpu.set_address8(destination, val1)
    }
    Operation::Decrement16(destination) => {
      cpu.tick(1)?;
      let reg = match destination {
        Address::Register(r) => Ok(r),
        _ => Err("Increment has bad destination".to_string()),
      }?;
      let v = cpu.get_reg16(reg)?.wrapping_sub(1);
      cpu.set_reg16(reg, v)
    }
    Operation::SetCarry => {
      cpu.set_flag(Flag::N, false);
      cpu.set_flag(Flag::C, true);
      cpu.set_flag(Flag::H, false);
      Ok(())
    }
    Operation::ComplementCarry => {
      let prev_carry = cpu.get_flag(Flag::C);
      cpu.set_flag(Flag::N, false);
      cpu.set_flag(Flag::C, !prev_carry);
      cpu.set_flag(Flag::H, false);
      Ok(())
    }
    Operation::Jump(condition, source) => {
      let addr: u16 = match source {
        Address::Immediate(imm) => {
          cpu.tick(2)?;
          Ok(imm)
        }
        Address::Register(r) => cpu.get_reg16(r),
        Address::Relative(rel) => {
          cpu.tick(1)?;
          let next_pc = cpu.get_reg16(Reg::PC)?;
          let e: i8 = rel as i8;
          Ok(next_pc.wrapping_add(e as u16))
        }
        _ => Err("Bad jump source".to_string()),
      }?;
      if cpu.check_condition(condition) {
        // JP (HL) doesn't have an extra tick on successful jump.
        match source {
          Address::Register(_) => Ok(()),
          _ => cpu.tick(1),
        }?;
        cpu.set_pc(addr)?;
      }
      Ok(())
    }
    Operation::Complement => {
      cpu.a = !cpu.a;
      cpu.set_flag(Flag::N, true);
      cpu.set_flag(Flag::H, true);
      Ok(())
    }
    Operation::Add(source) => {
      let val = cpu.get_address8(source)?;
      let old_a = cpu.a;
      let (next_a, c) = cpu.a.overflowing_add(val);
      cpu.set_flag(Flag::Z, next_a == 0);
      cpu.set_flag(Flag::N, false);
      cpu.set_flag(Flag::C, c);
      cpu.set_flag(Flag::H, half_carried(old_a, val));
      cpu.a = next_a;
      Ok(())
    }
    Operation::Add16(source) => {
      cpu.tick(1)?;
      let val = match source {
        Address::Register(r) => cpu.get_reg16(r)?,
        _ => panic!("Bad and16 source"),
      };
      let old_hl = cpu.get_reg16(Reg::HL)?;
      let (next_hl, c) = old_hl.overflowing_add(val);
      cpu.set_flag(Flag::N, false);
      cpu.set_flag(Flag::C, c);
      cpu.set_flag(Flag::H, (old_hl & 0xfff) + (val & 0xfff) > 0xfff);
      cpu.set_reg16(Reg::HL, next_hl)?;
      Ok(())
    }
    Operation::AddCarry(source) => {
      let val = cpu.get_address8(source)?;
      let cy = if cpu.get_flag(Flag::C) { 1 } else { 0 };
      let old_a = cpu.a;
      let next_a = cpu.a.wrapping_add(val).wrapping_add(cy);
      cpu.set_flag(Flag::Z, next_a == 0);
      cpu.set_flag(Flag::N, false);
      cpu.set_flag(
        Flag::C,
        u16::from(old_a) + u16::from(val) + u16::from(cy) > 0xff,
      );
      cpu.set_flag(Flag::H, (old_a & 0x0f) + (val & 0x0f) + cy > 0x0f);
      cpu.a = next_a;
      Ok(())
    }
    Operation::Sub(source) => {
      let val = cpu.get_address8(source)?;
      let (next_a, c) = cpu.a.overflowing_sub(val);
      let old_a = cpu.a;
      cpu.set_flag(Flag::Z, next_a == 0);
      cpu.set_flag(Flag::N, true);
      cpu.set_flag(Flag::C, c);
      cpu.set_flag(Flag::H, half_borrowed(old_a, val));
      cpu.a = next_a;
      Ok(())
    }
    Operation::SubCarry(source) => {
      let val = cpu.get_address8(source)?;
      let cy = if cpu.get_flag(Flag::C) { 1 } else { 0 };
      let old_a = cpu.a;
      let (va, cval) = cpu.a.overflowing_sub(val);
      let (next_a, ccy) = va.overflowing_sub(cy);
      cpu.set_flag(Flag::Z, next_a == 0);
      cpu.set_flag(Flag::N, true);
      cpu.set_flag(Flag::C, cval | ccy);
      cpu.set_flag(Flag::H, (old_a & 0x0f) < (val & 0x0f) + cy);
      cpu.a = next_a;
      Ok(())
    }
    Operation::And(source) => {
      cpu.a &= cpu.get_address8(source)?;
      let z = cpu.a == 0;
      cpu.set_flag(Flag::Z, z);
      cpu.set_flag(Flag::N, false);
      cpu.set_flag(Flag::C, false);
      cpu.set_flag(Flag::H, true);
      Ok(())
    }
    Operation::Xor(source) => {
      cpu.a ^= cpu.get_address8(source)?;
      let z = cpu.a == 0;
      cpu.set_flag(Flag::Z, z);
      cpu.set_flag(Flag::N, false);
      cpu.set_flag(Flag::C, false);
      cpu.set_flag(Flag::H, false);
      Ok(())
    }
    Operation::Or(source) => {
      cpu.a |= cpu.get_address8(source)?;
      let z = cpu.a == 0;
      cpu.set_flag(Flag::Z, z);
      cpu.set_flag(Flag::N, false);
      cpu.set_flag(Flag::C, false);
      cpu.set_flag(Flag::H, false);
      Ok(())
    }
    Operation::Compare(source) => {
      let val = cpu.get_address8(source)?;
      let (result, c) = cpu.a.overflowing_sub(val);
      let a = cpu.a;
      cpu.set_flag(Flag::Z, result == 0);
      cpu.set_flag(Flag::N, true);
      cpu.set_flag(Flag::C, c);
      cpu.set_flag(Flag::H, half_borrowed(a, val));
      Ok(())
    }
    Operation::Pop(destination) => {
      let val = cpu.pop_stack()?;
      match destination {
        Address::Register(r) => cpu.set_reg16(r, val),
        _ => unimplemented!("Load destination"),
      }
    }
    Operation::Push(source) => {
      cpu.tick(1)?;
      let val = match source {
        Address::Register(r) => cpu.get_reg16(r)?,
        _ => unimplemented!("Load destination"),
      };
      cpu.push_stack(val)
    }
    Operation::Call(condition, addr) => {
      cpu.tick(2)?;
      if cpu.check_condition(condition) {
        let next_pc = cpu.get_reg16(Reg::PC)?;
        cpu.push_stack(next_pc)?;
        cpu.tick(1)?;
        cpu.set_pc(addr)?;
      }
      Ok(())
    }
    Operation::Return(condition) => {
      // Conditional RETs take one extra tick.
      match condition {
        Condition::Unconditional => Ok(()),
        _ => cpu.tick(1),
      }?;
      if cpu.check_condition(condition) {
        let addr = cpu.pop_stack()?;
        cpu.tick(1)?;
        cpu.set_pc(addr)?;
      }
      Ok(())
    }
    Operation::DisableInterrupts => {
      cpu.interrupt_master_enable = false;
      Ok(())
    }
    Operation::EnableInterrupts => {
      cpu.interrupt_master_enable = true;
      Ok(())
    }
    Operation::ReturnFromInterrupt => {
      let addr = cpu.pop_stack()?;
      cpu.set_pc(addr)?;
      cpu.tick(1)?;
      cpu.interrupt_master_enable = true;
      Ok(())
    }
    Operation::AddStack(destination, source) => {
      cpu.tick(2)?;
      let src = match source {
        Address::StackRelative(rel) => Ok(rel as i8 as u16),
        _ => Err("AddStack source".to_string()),
      }?;
      let sp = cpu.get_reg16(Reg::SP)?;
      let val_next = sp.wrapping_add(src);
      cpu.set_flag(Flag::Z, false);
      cpu.set_flag(Flag::N, false);
      // NOTE: This contradicts the gameboy manual but passes blargg's.
      cpu.set_flag(Flag::H, (sp & 0x000f) + (src & 0x000f) > 0x000f);
      cpu.set_flag(Flag::C, (sp & 0x00ff) + (src & 0x00ff) > 0x00ff);
      match destination {
        Address::Register(Reg::SP) => {
          cpu.tick(1)?;
          cpu.set_reg16(Reg::SP, val_next)
        }
        Address::Register(Reg::HL) => cpu.set_reg16(Reg::HL, val_next),
        _ => Err("AddStack destination".to_string()),
      }
    }
    Operation::Reset(addr) => {
      cpu.tick(1)?;
      let next_pc = cpu.get_reg16(Reg::PC)?;
      cpu.push_stack(next_pc)?;
      cpu.set_pc(addr)
    }
    Operation::RotateLeftA(copy_carry) => {
      let val = cpu.get_reg8(Reg::A)?;
      let mut val_next = val << 1;
      if copy_carry {
        val_next |= val >> 7;
      } else if cpu.get_flag(Flag::C) {
        val_next |= 1;
      }
      cpu.set_flag(Flag::C, val & (1 << 7) > 0);
      cpu.set_flag(Flag::Z, false);
      cpu.set_flag(Flag::H, false);
      cpu.set_flag(Flag::N, false);
      cpu.set_reg8(Reg::A, val_next)
    }
    Operation::RotateRightA(copy_carry) => {
      let val = cpu.get_reg8(Reg::A)?;
      let mut val_next = val >> 1;
      if copy_carry {
        val_next |= (val & 1) << 7;
      } else if cpu.get_flag(Flag::C) {
        val_next |= 1 << 7;
      }
      cpu.set_flag(Flag::C, val & 1 > 0);
      cpu.set_flag(Flag::Z, false);
      cpu.set_flag(Flag::H, false);
      cpu.set_flag(Flag::N, false);
      cpu.set_reg8(Reg::A, val_next)
    }
    Operation::RotateLeft(copy_carry, destination) => {
      cpu.tick(1)?; // Tick for prefix
      let val = cpu.get_address8(destination)?;
      let mut val_next = val << 1;
      if copy_carry {
        val_next |= val >> 7;
      } else if cpu.get_flag(Flag::C) {
        val_next |= 1;
      }
      cpu.set_flag(Flag::C, val & (1 << 7) > 0);
      cpu.set_flag(Flag::Z, val_next == 0);
      cpu.set_flag(Flag::H, false);
      cpu.set_flag(Flag::N, false);
      cpu.set_address8(destination, val_next)
    }
    Operation::RotateRight(copy_carry, destination) => {
      cpu.tick(1)?; // Tick for prefix
      let val = cpu.get_address8(destination)?;
      let mut val_next = val >> 1;
      if copy_carry {
        val_next |= (val & 1) << 7;
      } else if cpu.get_flag(Flag::C) {
        val_next |= 1 << 7;
      }
      cpu.set_flag(Flag::C, val & 1 > 0);
      cpu.set_flag(Flag::Z, val_next == 0);
      cpu.set_flag(Flag::H, false);
      cpu.set_flag(Flag::N, false);
      cpu.set_address8(destination, val_next)
    }
    Operation::ShiftLeft(destination) => {
      cpu.tick(1)?; // Tick for prefix
      let val = cpu.get_address8(destination)?;
      let val_next = val << 1;
      cpu.set_flag(Flag::C, val & (1 << 7) > 0);
      cpu.set_flag(Flag::Z, val_next == 0);
      cpu.set_flag(Flag::H, false);
      cpu.set_flag(Flag::N, false);
      cpu.set_address8(destination, val_next)
    }
    Operation::ShiftRight(destination) => {
      cpu.tick(1)?; // Tick for prefix
      let val = cpu.get_address8(destination)?;
      let val_next = val >> 1 | (val & (1 << 7));
      cpu.set_flag(Flag::C, val & (1 << 0) > 0);
      cpu.set_flag(Flag::Z, val_next == 0);
      cpu.set_flag(Flag::H, false);
      cpu.set_flag(Flag::N, false);
      cpu.set_address8(destination, val_next)
    }
    Operation::ShiftRightLogical(destination) => {
      cpu.tick(1)?; // Tick for prefix
      let val = cpu.get_address8(destination)?;
      let val_next = val >> 1;
      cpu.set_flag(Flag::C, val & (1 << 0) > 0);
      cpu.set_flag(Flag::Z, val_next == 0);
      cpu.set_flag(Flag::H, false);
      cpu.set_flag(Flag::N, false);
      cpu.set_address8(destination, val_next)
    }
    Operation::Swap(destination) => {
      cpu.tick(1)?; // Tick for prefix
      let val = cpu.get_address8(destination)?;
      let val_next = ((val & 0x0f) << 4) | ((val & 0xf0) >> 4);
      cpu.set_flag(Flag::Z, val_next == 0);
      cpu.set_flag(Flag::C, false);
      cpu.set_flag(Flag::H, false);
      cpu.set_flag(Flag::N, false);
      cpu.set_address8(destination, val_next)
    }
    Operation::ResetBit(b, destination) => {
      cpu.tick(1)?; // Tick for prefix
      let val = cpu.get_address8(destination)?;
      cpu.set_address8(destination, val & !(1 << b))
    }
    Operation::SetBit(b, destination) => {
      cpu.tick(1)?; // Tick for prefix
      let val = cpu.get_address8(destination)?;
      cpu.set_address8(destination, val | (1 << b))
    }
    Operation::Bit(b, source) => {
      cpu.tick(1)?; // Tick for prefix
      let val = cpu.get_address8(source)?;
      cpu.set_flag(Flag::Z, val & (1 << b) == 0);
      cpu.set_flag(Flag::H, true);
      cpu.set_flag(Flag::N, false);
      Ok(())
    }
  }
}

fn get_inst(cpu: &Cpu) -> Result<(u16, Operation<Address>), String> {
  let pc = cpu.get_reg16(Reg::PC)?;
  let opcode = cpu.read_mem8(pc)?;
  let get_operand = |operand| cpu.read_mem8(pc + operand);
  decode::decode(opcode, get_operand)
}
