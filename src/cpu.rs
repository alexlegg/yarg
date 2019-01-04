#![allow(clippy::identity_op)]
use crate::asm::operation::{Address, Condition, Reg};
use crate::cartridge::Cartridge;
use crate::joypad::Joypad;
use crate::ppu::Ppu;
use crate::timer::Timer;
use serde::{Deserialize, Serialize};

#[derive(Copy, Clone, Debug)]
pub enum Flag {
  Z,
  N,
  H,
  C,
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum Interrupt {
  Joypad,
  Serial,
  Timer,
  LCDStat,
  VBlank,
}

fn interrupt_handler_addr(irq: Interrupt) -> u16 {
  match irq {
    Interrupt::Joypad => 0x60,
    Interrupt::Serial => 0x58,
    Interrupt::Timer => 0x50,
    Interrupt::LCDStat => 0x48,
    Interrupt::VBlank => 0x40,
  }
}

#[derive(Serialize, Deserialize)]
pub struct Cpu {
  pub a: u8,
  f: u8,
  b: u8,
  c: u8,
  d: u8,
  e: u8,
  h: u8,
  l: u8,
  pc: u16,
  sp: u16,

  // Work RAM: 8 banks of 4kb.
  // Bank 0: 0xc000 - 0xcfff
  // Switchable bank 1-7: 0xd000 - 0xdfff
  wram: Vec<Vec<u8>>,
  wram_bank: usize,

  // High RAM: 0xff80 - 0xfffe
  hram: Vec<u8>,

  #[serde(skip)]
  bootrom: Option<Vec<u8>>,
  #[serde(skip)]
  bootrom_enabled: bool,

  pub interrupt_master_enable: bool,
  interrupt_enable: u8,
  interrupt_flag: u8,

  cartridge: Cartridge,
  pub ppu: Ppu,

  #[serde(skip)]
  timer: Timer,

  #[serde(skip)]
  pub joypad: Joypad,

  // TODO refactor
  pub is_halted: bool,
}

pub trait TrapHandler {
  fn write(&mut self, addr: u16, val: u8) -> Result<(), String>;
  fn read(&self, addr: u16) -> Result<u8, String>;

  // TODO: Move this to a separate trait
  fn tick(&mut self, cycles: u16) -> Result<Option<Interrupt>, String>;
}

impl Cpu {
  pub fn new(bootrom: Option<Vec<u8>>, rom: Vec<u8>) -> Cpu {
    let has_bootrom = bootrom.is_some();
    Cpu {
      a: 0x01,
      f: 0x00,
      b: 0x00,
      c: 0x13,
      d: 0x00,
      e: 0xd8,
      h: 0x01,
      l: 0x4d,
      pc: if has_bootrom { 0x0000 } else { 0x0100 },
      sp: 0xfffe,

      wram: vec![vec![0; 0x1000]; 8],
      wram_bank: 1,
      hram: vec![0; 0x7f],
      bootrom,
      bootrom_enabled: has_bootrom,
      interrupt_master_enable: false,
      interrupt_enable: 0,
      interrupt_flag: 0,

      cartridge: Cartridge::new(rom),
      ppu: Ppu::new(),
      timer: Timer::new(),
      joypad: Joypad::new(),

      is_halted: false,
    }
  }

  pub fn dump_regs(&self) {
    println!(
      "A {:#04x} F {:#04x} B {:#04x} C {:#04x} D {:#04x} E {:#04x}",
      self.a, self.f, self.b, self.c, self.d, self.e
    );
    println!("H {:#04x} L {:#04x} SP {:#06x} ", self.h, self.l, self.sp);
  }

  pub fn dump_stack(&self) {
    let mut sp = self.sp.wrapping_add(2 * 4);
    println!("Dumping stack");
    for _ in 0..5 {
      match self.read_mem16(sp) {
        Ok(val) => println!("{:#06x}: {:#06x}", sp, val),
        Err(s) => println!("Failed to dump stack at {:#06x}: {:?}", sp, s),
      }
      sp = sp.wrapping_sub(2);
    }
    println!();
  }

  pub fn set_pc(&mut self, addr: u16) -> Result<(), String> {
    self.pc = addr;
    Ok(())
  }

  pub fn get_flag(&self, flag: Flag) -> bool {
    let mask = match flag {
      Flag::Z => 1 << 7,
      Flag::N => 1 << 6,
      Flag::H => 1 << 5,
      Flag::C => 1 << 4,
    };
    (self.f & mask) > 0
  }

  pub fn set_flag(&mut self, flag: Flag, val: bool) {
    let mask = match flag {
      Flag::Z => 1 << 7,
      Flag::N => 1 << 6,
      Flag::H => 1 << 5,
      Flag::C => 1 << 4,
    };
    if val {
      self.f |= mask;
    } else {
      self.f &= !mask;
    }
  }

  pub fn get_reg8(&self, reg: Reg) -> Result<u8, String> {
    match reg {
      Reg::A => Ok(self.a),
      Reg::B => Ok(self.b),
      Reg::C => Ok(self.c),
      Reg::D => Ok(self.d),
      Reg::E => Ok(self.e),
      Reg::H => Ok(self.h),
      Reg::L => Ok(self.l),
      _ => Err("Bad get_reg8".to_string()),
    }
  }

  pub fn set_reg8(&mut self, reg: Reg, val: u8) -> Result<(), String> {
    match reg {
      Reg::A => self.a = val,
      Reg::B => self.b = val,
      Reg::C => self.c = val,
      Reg::D => self.d = val,
      Reg::E => self.e = val,
      Reg::H => self.h = val,
      Reg::L => self.l = val,
      _ => return Err("Bad set_reg8".to_string()),
    }
    Ok(())
  }

  pub fn get_reg16(&self, reg: Reg) -> Result<u16, String> {
    match reg {
      Reg::AF => Ok((u16::from(self.a) << 8) | u16::from(self.f)),
      Reg::BC => Ok((u16::from(self.b) << 8) | u16::from(self.c)),
      Reg::DE => Ok((u16::from(self.d) << 8) | u16::from(self.e)),
      Reg::HL => Ok((u16::from(self.h) << 8) | u16::from(self.l)),
      Reg::PC => Ok(self.pc),
      Reg::SP => Ok(self.sp),
      _ => Err("Bad get_reg16 register".to_string()),
    }
  }

  pub fn set_reg16(&mut self, reg: Reg, val: u16) -> Result<(), String> {
    let high = (val >> 8) as u8;
    let low = (val & 0xff) as u8;
    match reg {
      Reg::AF => {
        self.a = high;
        // Lowest 4 bits of F are always zero.
        self.f = low & 0xf0;
      }
      Reg::BC => {
        self.b = high;
        self.c = low;
      }
      Reg::DE => {
        self.d = high;
        self.e = low;
      }
      Reg::HL => {
        self.h = high;
        self.l = low;
      }
      Reg::SP => {
        self.sp = val;
      }
      _ => return Err("Bad set_reg16 register".to_string()),
    }
    Ok(())
  }

  pub fn read_mem8(&self, addr: u16) -> Result<u8, String> {
    if self.bootrom_enabled && addr <= 0xff {
      if let Some(ref bootrom) = self.bootrom {
        return Ok(bootrom[addr as usize]);
      }
      Err("Bootrom is enabled but there is no bootrom".to_string())
    } else if addr <= 0x7fff {
      self.cartridge.read(addr)
    } else if addr >= 0x8000 && addr <= 0x9fff {
      self.ppu.read(addr)
    } else if addr >= 0xa000 && addr <= 0xbfff {
      self.cartridge.read(addr)
    } else if addr >= 0xc000 && addr <= 0xcfff {
      Ok(self.wram[0][(addr - 0xc000) as usize])
    } else if addr >= 0xd000 && addr <= 0xdfff {
      Ok(self.wram[self.wram_bank][(addr - 0xd000) as usize])
    } else if addr == 0xff00 {
      self.joypad.read(addr)
    } else if addr >= 0xff04 && addr <= 0xff07 {
      self.timer.read(addr)
    } else if addr >= 0xff40 && addr <= 0xff4b {
      self.ppu.read(addr)
    } else if addr == 0xff0f {
      // IF - Interrupt Flag
      Ok(self.interrupt_flag)
    } else if addr >= 0xff80 && addr <= 0xfffe {
      Ok(self.hram[(addr - 0xff80) as usize])
    } else if addr == 0xffff {
      // IE - Interrupt Enable
      Ok(self.interrupt_enable)
    } else if addr >= 0xff00 && addr <= 0xff7f {
      println!("Read IO register {:#06x} as 0", addr);
      // Some IO register
      Ok(0)
    } else {
      Err(format!("Bad read_mem8 addr: {:#06x}", addr))
    }
  }

  pub fn write_mem8(&mut self, addr: u16, val: u8) -> Result<(), String> {
    if addr <= 0x3fff {
      self.cartridge.write(addr, val)
    } else if addr >= 0x8000 && addr <= 0x9fff {
      self.ppu.write(addr, val)
    } else if addr >= 0xa000 && addr <= 0xbfff {
      self.cartridge.write(addr, val)
    } else if addr >= 0xc000 && addr <= 0xcfff {
      self.wram[0][(addr - 0xc000) as usize] = val;
      Ok(())
    } else if addr >= 0xd000 && addr <= 0xdfff {
      self.wram[self.wram_bank][(addr - 0xd000) as usize] = val;
      Ok(())
    } else if addr >= 0xfe00 && addr <= 0xfe9f {
      // Sprite attribute table
      self.ppu.write(addr, val)
    } else if addr >= 0xfea0 && addr <= 0xfeff {
      // Not usable - Ignore.
      Ok(())
    } else if addr >= 0xff80 && addr <= 0xfffe {
      self.hram[(addr - 0xff80) as usize] = val;
      Ok(())
    } else if addr == 0xff00 {
      self.joypad.write(addr, val)
    } else if addr >= 0xff30 && addr <= 0xff3f {
      // Ignore waveform registers
      Ok(())
    } else if addr >= 0xff40 && addr <= 0xff45 {
      self.ppu.write(addr, val)
    } else if addr == 0xff46 {
      // Handle DMA here
      self.handle_dma_transfer(u16::from(val) << 8)
    } else if addr >= 0xff47 && addr <= 0xff4b {
      self.ppu.write(addr, val)
    } else if addr == 0xff50 {
      if val & 0b1 == 0b1 {
        self.bootrom_enabled = false;
      }
      Ok(())
    } else if addr >= 0xff01 && addr <= 0xff02 {
      // Serial IO. Ignored.
      Ok(())
    } else if addr >= 0xff04 && addr <= 0xff07 {
      self.timer.write(addr, val)
    } else if addr >= 0xff10 && addr <= 0xff26 {
      // Sound IO. Ignored
      Ok(())
    } else if addr == 0xff0f {
      // IF - Interrupt Flag
      self.interrupt_flag = val;
      Ok(())
    } else if addr == 0xffff {
      // IE - Interrupt Enable
      self.interrupt_enable = val;
      Ok(())
    } else {
      Ok(())
    }
  }

  pub fn write_mem16(&mut self, addr: u16, val: u16) -> Result<(), String> {
    self.write_mem8(addr, val as u8)?;
    self.write_mem8(addr + 1, (val >> 8) as u8)
  }

  pub fn read_mem16(&self, addr: u16) -> Result<u16, String> {
    let v0 = u16::from(self.read_mem8(addr)?);
    let v1 = u16::from(self.read_mem8(addr + 1)?);
    Ok(v1 << 8 | v0)
  }

  pub fn push_stack(&mut self, val: u16) -> Result<(), String> {
    self.tick(2)?;
    self.sp -= 2;
    let sp = self.sp;
    self.write_mem16(sp, val)?;
    Ok(())
  }

  pub fn pop_stack(&mut self) -> Result<u16, String> {
    self.tick(2)?;
    let sp = self.sp;
    let val = self.read_mem16(sp)?;
    self.sp += 2;
    Ok(val)
  }

  pub fn get_address8(&mut self, addr: Address) -> Result<u8, String> {
    match addr {
      Address::Data8(v) => {
        self.tick(1)?;
        Ok(v)
      }
      Address::Indirect(r) => {
        self.tick(1)?;
        let addr = self.get_reg16(r)?;
        self.read_mem8(addr)
      }
      Address::ExtendedIndirect(r) => {
        self.tick(1)?;
        let addr = 0xff00 | u16::from(self.get_reg8(r)?);
        self.read_mem8(addr)
      }
      Address::Register(r) => self.get_reg8(r),
      Address::Extended(e) => {
        self.tick(2)?;
        let addr: u16 = 0xff00 | u16::from(e);
        self.read_mem8(addr)
      }
      Address::Immediate(addr) => {
        self.tick(3)?;
        self.read_mem8(addr)
      }
      _ => Err("Bad get_address8".to_string()),
    }
  }

  pub fn set_address8(&mut self, addr: Address, val: u8) -> Result<(), String> {
    match addr {
      Address::Register(r) => self.set_reg8(r, val),
      Address::Indirect(r) => {
        self.tick(1)?;
        let addr = self.get_reg16(r)?;
        self.write_mem8(addr, val)
      }
      Address::Immediate(addr) => {
        self.tick(3)?;
        self.write_mem8(addr, val)
      }
      Address::Extended(e) => {
        self.tick(2)?;
        let addr: u16 = 0xff00 | u16::from(e);
        self.write_mem8(addr, val)
      }
      Address::ExtendedIndirect(r) => {
        self.tick(1)?;
        let addr = 0xff00 | u16::from(self.get_reg8(r)?);
        self.write_mem8(addr, val)
      }
      _ => Err("Bad set_address8".to_string()),
    }
  }

  pub fn check_condition(&self, condition: Condition) -> bool {
    match condition {
      Condition::Unconditional => true,
      Condition::Zero => self.get_flag(Flag::Z),
      Condition::NonZero => !self.get_flag(Flag::Z),
      Condition::Carry => self.get_flag(Flag::C),
      Condition::NonCarry => !self.get_flag(Flag::C),
    }
  }

  fn handle_dma_transfer(&mut self, addr: u16) -> Result<(), String> {
    for off in 0..0x9f {
      let val = self.read_mem8(addr + off)?;
      self.write_mem8(0xfe00 + off, val)?;
    }
    Ok(())
  }

  pub fn active_interrupt(&self) -> Option<Interrupt> {
    if !self.interrupt_master_enable {
      None
    } else if (1 << 4) & self.interrupt_enable & self.interrupt_flag > 0 {
      Some(Interrupt::Joypad)
    } else if (1 << 3) & self.interrupt_enable & self.interrupt_flag > 0 {
      Some(Interrupt::Serial)
    } else if (1 << 2) & self.interrupt_enable & self.interrupt_flag > 0 {
      Some(Interrupt::Timer)
    } else if (1 << 1) & self.interrupt_enable & self.interrupt_flag > 0 {
      Some(Interrupt::LCDStat)
    } else if (1 << 0) & self.interrupt_enable & self.interrupt_flag > 0 {
      Some(Interrupt::VBlank)
    } else {
      None
    }
  }

  fn ack_interrupt(&mut self, irq: Interrupt) {
    match irq {
      Interrupt::Joypad => self.interrupt_flag &= !(1 << 4),
      Interrupt::Serial => self.interrupt_flag &= !(1 << 3),
      Interrupt::Timer => self.interrupt_flag &= !(1 << 2),
      Interrupt::LCDStat => self.interrupt_flag &= !(1 << 1),
      Interrupt::VBlank => self.interrupt_flag &= !(1 << 0),
    }
  }

  pub fn tick(&mut self, cycles: u16) -> Result<(), String> {
    let irq = self.ppu.tick(cycles)?;
    match irq {
      Some(Interrupt::VBlank) => self.interrupt_flag |= 1 << 0,
      Some(Interrupt::LCDStat) => self.interrupt_flag |= 1 << 1,
      _ => (),
    };

    let irq = self.timer.tick(cycles)?;
    if let Some(Interrupt::Timer) = irq {
      self.interrupt_flag |= 1 << 2;
    }

    if self.is_halted
      && !self.interrupt_master_enable
      && (self.interrupt_flag & self.interrupt_enable) > 0
    {
      self.is_halted = false;
    }

    Ok(())
  }

  pub fn check_for_interrupt(&mut self) -> Result<(), String> {
    if let Some(irq) = self.active_interrupt() {
      self.tick(2)?;
      let pc = self.pc;
      self.push_stack(pc)?;
      self.tick(2)?;
      self.pc = interrupt_handler_addr(irq);
      self.tick(1)?;
      self.ack_interrupt(irq);
      self.is_halted = false;
      self.interrupt_master_enable = false;
    }
    Ok(())
  }
}
