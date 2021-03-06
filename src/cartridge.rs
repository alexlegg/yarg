use crate::cpu::Interrupt;
use crate::cpu::TrapHandler;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Cartridge {
  rom: Vec<u8>,
  ram: Vec<u8>,
  rom_bank: usize,
}

impl Cartridge {
  pub fn new(rom: Vec<u8>) -> Cartridge {
    Cartridge {
      rom,
      ram: vec![0; 0x2000],
      rom_bank: 1,
    }
  }
}

impl TrapHandler for Cartridge {
  fn read(&self, addr: u16) -> Result<u8, String> {
    if addr <= 0x3fff {
      Ok(self.rom[addr as usize])
    } else if addr <= 0x7fff {
      let banked_addr = (addr as usize - 0x4000) + (self.rom_bank * 0x4000);
      Ok(self.rom[banked_addr as usize])
    } else if addr >= 0xa000 && addr <= 0xbfff {
      Ok(self.ram[(addr - 0xa000) as usize])
    } else {
      Err(format!("Bad read from Cartridge {:x}", addr))
    }
  }

  fn write(&mut self, addr: u16, val: u8) -> Result<(), String> {
    if addr <= 0x1fff {
      // Ignore ram enable/disable for now
      Ok(())
    } else if addr >= 0x2000 && addr <= 0x3fff {
      let bank = (val & 0x1f) as usize;
      if bank == 0 {
        self.rom_bank = 1;
      } else {
        self.rom_bank = (val & 0x1f) as usize;
      }
      Ok(())
    } else if addr >= 0xa000 && addr <= 0xbfff {
      self.ram[(addr - 0xa000) as usize] = val;
      Ok(())
    } else {
      Err(format!("Bad write to Cartridge {:x}", addr))
    }
  }

  fn tick(&mut self, _: u16) -> Result<Option<Interrupt>, String> {
    Ok(None)
  }
}
