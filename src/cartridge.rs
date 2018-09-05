use cpu::Interrupt;
use cpu::TrapHandler;

pub struct Cartridge {
    rom: Vec<u8>,
    bank: usize,
}

impl Cartridge {
    pub fn new(rom: Vec<u8>) -> Cartridge {
        Cartridge { rom: rom, bank: 1 }
    }
}

impl TrapHandler for Cartridge {
    fn read(&self, addr: u16) -> Result<u8, String> {
        if addr <= 0x3fff {
            Ok(self.rom[addr as usize])
        } else if addr <= 0x7fff {
            let banked_addr = (addr as usize - 0x4000) + (self.bank * 0x4000);
            Ok(self.rom[banked_addr as usize])
        } else {
            Err("Bad read from Cartridge".to_string())
        }
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), String> {
        if addr >= 0x2000 && addr <= 0x3fff {
            let bank = (val & 0x1f) as usize;
            if bank == 0 {
                self.bank = 1;
            } else {
                self.bank = (val & 0x1f) as usize;
            }
            Ok(())
        } else {
            Err("Bad write to Cartridge".to_string())
        }
    }

    fn tick(&mut self, _: u16) -> Result<Option<Interrupt>, String> {
        Ok(None)
    }
}
