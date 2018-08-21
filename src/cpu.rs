use ppu::Ppu;

#[derive(Copy, Clone, Debug)]
pub enum Flag {
    Z,
    N,
    /* H, */ C,
}

#[derive(Copy, Clone, Debug)]
pub enum Interrupt {
    Joypad,
    LCDStat,
    Timer,
    Serial,
    VBlank,
}

fn interrupt_handler_addr(irq: Interrupt) -> u16 {
    match irq {
        Interrupt::Joypad => 0x60,
        Interrupt::LCDStat => 0x58,
        Interrupt::Timer => 0x50,
        Interrupt::Serial => 0x48,
        Interrupt::VBlank => 0x40,
    }
}

pub struct Cpu {
    pub a: u8,
    pub f: u8,
    b: u8,
    pub c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    pc: u16,
    sp: u16,

    // Work RAM: 8 banks of 4kb.
    // Bank 0: 0xc000 - 0xcfff
    // Switchable bank 1-7: 0xd000 - 0xdfff
    wram: [[u8; 0x1000]; 8],
    wram_bank: usize,

    // High RAM: 0xff80 - 0xfffe
    hram: [u8; 0x7f],

    bootrom: Vec<u8>,
    bootrom_enabled: bool,

    rom: Vec<u8>,

    pub interrupt_master_enable: bool,
    interrupt_enable: u8,
    interrupt_flag: u8,

    // TODO refactor
    pub ppu: Ppu,

    // TODO refactor
    pub is_halted: bool,
}

#[derive(Copy, Clone, Debug)]
pub enum Reg {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    BC,
    DE,
    HL,
    SP,
    PC,
}

pub trait TrapHandler {
    fn write(&mut self, addr: u16, val: u8) -> Result<(), String>;
    fn read(&self, addr: u16) -> Result<u8, String>;

    // TODO: Move this to a separate trait
    fn tick(&mut self, cycles: u16) -> Result<Option<Interrupt>, String>;
}

impl Cpu {
    pub fn new(bootrom: Vec<u8>, rom: Vec<u8>) -> Cpu {
        Cpu {
            a: 0x01,
            f: 0xb0,
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xd8,
            h: 0x01,
            l: 0x4d,
            pc: 0x0000,
            sp: 0xfffe,

            wram: [[0; 0x1000]; 8],
            wram_bank: 1,
            hram: [0; 0x7f],
            bootrom: bootrom,
            bootrom_enabled: true,
            rom: rom,
            interrupt_master_enable: false,
            interrupt_enable: 0,
            interrupt_flag: 0,
            ppu: Ppu::new(),

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
        println!("");
    }

    pub fn set_pc(&mut self, addr: u16) -> Result<(), String> {
        self.pc = addr;
        Ok(())
    }

    pub fn get_flag(&self, flag: Flag) -> bool {
        let mask = match flag {
            Flag::Z => 1 << 7,
            Flag::N => 1 << 6,
            //Flag::H => 1 << 5,
            Flag::C => 1 << 4,
        };
        return (self.f & mask) > 0;
    }

    pub fn set_flag(&mut self, flag: Flag, val: bool) {
        let mask = match flag {
            Flag::Z => 1 << 7,
            Flag::N => 1 << 6,
            //Flag::H => 1 << 5,
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
            //Reg::F => Ok(self.f),
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
            //Reg::F => self.f = val,
            _ => return Err("Bad set_reg8".to_string()),
        }
        return Ok(());
    }

    pub fn get_reg16(&self, reg: Reg) -> Result<u16, String> {
        match reg {
            Reg::BC => Ok(((self.b as u16) << 8) | (self.c as u16)),
            Reg::DE => Ok(((self.d as u16) << 8) | (self.e as u16)),
            Reg::HL => Ok(((self.h as u16) << 8) | (self.l as u16)),
            Reg::PC => Ok(self.pc),
            Reg::SP => Ok(self.sp),
            _ => Err("Bad get_reg16 register".to_string()),
        }
    }

    pub fn set_reg16(&mut self, reg: Reg, val: u16) -> Result<(), String> {
        let high = (val >> 8) as u8;
        let low = (val & 0xff) as u8;
        match reg {
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
        //println!("read_mem8 {:#06x}", addr);
        if self.bootrom_enabled && addr <= 0xff {
            return Ok(self.bootrom[addr as usize]);
        } else if addr <= 0x7fff {
            return Ok(self.rom[addr as usize]);
        } else if addr >= 0xc000 && addr <= 0xcfff {
            return Ok(self.wram[0][(addr - 0xc000) as usize]);
        } else if addr >= 0xd000 && addr <= 0xdfff {
            return Ok(self.wram[self.wram_bank][(addr - 0xd000) as usize]);
        } else if addr >= 0xff40 && addr <= 0xff4b {
            return self.ppu.read(addr);
        } else if addr == 0xff00 {
            // Joypad IO.
            return Ok(0);
        } else if addr >= 0xff00 && addr <= 0xff7f {
            println!("Read IO register {:#06x} as 0", addr);
            // Some IO register
            return Ok(0);
        } else if addr >= 0xff80 && addr <= 0xfffe {
            return Ok(self.hram[(addr - 0xff80) as usize]);
        } else if addr == 0xff0f {
            // IF - Interrupt Flag
            return Ok(self.interrupt_flag);
        } else if addr == 0xffff {
            // IE - Interrupt Enable
            return Ok(self.interrupt_enable);
        } else {
            return Err(format!("Bad read_mem8 addr: {:#06x}", addr));
        }
    }

    pub fn write_mem8(&mut self, addr: u16, val: u8) -> Result<(), String> {
        //println!("write_mem8 {:#06x} to {:#04x}", addr, val);
        if addr <= 0x1fff {
            // RAM enable/disable.
            // Ignore for now.
            return Ok(());
        } else if addr >= 0x2000 && addr <= 0x3fff {
            // ROM bank select.
            if val != 0x00 && val != 0x01 {
                unimplemented!("ROM bank other than 1 selected")
            }
            return Ok(());
        } else if addr >= 0x8000 && addr <= 0x9fff {
            return self.ppu.write(addr, val);
        } else if addr >= 0xc000 && addr <= 0xcfff {
            self.wram[0][(addr - 0xc000) as usize] = val;
            return Ok(());
        } else if addr >= 0xd000 && addr <= 0xdfff {
            self.wram[self.wram_bank][(addr - 0xd000) as usize] = val;
            return Ok(());
        } else if addr >= 0xfe00 && addr <= 0xfe9f {
            // Sprite attribute table, ignored for now.
            return Ok(());
        } else if addr >= 0xfea0 && addr <= 0xfeff {
            // Not usable - Ignore.
            return Ok(());
        } else if addr >= 0xff80 && addr <= 0xfffe {
            self.hram[(addr - 0xff80) as usize] = val;
            return Ok(());
        } else if addr >= 0xff40 && addr <= 0xff45 {
            return self.ppu.write(addr, val);
        } else if addr == 0xff46 {
            // Handle DMA here
            self.handle_dma_transfer((val as u16) << 8)
        } else if addr >= 0xff47 && addr <= 0xff4b {
            return self.ppu.write(addr, val);
        } else if addr == 0xff50 {
            if val & 0b1 == 0b1 {
                self.bootrom_enabled = false;
            }
            return Ok(());
        } else if addr == 0xff00 {
            // Joypad IO. Ignored.
            return Ok(());
        } else if addr >= 0xff01 && addr <= 0xff02 {
            // Serial IO. Ignored.
            return Ok(());
        } else if addr >= 0xff04 && addr <= 0xff07 {
            // Timer IO. Ignored
            println!("Write to Timer register {:#06x}", addr);
            return Ok(());
        } else if addr >= 0xff10 && addr <= 0xff26 {
            // Sound IO. Ignored
            return Ok(());
        } else if addr == 0xff0f {
            println!("Write IF to {:?}", val);
            // IF - Interrupt Flag
            self.interrupt_flag = val;
            return Ok(());
        } else if addr == 0xffff {
            println!("Write IE to {:?}", val);
            // IE - Interrupt Enable
            self.interrupt_enable = val;
            return Ok(());
        } else {
            //println!("Write to unknown IO address {:#06x}", addr);
            return Ok(());
        }
    }

    pub fn write_mem16(&mut self, addr: u16, val: u16) -> Result<(), String> {
        self.write_mem8(addr, val as u8)?;
        self.write_mem8(addr + 1, (val >> 8) as u8)
    }

    pub fn read_mem16(&self, addr: u16) -> Result<u16, String> {
        let v0 = self.read_mem8(addr)? as u16;
        let v1 = self.read_mem8(addr + 1)? as u16;
        return Ok(v1 << 8 | v0);
    }

    pub fn push_stack(&mut self, val: u16) -> Result<(), String> {
        self.sp -= 2;
        let sp = self.sp;
        self.write_mem16(sp, val)?;
        return Ok(());
    }

    pub fn pop_stack(&mut self) -> Result<u16, String> {
        let sp = self.sp;
        let val = self.read_mem16(sp)?;
        self.sp += 2;
        return Ok(val);
    }

    fn handle_dma_transfer(&mut self, addr : u16) -> Result<(), String> {
        for off in 0..0x9f {
            let val = self.read_mem8(addr + off)?;
            self.write_mem8(0xfe00 + off, val)?;
        }
        Ok(())
    }

    pub fn active_interrupt(&self) -> Option<Interrupt> {
        if !self.interrupt_master_enable {
            return None;
        } else if (1 << 0) & self.interrupt_enable & self.interrupt_flag > 0 {
            return Some(Interrupt::VBlank);
        } else if (1 << 1) & self.interrupt_enable & self.interrupt_flag > 0 {
            return Some(Interrupt::Serial);
        } else if (1 << 2) & self.interrupt_enable & self.interrupt_flag > 0 {
            return Some(Interrupt::Timer);
        } else if (1 << 3) & self.interrupt_enable & self.interrupt_flag > 0 {
            return Some(Interrupt::LCDStat);
        } else if (1 << 4) & self.interrupt_enable & self.interrupt_flag > 0 {
            return Some(Interrupt::Joypad);
        }
        return None;
    }

    fn ack_interrupt(&mut self, irq : Interrupt) {
        match irq {
            Interrupt::Joypad => self.interrupt_flag &= !(1 << 4),
            Interrupt::LCDStat => self.interrupt_flag &= !(1 << 3),
            Interrupt::Timer => self.interrupt_flag &= !(1 << 2),
            Interrupt::Serial => self.interrupt_flag &= !(1 << 1),
            Interrupt::VBlank => self.interrupt_flag &= !(1 << 0),
        }
    }

    pub fn tick(&mut self, cycles: u16) -> Result<(), String> {
        let irq = self.ppu.tick(cycles)?;
        match irq {
            Some(Interrupt::VBlank) => self.interrupt_flag |= 1 << 0,
            Some(Interrupt::LCDStat) => self.interrupt_flag |= 1 << 4,
            _ => (),
        };

        if let Some(irq) = self.active_interrupt() {
            println!("IRQ fired: {:?}", irq);
            let pc = self.pc;
            self.push_stack(pc)?;
            self.pc = interrupt_handler_addr(irq);
            self.ack_interrupt(irq);
            self.is_halted = false;
            self.interrupt_master_enable = false;
        }
        Ok(())
    }
}
