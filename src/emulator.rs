use cpu::Cpu;
use cpu::Flag;
use cpu::Reg;
use opcode::Address;
use opcode::Condition;
use opcode::Operation;
use std::collections::VecDeque;
use std::fs;

const DEBUG_STREAM_SIZE: usize = 15;

pub struct Emulator {
    instruction_stream: VecDeque<(u16, Operation)>,
    cpu: Cpu,
    breakpoint: bool,
}

impl Emulator {
    pub fn new() -> Emulator {
        let bootrom = fs::read("roms/bootrom.gb").unwrap();
        let rom = fs::read("roms/drmario.gb").unwrap();
        let mut emu = Emulator {
            instruction_stream: VecDeque::with_capacity(DEBUG_STREAM_SIZE),
            cpu: Cpu::new(bootrom, rom),
            breakpoint: false,
        };
        for _ in 0..DEBUG_STREAM_SIZE {
            emu.instruction_stream.push_back((0, Operation::Nop));
        }
        return emu;
    }

    pub fn emu_loop(&mut self) -> Result<(), String> {
        let ret = cpu_loop(self);
        if ret.is_err() {
            println!("");
            self.cpu.dump_stack();
            self.cpu.dump_regs();
            println!("");
            println!("Dumping instruction stream");
            for (pc, inst) in &self.instruction_stream {
                println!("{:#06x}: {:?}", pc, inst);
            }
        }
        ret
    }

    pub fn screen_buffer(&self) -> &[u8] {
        return &self.cpu.ppu.screen_buffer;
    }

    pub fn should_draw(&self) -> bool {
        return self.cpu.ppu.should_draw();
    }
}

fn cpu_loop(emu: &mut Emulator) -> Result<(), String> {
    if emu.breakpoint {
        return Ok(());
    }

    let cpu = &mut emu.cpu;

    cpu.active_interrupt();

    cpu.tick(1)?;

    if cpu.is_halted {
        return Ok(());
    }

    let pc = cpu.get_reg16(Reg::PC)?;

    /*
  if pc == 0x100 {
    emu.breakpoint = true;
    return Ok(());
  } */

    let (inst_size, inst) = get_inst(&cpu)?;
    cpu.set_pc(pc + inst_size)?;

    /* if pc >= 0xe6 {
    cpu.dump_regs();
    println!("PC: {:#06x}, opcode: {:?}", pc, inst);
  } */

    emu.instruction_stream.pop_front();
    emu.instruction_stream.push_back((pc, inst));

    match inst {
        Operation::Nop => Ok(()),
        Operation::Halt => {
            println!("halt");
            cpu.is_halted = true;
            Ok(())
        }
        Operation::Load8(destination, source) => {
            let src = get_operand_value8(cpu, source)?;
            match destination {
                Address::Register(r) => cpu.set_reg8(r, src)?,
                Address::Immediate(addr) => cpu.write_mem8(addr, src)?,
                Address::Indirect(Reg::C) => {
                    let addr = 0xff00 | (cpu.get_reg8(Reg::C)? as u16);
                    cpu.write_mem8(addr, src)?
                }
                Address::Indirect(r) => {
                    let addr = cpu.get_reg16(r)?;
                    cpu.write_mem8(addr, src)?
                }
                Address::Extended(e) => {
                    let addr: u16 = 0xff00 | (e as u16);
                    cpu.write_mem8(addr, src)?;
                }
                _ => unimplemented!("Load destination"),
            }
            Ok(())
        }
        Operation::Load16(destination, source) => {
            let src = match source {
                Address::Data16(imm) => imm,
                _ => unimplemented!("Load source"),
            };
            match destination {
                Address::Register(r) => cpu.set_reg16(r, src),
                _ => unimplemented!("Load destination"),
            }
        }
        Operation::LoadIncrement(destination, source) => {
            let src = match source {
                Address::Register(r) => cpu.get_reg8(r)?,
                Address::Indirect(Reg::HL) => {
                    let addr = cpu.get_reg16(Reg::HL)?;
                    cpu.read_mem8(addr)?
                }
                _ => unimplemented!("LDI source"),
            };
            match destination {
                Address::Register(r) => cpu.set_reg8(r, src)?,
                Address::Indirect(Reg::HL) => {
                    let addr = cpu.get_reg16(Reg::HL)?;
                    cpu.write_mem8(addr, src)?;
                }
                _ => unimplemented!("LDI dest"),
            }
            let hl = cpu.get_reg16(Reg::HL)?;
            // TODO: Fix overflow
            cpu.set_reg16(Reg::HL, hl + 1)?;
            // TODO: Check if zero flag should actually be set here.
            cpu.set_flag(Flag::Z, hl + 1 == 0);
            Ok(())
        }
        Operation::LoadDecrement(destination, source) => {
            let src = match source {
                Address::Register(r) => cpu.get_reg8(r)?,
                _ => unimplemented!("LDD source"),
            };
            let addr = match destination {
                Address::Indirect(Reg::HL) => cpu.get_reg16(Reg::HL)?,
                _ => unimplemented!("LDD dest"),
            };
            cpu.write_mem8(addr, src)?;
            // TODO: Fix underflow
            cpu.set_reg16(Reg::HL, addr - 1)?;
            // TODO: Check if zero flag should actually be set here.
            cpu.set_flag(Flag::Z, addr - 1 == 0);
            Ok(())
        }
        Operation::Increment(destination) => {
            let val0 = get_operand_value8(cpu, destination)?;
            let (val1, _carry) = val0.overflowing_add(1);
            set_operand_value8(cpu, destination, val1)?;
            cpu.set_flag(Flag::Z, val1 == 0);
            cpu.set_flag(Flag::N, false);
            Ok(())
        }
        Operation::Increment16(destination) => {
            let reg = match destination {
                Address::Register(r) => Ok(r),
                _ => Err("Increment has bad destination".to_string()),
            }?;
            let (v, _) = cpu.get_reg16(reg)?.overflowing_add(1);
            cpu.set_reg16(reg, v)?;
            cpu.set_flag(Flag::Z, v == 0);
            Ok(())
        }
        Operation::Decrement(destination) => {
            let v = match destination {
                Address::Register(r) => {
                    let v = (cpu.get_reg8(r)?).wrapping_sub(1);
                    cpu.set_reg8(r, v)?;
                    Ok(v)
                }
                Address::Indirect(r) => {
                    let addr = cpu.get_reg16(r)?;
                    let v = (cpu.read_mem8(addr)?).wrapping_sub(1);
                    cpu.write_mem8(addr, v)?;
                    Ok(v)
                }
                _ => Err("Decrement has bad destination".to_string()),
            }?;
            cpu.set_flag(Flag::Z, v == 0);
            Ok(())
        }
        Operation::Decrement16(destination) => {
            let reg = match destination {
                Address::Register(r) => Ok(r),
                _ => Err("Increment has bad destination".to_string()),
            }?;
            let (v, _) = cpu.get_reg16(reg)?.overflowing_sub(1);
            cpu.set_reg16(reg, v)?;
            cpu.set_flag(Flag::Z, v == 0);
            Ok(())
        }
        Operation::Jump(condition, source) => {
            let addr: u16 = match source {
                Address::Immediate(imm) => Ok(imm),
                Address::Register(r) => cpu.get_reg16(r),
                Address::Relative(rel) => {
                    let e: i8 = (rel as i8) + 2;
                    Ok(((pc as i16) + (e as i16)) as u16)
                }
                _ => Err("Bad jump source".to_string()),
            }?;
            match condition {
                Condition::Unconditional => cpu.set_pc(addr)?,
                Condition::Zero => {
                    if cpu.get_flag(Flag::Z) {
                        cpu.set_pc(addr)?;
                    }
                }
                Condition::NonZero => {
                    if !cpu.get_flag(Flag::Z) {
                        cpu.set_pc(addr)?;
                    }
                }
                _ => unimplemented!("Unimplemented jump condition {:?}", condition),
            };
            Ok(())
        }
        Operation::Complement => {
            cpu.a = !cpu.a;
            Ok(())
        }
        Operation::Add(source) => {
            let val = get_operand_value8(cpu, source)?;
            cpu.a = cpu.a.wrapping_add(val);
            let z = cpu.a == 0;
            cpu.set_flag(Flag::Z, z);
            cpu.set_flag(Flag::N, false);
            Ok(())
        }
        Operation::Add16(source) => {
            let val = match source {
                Address::Register(r) => cpu.get_reg16(r)?,
                _ => panic!("Bad and16 source"),
            };
            let hl = cpu.get_reg16(Reg::HL)?;
            cpu.set_reg16(Reg::HL, hl + val)?;
            let z = cpu.a == 0;
            cpu.set_flag(Flag::Z, z);
            Ok(())
        }
        Operation::Sub(source) => {
            let val = get_operand_value8(cpu, source)?;
            let (new_val, overflow) = cpu.a.overflowing_sub(val);
            cpu.a = new_val;
            let z = new_val == 0;
            cpu.set_flag(Flag::Z, z);
            cpu.set_flag(Flag::N, true);
            cpu.set_flag(Flag::C, overflow);
            Ok(())
        }
        Operation::And(source) => {
            cpu.a &= match source {
                Address::Register(r) => cpu.get_reg8(r)?,
                Address::Data8(v) => v,
                _ => panic!("Bad and source"),
            };
            let z = cpu.a == 0;
            cpu.set_flag(Flag::Z, z);
            Ok(())
        }
        Operation::Xor(source) => {
            cpu.a ^= match source {
                Address::Register(r) => cpu.get_reg8(r)?,
                _ => panic!("Bad xor source"),
            };
            let z = cpu.a == 0;
            cpu.set_flag(Flag::Z, z);
            Ok(())
        }
        Operation::Or(source) => {
            cpu.a |= match source {
                Address::Register(r) => cpu.get_reg8(r)?,
                _ => panic!("Bad or source"),
            };
            let z = cpu.a == 0;
            cpu.set_flag(Flag::Z, z);
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
            let val = match source {
                Address::Register(r) => cpu.get_reg16(r)?,
                _ => unimplemented!("Load destination"),
            };
            cpu.push_stack(val)
        }
        Operation::Compare(source) => {
            let src = get_operand_value8(cpu, source)?;
            let z = cpu.a == src;
            cpu.set_flag(Flag::Z, z);
            cpu.set_flag(Flag::N, true);
            Ok(())
        }
        Operation::Call(condition, address) => {
            let addr = match address {
                Address::Immediate(imm) => imm,
                _ => unimplemented!("call address"),
            };
            let cond = match condition {
                Condition::Unconditional => true,
                _ => unimplemented!("call condition"),
            };
            if cond {
                let next_pc = cpu.get_reg16(Reg::PC)?;
                cpu.push_stack(next_pc)?;
                //println!("Call from {:#06x} to {:#06x}", pc, addr);
                cpu.set_pc(addr)?;
            }
            Ok(())
        }
        Operation::Return(condition) => {
            let cond = match condition {
                Condition::Unconditional => true,
                Condition::Zero => cpu.get_flag(Flag::Z),
                Condition::NonZero => !cpu.get_flag(Flag::Z),
                _ => unimplemented!("return condition"),
            };
            if cond {
                let addr = cpu.pop_stack()?;
                //println!("Return from {:#06x} to {:#06x}", pc, addr);
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
        Operation::Reset(source) => {
            let addr = match source {
                Address::Fixed(v) => v,
                _ => unimplemented!("reset source"),
            };
            let next_pc = cpu.get_reg16(Reg::PC)?;
            cpu.push_stack(next_pc)?;
            println!("Reset from {:#06x} to {:#06x}", pc, addr);
            cpu.set_pc(addr)
        }
        Operation::RotateLeft(destination) => {
            let val = get_operand_value8(cpu, destination)?;
            let mut val_next = val << 1;
            if cpu.get_flag(Flag::C) {
                val_next |= 1;
            }
            cpu.set_flag(Flag::C, val & (1 << 7) > 0);
            cpu.set_flag(Flag::Z, val_next == 0);
            set_operand_value8(cpu, destination, val_next)
        }
        Operation::Swap(destination) => match destination {
            Address::Register(r) => {
                let val = cpu.get_reg8(r)?;
                cpu.set_reg8(r, ((val & 0x0f) << 4) | ((val & 0xf0) >> 4))
            }
            _ => Err("bad swap source".to_string()),
        },
        Operation::ResetBit(b, destination) => match destination {
            Address::Register(r) => {
                let val = cpu.get_reg8(r)?;
                cpu.set_reg8(r, val & !b)
            }
            _ => Err("Bad reset bit destination".to_string()),
        },
        Operation::Bit(b, source) => {
            let val = get_operand_value8(cpu, source)?;
            cpu.set_flag(Flag::Z, val & (1 << b) == 0);
            Ok(())
        }
        _ => Err("Unrecognised instruction".to_string()),
    }
}

pub fn get_operand_value8(cpu: &Cpu, addr: Address) -> Result<u8, String> {
    match addr {
        Address::Data8(v) => Ok(v),
        Address::Indirect(r) => {
            let addr = cpu.get_reg16(r)?;
            cpu.read_mem8(addr)
        }
        Address::Register(r) => cpu.get_reg8(r),
        Address::Extended(e) => {
            let addr: u16 = 0xff00 | (e as u16);
            cpu.read_mem8(addr)
        }
        Address::Immediate(addr) => cpu.read_mem8(addr),
        _ => Err("Bad get_operand_value8".to_string()),
    }
}

pub fn set_operand_value8(cpu: &mut Cpu, addr: Address, val: u8) -> Result<(), String> {
    match addr {
        Address::Indirect(r) => {
            let addr = cpu.get_reg16(r)?;
            cpu.write_mem8(addr, val)
        }
        Address::Register(r) => cpu.set_reg8(r, val),
        Address::Immediate(addr) => cpu.write_mem8(addr, val),
        _ => Err("Bad set_operand_value8".to_string()),
    }
}

fn get_inst(cpu: &Cpu) -> Result<(u16, ::opcode::Operation), String> {
    let pc = cpu.get_reg16(Reg::PC)?;
    let opcode = cpu.read_mem8(pc)?;
    let get_operand = |operand| cpu.read_mem8(pc + operand);
    ::opcode::decode(opcode, get_operand)
}
