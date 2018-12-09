use crate::asm::operation::{Address, Condition, Operation, Reg};
use crate::util::bits;

fn opcode_reg(opcode: u8) -> Address {
  match opcode & 0b0000_0111 {
    0b000 => Address::Register(Reg::B),
    0b001 => Address::Register(Reg::C),
    0b010 => Address::Register(Reg::D),
    0b011 => Address::Register(Reg::E),
    0b100 => Address::Register(Reg::H),
    0b101 => Address::Register(Reg::L),
    0b110 => Address::Indirect(Reg::HL),
    0b111 => Address::Register(Reg::A),
    _ => unreachable!("Bad LD source"),
  }
}

fn ld_dest(opcode: u8) -> Address {
  match opcode {
    0x40...0x47 => Address::Register(Reg::B),
    0x48...0x4f => Address::Register(Reg::C),
    0x50...0x57 => Address::Register(Reg::D),
    0x58...0x5f => Address::Register(Reg::E),
    0x60...0x67 => Address::Register(Reg::H),
    0x68...0x6f => Address::Register(Reg::L),
    // NOTE: 0x76 is HALT.
    0x70...0x75 | 0x77 => Address::Indirect(Reg::HL),
    0x78...0x7f => Address::Register(Reg::A),
    _ => unreachable!("Bad LD destination"),
  }
}

fn arithmetic16_source(opcode: u8) -> Address {
  match (opcode & 0b0011_0000) >> 4 {
    0b00 => Address::Register(Reg::BC),
    0b01 => Address::Register(Reg::DE),
    0b10 => Address::Register(Reg::HL),
    0b11 => Address::Register(Reg::SP),
    _ => unreachable!("Bad arithmetic 16 source"),
  }
}

fn push_pop_source(opcode: u8) -> Address {
  match (opcode & 0b0011_0000) >> 4 {
    0b00 => Address::Register(Reg::BC),
    0b01 => Address::Register(Reg::DE),
    0b10 => Address::Register(Reg::HL),
    0b11 => Address::Register(Reg::AF),
    _ => unreachable!("Bad arithmetic 16 source"),
  }
}

fn decode_condition(c: u8) -> Condition {
  match c {
    0b00 => Condition::NonZero,
    0b01 => Condition::Zero,
    0b10 => Condition::NonCarry,
    0b11 => Condition::Carry,
    _ => unreachable!("Bad jump condition"),
  }
}

pub fn decode<F>(opcode: u8, read_operand8: F) -> Result<(u16, Operation<Address>), String>
where
  F: Fn(u16) -> Result<u8, String>,
{
  // TODO refactor this
  let read_operand16 = |o| -> Result<u16, String> {
    let op1 = u16::from(read_operand8(o)?);
    let op2 = u16::from(read_operand8(o + 1)?);
    Ok((op2 << 8) | op1)
  };

  match opcode {
    0x00 => Ok((1, Operation::Nop)),
    0x10 => Ok((1, Operation::Stop)),
    // LD <reg16>, <imm16>
    0x01 | 0x11 | 0x21 | 0x31 => {
      let destination = arithmetic16_source(opcode);
      let source = Address::Data16(read_operand16(1)?);
      Ok((3, Operation::Load16(destination, source)))
    }
    // LD (BC), A
    0x02 => {
      let destination = Address::Indirect(Reg::BC);
      let source = Address::Register(Reg::A);
      Ok((1, Operation::Load8(destination, source)))
    }
    // INC <reg16>
    0x03 | 0x13 | 0x23 | 0x33 => {
      let destination = arithmetic16_source(opcode);
      Ok((1, Operation::Increment16(destination)))
    }
    // INC <reg8>
    0x04 | 0x0c | 0x14 | 0x1c | 0x24 | 0x2c | 0x34 | 0x3c => {
      let destination = opcode_reg(bits(5, 3, opcode));
      Ok((1, Operation::Increment(destination)))
    }
    // LD <reg>, <data8>
    0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x36 | 0x3e => {
      let destination = opcode_reg(bits(5, 3, opcode));
      let source = Address::Data8(read_operand8(1)?);
      Ok((2, Operation::Load8(destination, source)))
    }
    // RLCA
    0x07 => Ok((1, Operation::RotateLeftA(true))),
    // DEC <reg>
    0x05 | 0x0d | 0x15 | 0x1d | 0x25 | 0x2d | 0x35 | 0x3d => {
      let destination = opcode_reg(bits(5, 3, opcode));
      Ok((1, Operation::Decrement(destination)))
    }
    // LD <addr16>, SP
    0x08 => {
      let source = Address::Register(Reg::SP);
      let destination = Address::Immediate(read_operand16(1)?);
      Ok((3, Operation::Load16(destination, source)))
    }
    // ADD HL, <reg16>
    0x09 | 0x19 | 0x29 | 0x39 => {
      let source = arithmetic16_source(opcode);
      Ok((1, Operation::Add16(source)))
    }
    // DEC <reg16>
    0x0b | 0x1b | 0x2b | 0x3b => {
      let destination = arithmetic16_source(opcode);
      Ok((1, Operation::Decrement16(destination)))
    }
    // LD A, (BC)
    0x0a => {
      let destination = Address::Register(Reg::A);
      let source = Address::Indirect(Reg::BC);
      Ok((1, Operation::Load8(destination, source)))
    }
    // RRCA
    0x0f => Ok((1, Operation::RotateRightA(true))),
    // LD (DE), A
    0x12 => {
      let destination = Address::Indirect(Reg::DE);
      let source = Address::Register(Reg::A);
      Ok((1, Operation::Load8(destination, source)))
    }
    // RLA
    0x17 => Ok((1, Operation::RotateLeftA(false))),
    // JR <rel8>
    0x18 => {
      let condition = Condition::Unconditional;
      let source = Address::Relative(read_operand8(1)?);
      Ok((2, Operation::Jump(condition, source)))
    }
    // LD A, (DE)
    0x1a => {
      let destination = Address::Register(Reg::A);
      let source = Address::Indirect(Reg::DE);
      Ok((1, Operation::Load8(destination, source)))
    }
    // RRA
    0x1f => Ok((1, Operation::RotateRightA(false))),
    // JR <cond>, <rel8>
    0x20 | 0x28 | 0x30 | 0x38 => {
      let condition = decode_condition(bits(4, 3, opcode));
      let source = Address::Relative(read_operand8(1)?);
      Ok((2, Operation::Jump(condition, source)))
    }
    // LDI (HL), A
    0x22 => {
      let source = Address::Register(Reg::A);
      let destination = Address::Indirect(Reg::HL);
      Ok((1, Operation::LoadIncrement(destination, source)))
    }
    // DAA
    0x27 => Ok((1, Operation::DecimalAdjustAccumulator)),
    // LDI A, (HL)
    0x2a => {
      let destination = Address::Register(Reg::A);
      let source = Address::Indirect(Reg::HL);
      Ok((1, Operation::LoadIncrement(destination, source)))
    }
    // CPL
    0x2f => Ok((1, Operation::Complement)),
    // LDD (HL), A
    0x32 => {
      let destination = Address::Indirect(Reg::HL);
      let source = Address::Register(Reg::A);
      Ok((1, Operation::LoadDecrement(destination, source)))
    }
    // SCF
    0x37 => Ok((1, Operation::SetCarry)),
    // LDD A, (HL)
    0x3a => {
      let destination = Address::Register(Reg::A);
      let source = Address::Indirect(Reg::HL);
      Ok((1, Operation::LoadDecrement(destination, source)))
    }
    // CCF
    0x3f => Ok((1, Operation::ComplementCarry)),
    // LD <reg>, <reg>
    0x40...0x75 | 0x77...0x7f => {
      // NOTE: Gap for 0x76
      let destination = ld_dest(opcode);
      let source = opcode_reg(bits(3, 0, opcode));
      Ok((1, Operation::Load8(destination, source)))
    }
    0x76 => Ok((1, Operation::Halt)),
    // ADD <reg>
    0x80...0x87 => {
      let source = opcode_reg(opcode);
      Ok((1, Operation::Add(source)))
    }
    // ADC <reg>
    0x88...0x8f => {
      let source = opcode_reg(opcode);
      Ok((1, Operation::AddCarry(source)))
    }
    // SUB <reg>
    0x90...0x97 => {
      let source = opcode_reg(opcode);
      Ok((1, Operation::Sub(source)))
    }
    // SBC <reg>
    0x98...0x9f => {
      let source = opcode_reg(opcode);
      Ok((1, Operation::SubCarry(source)))
    }
    // AND <reg>
    0xa0...0xa7 => {
      let source = opcode_reg(opcode);
      Ok((1, Operation::And(source)))
    }
    // XOR <reg>
    0xa8...0xaf => {
      let source = opcode_reg(opcode);
      Ok((1, Operation::Xor(source)))
    }
    // OR <reg>
    0xb0...0xb7 => {
      let source = opcode_reg(opcode);
      Ok((1, Operation::Or(source)))
    }
    // CP <reg>
    0xb8...0xbf => {
      let source = opcode_reg(opcode);
      Ok((1, Operation::Compare(source)))
    }
    // RET <cond>
    0xc0 | 0xc8 | 0xd0 | 0xd8 => {
      let condition = decode_condition(bits(4, 3, opcode));
      Ok((1, Operation::Return(condition)))
    }
    // POP <reg16>
    0xc1 | 0xd1 | 0xe1 | 0xf1 => {
      let destination = push_pop_source(opcode);
      Ok((1, Operation::Pop(destination)))
    }
    // JP <cond>, <addr16>
    0xc2 | 0xca | 0xd2 | 0xda => {
      let condition = decode_condition(bits(4, 3, opcode));
      let source = Address::Immediate(read_operand16(1)?);
      Ok((3, Operation::Jump(condition, source)))
    }
    // JP <addr16>
    0xc3 => {
      let condition = Condition::Unconditional;
      let source = Address::Immediate(read_operand16(1)?);
      Ok((3, Operation::Jump(condition, source)))
    }
    // CALL NZ,<addr16>
    0xc4 => {
      let condition = Condition::NonZero;
      Ok((3, Operation::Call(condition, read_operand16(1)?)))
    }
    // PUSH <reg16>
    0xc5 | 0xd5 | 0xe5 | 0xf5 => {
      let source = push_pop_source(opcode);
      Ok((1, Operation::Push(source)))
    }
    // ADD <data8>
    0xc6 => {
      let source = Address::Data8(read_operand8(1)?);
      Ok((2, Operation::Add(source)))
    }
    // RST <fixed>
    0xc7 | 0xcf | 0xd7 | 0xdf | 0xe7 | 0xef | 0xf7 | 0xff => {
      // See Z80 Manual, Page 292.
      Ok((1, Operation::Reset(u16::from(opcode & 0b0011_1000))))
    }
    // RET
    0xc9 => {
      let condition = Condition::Unconditional;
      Ok((1, Operation::Return(condition)))
    }
    // CB Prefix
    0xcb => {
      let op = decode_prefixed(read_operand8(1)?)?;
      Ok((2, op))
    }
    // CALL Z,<addr16>
    0xcc => {
      let condition = Condition::Zero;
      Ok((3, Operation::Call(condition, read_operand16(1)?)))
    }
    // CALL <addr16>
    0xcd => {
      let condition = Condition::Unconditional;
      Ok((3, Operation::Call(condition, read_operand16(1)?)))
    }
    // ADC <data8>
    0xce => {
      let source = Address::Data8(read_operand8(1)?);
      Ok((2, Operation::AddCarry(source)))
    }
    // CALL NC,<addr16>
    0xd4 => {
      let condition = Condition::NonCarry;
      Ok((3, Operation::Call(condition, read_operand16(1)?)))
    }
    // SUB <data8>
    0xd6 => {
      let source = Address::Data8(read_operand8(1)?);
      Ok((2, Operation::Sub(source)))
    }
    // RETI
    0xd9 => Ok((1, Operation::ReturnFromInterrupt)),
    // CALL C,<addr16>
    0xdc => {
      let condition = Condition::Carry;
      Ok((3, Operation::Call(condition, read_operand16(1)?)))
    }
    // SBC <data8>
    0xde => {
      let source = Address::Data8(read_operand8(1)?);
      Ok((2, Operation::SubCarry(source)))
    }
    // LDH <addr8>, A
    0xe0 => {
      let destination = Address::Extended(read_operand8(1)?);
      let source = Address::Register(Reg::A);
      Ok((2, Operation::Load8(destination, source)))
    }
    // LDH (C), A
    0xe2 => {
      let destination = Address::ExtendedIndirect(Reg::C);
      let source = Address::Register(Reg::A);
      Ok((1, Operation::Load8(destination, source)))
    }
    // AND <data8>
    0xe6 => {
      let source = Address::Data8(read_operand8(1)?);
      Ok((2, Operation::And(source)))
    }
    // ADD SP, <rel8>
    0xe8 => {
      let destination = Address::Register(Reg::SP);
      let source = Address::StackRelative(read_operand8(1)?);
      Ok((2, Operation::AddStack(destination, source)))
    }
    // JP (HL)
    0xe9 => {
      let condition = Condition::Unconditional;
      let source = Address::Register(Reg::HL);
      Ok((1, Operation::Jump(condition, source)))
    }
    // LD <addr16>, A
    0xea => {
      let destination = Address::Immediate(read_operand16(1)?);
      let source = Address::Register(Reg::A);
      Ok((3, Operation::Load8(destination, source)))
    }
    // XOR <data8>
    0xee => {
      let source = Address::Data8(read_operand8(1)?);
      Ok((2, Operation::Xor(source)))
    }
    // LDH A, <addr8>
    0xf0 => {
      let destination = Address::Register(Reg::A);
      let source = Address::Extended(read_operand8(1)?);
      Ok((2, Operation::Load8(destination, source)))
    }
    // LDH A, (C)
    0xf2 => {
      let destination = Address::Register(Reg::A);
      let source = Address::ExtendedIndirect(Reg::C);
      Ok((1, Operation::Load8(destination, source)))
    }
    // OR <data8>
    0xf6 => {
      let source = Address::Data8(read_operand8(1)?);
      Ok((2, Operation::Or(source)))
    }
    // CP <data8>
    0xfe => {
      let source = Address::Data8(read_operand8(1)?);
      Ok((2, Operation::Compare(source)))
    }
    // DI
    0xf3 => Ok((1, Operation::DisableInterrupts)),
    // LD HL, SP + <rel8>
    0xf8 => {
      let destination = Address::Register(Reg::HL);
      let source = Address::StackRelative(read_operand8(1)?);
      Ok((2, Operation::AddStack(destination, source)))
    }
    // LD SP, HL
    0xf9 => {
      let source = Address::Register(Reg::HL);
      let destination = Address::Register(Reg::SP);
      Ok((1, Operation::Load16(destination, source)))
    }
    // LD A, <addr16>
    0xfa => {
      let source = Address::Immediate(read_operand16(1)?);
      let destination = Address::Register(Reg::A);
      Ok((3, Operation::Load8(destination, source)))
    }
    // EI
    0xfb => Ok((1, Operation::EnableInterrupts)),
    _ => Err(format!("Unrecognised opcode: {:#04x}", opcode)),
  }
}

pub fn decode_prefixed(opcode: u8) -> Result<Operation<Address>, String> {
  match opcode {
    // RLC <reg8>
    0x00...0x07 => {
      let destination = opcode_reg(opcode);
      Ok(Operation::RotateLeft(true, destination))
    }
    // RRC <reg8>
    0x08...0x0f => {
      let destination = opcode_reg(opcode);
      Ok(Operation::RotateRight(true, destination))
    }
    // RL <reg8>
    0x10...0x17 => {
      let destination = opcode_reg(opcode);
      Ok(Operation::RotateLeft(false, destination))
    }
    // RR <reg8>
    0x18...0x1f => {
      let destination = opcode_reg(opcode);
      Ok(Operation::RotateRight(false, destination))
    }
    // SLA <reg8>
    0x20...0x27 => {
      let destination = opcode_reg(opcode);
      Ok(Operation::ShiftLeft(destination))
    }
    // SRA <reg8>
    0x28...0x2f => {
      let destination = opcode_reg(opcode);
      Ok(Operation::ShiftRight(destination))
    }
    // SWAP <reg8>
    0x30...0x37 => {
      let destination = opcode_reg(opcode);
      Ok(Operation::Swap(destination))
    }
    // SRL <reg8>
    0x38...0x3f => {
      let destination = opcode_reg(opcode);
      Ok(Operation::ShiftRightLogical(destination))
    }
    // BIT b,<reg8>
    0x40...0x7f => {
      let source = opcode_reg(opcode);
      let b = bits(5, 3, opcode);
      Ok(Operation::Bit(b, source))
    }
    // RES b,<reg8>
    0x80...0xbf => {
      let destination = opcode_reg(opcode);
      Ok(Operation::ResetBit(bits(5, 3, opcode), destination))
    }
    // SET b,<reg8>
    0xc0...0xff => {
      let destination = opcode_reg(opcode);
      Ok(Operation::SetBit(bits(5, 3, opcode), destination))
    }
    _ => Err(format!("Unrecognised prefixed opcode: {:#04x}", opcode)),
  }
}
