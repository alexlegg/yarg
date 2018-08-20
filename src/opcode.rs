use cpu::Reg;

#[derive(Copy, Clone, Debug)]
pub enum Operation {
    Nop,
    Stop,
    Halt,

    Load8(Address, Address),
    Load16(Address, Address),
    LoadDecrement(Address, Address),
    LoadIncrement(Address, Address),
    Increment(Address),
    Increment16(Address),
    Decrement(Address),
    Decrement16(Address),
    Jump(Condition, Address),
    Complement,
    Add(Address),
    Add16(Address),
    Sub(Address),
    And(Address),
    Xor(Address),
    Or(Address),
    Pop(Address),
    Push(Address),
    Compare(Address),
    Call(Condition, Address),
    Return(Condition),
    Reset(Address),
    DisableInterrupts,
    EnableInterrupts,
    RotateLeft(Address),
    RotateRight(Address),
    Swap(Address),
    Bit(u8, Address),
    ResetBit(u8, Address),
}

// TODO rename to Operand
#[derive(Copy, Clone, Debug)]
pub enum Address {
    Register(Reg),
    Indirect(Reg),
    Data8(u8),
    Data16(u16),
    Immediate(u16),
    Relative(u8),
    Extended(u8),
    Fixed(u16),
}

#[derive(Copy, Clone, Debug)]
pub enum Condition {
    Unconditional,
    Carry,
    NonCarry,
    Zero,
    NonZero,
}

fn bits(high: u8, low: u8, val: u8) -> u8 {
    (val & ((1 << (high + 1)) - 1)) >> low
}

fn opcode_reg(opcode: u8) -> Address {
    match opcode & 0b00000111 {
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
    match (opcode & 0b00110000) >> 4 {
        0b00 => Address::Register(Reg::BC),
        0b01 => Address::Register(Reg::DE),
        0b10 => Address::Register(Reg::HL),
        0b11 => Address::Register(Reg::SP),
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

pub fn decode<F>(opcode: u8, read_operand8: F) -> Result<(u16, Operation), String>
where
    F: Fn(u16) -> Result<u8, String>,
{
    // TODO refactor this
    let read_operand16 = |o| -> Result<u16, String> {
        let op1 = read_operand8(o)? as u16;
        let op2 = read_operand8(o + 1)? as u16;
        return Ok((op2 << 8) | op1);
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
        // DEC <reg>
        0x05 | 0x0d | 0x15 | 0x1d | 0x25 | 0x2d | 0x35 | 0x3d => {
            let destination = opcode_reg(bits(5, 3, opcode));
            Ok((1, Operation::Decrement(destination)))
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
        // LD (DE), A
        0x12 => {
            let destination = Address::Indirect(Reg::DE);
            let source = Address::Register(Reg::A);
            Ok((1, Operation::Load8(destination, source)))
        }
        // RLA
        // TODO: This needs to be distinct from RL A (a prefixed instruction) for timing reasons.
        0x17 => Ok((1, Operation::RotateLeft(Address::Register(Reg::A)))),
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
        // LD <reg>, <reg>
        0x40...0x75 | 0x77...0x7f => {
            // NOTE: Gap for 0x76
            let destination = ld_dest(opcode);
            let source = opcode_reg(bits(3, 0, opcode));
            Ok((1, Operation::Load8(destination, source)))
        }
        0x76 => Ok((1, Operation::Halt)),
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
        // RET
        0xc9 => {
            let condition = Condition::Unconditional;
            Ok((1, Operation::Return(condition)))
        }
        // CALL <addr16>
        0xcd => {
            let condition = Condition::Unconditional;
            let source = Address::Immediate(read_operand16(1)?);
            Ok((3, Operation::Call(condition, source)))
        }
        // ADD <reg>
        0x80...0x87 => {
            let source = opcode_reg(opcode);
            Ok((1, Operation::Add(source)))
        }
        // SUB <reg>
        0x90...0x97 => {
            let source = opcode_reg(opcode);
            Ok((1, Operation::Sub(source)))
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
        0xc1 | 0xd1 | 0xe1 | 0xf1 => {
            let destination = arithmetic16_source(opcode);
            Ok((1, Operation::Pop(destination)))
        }
        0xc5 | 0xd5 | 0xe5 | 0xf5 => {
            let source = arithmetic16_source(opcode);
            Ok((1, Operation::Push(source)))
        }
        // CB Prefix
        0xcb => {
            let op = decode_prefixed(read_operand8(1)?)?;
            Ok((2, op))
        }
        // LDH <addr8>, A
        0xe0 => {
            let destination = Address::Extended(read_operand8(1)?);
            let source = Address::Register(Reg::A);
            Ok((2, Operation::Load8(destination, source)))
        }
        // LDH (C), A
        0xe2 => {
            let destination = Address::Indirect(Reg::C);
            let source = Address::Register(Reg::A);
            Ok((1, Operation::Load8(destination, source)))
        }
        // AND <data8>
        0xe6 => {
            let source = Address::Data8(read_operand8(1)?);
            Ok((2, Operation::And(source)))
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
        0xef => {
            let source = Address::Fixed(0x28);
            Ok((1, Operation::Reset(source)))
        }
        // LDH A, <addr8>
        0xf0 => {
            let destination = Address::Register(Reg::A);
            let source = Address::Extended(read_operand8(1)?);
            Ok((2, Operation::Load8(destination, source)))
        }
        // CP <data8>
        0xfe => {
            let source = Address::Data8(read_operand8(1)?);
            Ok((2, Operation::Compare(source)))
        }
        // DI
        0xf3 => Ok((1, Operation::DisableInterrupts)),
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

pub fn decode_prefixed(opcode: u8) -> Result<Operation, String> {
    match opcode {
        // RL <reg8>
        0x10...0x17 => {
            let destination = opcode_reg(opcode);
            Ok(Operation::RotateLeft(destination))
        }
        // RR <reg8>
        0x18...0x1f => {
            let destination = opcode_reg(opcode);
            Ok(Operation::RotateRight(destination))
        }
        // SWAP <reg8>
        0x30...0x37 => {
            let destination = opcode_reg(opcode);
            Ok(Operation::Swap(destination))
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
            Ok(Operation::ResetBit(0, destination))
        }
        _ => Err(format!("Unrecognised prefixed opcode: {:#04x}", opcode)),
    }
}
