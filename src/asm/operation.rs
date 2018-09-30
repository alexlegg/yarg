use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Operation {
  Nop,
  Stop,
  Halt,
  DecimalAdjustAccumulator,
  Load8(Address, Address),
  Load16(Address, Address),
  LoadDecrement(Address, Address),
  LoadIncrement(Address, Address),
  Increment(Address),
  Increment16(Address),
  Decrement(Address),
  Decrement16(Address),
  SetCarry,
  ComplementCarry,
  Jump(Condition, Address),
  Complement,
  Add(Address),
  AddCarry(Address),
  Add16(Address),
  Sub(Address),
  SubCarry(Address),
  And(Address),
  Xor(Address),
  Or(Address),
  Pop(Address),
  Push(Address),
  Compare(Address),
  Call(Condition, u16),
  Return(Condition),
  Reset(u16),
  DisableInterrupts,
  EnableInterrupts,
  ReturnFromInterrupt,
  AddStack(Address, Address),
  RotateLeftA(bool, Address),
  RotateRightA(bool, Address),
  RotateLeft(bool, Address),
  RotateRight(bool, Address),
  ShiftLeft(Address),
  ShiftRight(Address),
  ShiftRightLogical(Address),
  Swap(Address),
  Bit(u8, Address),
  ResetBit(u8, Address),
  SetBit(u8, Address),
}

// TODO rename to Operand
#[derive(Copy, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Address {
  Register(Reg),
  Indirect(Reg),
  Data8(u8),
  Data16(u16),
  Immediate(u16),
  Relative(u8),
  StackRelative(u8),
  Extended(u8),
  ExtendedIndirect(Reg),
}

impl fmt::Debug for Address {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Address::Register(r) => write!(f, "{:?}", r),
      Address::Indirect(r) => write!(f, "({:?})", r),
      Address::Data8(d) => write!(f, "${:#04x}", d),
      Address::Data16(d) => write!(f, "${:#06x}", d),
      Address::Immediate(i) => write!(f, "${:#06x}", i),
      Address::Relative(r) | Address::StackRelative(r) => {
        let e = (*r as i8) + 2;
        if e > 0 {
          write!(f, "+{:#04x}", e)
        } else {
          write!(f, "-{:#04x}", e * -1)
        }
      }
      Address::Extended(e) => write!(f, "$0xff00 + {:02x}", e),
      Address::ExtendedIndirect(r) => write!(f, "$0xff00 + ({:?})", r),
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Condition {
  Unconditional,
  Carry,
  NonCarry,
  Zero,
  NonZero,
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Reg {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
  AF,
  BC,
  DE,
  HL,
  SP,
  PC,
}
