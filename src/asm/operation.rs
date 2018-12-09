use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Operation<A> {
  Nop,
  Stop,
  Halt,
  DecimalAdjustAccumulator,
  Load8(A, A),
  Load16(A, A),
  LoadDecrement(A, A),
  LoadIncrement(A, A),
  Increment(A),
  Increment16(A),
  Decrement(A),
  Decrement16(A),
  SetCarry,
  ComplementCarry,
  Jump(Condition, A),
  Complement,
  Add(A),
  AddCarry(A),
  Add16(A),
  Sub(A),
  SubCarry(A),
  And(A),
  Xor(A),
  Or(A),
  Pop(A),
  Push(A),
  Compare(A),
  Call(Condition, u16),
  Return(Condition),
  Reset(u16),
  DisableInterrupts,
  EnableInterrupts,
  ReturnFromInterrupt,
  AddStack(A, A),
  RotateLeftA(bool),
  RotateRightA(bool),
  RotateLeft(bool, A),
  RotateRight(bool, A),
  ShiftLeft(A),
  ShiftRight(A),
  ShiftRightLogical(A),
  Swap(A),
  Bit(u8, A),
  ResetBit(u8, A),
  SetBit(u8, A),
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
          write!(f, "-{:#04x}", -e)
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
