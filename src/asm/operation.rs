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

impl<A> fmt::Display for Operation<A>
where
  A: std::fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Operation::Nop => write!(f, "nop"),
      Operation::Stop => write!(f, "stop"),
      Operation::Halt => write!(f, "halt"),
      Operation::DecimalAdjustAccumulator => write!(f, "daa"),
      Operation::Load8(dest, src) => write!(f, "ld {:?}, {:?}", dest, src),
      Operation::Load16(dest, src) => write!(f, "ld {:?}, {:?}", dest, src),
      Operation::LoadDecrement(dest, src) => write!(f, "ldd {:?}, {:?}", dest, src),
      Operation::LoadIncrement(dest, src) => write!(f, "ldi {:?}, {:?}", dest, src),
      Operation::Increment(dest) => write!(f, "inc {:?}", dest),
      Operation::Increment16(dest) => write!(f, "inc {:?}", dest),
      Operation::Decrement(dest) => write!(f, "dec {:?}", dest),
      Operation::Decrement16(dest) => write!(f, "dec {:?}", dest),
      Operation::SetCarry => write!(f, "scf"),
      Operation::ComplementCarry => write!(f, "ccf"),
      Operation::Jump(Condition::Unconditional, dest) => write!(f, "jp {:?}", dest),
      Operation::Jump(cond, dest) => write!(f, "jp {}, {:?}", cond, dest),
      Operation::Complement => write!(f, "cpl"),
      Operation::Add(src) => write!(f, "add {:?}", src),
      Operation::AddCarry(src) => write!(f, "adc {:?}", src),
      Operation::Add16(src) => write!(f, "add {:?}", src),
      Operation::Sub(src) => write!(f, "sub {:?}", src),
      Operation::SubCarry(src) => write!(f, "sbc {:?}", src),
      Operation::And(src) => write!(f, "and {:?}", src),
      Operation::Xor(src) => write!(f, "xor {:?}", src),
      Operation::Or(src) => write!(f, "or {:?}", src),
      Operation::Pop(src) => write!(f, "pop {:?}", src),
      Operation::Push(src) => write!(f, "push {:?}", src),
      Operation::Compare(src) => write!(f, "cmp {:?}", src),
      Operation::Call(Condition::Unconditional, src) => write!(f, "call {:?}", src),
      Operation::Call(cond, src) => write!(f, "call {},{:?}", cond, src),
      Operation::Return(Condition::Unconditional) => write!(f, "ret"),
      Operation::Return(cond) => write!(f, "ret {}", cond),
      Operation::Reset(dest) => write!(f, "rst {:?}", dest),
      Operation::DisableInterrupts => write!(f, "di"),
      Operation::EnableInterrupts => write!(f, "ei"),
      Operation::ReturnFromInterrupt => write!(f, "reti"),
      Operation::AddStack(dest, src) => write!(f, "add {:?},{:?}", dest, src),
      Operation::RotateLeftA(true) => write!(f, "rla"),
      Operation::RotateLeftA(false) => write!(f, "rlca"),
      Operation::RotateRightA(true) => write!(f, "rra"),
      Operation::RotateRightA(false) => write!(f, "rrca"),
      Operation::RotateLeft(true, dest) => write!(f, "rl {:?}", dest),
      Operation::RotateLeft(false, dest) => write!(f, "rlc {:?}", dest),
      Operation::RotateRight(true, dest) => write!(f, "rr {:?}", dest),
      Operation::RotateRight(false, dest) => write!(f, "rrc {:?}", dest),
      Operation::ShiftLeft(dest) => write!(f, "sla {:?}", dest),
      Operation::ShiftRight(dest) => write!(f, "sra {:?}", dest),
      Operation::ShiftRightLogical(dest) => write!(f, "srl {:?}", dest),
      Operation::Swap(dest) => write!(f, "swap {:?}", dest),
      Operation::Bit(bit, dest) => write!(f, "bit {}, {:?}", bit, dest),
      Operation::ResetBit(bit, dest) => write!(f, "res {}, {:?}", bit, dest),
      Operation::SetBit(bit, dest) => write!(f, "set {}, {:?}", bit, dest),
    }
  }
}

impl fmt::Debug for Address {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Address::Register(r) => write!(f, "{:?}", r),
      Address::Indirect(r) => write!(f, "({:?})", r),
      Address::Data8(d) => write!(f, "{:#04x}", d),
      Address::Data16(d) => write!(f, "{:#06x}", d),
      Address::Immediate(i) => write!(f, "{:#06x}", i),
      Address::Relative(r) | Address::StackRelative(r) => {
        let e = (*r as i8).wrapping_add(2);
        if e > 0 {
          write!(f, "+{:#04x}", e)
        } else {
          write!(f, "-{:#04x}", -e)
        }
      }
      Address::Extended(e) => write!(f, "0xff00 + {:02x}", e),
      Address::ExtendedIndirect(r) => write!(f, "0xff00 + ({:?})", r),
    }
  }
}

impl fmt::Display for Condition {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Condition::Unconditional => write!(f, ""),
      Condition::Carry => write!(f, "c"),
      Condition::NonCarry => write!(f, "nc"),
      Condition::Zero => write!(f, "z"),
      Condition::NonZero => write!(f, "nz"),
    }
  }
}
