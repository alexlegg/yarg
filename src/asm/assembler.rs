use crate::asm::cartridge::Header;
use crate::asm::operation::{Address, Condition, Operation, Reg};
use crate::asm::parser::{LabelOrAddress, Statement};
use std::collections::HashMap;

type AddrEncodingFn = Fn(usize, &mut Vec<u8>) -> Result<(), String>;

struct Assembler {
  data: Vec<u8>,
  cur: usize,
  labels: HashMap<String, usize>,
  unresolved: HashMap<String, Vec<Box<AddrEncodingFn>>>,
}

pub fn assemble(statements: Vec<Statement>) -> Result<Vec<u8>, String> {
  Assembler::new().assemble(statements)?.encode()
}

impl Assembler {
  fn new() -> Assembler {
    Assembler {
      data: vec![0; 0x7fff],
      cur: 0x150,
      labels: HashMap::new(),
      unresolved: HashMap::new(),
    }
  }

  fn assemble(mut self, statements: Vec<Statement>) -> Result<Assembler, String> {
    for statement in statements {
      match statement {
        Statement::Instruction(op) => {
          self.encode_operation(op)?;
        }
        Statement::Label(s) => {
          if let Some(unresolved) = self.unresolved.get_mut(&s) {
            for encode_fn in unresolved.drain(..) {
              encode_fn(self.cur, &mut self.data)?;
            }
          }
          self.labels.insert(s, self.cur);
        }
        Statement::Directive(_, _) => {
          return Err("Directive!?".to_string());
        }
      }
    }
    if self.unresolved.values().any(|v| !v.is_empty()) {
      return Err("There are unresolved labels".to_string());
    }
    Ok(self)
  }

  fn encode(mut self) -> Result<Vec<u8>, String> {
    let mut header = Header::new();
    header
      .title
      .copy_from_slice("test rom\0\0\0\0\0\0\0\0".as_bytes());
    header.set_header_checksum();
    self.data.splice(0x100..0x14e, header.serialise());
    let mut checksum: u16 = 0;
    for byte in &self.data {
      checksum = checksum.wrapping_add(*byte as u16);
    }
    self.data[0x14e] = (checksum >> 8) as u8;
    self.data[0x14f] = (checksum & 0xff) as u8;
    Ok(self.data)
  }

  fn insert(&mut self, byte: u8) {
    self.data[self.cur] = byte;
    self.cur += 1;
  }

  fn encode_operation(&mut self, operation: Operation<LabelOrAddress>) -> Result<(), String> {
    match operation {
      Operation::Nop => {
        self.insert(0x00);
      }
      Operation::Stop => {
        self.insert(0x10);
      }
      Operation::Halt => {
        self.insert(0x76);
      }
      Operation::DisableInterrupts => {
        self.insert(0xf3);
      }
      Operation::EnableInterrupts => {
        self.insert(0xfb);
      }
      Operation::DecimalAdjustAccumulator => {
        self.insert(0x27);
      }
      Operation::Complement => {
        self.insert(0x2f);
      }
      Operation::ComplementCarry => {
        self.insert(0x3f);
      }
      Operation::Jump(Condition::Unconditional, LabelOrAddress::RelativeLabel(label)) => {
        self.insert(0x18);
        let jump_addr = self.cur;
        self.insert(0 as u8);
        self.encode_label(
          move |addr, data| encode_relative_addr(jump_addr, addr, data),
          label,
        )?;
      }
      Operation::SetCarry => {
        self.insert(0x37);
      }
      Operation::RotateLeftA(true) => {
        self.insert(0x07);
      }
      Operation::RotateLeftA(false) => {
        self.insert(0x17);
      }
      Operation::RotateRightA(true) => {
        self.insert(0x0f);
      }
      Operation::RotateRightA(false) => {
        self.insert(0x1f);
      }
      Operation::Return(Condition::Unconditional) => {
        self.insert(0xc9);
      }
      Operation::Return(condition) => {
        self.insert(0xc0 | encode_condition(condition) << 3);
      }
      Operation::ReturnFromInterrupt => {
        self.insert(0xd9);
      }
      Operation::Load8(
        LabelOrAddress::Resolved(Address::Register(dest)),
        LabelOrAddress::Resolved(Address::Register(source)),
      ) => {
        self.insert(0x40 | (encode_reg(dest) << 3) | (encode_reg(source)));
      }
      Operation::Load8(
        LabelOrAddress::Resolved(Address::Register(dest)),
        LabelOrAddress::Resolved(Address::Data8(val)),
      ) => {
        self.insert(0x06 | (encode_reg(dest) << 3));
        self.insert(val);
      }
      Operation::Decrement(LabelOrAddress::Resolved(Address::Register(r))) => {
        self.insert(0x05 | (encode_reg(r) << 3));
      }
      _ => {
        return Err(format!("Unrecognised operation {:?}", operation));
      }
    }
    Ok(())
  }

  fn encode_label<F: 'static>(&mut self, encoding_fn: F, label: String) -> Result<(), String>
  where
    F: Fn(usize, &mut Vec<u8>) -> Result<(), String>,
  {
    match self.labels.get(&label) {
      Some(addr) => {
        encoding_fn(*addr, &mut self.data)?;
      }
      None => {
        self
          .unresolved
          .entry(label)
          .or_default()
          .push(Box::new(encoding_fn));
      }
    }
    Ok(())
  }
}

fn encode_reg(reg: Reg) -> u8 {
  match reg {
    Reg::B => 0b000,
    Reg::C => 0b001,
    Reg::D => 0b010,
    Reg::E => 0b011,
    Reg::H => 0b100,
    Reg::L => 0b101,
    Reg::HL => 0b110,
    Reg::A => 0b111,
    _ => panic!("Passed reg16 to reg8 encoding fn"),
  }
}

fn encode_condition(condition: Condition) -> u8 {
  match condition {
    Condition::NonZero => 0b00,
    Condition::Zero => 0b01,
    Condition::NonCarry => 0b10,
    Condition::Carry => 0b11,
    Condition::Unconditional => panic!("Passed Unconditional to encode_condition"),
  }
}

fn encode_relative_addr(jump_addr: usize, addr: usize, data: &mut Vec<u8>) -> Result<(), String> {
  let pc = jump_addr as isize;
  let rel = (addr as isize)
    .checked_sub(pc + 1)
    .ok_or("Bad relative jump".to_string())?;
  if rel < i8::min_value() as isize || rel > i8::max_value() as isize {
    return Err("Bad relative jump".to_string());
  }
  data[jump_addr] = rel as u8;
  Ok(())
}

#[cfg(test)]
mod test {
  use crate::asm::operation::Address::*;
  use crate::asm::operation::Condition::*;
  use crate::asm::operation::Operation::*;
  use crate::asm::operation::Reg;
  use crate::asm::parser::LabelOrAddress::*;
  use crate::asm::parser::Statement::*;

  #[test]
  fn zero_opcodes() {
    let instructions = vec![Instruction(Stop)];
    let result = super::assemble(instructions);
    assert!(result.is_ok(), "Result not ok: {:?}", result);
    let rom = result.unwrap();
    assert_eq!(rom[0x150], 0x10);
  }

  #[test]
  fn register_opcode() {
    let instructions = vec![Instruction(Decrement(Resolved(Register(Reg::C))))];
    let result = super::assemble(instructions);
    assert!(result.is_ok(), "Result not ok: {:?}", result);
    let rom = result.unwrap();
    assert_eq!(rom[0x150], 0x0d);
  }

  #[test]
  fn condition() {
    let instructions = vec![Instruction(Return(Zero))];
    let result = super::assemble(instructions);
    assert!(result.is_ok(), "Result not ok: {:?}", result);
    let rom = result.unwrap();
    assert_eq!(rom[0x150], 0xc8);
  }

  #[test]
  fn jump_backwards_relative() {
    let instructions = vec![
      Label("abcd".to_string()),
      Instruction(Stop),
      Instruction(Stop),
      Instruction(Jump(Unconditional, RelativeLabel("abcd".to_string()))),
    ];
    let result = super::assemble(instructions);
    assert!(result.is_ok(), "Result not ok: {:?}", result);
    let rom = result.unwrap();
    assert_eq!(rom[0x150], 0x10);
    assert_eq!(rom[0x151], 0x10);
    assert_eq!(rom[0x152], 0x18);
    assert_eq!(rom[0x153], (-4 as i8) as u8);
  }

  #[test]
  fn jump_forwards_relative() {
    let instructions = vec![
      Instruction(Jump(Unconditional, RelativeLabel("abcd".to_string()))),
      Instruction(Stop),
      Instruction(Stop),
      Label("abcd".to_string()),
    ];
    let result = super::assemble(instructions);
    assert!(result.is_ok(), "Result not ok: {:?}", result);
    let rom = result.unwrap();
    assert_eq!(rom[0x150], 0x18);
    assert_eq!(rom[0x151], (2 as i8) as u8);
    assert_eq!(rom[0x152], 0x10);
    assert_eq!(rom[0x153], 0x10);
  }
}
