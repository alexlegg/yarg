use cartridge::Header;
use operation::{Address, Condition, Operation, Reg};
use parser::{LabelOrAddress, Statement};
use std::collections::HashMap;

struct Assembler {
  data: Vec<u8>,
  cur: usize,
  labels: HashMap<String, usize>,
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
    }
  }

  fn assemble(mut self, statements: Vec<Statement>) -> Result<Assembler, String> {
    for statement in statements {
      match statement {
        Statement::Instruction(op) => {
          self.encode_operation(&op)?;
        }
        Statement::Label(s) => {
          self.labels.insert(s, self.cur);
        }
        Statement::Directive(_, _) => {
          return Err("Directive!?".to_string());
        }
      }
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

  fn find_label(&self, label: &str) -> Option<usize> {
    self.labels.get(label).cloned()
  }

  fn encode_operation(&mut self, operation: &Operation<LabelOrAddress>) -> Result<(), String> {
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
      Operation::Jump(Condition::Unconditional, LabelOrAddress::RelativeLabel(s)) => {
        self.insert(0x18);
        let pc = self.cur as isize;
        match self.find_label(&s) {
          Some(addr) => {
            let rel = (addr as isize)
              .checked_sub(pc + 1)
              .ok_or("Bad relative jump".to_string())?;
            if rel < i8::min_value() as isize || rel > i8::max_value() as isize {
              return Err("Bad relative jump".to_string());
            }
            self.insert(rel as u8);
          }
          None => {
            return Err("Not implemented".to_string());
          }
        }
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
        self.insert(0xc0 | encode_condition(*condition) << 3);
      }
      Operation::ReturnFromInterrupt => {
        self.insert(0xd9);
      }
      Operation::Load8(
        LabelOrAddress::Resolved(Address::Register(dest)),
        LabelOrAddress::Resolved(Address::Register(source)),
      ) => {
        self.insert(0x40 | (encode_reg(*dest) << 3) | (encode_reg(*source)));
      }
      Operation::Load8(
        LabelOrAddress::Resolved(Address::Register(dest)),
        LabelOrAddress::Resolved(Address::Data8(val)),
      ) => {
        self.insert(0x06 | (encode_reg(*dest) << 3));
        self.insert(*val);
      }
      Operation::Decrement(LabelOrAddress::Resolved(Address::Register(r))) => {
        self.insert(0x05 | (encode_reg(*r) << 3));
      }
      _ => {
        return Err(format!("Unrecognised operation {:?}", operation));
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

#[cfg(test)]
mod test {
  use operation::Address::*;
  use operation::Condition::*;
  use operation::Operation::*;
  use operation::Reg;
  use parser::LabelOrAddress::*;
  use parser::Statement::*;

  #[test]
  fn zero_opcodes() {
    let instructions = vec![Instruction(Stop)];
    let result = super::assemble(instructions);
    assert!(result.is_ok());
    let rom = result.unwrap();
    assert_eq!(rom[0x150], 0x10);
  }

  #[test]
  fn register_opcode() {
    let instructions = vec![Instruction(Decrement(Resolved(Register(Reg::C))))];
    let result = super::assemble(instructions);
    assert!(result.is_ok());
    let rom = result.unwrap();
    assert_eq!(rom[0x150], 0x0d);
  }

  #[test]
  fn condition() {
    let instructions = vec![Instruction(Return(Zero))];
    let result = super::assemble(instructions);
    assert!(result.is_ok());
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
    assert!(result.is_ok());
    let rom = result.unwrap();
    assert_eq!(rom[0x150], 0x10);
    assert_eq!(rom[0x151], 0x10);
    assert_eq!(rom[0x152], 0x18);
    assert_eq!(rom[0x153], (-4 as i8) as u8);
  }
}
