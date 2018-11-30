use cartridge::Header;
use operation::{Address, Condition, Operation, Reg};
use parser::Statement;

struct Assembler {
  data: Vec<u8>,
  cur: usize,
}

pub fn assemble(statements: Vec<Statement>) -> Result<Vec<u8>, String> {
  let mut assembler = Assembler::new();
  for statement in statements {
    match statement {
      Statement::Instruction(op) => {
        assembler.encode_operation(op.clone())?;
      }
      Statement::Label(_) => return Err("Label!?".to_string()),
      Statement::Directive(_, _) => return Err("Directive!?".to_string()),
    }
  }
  assembler.encode()
}

impl Assembler {
  fn new() -> Assembler {
    Assembler {
      data: vec![0; 0x7fff],
      cur: 0x150,
    }
  }

  fn encode(mut self) -> Result<Vec<u8>, String> {
    let mut header = Header::new();
    header
      .title
      .copy_from_slice("test rom\0\0\0\0\0\0\0\0".as_bytes());
    header.set_header_checksum();
    self.data.splice(0x100..0x14d, header.serialise());
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

  fn encode_operation(&mut self, operation: Operation) -> Result<(), String> {
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
      Operation::SetCarry => {
        self.insert(0x37);
      }
      Operation::RotateLeftA(true, _) => {
        self.insert(0x07);
      }
      Operation::RotateLeftA(false, _) => {
        self.insert(0x17);
      }
      Operation::RotateRightA(true, _) => {
        self.insert(0x0f);
      }
      Operation::RotateRightA(false, _) => {
        self.insert(0x1f);
      }
      Operation::Return(Condition::Unconditional) => {
        self.insert(0xc9);
      }
      Operation::ReturnFromInterrupt => {
        self.insert(0xd9);
      }
      Operation::Load8(Address::Register(dest), Address::Register(source)) => {
        self.insert(0x40 | (encode_reg(dest) << 3) | (encode_reg(source)));
      }
      Operation::Load8(Address::Register(dest), Address::Data8(val)) => {
        self.insert(0x06 | (encode_reg(dest) << 3));
        self.insert(val);
      }
      Operation::Decrement(Address::Register(r)) => {
        self.insert(0x05 | (encode_reg(r) << 3));
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
