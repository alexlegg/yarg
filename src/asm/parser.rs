use lexer::Token;
use ll1::{Parser, Symbol, Terminal};
use operation::{Address, Operation, Reg};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  Directive(String),
  Label(String),
  Instruction(Operation),
}

pub fn parse<I: Iterator<Item = Token>>(token_iter: I) -> Result<Vec<Statement>, String> {
  let parser = Parser::new(token_iter);
  let symbols = parser.parse()?;
  let program = match_program(&mut symbols.iter())?.ok_or("error".to_string())?;
  Ok(vec![program])
}

fn match_program(out: &mut Iterator<Item = &Symbol>) -> Result<Option<Statement>, String> {
  match out.next() {
    Some(Symbol::Statement) => match_statement(out),
    s @ _ => Err(format!("Unexpected symbol in program: {:?}", s)),
  }
}

fn match_statement(out: &mut Iterator<Item = &Symbol>) -> Result<Option<Statement>, String> {
  match out.next() {
    Some(Symbol::Instruction) => {
      let instr = match_instruction(out)?;
      Ok(instr.map(|i| Statement::Instruction(i)))
    }
    None => Ok(None),
    s @ _ => Err(format!("Unexpected symbol in statement: {:?}", s)),
  }
}

fn match_instruction(out: &mut Iterator<Item = &Symbol>) -> Result<Option<Operation>, String> {
  match out.next() {
    Some(Symbol::Opcode0) => match_opcode0(out),
    Some(Symbol::Opcode1) => match_opcode1(out),
    Some(Symbol::Opcode2) => match_opcode2(out),
    None => Ok(None),
    s @ _ => Err(format!("Unexpected symbol in instruction: {:?}", s)),
  }
}

fn match_opcode0(out: &mut Iterator<Item = &Symbol>) -> Result<Option<Operation>, String> {
  match out.next() {
    Some(Symbol::Terminal(Terminal::Token(Token::Word(opcode)))) => match opcode.as_ref() {
      "nop" => Ok(Some(Operation::Nop)),
      o @ _ => Err(format!("Unexpected opcode: {:?}", o)),
    },
    None => Ok(None),
    s @ _ => Err(format!("Unexpected symbol in opcode: {:?}", s)),
  }
}

fn match_register(out: &mut Iterator<Item = &Symbol>) -> Result<Option<Reg>, String> {
  match out.next() {
    Some(Symbol::Terminal(Terminal::Token(Token::Word(reg)))) => match reg.as_ref() {
      "a" => Ok(Some(Reg::A)),
      "b" => Ok(Some(Reg::B)),
      "c" => Ok(Some(Reg::C)),
      "d" => Ok(Some(Reg::D)),
      "e" => Ok(Some(Reg::E)),
      "h" => Ok(Some(Reg::H)),
      "l" => Ok(Some(Reg::L)),
      "af" => Ok(Some(Reg::AF)),
      "bc" => Ok(Some(Reg::BC)),
      "de" => Ok(Some(Reg::DE)),
      "hl" => Ok(Some(Reg::HL)),
      "sp" => Ok(Some(Reg::SP)),
      "pc" => Ok(Some(Reg::PC)),
      r @ _ => Err(format!("Unexpected register: {:?}", r)),
    },
    None => Ok(None),
    s @ _ => Err(format!("Unexpected symbol in register: {:?}", s)),
  }
}

fn match_operand(out: &mut Iterator<Item = &Symbol>) -> Result<Option<Address>, String> {
  match out.next() {
    Some(Symbol::Register) => {
      let reg = match_register(out)?;
      Ok(reg.map(|r| Address::Register(r)))
    }
    None => Ok(None),
    s @ _ => Err(format!("Unexpected symbol in operand: {:?}", s)),
  }
}

fn match_opcode1(out: &mut Iterator<Item = &Symbol>) -> Result<Option<Operation>, String> {
  let opcode = match out.next() {
    Some(Symbol::Terminal(Terminal::Token(Token::Word(opcode)))) => opcode,
    None => return Ok(None),
    s @ _ => return Err(format!("Unexpected symbol in opcode: {:?}", s)),
  };
  let operand = match out.next() {
    Some(Symbol::Operand) => match_operand(out)?,
    None => return Ok(None),
    s @ _ => return Err(format!("Unexpected symbol in opcode: {:?}", s)),
  };
  if operand.is_none() {
    return Ok(None);
  }
  match opcode.as_ref() {
    "dec" => Ok(Some(Operation::Decrement(operand.unwrap()))),
    _ => Err(format!("Bad operand: {:?}", opcode)),
  }
}

fn match_opcode2(out: &mut Iterator<Item = &Symbol>) -> Result<Option<Operation>, String> {
  let opcode = match out.next() {
    Some(Symbol::Terminal(Terminal::Token(Token::Word(opcode)))) => opcode,
    None => return Ok(None),
    s @ _ => return Err(format!("Unexpected symbol in opcode: {:?}", s)),
  };
  let operand1 = match out.next() {
    Some(Symbol::Operand) => match_operand(out)?,
    None => return Ok(None),
    s @ _ => return Err(format!("Unexpected symbol in opcode: {:?}", s)),
  };
  if operand1.is_none() {
    return Ok(None);
  }
  match opcode.as_ref() {
    "ld" => Ok(Some(Operation::Decrement(operand1.unwrap()))),
    _ => Err(format!("Bad operand: {:?}", opcode)),
  }
}
