use lexer::{Lexer, Token};
use ll1::{Ll1Parser, Symbol, Terminal};
use operation::{Address, Operation, Reg};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  Directive(String),
  Label(String),
  LabelAndInstruction(String, Operation),
  Instruction(Operation),
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
  ll1: Peekable<Ll1Parser<Lexer<'a>>>,
}

impl<'a> Parser<'a> {
  pub fn new(input: Chars<'a>) -> Parser<'a> {
    Parser {
      ll1: Ll1Parser::new(Lexer::new(input.peekable())).peekable(),
    }
  }

  pub fn parse(self) -> Result<Vec<Statement>, String> {
    let mut r = Vec::new();
    for s in self {
      r.push(s?);
    }
    Ok(r)
  }

  fn next_symbol(&mut self) -> Result<Symbol, String> {
    match self.ll1.next() {
      Some(s) => s,
      None => Err(format!("Unexpected end of input")),
    }
  }

  fn next_word(&mut self) -> Result<String, String> {
    match self.next_symbol()? {
      Symbol::Terminal(Terminal::Token(Token::Word(word))) => Ok(word),
      s @ _ => Err(format!("Unexpected symbol: {:?}", s)),
    }
  }

  fn expect(&mut self, symbol: Symbol) -> Result<(), String> {
    let next = self.next_symbol()?;
    if next == symbol {
      Ok(())
    } else {
      Err(format!("Unexpected symbol {:?}", next))
    }
  }

  fn expect_token(&mut self, token: Token) -> Result<(), String> {
    self.expect(Symbol::Terminal(Terminal::Token(token)))
  }

  fn match_program(&mut self) -> Option<Result<Statement, String>> {
    self.ll1.next().map(|n| match n? {
      Symbol::Statement => {
        let statement = self.match_statement();
        self.expect_token(Token::Newline)?;
        statement
      }
      s @ _ => Err(format!("Unexpected symbol {:?}", s)),
    })
  }

  fn match_statement(&mut self) -> Result<Statement, String> {
    match self.next_symbol()? {
      Symbol::Instruction => self.match_instruction().map(|i| Statement::Instruction(i)),
      Symbol::Label => {
        let label = self.match_label()?;
        self.expect(Symbol::MaybeInstruction)?;
        if let Some(Ok(Symbol::Instruction)) = self.ll1.peek() {
          self.ll1.next();
          let inst = self.match_instruction()?;
          Ok(Statement::LabelAndInstruction(label, inst))
        } else {
          Ok(Statement::Label(label))
        }
      }
      s @ _ => Err(format!("Unexpected symbol {:?}", s)),
    }
  }

  fn match_instruction(&mut self) -> Result<Operation, String> {
    match self.next_symbol()? {
      Symbol::Opcode0 => self.match_opcode0(),
      Symbol::Opcode1 => self.match_opcode1(),
      Symbol::Opcode2 => self.match_opcode2(),
      s @ _ => Err(format!("Unexpected symbol: {:?}", s)),
    }
  }

  fn match_label(&mut self) -> Result<String, String> {
    let name = self.next_word()?;
    self.expect_token(Token::Colon)?;
    Ok(name)
  }

  fn match_opcode0(&mut self) -> Result<Operation, String> {
    match self.next_word()?.as_ref() {
      "nop" => Ok(Operation::Nop),
      s @ _ => Err(format!("Unexpected symbol: {:?}", s)),
    }
  }

  fn match_opcode1(&mut self) -> Result<Operation, String> {
    let opcode = self.next_word()?;
    self.expect(Symbol::Operand)?;
    let operand = self.match_operand()?;
    match opcode.as_ref() {
      "dec" => Ok(Operation::Decrement(operand)),
      s @ _ => Err(format!("Unexpected symbol: {:?}", s)),
    }
  }

  fn match_opcode2(&mut self) -> Result<Operation, String> {
    let opcode = self.next_word()?;
    self.expect(Symbol::Operand)?;
    let operand1 = self.match_operand()?;
    self.expect_token(Token::Comma)?;
    self.expect(Symbol::Operand)?;
    let operand2 = self.match_operand()?;
    match opcode.as_ref() {
      "ld" => Ok(Operation::Load8(operand1, operand2)),
      s @ _ => Err(format!("Unexpected symbol: {:?}", s)),
    }
  }

  fn match_operand(&mut self) -> Result<Address, String> {
    match self.next_symbol()? {
      Symbol::Register => self.match_register().map(|r| Address::Register(r)),
      s @ _ => Err(format!("Unexpected symbol in operand: {:?}", s)),
    }
  }

  fn match_register(&mut self) -> Result<Reg, String> {
    match self.next_word()?.as_ref() {
      "a" => Ok(Reg::A),
      "b" => Ok(Reg::B),
      "c" => Ok(Reg::C),
      "d" => Ok(Reg::D),
      "e" => Ok(Reg::E),
      "h" => Ok(Reg::H),
      "l" => Ok(Reg::L),
      "af" => Ok(Reg::AF),
      "bc" => Ok(Reg::BC),
      "de" => Ok(Reg::DE),
      "hl" => Ok(Reg::HL),
      "sp" => Ok(Reg::SP),
      "pc" => Ok(Reg::PC),
      s @ _ => Err(format!("Unexpected symbol: {:?}", s)),
    }
  }
}

impl<'a> Iterator for Parser<'a> {
  type Item = Result<Statement, String>;

  fn next(&mut self) -> Option<Result<Statement, String>> {
    match self.ll1.next() {
      Some(Ok(Symbol::Program)) => self.match_program(),
      Some(Err(e)) => Some(Err(e)),
      s => Some(Err(format!("Unexpected symbol {:?}", s))),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use operation::Address::*;
  use operation::Operation::*;
  use parser::Statement::*;

  #[test]
  fn nop() {
    let input = "nop\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(parser.parse(), Ok(vec![Instruction(Nop)]));
  }

  #[test]
  fn dec() {
    let input = "dec a\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![Instruction(Decrement(Register(Reg::A)))])
    );
  }

  #[test]
  fn ld() {
    let input = "ld a, b\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![Instruction(Load8(Register(Reg::A), Register(Reg::B)))])
    );
  }

  #[test]
  fn label() {
    let input = "label:\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(parser.parse(), Ok(vec![Label("label".to_string())]));
  }

  #[test]
  fn label_and_instruction() {
    let input = "label: nop\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![LabelAndInstruction("label".to_string(), Nop)])
    );
  }
}
