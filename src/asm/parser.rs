use lexer::{Lexer, Token};
use ll1::{Ll1Parser, Symbol, Terminal};
use operation::{Address, Condition, Operation, Reg};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  Directive(String, String),
  Label(String),
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
      s @ _ => Err(format!("Expected word, got symbol: {:?}", s)),
    }
  }

  fn expect(&mut self, symbol: Symbol) -> Result<(), String> {
    let next = self.next_symbol()?;
    if next == symbol {
      Ok(())
    } else {
      Err(format!("Expected {:?}, got symbol {:?}", symbol, next))
    }
  }

  fn maybe_expect(&mut self, symbol: Symbol) -> Result<bool, String> {
    let peek_matches = match self.ll1.peek() {
      Some(Ok(next)) => *next == symbol,
      _ => false,
    };
    if peek_matches {
      self.expect(symbol)?;
    }
    Ok(peek_matches)
  }

  fn expect_token(&mut self, token: Token) -> Result<(), String> {
    self.expect(Symbol::Terminal(Terminal::Token(token)))
  }

  fn maybe_expect_token(&mut self, token: Token) -> Result<bool, String> {
    self.maybe_expect(Symbol::Terminal(Terminal::Token(token)))
  }

  fn match_program(&mut self) -> Option<Result<Statement, String>> {
    // We may be in two states here: at the beginning of a Program, or at the
    // beginning of an Instruction following MaybeInstruction.
    match self.ll1.next()? {
      Ok(Symbol::Program) => self.match_program(),
      Ok(Symbol::Statement) => {
        let statement = self.match_statement();
        if let Some(Ok(Symbol::Instruction)) = self.ll1.peek() {
          return Some(statement);
        }
        if let Err(e) = self.expect_token(Token::Newline) {
          return Some(Err(e));
        }
        Some(statement)
      }
      Ok(Symbol::Instruction) => {
        let statement = self.match_instruction().map(|i| Statement::Instruction(i));
        if let Err(e) = self.expect_token(Token::Newline) {
          return Some(Err(e));
        }
        Some(statement)
      }
      s @ Ok(_) => Some(Err(format!(
        "Expected Program, Statement, or Instruction. Got {:?}",
        s
      ))),
      Err(e) => Some(Err(e)),
    }
  }

  fn match_statement(&mut self) -> Result<Statement, String> {
    match self.next_symbol()? {
      Symbol::Instruction => self.match_instruction().map(|i| Statement::Instruction(i)),
      Symbol::Label => {
        let label = self.match_label()?;
        self.expect(Symbol::MaybeInstruction)?;
        Ok(Statement::Label(label))
      }
      Symbol::Directive => self
        .match_directive()
        .map(|(d, v)| Statement::Directive(d, v)),
      s @ _ => Err(format!(
        "Expected Instruction, Label, or Directive. Got {:?}",
        s
      )),
    }
  }

  fn match_label(&mut self) -> Result<String, String> {
    let name = self.next_word()?;
    self.expect_token(Token::Colon)?;
    Ok(name)
  }

  fn match_directive(&mut self) -> Result<(String, String), String> {
    self.expect_token(Token::Dot)?;
    let name = self.next_word()?;
    let value = self.next_word()?;
    Ok((name, value))
  }

  fn expect_zero_operands(&mut self) -> Result<(), String> {
    if self.maybe_expect(Symbol::Operand)? {
      Err("Expected zero operands".to_string())
    } else {
      Ok(())
    }
  }

  fn expect_operand(&mut self) -> Result<Address, String> {
    self.expect(Symbol::Operand)?;
    let operand = self.match_operand()?;
    self.expect(Symbol::MaybeOperand)?;
    Ok(operand)
  }

  fn expect_second_operand(&mut self) -> Result<Address, String> {
    self.expect_token(Token::Comma)?;
    self.expect(Symbol::Operand)?;
    let operand = self.match_operand()?;
    Ok(operand)
  }

  fn match_instruction(&mut self) -> Result<Operation, String> {
    self.expect(Symbol::Opcode)?;
    let opcode = self.next_word()?;
    self.expect(Symbol::MaybeOperands)?;
    match opcode.as_ref() {
      "nop" => {
        self.expect_zero_operands()?;
        Ok(Operation::Nop)
      }
      "daa" => {
        self.expect_zero_operands()?;
        Ok(Operation::DecimalAdjustAccumulator)
      }
      "cpl" => {
        self.expect_zero_operands()?;
        Ok(Operation::Complement)
      }
      "ccf" => {
        self.expect_zero_operands()?;
        Ok(Operation::ComplementCarry)
      }
      "scf" => {
        self.expect_zero_operands()?;
        Ok(Operation::SetCarry)
      }
      "halt" => {
        self.expect_zero_operands()?;
        Ok(Operation::Halt)
      }
      "stop" => {
        self.expect_zero_operands()?;
        Ok(Operation::Stop)
      }
      "ei" => {
        self.expect_zero_operands()?;
        Ok(Operation::EnableInterrupts)
      }
      "di" => {
        self.expect_zero_operands()?;
        Ok(Operation::DisableInterrupts)
      }
      "rlca" => {
        self.expect_zero_operands()?;
        Ok(Operation::RotateLeftA(true, Address::Register(Reg::A)))
      }
      "rla" => {
        self.expect_zero_operands()?;
        Ok(Operation::RotateLeftA(false, Address::Register(Reg::A)))
      }
      "rrca" => {
        self.expect_zero_operands()?;
        Ok(Operation::RotateRightA(true, Address::Register(Reg::A)))
      }
      "rra" => {
        self.expect_zero_operands()?;
        Ok(Operation::RotateRightA(false, Address::Register(Reg::A)))
      }
      "ret" => {
        self.expect_zero_operands()?;
        Ok(Operation::Return(Condition::Unconditional))
      }
      "reti" => {
        self.expect_zero_operands()?;
        Ok(Operation::ReturnFromInterrupt)
      }
      "dec" => {
        let operand = self.expect_operand()?;
        Ok(Operation::Decrement(operand))
      }
      "ld" => {
        let destination = self.expect_operand()?;
        let source = self.expect_second_operand()?;
        Ok(Operation::Load8(destination, source))
      }
      s @ _ => Err(format!("Expected opcode, got {:?}", s)),
    }
  }

  fn match_operand(&mut self) -> Result<Address, String> {
    match self.next_symbol()? {
      Symbol::Register => self.match_register().map(|r| Address::Register(r)),
      Symbol::Constant => self.match_constant().map(|c| Address::Data8(c)),
      s @ _ => Err(format!("Expected Register, or Constant. Got {:?}", s)),
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
      s @ _ => Err(format!("Expected register, got {:?}", s)),
    }
  }

  fn match_constant(&mut self) -> Result<u8, String> {
    self
      .next_word()?
      .parse::<u8>()
      .map_err(|e| format!("Failed to parse number: {:?}", e))
  }
}

impl<'a> Iterator for Parser<'a> {
  type Item = Result<Statement, String>;

  fn next(&mut self) -> Option<Result<Statement, String>> {
    self.match_program()
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
  fn constant() {
    let input = "ld a, 10\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![Instruction(Load8(Register(Reg::A), Data8(10)))])
    );
  }

  #[test]
  fn directive() {
    let input = ".bank 0\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![Directive("bank".to_string(), "0".to_string())])
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
      Ok(vec![Label("label".to_string()), Instruction(Nop)])
    );
  }

  #[test]
  fn multiple_statements() {
    let input = "label:\nnop\nld a, b\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Label("label".to_string()),
        Instruction(Nop),
        Instruction(Load8(Register(Reg::A), Register(Reg::B))),
      ])
    );
  }

  #[test]
  fn statements_with_line_comment() {
    let input = "nop; this is a comment\nld a, b\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Instruction(Nop),
        Instruction(Load8(Register(Reg::A), Register(Reg::B))),
      ])
    );
  }

  #[test]
  fn statements_with_multiline_comment() {
    let input = "nop /* this\nis\na\ncomment\n*/\nld a, b\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Instruction(Nop),
        Instruction(Load8(Register(Reg::A), Register(Reg::B))),
      ])
    );
  }
}
