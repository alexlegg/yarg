use lexer::{Lexer, Token};
use ll1::{Ll1Parser, Symbol, Terminal};
use operation::{Address, Condition, Operation, Reg};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  Directive(String, String),
  Label(String),
  Instruction(Operation<LabelOrAddress>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LabelOrAddress {
  AbsoluteLabel(String),
  RelativeLabel(String),
  DataLabel(String),
  Resolved(Address),
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
  ll1: Peekable<Ll1Parser<Lexer<'a>>>,
  pending_statement: Option<Statement>,
}

impl<'a> Parser<'a> {
  pub fn new(input: Chars<'a>) -> Parser<'a> {
    Parser {
      ll1: Ll1Parser::new(Lexer::new(input.peekable())).peekable(),
      pending_statement: None,
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

  fn expect_word(&mut self, word: &str) -> Result<(), String> {
    let next = self.next_word()?;
    if next == word {
      Ok(())
    } else {
      Err(format!("Expected {:?}, got {:?}", word, next))
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

  fn match_program(&mut self) -> Result<Option<Statement>, String> {
    self.expect(Symbol::Program)?;
    match self.ll1.next() {
      Some(Ok(Symbol::Statement)) => {
        let statement = self.match_statement()?;
        self.expect_token(Token::Newline)?;
        Ok(Some(statement))
      }
      Some(Ok(s)) => Err(format!("Expected Statement, got {:?}", s)),
      None => Ok(None),
      Some(Err(e)) => Err(e),
    }
  }

  fn match_statement(&mut self) -> Result<Statement, String> {
    match self.next_symbol()? {
      Symbol::Instruction => {
        let instr = self.match_instruction()?;
        Ok(Statement::Instruction(instr))
      }
      Symbol::Label => {
        let label = self.match_label()?;
        self.expect(Symbol::MaybeInstruction)?;
        if self.maybe_expect(Symbol::Instruction)? {
          let instr = self.match_instruction()?;
          self.pending_statement = Some(Statement::Instruction(instr))
        }
        Ok(Statement::Label(label))
      }
      Symbol::Directive => {
        let (d, v) = self.match_directive()?;
        Ok(Statement::Directive(d, v))
      }
      s => Err(format!("Expected a statement, got {:?}", s)),
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

  fn match_instruction(&mut self) -> Result<Operation<LabelOrAddress>, String> {
    match self.next_symbol()? {
      Symbol::Nop => {
        self.expect_word("nop")?;
        Ok(Operation::Nop)
      }
      Symbol::Daa => {
        self.expect_word("daa")?;
        Ok(Operation::DecimalAdjustAccumulator)
      }
      Symbol::Cpl => {
        self.expect_word("cpl")?;
        Ok(Operation::Complement)
      }
      Symbol::Ccf => {
        self.expect_word("ccf")?;
        Ok(Operation::ComplementCarry)
      }
      Symbol::Scf => {
        self.expect_word("scf")?;
        Ok(Operation::SetCarry)
      }
      Symbol::Halt => {
        self.expect_word("halt")?;
        Ok(Operation::Halt)
      }
      Symbol::Stop => {
        self.expect_word("stop")?;
        Ok(Operation::Stop)
      }
      Symbol::Ei => {
        self.expect_word("ei")?;
        Ok(Operation::EnableInterrupts)
      }
      Symbol::Di => {
        self.expect_word("di")?;
        Ok(Operation::DisableInterrupts)
      }
      Symbol::Rlca => {
        self.expect_word("rlca")?;
        Ok(Operation::RotateLeftA(true))
      }
      Symbol::Rla => {
        self.expect_word("rla")?;
        Ok(Operation::RotateLeftA(false))
      }
      Symbol::Rrca => {
        self.expect_word("rrca")?;
        Ok(Operation::RotateRightA(true))
      }
      Symbol::Rra => {
        self.expect_word("rra")?;
        Ok(Operation::RotateRightA(false))
      }
      Symbol::Reti => {
        self.expect_word("reti")?;
        Ok(Operation::ReturnFromInterrupt)
      }
      Symbol::Inc => {
        self.expect_word("inc")?;
        self.expect(Symbol::Operand)?;
        let destination = self.match_data_operand()?;
        Ok(Operation::Increment(destination))
      }
      Symbol::Dec => {
        self.expect_word("dec")?;
        self.expect(Symbol::Operand)?;
        let destination = self.match_data_operand()?;
        Ok(Operation::Decrement(destination))
      }
      Symbol::Sub => {
        self.expect_word("sub")?;
        self.expect(Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Sub(source))
      }
      Symbol::And => {
        self.expect_word("and")?;
        self.expect(Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::And(source))
      }
      Symbol::Xor => {
        self.expect_word("xor")?;
        self.expect(Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Xor(source))
      }
      Symbol::Or => {
        self.expect_word("or")?;
        self.expect(Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Or(source))
      }
      Symbol::Cp => {
        self.expect_word("cp")?;
        self.expect(Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Compare(source))
      }
      Symbol::Push => {
        self.expect_word("push")?;
        self.expect(Symbol::Operand)?;
        let value = self.match_data_operand()?;
        Ok(Operation::Push(value))
      }
      Symbol::Pop => {
        self.expect_word("pop")?;
        self.expect(Symbol::Operand)?;
        let value = self.match_data_operand()?;
        Ok(Operation::Pop(value))
      }
      Symbol::Ret => {
        self.expect_word("ret")?;
        self.expect(Symbol::MaybeConditionOnly)?;
        if self.maybe_expect(Symbol::Condition)? {
          let condition = self.match_condition()?;
          Ok(Operation::Return(condition))
        } else {
          Ok(Operation::Return(Condition::Unconditional))
        }
      }
      Symbol::Jr => {
        self.expect_word("jr")?;
        self.expect(Symbol::MaybeCondition)?;
        if self.maybe_expect(Symbol::Condition)? {
          let condition = self.match_condition()?;
          self.expect_token(Token::Comma)?;
          let source = self.match_jr_operand()?;
          Ok(Operation::Jump(condition, source))
        } else {
          let source = self.match_jr_operand()?;
          Ok(Operation::Jump(Condition::Unconditional, source))
        }
      }
      Symbol::Ld => {
        self.expect_word("ld")?;
        self.expect(Symbol::Operand)?;
        let destination = self.match_data_operand()?;
        self.expect_token(Token::Comma)?;
        self.expect(Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Load8(destination, source))
      }
      s => Err(format!("Expected instruction, got {:?}", s)),
    }
  }

  fn match_data_operand(&mut self) -> Result<LabelOrAddress, String> {
    self.match_operand(
      |constant| LabelOrAddress::Resolved(Address::Data8(constant)),
      |identifier| LabelOrAddress::DataLabel(identifier),
    )
  }

  fn match_jr_operand(&mut self) -> Result<LabelOrAddress, String> {
    self.match_operand(
      |constant| LabelOrAddress::Resolved(Address::Relative(constant)),
      |identifier| LabelOrAddress::RelativeLabel(identifier),
    )
  }

  fn match_operand<F, G>(&mut self, f: F, g: G) -> Result<LabelOrAddress, String>
  where
    F: FnOnce(u8) -> LabelOrAddress,
    G: FnOnce(String) -> LabelOrAddress,
  {
    match self.next_symbol()? {
      Symbol::Value => (),
      Symbol::Register => {
        return self
          .match_register()
          .map(|r| LabelOrAddress::Resolved(Address::Register(r)));
      }
      s => {
        return Err(format!("Expected Register or Value. Got {:?}", s));
      }
    }
    match self.next_symbol()? {
      Symbol::Constant => self.match_constant().map(f),
      Symbol::Identifier => self.next_word().map(g),
      s => Err(format!("Expected Register or Value. Got {:?}", s)),
    }
  }

  fn match_constant<T>(&mut self) -> Result<T, String>
  where
    T: std::str::FromStr,
  {
    self
      .next_word()?
      .parse::<T>()
      .map_err(|e| format!("Failed to parse constant"))
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

  fn match_condition(&mut self) -> Result<Condition, String> {
    match self.next_word()?.as_ref() {
      "c" => Ok(Condition::Carry),
      "nc" => Ok(Condition::NonCarry),
      "z" => Ok(Condition::Zero),
      "nz" => Ok(Condition::NonZero),
      s => Err(format!("Expected condition, got {:?}", s)),
    }
  }
}

impl<'a> Iterator for Parser<'a> {
  type Item = Result<Statement, String>;

  fn next(&mut self) -> Option<Result<Statement, String>> {
    if let Some(statement) = self.pending_statement.take() {
      return Some(Ok(statement));
    }
    // Convert Result<Option> to Option<Result>.
    match self.match_program() {
      Ok(Some(statement)) => Some(Ok(statement)),
      Ok(None) => None,
      Err(e) => Some(Err(e)),
    }
  }
}

#[cfg(test)]
mod test {
  use super::LabelOrAddress::*;
  use super::*;
  use operation::Address::*;
  use operation::Operation::*;
  use parser::Statement::*;

  #[test]
  fn zero_operands() {
    let input = "nop\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(parser.parse(), Ok(vec![Instruction(Nop)]));
  }

  #[test]
  fn one_operand() {
    let input = "dec a\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![Instruction(Decrement(Resolved(Register(Reg::A))))])
    );
  }

  #[test]
  fn two_operands() {
    let input = "ld a, b\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![Instruction(Load8(
        Resolved(Register(Reg::A)),
        Resolved(Register(Reg::B))
      ))])
    );
  }

  #[test]
  fn optional_condition() {
    let input = "ret\nret nz\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Instruction(Return(Condition::Unconditional)),
        Instruction(Return(Condition::NonZero)),
      ])
    );
  }

  #[test]
  fn operand_with_optional_condition() {
    let input = "jr 42\njr nz, 42\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Instruction(Jump(Condition::Unconditional, Resolved(Relative(42)))),
        Instruction(Jump(Condition::NonZero, Resolved(Relative(42)))),
      ])
    );
  }

  #[test]
  fn constant() {
    let input = "ld a, 10\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![Instruction(Load8(
        Resolved(Register(Reg::A)),
        Resolved(Data8(10))
      ))])
    );
  }

  #[test]
  fn relative_label() {
    let input = "jr nz, abcd\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![Instruction(Jump(
        Condition::NonZero,
        RelativeLabel("abcd".to_string())
      )),])
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
        Instruction(Load8(
          Resolved(Register(Reg::A)),
          Resolved(Register(Reg::B))
        )),
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
        Instruction(Load8(
          Resolved(Register(Reg::A)),
          Resolved(Register(Reg::B))
        )),
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
        Instruction(Load8(
          Resolved(Register(Reg::A)),
          Resolved(Register(Reg::B))
        )),
      ])
    );
  }
}
