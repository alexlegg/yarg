use crate::asm::lexer::{Lexer, Token};
use crate::asm::ll1::{Ll1Parser, Symbol, Terminal};
use crate::asm::operation::{Address, Condition, Operation, Reg};
use failure::*;
use num_traits::Num;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  Directive(Directive),
  Label(String),
  Instruction(Operation<LabelOrAddress>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Directive {
  Section(usize),
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

  fn next_symbol(&mut self) -> Result<Symbol, Error> {
    match self.ll1.next() {
      Some(s) => s.map_err(Error::from),
      None => Err(format_err!("Unexpected end of input")),
    }
  }

  fn next_word(&mut self) -> Result<String, Error> {
    match self.next_symbol()? {
      Symbol::Terminal(Terminal::Token(Token::Word(word))) => Ok(word),
      s => Err(format_err!("Expected word, got symbol: {:?}", s)),
    }
  }

  fn expect(&mut self, symbol: &Symbol) -> Result<(), Error> {
    let next = self.next_symbol()?;
    if next == *symbol {
      Ok(())
    } else {
      Err(format_err!("Expected {:?}, got symbol {:?}", symbol, next))
    }
  }

  fn expect_word(&mut self, word: &str) -> Result<(), Error> {
    let next = self.next_word()?;
    if next == word {
      Ok(())
    } else {
      Err(format_err!("Expected {:?}, got {:?}", word, next))
    }
  }

  fn maybe_expect(&mut self, symbol: &Symbol) -> Result<bool, Error> {
    let peek_matches = match self.ll1.peek() {
      Some(Ok(next)) => *next == *symbol,
      _ => false,
    };
    if peek_matches {
      self.expect(symbol)?;
    }
    Ok(peek_matches)
  }

  fn expect_token(&mut self, token: Token) -> Result<(), Error> {
    self.expect(&Symbol::Terminal(Terminal::Token(token)))
  }

  fn match_program(&mut self) -> Result<Option<Statement>, Error> {
    self.expect(&Symbol::Program)?;
    match self.ll1.next() {
      Some(Ok(Symbol::Statement)) => {
        let statement = self.match_statement()?;
        self.expect_token(Token::Newline)?;
        Ok(Some(statement))
      }
      Some(Ok(s)) => Err(format_err!("Expected Statement, got {:?}", s)),
      None => Ok(None),
      Some(Err(e)) => Err(Error::from(e)),
    }
  }

  fn match_statement(&mut self) -> Result<Statement, Error> {
    match self.next_symbol()? {
      Symbol::Instruction => {
        let instr = self.match_instruction()?;
        Ok(Statement::Instruction(instr))
      }
      Symbol::Label => {
        let label = self.match_label()?;
        self.expect(&Symbol::MaybeInstruction)?;
        if self.maybe_expect(&Symbol::Instruction)? {
          let instr = self.match_instruction()?;
          self.pending_statement = Some(Statement::Instruction(instr))
        }
        Ok(Statement::Label(label))
      }
      Symbol::Directive => {
        let directive = self.match_directive()?;
        Ok(Statement::Directive(directive))
      }
      s => Err(format_err!("Expected a statement, got {:?}", s)),
    }
  }

  fn match_label(&mut self) -> Result<String, Error> {
    let name = self.next_word()?;
    self.expect_token(Token::Colon)?;
    Ok(name)
  }

  fn match_directive(&mut self) -> Result<Directive, Error> {
    self.expect_token(Token::Dot)?;
    self.expect_word("section")?;
    let value = self.match_constant()?;
    Ok(Directive::Section(value))
  }

  fn match_instruction(&mut self) -> Result<Operation<LabelOrAddress>, Error> {
    match self.next_symbol()? {
      // Arithmetic and logic
      Symbol::Adc => {
        self.expect_word("adc")?;
        self.expect(&Symbol::Operand)?;
        self.match_data_operand()?;
        self.expect_token(Token::Comma)?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::AddCarry(source))
      }
      Symbol::Add => {
        self.expect_word("add")?;
        self.expect(&Symbol::Operand)?;
        self.match_data_operand()?;
        self.expect_token(Token::Comma)?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Add(source))
      }
      Symbol::And => {
        self.expect_word("and")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::And(source))
      }
      Symbol::Cp => {
        self.expect_word("cp")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Compare(source))
      }
      Symbol::Dec => {
        self.expect_word("dec")?;
        self.expect(&Symbol::Operand)?;
        let destination = self.match_data_operand()?;
        Ok(Operation::Decrement(destination))
      }
      Symbol::Inc => {
        self.expect_word("inc")?;
        self.expect(&Symbol::Operand)?;
        let destination = self.match_data_operand()?;
        Ok(Operation::Increment(destination))
      }
      Symbol::Or => {
        self.expect_word("or")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Or(source))
      }
      Symbol::Sbc => {
        self.expect_word("sbc")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Or(source))
      }
      Symbol::Sub => {
        self.expect_word("sub")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Sub(source))
      }
      Symbol::Xor => {
        self.expect_word("xor")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Xor(source))
      }

      // Bit operations
      Symbol::Bit => {
        self.expect_word("bit")?;
        let bit = self.match_constant()?;
        self.expect_token(Token::Comma)?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Bit(bit, source))
      }
      Symbol::Res => {
        self.expect_word("res")?;
        let bit = self.match_constant()?;
        self.expect_token(Token::Comma)?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::ResetBit(bit, source))
      }
      Symbol::Set => {
        self.expect_word("set")?;
        let bit = self.match_constant()?;
        self.expect_token(Token::Comma)?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::SetBit(bit, source))
      }
      Symbol::Swap => {
        self.expect_word("swap")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Swap(source))
      }

      // Shift and rotate operations
      Symbol::Rl => {
        self.expect_word("rl")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::RotateLeft(false, source))
      }
      Symbol::Rla => {
        self.expect_word("rla")?;
        Ok(Operation::RotateLeftA(false))
      }
      Symbol::Rlc => {
        self.expect_word("rlc")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::RotateLeft(true, source))
      }
      Symbol::Rlca => {
        self.expect_word("rlca")?;
        Ok(Operation::RotateLeftA(true))
      }
      Symbol::Rr => {
        self.expect_word("rr")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::RotateRight(false, source))
      }
      Symbol::Rra => {
        self.expect_word("rra")?;
        Ok(Operation::RotateRightA(false))
      }
      Symbol::Rrc => {
        self.expect_word("rrc")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::RotateRight(true, source))
      }
      Symbol::Rrca => {
        self.expect_word("rrca")?;
        Ok(Operation::RotateRightA(true))
      }
      Symbol::Sla => {
        self.expect_word("sla")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::ShiftLeft(source))
      }
      Symbol::Sra => {
        self.expect_word("sra")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::ShiftRight(source))
      }
      Symbol::Srl => {
        self.expect_word("srl")?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::ShiftRightLogical(source))
      }

      // Load operations
      Symbol::Ld => {
        self.expect_word("ld")?;
        self.expect(&Symbol::Operand)?;
        let destination = self.match_data_operand()?;
        self.expect_token(Token::Comma)?;
        self.expect(&Symbol::Operand)?;
        let source = self.match_data_operand()?;
        Ok(Operation::Load8(destination, source))
      }
      Symbol::Ldh => Err(format_err!("Not implemented")),
      Symbol::Ldi => Err(format_err!("Not implemented")),
      Symbol::Ldd => Err(format_err!("Not implemented")),

      // Jump and call operations
      Symbol::Call => Err(format_err!("Not implemented")),
      Symbol::Jp => {
        self.expect_word("jp")?;
        self.match_jump(false)
      }
      Symbol::Jr => {
        self.expect_word("jr")?;
        self.match_jump(true)
      }
      Symbol::Ret => {
        self.expect_word("ret")?;
        self.expect(&Symbol::MaybeConditionOnly)?;
        if self.maybe_expect(&Symbol::Condition)? {
          let condition = self.match_condition()?;
          Ok(Operation::Return(condition))
        } else {
          Ok(Operation::Return(Condition::Unconditional))
        }
      }
      Symbol::Reti => {
        self.expect_word("reti")?;
        Ok(Operation::ReturnFromInterrupt)
      }
      Symbol::Rst => {
        self.expect_word("rst")?;
        let destination = self.match_constant()?;
        Ok(Operation::Reset(destination))
      }

      // Stack operations
      Symbol::Pop => {
        self.expect_word("pop")?;
        self.expect(&Symbol::Operand)?;
        let value = self.match_data_operand()?;
        Ok(Operation::Pop(value))
      }
      Symbol::Push => {
        self.expect_word("push")?;
        self.expect(&Symbol::Operand)?;
        let value = self.match_data_operand()?;
        Ok(Operation::Push(value))
      }

      // Misc operations
      Symbol::Ccf => {
        self.expect_word("ccf")?;
        Ok(Operation::ComplementCarry)
      }
      Symbol::Cpl => {
        self.expect_word("cpl")?;
        Ok(Operation::Complement)
      }
      Symbol::Daa => {
        self.expect_word("daa")?;
        Ok(Operation::DecimalAdjustAccumulator)
      }
      Symbol::Di => {
        self.expect_word("di")?;
        Ok(Operation::DisableInterrupts)
      }
      Symbol::Ei => {
        self.expect_word("ei")?;
        Ok(Operation::EnableInterrupts)
      }
      Symbol::Halt => {
        self.expect_word("halt")?;
        Ok(Operation::Halt)
      }
      Symbol::Nop => {
        self.expect_word("nop")?;
        Ok(Operation::Nop)
      }
      Symbol::Scf => {
        self.expect_word("scf")?;
        Ok(Operation::SetCarry)
      }
      Symbol::Stop => {
        self.expect_word("stop")?;
        Ok(Operation::Stop)
      }

      s => Err(format_err!("Expected instruction, got {:?}", s)),
    }
  }

  fn match_data_operand(&mut self) -> Result<LabelOrAddress, Error> {
    self.match_operand(
      |constant| LabelOrAddress::Resolved(Address::Data8(constant)),
      LabelOrAddress::DataLabel,
    )
  }

  fn match_jr_operand(&mut self) -> Result<LabelOrAddress, Error> {
    self.match_operand(
      |constant| LabelOrAddress::Resolved(Address::Relative(constant)),
      LabelOrAddress::RelativeLabel,
    )
  }

  fn match_jp_operand(&mut self) -> Result<LabelOrAddress, Error> {
    self.match_operand(
      |constant| LabelOrAddress::Resolved(Address::Immediate(constant)),
      LabelOrAddress::AbsoluteLabel,
    )
  }

  fn match_operand<T, F, G>(&mut self, f: F, g: G) -> Result<LabelOrAddress, Error>
  where
    T: Num,
    F: FnOnce(T) -> LabelOrAddress,
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
        return Err(format_err!("Expected Register or Value. Got {:?}", s));
      }
    }
    match self.next_symbol()? {
      Symbol::Constant => self.match_constant().map(f),
      Symbol::Identifier => self.next_word().map(g),
      s => Err(format_err!("Expected Register or Value. Got {:?}", s)),
    }
  }

  fn match_constant<T>(&mut self) -> Result<T, Error>
  where
    T: Num,
  {
    let word = self.next_word()?;
    if word.starts_with("0x") {
      T::from_str_radix(&word[2..], 16)
    } else if word.ends_with('h') {
      T::from_str_radix(&word[..word.len() - 1], 16)
    } else if word.starts_with("0b") {
      T::from_str_radix(&word[2..], 2)
    } else {
      T::from_str_radix(&word, 10)
    }
    .map_err(|_| format_err!("Failed to parse constant"))
  }

  fn match_register(&mut self) -> Result<Reg, Error> {
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
      s => Err(format_err!("Expected register, got {:?}", s)),
    }
  }

  fn match_condition(&mut self) -> Result<Condition, Error> {
    match self.next_word()?.as_ref() {
      "c" => Ok(Condition::Carry),
      "nc" => Ok(Condition::NonCarry),
      "z" => Ok(Condition::Zero),
      "nz" => Ok(Condition::NonZero),
      s => Err(format_err!("Expected condition, got {:?}", s)),
    }
  }

  fn match_jump(&mut self, relative: bool) -> Result<Operation<LabelOrAddress>, Error> {
    self.expect(&Symbol::MaybeCondition)?;
    let condition = if self.maybe_expect(&Symbol::Condition)? {
      let c = self.match_condition()?;
      self.expect_token(Token::Comma)?;
      c
    } else {
      Condition::Unconditional
    };
    let source = if relative {
      self.match_jr_operand()?
    } else {
      self.match_jp_operand()?
    };
    Ok(Operation::Jump(condition, source))
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
      Err(e) => Some(Err(e.to_string())),
    }
  }
}

#[cfg(test)]
mod test {
  use super::LabelOrAddress::*;
  use super::*;
  use crate::asm::operation::Address::*;
  use crate::asm::operation::Operation::*;
  use crate::asm::parser::Directive::*;
  use crate::asm::parser::Statement::*;

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
  fn number() {
    let input = "rst 8\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(parser.parse(), Ok(vec![Instruction(Reset(8))]));

    let input = "rst 0xabcd\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(parser.parse(), Ok(vec![Instruction(Reset(0xabcd))]));

    let input = "rst aB4Dh\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(parser.parse(), Ok(vec![Instruction(Reset(0xab4d))]));

    let input = "rst 0b0110\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(parser.parse(), Ok(vec![Instruction(Reset(0b0110))]));
  }

  #[test]
  fn number_and_operand() {
    let input = "bit 3, c\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(
      parser.parse(),
      Ok(vec![Instruction(Bit(3, Resolved(Register(Reg::C))))])
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
    let input = ".section 1234\n".to_string();
    let parser = Parser::new(input.chars());
    assert_eq!(parser.parse(), Ok(vec![Directive(Section(1234))]));
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
