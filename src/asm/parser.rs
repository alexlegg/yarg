use lexer::Token;
use operation::{Address, Operation, Reg};
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

/* Grammar

Program -> Epsilon | Statement newline Program.
Statement -> Label MaybeInstruction | Instruction | Directive.
MaybeInstruction -> | Instruction .
Directive -> dot section identifier.
Label -> identifier colon.
Operand -> Register | leftparens number rightparens | number.
Opcode0 -> nop.
Opcode1 -> dec.
Opcode2 -> ld.
Instruction -> Opcode0 | Opcode1 Operand | Opcode2 Operand comma Operand.
Register -> "A" | "B" | "C" | "D" | "E" | "F" | "AF" | "BC" | "DE" | "HL" | "SP" | "PC"

*/

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  Directive(String),
  Label(String),
  Instruction(Operation),
}

#[derive(Debug, Clone)]
pub struct Parser<I: Iterator<Item = Token>> {
  token_iter: I,
  table: HashMap<Symbol, HashMap<Terminal, Vec<Symbol>>>,
  program: Vec<Statement>,
  output: VecDeque<Symbol>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
  pub fn new(token_iter: I) -> Parser<I> {
    Parser {
      token_iter,
      table: HashMap::new(),
      program: Vec::new(),
      output: VecDeque::new(),
    }
  }

  fn generate_table(&mut self) -> Result<(), String> {
    use lexer::Token::*;
    use parser::Symbol::*;
    use parser::Terminal::*;
    self.table = ParseTableBuilder::new()
      .add_rule(Program, vec![Terminal(Epsilon)])
      .add_rule(Program, vec![Statement, Terminal(Token(Newline)), Program])
      .add_rule(Statement, vec![Label, MaybeInstruction])
      .add_rule(Statement, vec![Instruction])
      .add_rule(Statement, vec![Directive])
      .add_rule(MaybeInstruction, vec![Terminal(Epsilon)])
      .add_rule(MaybeInstruction, vec![Instruction])
      .add_rule(
        Directive,
        vec![
          Terminal(Token(Dot)),
          Terminal(Token(Word("section".to_string()))),
        ],
      )
      .add_rule(Label, vec![Terminal(Alphanumeric), Terminal(Token(Colon))])
      .add_rule(Operand, vec![Register])
      .add_rule(
        Operand,
        vec![
          Terminal(Token(LeftParens)),
          Terminal(Number),
          Terminal(Token(RightParens)),
        ],
      )
      .add_rule(Operand, vec![Terminal(Number)])
      .add_rule(Opcode0, vec![Terminal(Token(Word("nop".to_string())))])
      .add_rule(Opcode1, vec![Terminal(Token(Word("dec".to_string())))])
      .add_rule(Opcode2, vec![Terminal(Token(Word("ld".to_string())))])
      .add_rule(Instruction, vec![Opcode0])
      .add_rule(Instruction, vec![Opcode1, Operand])
      .add_rule(
        Instruction,
        vec![Opcode2, Operand, Terminal(Token(Comma)), Operand],
      )
      .add_rule(Register, vec![Terminal(Token(Word("a".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("b".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("c".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("d".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("e".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("f".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("af".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("bc".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("de".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("hl".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("sp".to_string())))])
      .add_rule(Register, vec![Terminal(Token(Word("pc".to_string())))])
      .build()?;
    Ok(())
  }

  fn table_lookup(&self, symbol: &Symbol, token: &Option<Token>) -> Option<&Vec<Symbol>> {
    let row = self.table.get(&symbol)?;
    match token {
      Some(w @ Token::Word(_)) => {
        // Try looking up the word as a keyword and fall back on Alphanumeric.
        row
          .get(&Terminal::Token(w.clone()))
          .or(row.get(&Terminal::Alphanumeric))
      }
      Some(t) => row.get(&Terminal::Token(t.clone())),
      None => row.get(&Terminal::End),
    }
  }

  fn update_output(&mut self, symbol: Symbol) -> Result<(), String> {
    match symbol {
      Symbol::Program | Symbol::Terminal(Terminal::Token(Token::Newline)) => {
        return Ok(());
      }
      _ => (),
    }
    self.output.push_back(symbol);
    let statement = match_program(&mut self.output.iter());
    match statement? {
      Some(s) => {
        self.program.push(s);
        self.output.clear();
        Ok(())
      }
      None => Ok(()),
    }
  }

  pub fn parse(mut self) -> Result<Vec<Statement>, String> {
    self.generate_table()?;
    let mut stack: Vec<Symbol> = vec![Symbol::Program];
    let mut token = self.token_iter.next();
    while !stack.is_empty() {
      println!("stack: {:?}", stack);
      let top = stack
        .pop()
        .ok_or("Unexpected empty parse stack".to_string())?
        .clone();
      let token_symbol =
        Symbol::Terminal(token.clone().map_or(Terminal::End, |t| Terminal::Token(t)));
      if top == token_symbol {
        self.update_output(top.clone())?;
        token = self.token_iter.next();
        continue;
      }
      match &top {
        Symbol::Terminal(Terminal::Alphanumeric) => {
          let t = token.ok_or(format!("Expected Alphanumeric, got end of input"))?;
          if t.is_alphanumeric_word() {
            self.update_output(token_symbol)?;
            token = self.token_iter.next();
            continue;
          } else {
            return Err(format!("Expected Alphanumeric, got {:?}", t));
          }
        }
        Symbol::Terminal(t) => {
          return Err(format!("Expected {:?}, got {:?}", t, token_symbol));
        }
        _ => {
          self.update_output(top.clone())?;
        }
      }
      let symbols = self
        .table_lookup(&top, &token)
        .ok_or(format!("Failed on table lookup: {:?} {:?}", top, token))?;
      for s in symbols.iter().rev() {
        if let Symbol::Terminal(Terminal::Epsilon) = s {
          continue;
        }
        stack.push(s.clone());
      }
    }
    if self.output.is_empty() {
      Ok(self.program)
    } else {
      Err(format!("Unmatched output: {:?}", self.output))
    }
  }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Symbol {
  Program,
  Statement,
  MaybeInstruction,
  Directive,
  Label,
  Operand,
  Opcode0,
  Opcode1,
  Opcode2,
  Instruction,
  Register,
  Terminal(Terminal),
}

impl Symbol {
  fn is_terminal(&self) -> bool {
    match self {
      Symbol::Terminal(_) => true,
      _ => false,
    }
  }

  fn is_nonterminal(&self) -> bool {
    !self.is_terminal()
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Terminal {
  Token(Token),
  Alphanumeric,
  Number,
  End,
  Epsilon,
}

struct ParseTableBuilder {
  rules: Vec<(Symbol, Vec<Symbol>)>,
}

impl ParseTableBuilder {
  pub fn new() -> ParseTableBuilder {
    ParseTableBuilder { rules: Vec::new() }
  }

  pub fn add_rule(mut self, symbol: Symbol, rule: Vec<Symbol>) -> ParseTableBuilder {
    self.rules.push((symbol, rule));
    self
  }

  pub fn build(self) -> Result<HashMap<Symbol, HashMap<Terminal, Vec<Symbol>>>, String> {
    // TODO: Clean up all the unwraps in this function.
    let mut first: HashMap<Symbol, HashSet<Terminal>> = HashMap::new();
    let mut follow: HashMap<Symbol, HashSet<Terminal>> = HashMap::new();
    let mut epsilon: HashSet<Symbol> = HashSet::new();

    // Initialise first with {symbol} for terminals, or {} for non-terminals.
    for symbol in self.rules.iter().flat_map(|(_, r)| r.iter()) {
      first.entry(symbol.clone()).or_insert_with(|| match symbol {
        Symbol::Terminal(term) => [term.clone()].iter().cloned().collect(),
        _ => HashSet::new(),
      });
    }

    // Initialise epsilon set
    epsilon.insert(Symbol::Terminal(Terminal::Epsilon));

    // Initialise follow with {} for all symbols, except Program which gets {End}.
    for symbol in self.rules.iter().flat_map(|(_, r)| r.iter()) {
      follow.entry(symbol.clone()).or_insert(HashSet::new());
    }
    follow
      .get_mut(&Symbol::Program)
      .unwrap()
      .insert(Terminal::End);

    'fixed_point: loop {
      let mut updated = false;
      for (nonterminal, production) in self.rules.clone() {
        for p in production.iter() {
          let first_p = first.get(&p).unwrap().clone();
          updated |= insert_all(first.get_mut(&nonterminal).unwrap(), first_p);
          if epsilon.contains(&p) {
            updated |= epsilon.insert(nonterminal.clone());
          } else {
            break;
          }
        }

        let mut curr_follow = follow.get(&nonterminal).unwrap().clone();
        for p in production.iter().rev() {
          if p.is_nonterminal() {
            updated |= insert_all(follow.get_mut(&p).unwrap(), curr_follow.clone());
          }
          if epsilon.contains(&p) {
            insert_all(&mut curr_follow, first.get(&p).unwrap().clone());
          } else {
            curr_follow = first.get(&p).unwrap().clone();
          }
        }
      }
      if !updated {
        break 'fixed_point;
      }
    }

    let mut table = HashMap::new();
    for (symbol, rule) in self.rules {
      let mut first_contains_epsilon = false;
      for f in first.get(rule.first().unwrap()).unwrap() {
        if *f == Terminal::Epsilon {
          first_contains_epsilon = true;
          continue;
        }
        let entry = table.entry(symbol.clone()).or_insert(HashMap::new());
        if entry.get(f).is_some() {
          return Err(format!(
            "Table entry [{:?}, {:?}] contains two rules",
            symbol, f
          ));
        }
        entry.insert(f.clone(), rule.clone());
      }
      if first_contains_epsilon {
        for f in follow.get(&symbol).unwrap() {
          let entry = table.entry(symbol.clone()).or_insert(HashMap::new());
          if entry.get(f).is_some() {
            return Err(format!(
              "Table entry [{:?}, {:?}] contains two rules",
              symbol, f
            ));
          }
          entry.insert(f.clone(), rule.clone());
        }
      }
    }

    return Ok(table);
  }
}

fn insert_all<T>(target: &mut HashSet<T>, source: HashSet<T>) -> bool
where
  T: Eq + Hash,
{
  let mut inserted = false;
  for s in source {
    inserted |= target.insert(s);
  }
  return inserted;
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn opcode0() {
    let tokens = vec![Token::Word("nop".to_string()), Token::Newline];
    let parser = Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![Statement::Instruction(Operation::Nop)])
    );
  }

  #[test]
  fn opcode1() {
    let tokens = vec![
      Token::Word("dec".to_string()),
      Token::Word("a".to_string()),
      Token::Newline,
    ];
    let parser = Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![Statement::Instruction(Operation::Decrement(
        Address::Register(Reg::A),
      ))])
    );
  }

  #[test]
  fn opcode2() {
    let tokens = vec![
      Token::Word("ld".to_string()),
      Token::Word("a".to_string()),
      Token::Comma,
      Token::Word("10".to_string()),
      Token::Newline,
    ];
    let parser = Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![Statement::Instruction(Operation::Decrement(
        Address::Register(Reg::A),
      ))])
    );
  }

  #[test]
  fn two_operations() {
    let tokens = vec![
      Token::Word("nop".to_string()),
      Token::Newline,
      Token::Word("nop".to_string()),
      Token::Newline,
    ];
    let parser = Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Statement::Instruction(Operation::Nop),
        Statement::Instruction(Operation::Nop),
      ])
    );
  }
}
