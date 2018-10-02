use lexer::Token;
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

#[derive(Debug, Clone)]
pub struct Parser<I: Iterator<Item = Token>> {
  token_iter: I,
  table: HashMap<Symbol, HashMap<Terminal, Vec<Symbol>>>,
  output: VecDeque<Symbol>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
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
  pub fn is_terminal(&self) -> bool {
    match self {
      Symbol::Terminal(_) => true,
      _ => false,
    }
  }

  pub fn is_nonterminal(&self) -> bool {
    !self.is_terminal()
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Terminal {
  Token(Token),
  Alphanumeric,
  Number,
  End,
  Epsilon,
}

impl<I: Iterator<Item = Token>> Parser<I> {
  pub fn new(token_iter: I) -> Parser<I> {
    Parser {
      token_iter,
      table: HashMap::new(),
      output: VecDeque::new(),
    }
  }

  fn generate_table(&mut self) -> Result<(), String> {
    use lexer::Token::*;
    use ll1::Symbol::*;
    use ll1::Terminal::*;
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

  pub fn parse(mut self) -> Result<Vec<Symbol>, String> {
    self.generate_table()?;
    let mut stack: Vec<Symbol> = vec![Symbol::Program];
    let mut token = self.token_iter.next();
    let mut output: Vec<Symbol> = Vec::new();
    while !stack.is_empty() {
      println!("stack: {:?}", stack);
      let top = stack
        .pop()
        .ok_or("Unexpected empty parse stack".to_string())?
        .clone();
      let token_symbol =
        Symbol::Terminal(token.clone().map_or(Terminal::End, |t| Terminal::Token(t)));
      if top == token_symbol {
        output.push(top.clone());
        token = self.token_iter.next();
        continue;
      }
      match &top {
        Symbol::Terminal(Terminal::Alphanumeric) => {
          let t = token.ok_or(format!("Expected Alphanumeric, got end of input"))?;
          if t.is_alphanumeric_word() {
            output.push(top.clone());
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
          output.push(top.clone());
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
    Ok(output)
  }
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
  use lexer::Token::*;
  use ll1::Symbol::*;
  use ll1::Terminal::*;

  #[test]
  fn opcode0() {
    let tokens = vec![Word("nop".to_string()), Newline];
    let parser = Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Opcode0,
        Terminal(Token(Word("nop".to_string()))),
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn opcode1() {
    let tokens = vec![Word("dec".to_string()), Word("a".to_string()), Newline];
    let parser = Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Opcode1,
        Terminal(Token(Word("dec".to_string()))),
        Operand,
        Register,
        Terminal(Token(Word("a".to_string()))),
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn opcode2() {
    let tokens = vec![
      Word("ld".to_string()),
      Word("a".to_string()),
      Comma,
      Word("b".to_string()),
      Newline,
    ];
    let parser = Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Opcode2,
        Terminal(Token(Word("ld".to_string()))),
        Operand,
        Register,
        Terminal(Token(Word("a".to_string()))),
        Terminal(Token(Comma)),
        Operand,
        Register,
        Terminal(Token(Word("b".to_string()))),
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn two_operations() {
    let tokens = vec![
      Word("nop".to_string()),
      Newline,
      Word("nop".to_string()),
      Newline,
    ];
    let parser = Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Opcode0,
        Terminal(Token(Word("nop".to_string()))),
        Terminal(Token(Newline)),
        Program,
        Statement,
        Instruction,
        Opcode0,
        Terminal(Token(Word("nop".to_string()))),
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }
}
