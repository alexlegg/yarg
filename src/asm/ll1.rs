use lexer::Token;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::iter::Peekable;

macro_rules! term {
  ($x:expr) => {
    Terminal($x)
  };
}

macro_rules! tkn {
  ($x:expr) => {
    Terminal(Token($x))
  };
}

macro_rules! word {
  ($x:expr) => {
    Terminal(Token(Word($x.to_string())))
  };
}

macro_rules! grammar {
  ($($symbol:ident := $( [ $($p:expr) * ] ) *)*) => {{
    ParseTableBuilder::new()
      $($( .add_rule($symbol, vec!($( $p, )*)) )*)*
      .build()
  }}
}

lazy_static! {
  static ref PARSE_TABLE: HashMap<Symbol, HashMap<Terminal, Vec<Symbol>>> = {
    use lexer::Token::*;
    use ll1::Symbol::*;
    use ll1::Terminal::*;
    grammar!(
      Program           := [ term!(Epsilon) ]
                           [ Statement tkn!(Newline) Program ]
      Statement         := [ Label MaybeInstruction ]
                           [ Instruction ]
                           [ Directive ]
      MaybeInstruction  := [ term!(Epsilon) ]
                           [ Instruction ]
      Directive         := [ tkn!(Dot) word!("bank") term!(Alphanumeric) ]
      Label             := [ term!(Alphanumeric) tkn!(Colon) ]
      Opcode            := [ word!("nop") ]
                           [ word!("daa") ]
                           [ word!("cpl") ]
                           [ word!("ccf") ]
                           [ word!("scf") ]
                           [ word!("halt") ]
                           [ word!("stop") ]
                           [ word!("ei") ]
                           [ word!("di") ]
                           [ word!("rlca") ]
                           [ word!("rla") ]
                           [ word!("rrca") ]
                           [ word!("rra") ]
                           [ word!("reti") ]
                           [ word!("inc") ]
                           [ word!("dec") ]
                           [ word!("sub") ]
                           [ word!("and") ]
                           [ word!("xor") ]
                           [ word!("or") ]
                           [ word!("cp") ]
                           [ word!("push") ]
                           [ word!("pop") ]
                           [ word!("ret") ]
                           [ word!("ld") ]
      Operand           := [ Register ]
                           [ tkn!(LeftParens) term!(Number) tkn!(RightParens) ]
                           [ Constant ]
      MaybeOperands     := [ term!(Epsilon) ]
                           [ Operand MaybeOperand ]
      MaybeOperand      := [ term!(Epsilon) ]
                           [ tkn!(Comma) Operand ]
      Instruction       := [ Opcode MaybeOperands ]
      Register          := [ word!("a") ] [ word!("b") ] [ word!("c") ]
                           [ word!("d") ] [ word!("e") ] [ word!("f") ]
                           [ word!("af") ] [ word!("bc") ] [ word!("de") ]
                           [ word!("hl") ] [ word!("sp") ] [ word!("pc") ]
      Constant          := [ term!(Number) ]
    ).unwrap()
  };
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
  Program,
  Statement,
  MaybeInstruction,
  Directive,
  Label,
  Opcode,
  Operand,
  MaybeOperands,
  MaybeOperand,
  Instruction,
  Register,
  Constant,
  Terminal(Terminal),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Terminal {
  Token(Token),
  Alphanumeric,
  Number,
  End,
  Epsilon,
}

#[derive(Debug, Clone)]
pub struct Ll1Parser<I: Iterator<Item = Token>> {
  token_iter: Peekable<I>,
  stack: Vec<Symbol>,
}

impl<I: Iterator<Item = Token>> Ll1Parser<I> {
  pub fn new(token_iter: I) -> Ll1Parser<I> {
    Ll1Parser {
      token_iter: token_iter.peekable(),
      stack: vec![Symbol::Program],
    }
  }

  fn table_lookup(&mut self, symbol: &Symbol) -> Option<Vec<Symbol>> {
    let row = PARSE_TABLE.get(&symbol)?;
    let entry = match self.token_iter.peek() {
      Some(w @ Token::Word(_)) => {
        // Try looking up the word as a keyword and fall back on Alphanumeric/Number.
        row
          .get(&Terminal::Token(w.clone()))
          .or(if w.is_numeric_word() {
            row.get(&Terminal::Number)
          } else {
            row.get(&Terminal::Alphanumeric)
          })
      }
      Some(t) => row.get(&Terminal::Token(t.clone())),
      None => row.get(&Terminal::End),
    };
    entry.map(|v| v.clone())
  }

  pub fn parse(self) -> Result<Vec<Symbol>, String> {
    let mut r = Vec::new();
    for s in self {
      r.push(s?);
    }
    Ok(r)
  }
}

impl<I: Iterator<Item = Token>> Iterator for Ll1Parser<I> {
  type Item = Result<Symbol, String>;

  fn next(&mut self) -> Option<Result<Symbol, String>> {
    match self.stack.pop()? {
      Symbol::Terminal(Terminal::Alphanumeric) => {
        if self
          .token_iter
          .peek()
          .map_or(false, |t| t.is_alphanumeric_word())
        {
          return Some(Ok(Symbol::Terminal(Terminal::Token(
            self.token_iter.next().unwrap(),
          ))));
        } else {
          return Some(Err(format!(
            "Expected Alphanumeric, got {:?}",
            self.token_iter.peek()
          )));
        }
      }
      Symbol::Terminal(Terminal::Number) => {
        if self
          .token_iter
          .peek()
          .map_or(false, |t| t.is_numeric_word())
        {
          return Some(Ok(Symbol::Terminal(Terminal::Token(
            self.token_iter.next().unwrap(),
          ))));
        } else {
          return Some(Err(format!(
            "Expected Numeric, got {:?}",
            self.token_iter.peek()
          )));
        }
      }
      Symbol::Terminal(Terminal::Token(token)) => {
        if self.token_iter.peek().map_or(false, |t| *t == token) {
          self.token_iter.next();
          return Some(Ok(Symbol::Terminal(Terminal::Token(token))));
        } else {
          return Some(Err(format!(
            "Expected {:?}, got {:?}",
            token,
            self.token_iter.peek()
          )));
        }
      }
      Symbol::Terminal(Terminal::End) => {
        if self.token_iter.peek().is_none() {
          return None;
        } else {
          return Some(Err(format!(
            "Expected end of input, got {:?}",
            self.token_iter.peek()
          )));
        }
      }
      top @ _ => {
        let symbols = self.table_lookup(&top);
        if symbols.is_none() {
          return Some(Err(format!("Failed on table lookup: {:?}", top)));
        }
        self
          .stack
          .extend(symbols.unwrap().into_iter().rev().filter(|s| match s {
            Symbol::Terminal(Terminal::Epsilon) => false,
            _ => true,
          }));
        return Some(Ok(top));
      }
    }
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

#[cfg(test)]
mod test {
  use super::*;
  use lexer::Token::*;
  use ll1::Symbol::*;
  use ll1::Terminal::*;

  #[test]
  fn zero_operands() {
    let tokens = vec![Word("nop".to_string()), Newline];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Opcode,
        Terminal(Token(Word("nop".to_string()))),
        MaybeOperands,
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn one_operand() {
    let tokens = vec![Word("dec".to_string()), Word("a".to_string()), Newline];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Opcode,
        Terminal(Token(Word("dec".to_string()))),
        MaybeOperands,
        Operand,
        Register,
        Terminal(Token(Word("a".to_string()))),
        MaybeOperand,
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn two_operands() {
    let tokens = vec![
      Word("ld".to_string()),
      Word("a".to_string()),
      Comma,
      Word("b".to_string()),
      Newline,
    ];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Opcode,
        Terminal(Token(Word("ld".to_string()))),
        MaybeOperands,
        Operand,
        Register,
        Terminal(Token(Word("a".to_string()))),
        MaybeOperand,
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
  fn constant() {
    let tokens = vec![
      Word("ld".to_string()),
      Word("a".to_string()),
      Comma,
      Word("10".to_string()),
      Newline,
    ];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Opcode,
        Terminal(Token(Word("ld".to_string()))),
        MaybeOperands,
        Operand,
        Register,
        Terminal(Token(Word("a".to_string()))),
        MaybeOperand,
        Terminal(Token(Comma)),
        Operand,
        Constant,
        Terminal(Token(Word("10".to_string()))),
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
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Opcode,
        Terminal(Token(Word("nop".to_string()))),
        MaybeOperands,
        Terminal(Token(Newline)),
        Program,
        Statement,
        Instruction,
        Opcode,
        Terminal(Token(Word("nop".to_string()))),
        MaybeOperands,
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn label() {
    let tokens = vec![Word("label".to_string()), Colon, Newline];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Label,
        Terminal(Token(Word("label".to_string()))),
        Terminal(Token(Colon)),
        MaybeInstruction,
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn label_and_instruction() {
    let tokens = vec![
      Word("label".to_string()),
      Colon,
      Word("nop".to_string()),
      Newline,
    ];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Label,
        Terminal(Token(Word("label".to_string()))),
        Terminal(Token(Colon)),
        MaybeInstruction,
        Instruction,
        Opcode,
        Terminal(Token(Word("nop".to_string()))),
        MaybeOperands,
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn directive() {
    let tokens = vec![
      Dot,
      Word("bank".to_string()),
      Word("0".to_string()),
      Newline,
    ];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Directive,
        Terminal(Token(Dot)),
        Terminal(Token(Word("bank".to_string()))),
        Terminal(Token(Word("0".to_string()))),
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }
}
