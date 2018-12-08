use crate::asm::lexer::Token;
use lazy_static::lazy_static;
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
    use crate::asm::lexer::Token::*;
    use crate::asm::ll1::Symbol::*;
    use crate::asm::ll1::Terminal::*;
    grammar!(
      Program             := [ term!(Epsilon) ]
                             [ Statement tkn!(Newline) Program ]
      Statement           := [ Label MaybeInstruction ]
                             [ Instruction ]
                             [ Directive ]
      Directive           := [ tkn!(Dot) word!("bank") term!(Alphanumeric) ]
      Label               := [ term!(Alphanumeric) tkn!(Colon) ]

      // Operands
      Operand             := [ Register ]
                             [ tkn!(LeftParens) term!(Number) tkn!(RightParens) ]
                             [ Value ]
      Register            := [ word!("a") ] [ word!("b") ] [ word!("c") ]
                             [ word!("d") ] [ word!("e") ] [ word!("f") ]
                             [ word!("af") ] [ word!("bc") ] [ word!("de") ]
                             [ word!("hl") ] [ word!("sp") ] [ word!("pc") ]
      Value               := [ Constant ] [ Identifier ]
      Constant            := [ term!(Number) ]
      Identifier          := [ term!(Alphanumeric) ]

      // Conditions
      Condition           := [ word!("nz") ] [ word!("z") ]
                             [ word!("nc") ] [ word!("c") ]
      MaybeConditionOnly  := [ term!(Epsilon) ] [ Condition ]
      MaybeCondition      := [ term!(Epsilon) ] [ Condition tkn!(Comma) ]

      // Instructions
      Instruction         := [ Adc ] [ Add ] [ And ] [ Cp ] [ Dec ] [ Inc ]
                             [ Or ] [ Sbc ] [ Sub ] [ Xor ]
                             [ Bit ] [ Res ] [ Set ] [ Swap ]
                             [ Rl ] [ Rla ] [ Rlc ] [ Rlca ] [ Rr ] [ Rra ]
                             [ Rrc ] [ Rrca ] [ Sla ] [ Sra ] [ Srl ]
                             [ Ld ] [ Ldh ] [ Ldi ] [ Ldd ]
                             [ Call ] [ Jp ] [ Jr ] [ Ret ] [ Reti ] [ Rst ]
                             [ Pop ] [ Push ]
                             [ Ccf ] [ Cpl ] [ Daa ] [ Di ] [ Ei ] [ Halt ]
                             [ Nop ] [ Scf ] [ Stop ]
      MaybeInstruction    := [ term!(Epsilon) ]
                             [ Instruction ]

      // Arithmetic and logic
      Adc                 := [ word!("adc") Operand tkn!(Comma) Operand ]
      Add                 := [ word!("add") Operand tkn!(Comma) Operand ]
      And                 := [ word!("and") Operand ]
      Cp                  := [ word!("cp") Operand ]
      Dec                 := [ word!("dec") Operand ]
      Inc                 := [ word!("inc") Operand ]
      Or                  := [ word!("or") Operand ]
      Sbc                 := [ word!("sbc") Operand tkn!(Comma) Operand ]
      Sub                 := [ word!("sub") Operand ]
      Xor                 := [ word!("xor") Operand ]

      // Bit operations
      Bit                 := [ word!("bit") term!(Number) tkn!(Comma) Operand ]
      Res                 := [ word!("res") term!(Number) tkn!(Comma) Operand ]
      Set                 := [ word!("set") term!(Number) tkn!(Comma) Operand ]
      Swap                := [ word!("swap") Operand ]

      // Shift and rotate operations
      Rl                  := [ word!("rl") Operand ]
      Rla                 := [ word!("rla") ]
      Rlc                 := [ word!("rlc") Operand ]
      Rlca                := [ word!("rlca") ]
      Rr                  := [ word!("rr") Operand ]
      Rra                 := [ word!("rra") ]
      Rrc                 := [ word!("rrc") Operand ]
      Rrca                := [ word!("rrca") ]
      Sla                 := [ word!("sla") Operand ]
      Sra                 := [ word!("sra") Operand ]
      Srl                 := [ word!("srl") Operand ]

      // Load operations
      Ld                  := [ word!("ld") Operand tkn!(Comma) Operand ]
      Ldh                 := [ word!("ldh") Operand tkn!(Comma) Operand ]
      Ldi                 := [ word!("ldi") Operand tkn!(Comma) Operand ]
      Ldd                 := [ word!("ldd") Operand tkn!(Comma) Operand ]

      // Jump and call operations
      Call                := [ word!("call") MaybeCondition Value ]
      Jp                  := [ word!("jp") MaybeCondition Value ]
      Jr                  := [ word!("jr") MaybeCondition Value ]
      Ret                 := [ word!("ret") MaybeConditionOnly ]
      Reti                := [ word!("reti") ]
      Rst                 := [ word!("rst") term!(Number) ]

      // Stack operations
      Pop                 := [ word!("pop") Operand ]
      Push                := [ word!("push") Operand ]

      // Misc operations
      Ccf                 := [ word!("ccf") ]
      Cpl                 := [ word!("cpl") ]
      Daa                 := [ word!("daa") ]
      Di                  := [ word!("di") ]
      Ei                  := [ word!("ei") ]
      Halt                := [ word!("halt") ]
      Nop                 := [ word!("nop") ]
      Scf                 := [ word!("scf") ]
      Stop                := [ word!("stop") ]
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
  Operand,
  Instruction,
  Register,
  Value,
  Constant,
  Identifier,
  MaybeConditionOnly,
  MaybeCondition,
  Condition,
  Terminal(Terminal),

  // Arithmetic and logic
  Adc,
  Add,
  And,
  Cp,
  Dec,
  Inc,
  Or,
  Sbc,
  Sub,
  Xor,

  // Bit operations
  Bit,
  Res,
  Set,
  Swap,

  // Shift and rotate operations
  Rl,
  Rla,
  Rlc,
  Rlca,
  Rr,
  Rra,
  Rrc,
  Rrca,
  Sla,
  Sra,
  Srl,

  // Load operations
  Ld,
  Ldh,
  Ldi,
  Ldd,

  // Jump and call operations
  Call,
  Jp,
  Jr,
  Ret,
  Reti,
  Rst,

  // Stack operations
  Pop,
  Push,

  // Misc operations
  Ccf,
  Cpl,
  Daa,
  Di,
  Ei,
  Halt,
  Nop,
  Scf,
  Stop,
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
            "LL1: Expected Alphanumeric, got {:?}",
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
            "LL1: Expected Numeric, got {:?}",
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
            "LL1: Expected {:?}, got {:?}",
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
            "LL1: Expected end of input, got {:?}",
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
  use crate::asm::lexer::Token::*;
  use crate::asm::ll1::Symbol::*;
  use crate::asm::ll1::Terminal::*;

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
        Nop,
        Terminal(Token(Word("nop".to_string()))),
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
        Dec,
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
  fn number() {
    let tokens = vec![Word("rst".to_string()), Word("2".to_string()), Newline];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Rst,
        Terminal(Token(Word("rst".to_string()))),
        Terminal(Token(Word("2".to_string()))),
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn number_and_operand() {
    let tokens = vec![
      Word("set".to_string()),
      Word("2".to_string()),
      Comma,
      Word("a".to_string()),
      Newline,
    ];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Set,
        Terminal(Token(Word("set".to_string()))),
        Terminal(Token(Word("2".to_string()))),
        Terminal(Token(Comma)),
        Operand,
        Register,
        Terminal(Token(Word("a".to_string()))),
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn maybe_condition_only_not_present() {
    let tokens = vec![Word("ret".to_string()), Newline];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Ret,
        Terminal(Token(Word("ret".to_string()))),
        MaybeConditionOnly,
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn maybe_condition_only_present() {
    let tokens = vec![Word("ret".to_string()), Word("nz".to_string()), Newline];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Ret,
        Terminal(Token(Word("ret".to_string()))),
        MaybeConditionOnly,
        Condition,
        Terminal(Token(Word("nz".to_string()))),
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn maybe_condition_not_present() {
    let tokens = vec![Word("jr".to_string()), Word("123".to_string()), Newline];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Jr,
        Terminal(Token(Word("jr".to_string()))),
        MaybeCondition,
        Value,
        Constant,
        Terminal(Token(Word("123".to_string()))),
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn maybe_condition_present() {
    let tokens = vec![
      Word("jr".to_string()),
      Word("nz".to_string()),
      Comma,
      Word("123".to_string()),
      Newline,
    ];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Jr,
        Terminal(Token(Word("jr".to_string()))),
        MaybeCondition,
        Condition,
        Terminal(Token(Word("nz".to_string()))),
        Terminal(Token(Comma)),
        Value,
        Constant,
        Terminal(Token(Word("123".to_string()))),
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
        Ld,
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
        Ld,
        Terminal(Token(Word("ld".to_string()))),
        Operand,
        Register,
        Terminal(Token(Word("a".to_string()))),
        Terminal(Token(Comma)),
        Operand,
        Value,
        Constant,
        Terminal(Token(Word("10".to_string()))),
        Terminal(Token(Newline)),
        Program,
      ])
    );
  }

  #[test]
  fn identifier() {
    let tokens = vec![Word("jp".to_string()), Word("abcd".to_string()), Newline];
    let parser = Ll1Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![
        Program,
        Statement,
        Instruction,
        Jp,
        Terminal(Token(Word("jp".to_string()))),
        MaybeCondition,
        Value,
        Identifier,
        Terminal(Token(Word("abcd".to_string()))),
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
        Nop,
        Terminal(Token(Word("nop".to_string()))),
        Terminal(Token(Newline)),
        Program,
        Statement,
        Instruction,
        Nop,
        Terminal(Token(Word("nop".to_string()))),
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
        Nop,
        Terminal(Token(Word("nop".to_string()))),
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
