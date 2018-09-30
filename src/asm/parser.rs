use lexer::Token;
use operation::Operation;
use std::collections::HashMap;
use std::collections::HashSet;

/* Grammar

Program -> Statement | newline Program.
Statement -> Label MaybeInstruction | Instruction | Directive.
MaybeInstruction -> | Instruction .
Directive -> dot section identifier.
Label -> identifier colon.
Operand -> register | leftparens number rightparens | number.
Opcode0 -> nop.
Opcode1 -> dec.
Opcode2 -> ld.
Instruction -> Opcode0 | Opcode1 Operand | Opcode2 Operand comma Operand.

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
}

impl<I: Iterator<Item = Token>> Parser<I> {
  pub fn new(token_iter: I) -> Parser<I> {
    let mut table = HashMap::new();
    Parser { token_iter, table }
  }

  fn generate_table(&mut self) -> Result<(), String> {
    use lexer::Token::*;
    use parser::Symbol::*;
    use parser::Terminal::*;
    self.table = ParseTableBuilder::new()
      .add_rule(Program, vec![Statement])
      .add_rule(Program, vec![Terminal(Token(Newline)), Program])
      .add_rule(Statement, vec![Label, MaybeInstruction])
      .add_rule(Statement, vec![Instruction])
      .add_rule(Statement, vec![Directive])
      .add_rule(MaybeInstruction, vec![Epsilon])
      .add_rule(MaybeInstruction, vec![Instruction])
      .add_rule(
        Directive,
        vec![
          Terminal(Token(Dot)),
          Terminal(Token(Word("section".to_string()))),
        ],
      )
      .add_rule(Label, vec![Terminal(Alphanumeric), Terminal(Token(Colon))])
      .add_rule(Operand, vec![Terminal(Alphanumeric)])
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
      .add_rule(Opcode1, vec![Terminal(Token(Word("ld".to_string())))])
      .add_rule(Instruction, vec![Opcode0])
      .add_rule(Instruction, vec![Opcode1, Operand])
      .add_rule(
        Instruction,
        vec![Opcode2, Operand, Terminal(Token(Comma)), Operand],
      )
      .build()?;
    Ok(())
  }

  fn table_lookup(&self, symbol: &Symbol, token: &Token) -> Option<&Vec<Symbol>> {
    let row = self.table.get(&symbol)?;
    match token {
      w @ Token::Word(_) => {
        // Try looking up the word as a keyword and fall back on Alphanumeric.
        row
          .get(&Terminal::Token(w.clone()))
          .or(row.get(&Terminal::Alphanumeric))
      }
      t => row.get(&Terminal::Token(t.clone())),
    }
  }

  pub fn parse(&mut self) -> Result<Vec<Statement>, String> {
    self.generate_table()?;
    let mut program: Vec<Statement> = Vec::new();
    let mut stack: Vec<Symbol> = vec![Symbol::Program];
    let mut token = self.token_iter.next().ok_or("Failed to parse".to_string())?;
    while !stack.is_empty() {
      let top = stack
        .pop()
        .ok_or("Unexpected empty parse stack".to_string())?
        .clone();
      match &top {
        Symbol::Terminal(Terminal::Token(term)) if *term == token => {
          token = self.token_iter.next().ok_or("Failed to parse".to_string())?;
          continue;
        }
        Symbol::Terminal(Terminal::Alphanumeric) if token.is_alphanumeric_word() => {
          token = self.token_iter.next().ok_or("Failed to parse".to_string())?;
          continue;
        }
        _ => (),
      }
      let symbols = self
        .table_lookup(&top, &token)
        .ok_or("Failed to parse".to_string())?;
      for s in symbols.iter().rev() {
        stack.push(s.clone());
      }
    }
    Ok(program)
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
  Terminal(Terminal),
  Epsilon,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Terminal {
  Token(Token),
  Alphanumeric,
  Number,
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
    let mut first: HashMap<Symbol, HashSet<Terminal>> = HashMap::new();

    // Initialise first with {symbol} for terminals, or {} for non-terminals.
    for symbol in self.rules.iter().flat_map(|(s, r)| r.iter()) {
      first.entry(symbol.clone()).or_insert_with(|| match symbol {
        Symbol::Terminal(term) => [term.clone()].iter().cloned().collect(),
        _ => HashSet::new(),
      });
    }

    'fixed_point: loop {
      let mut updated = false;
      for (nonterminal, rule) in self.rules.clone() {
        let f = first.get(rule.first().unwrap()).unwrap().clone();
        for s in first.get(rule.first().unwrap()).unwrap().clone() {
          updated |= first.get_mut(&nonterminal).unwrap().insert(s);
        }
      }
      if !updated {
        break 'fixed_point;
      }
    }

    let mut table = HashMap::new();
    for (symbol, rule) in self.rules {
      for f in first.get(rule.first().unwrap()).unwrap() {
        let entry = table.entry(symbol.clone()).or_insert(HashMap::new());
        if entry.get(f).is_some() {
          return Err("Table entry contains two rules".to_string());
        }
        entry.insert(f.clone(), rule.clone());
      }
    }

    return Ok(table);
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn label() {
    let tokens = vec![Token::Word("nop".to_string()), Token::Newline];
    let mut parser = Parser::new(tokens.into_iter());
    assert_eq!(
      parser.parse(),
      Ok(vec![Statement::Instruction(Operation::Nop)])
    );
  }
}
