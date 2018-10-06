use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
  Newline,
  LeftParens,
  RightParens,
  Comma,
  Dot,
  Colon,
  LineCommentStart,
  CommentStart,
  CommentEnd,
  Word(String),
}

impl Token {
  pub fn is_alphanumeric_word(&self) -> bool {
    match self {
      Token::Word(_) => true,
      _ => false,
    }
  }

  pub fn is_numeric_word(&self) -> bool {
    match self {
      Token::Word(_) => true,
      _ => false,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
  iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
  pub fn new(input: Peekable<Chars<'a>>) -> Lexer<'a> {
    Lexer { iter: input }
  }
}

fn peeking_take_while<F: Copy>(start: char, pred: F, iter: &mut Peekable<Chars>) -> String
where
  F: Fn(&char) -> bool,
{
  let mut ret = start.to_string();
  loop {
    let take = (*iter).peek().map(pred).unwrap_or(false);
    if take {
      // We use unwrap because if take is true and next() returns None then
      // something has gone seriously wrong anyway.
      ret.push((*iter).next().unwrap());
    } else {
      return ret;
    }
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Token;

  fn next(&mut self) -> Option<Token> {
    match self.iter.next()? {
      '\n' => Some(Token::Newline),
      '(' => Some(Token::LeftParens),
      ')' => Some(Token::RightParens),
      ',' => Some(Token::Comma),
      '.' => Some(Token::Dot),
      ':' => Some(Token::Colon),
      ';' => Some(Token::LineCommentStart),
      '/' => match self.iter.next()? {
        '/' => Some(Token::LineCommentStart),
        '*' => Some(Token::CommentStart),
        _ => None,
      },
      '*' => {
        if self.iter.next()? == '/' {
          Some(Token::CommentEnd)
        } else {
          None
        }
      }
      c if c.is_whitespace() => {
        peeking_take_while(c, |c| c.is_whitespace(), &mut self.iter);
        self.next()
      }
      c if c.is_alphanumeric() => Some(Token::Word(peeking_take_while(
        c,
        |c| c.is_alphanumeric() || *c == '_',
        &mut self.iter,
      ))),
      _ => None,
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn symbols() {
    let s = "(),:\n.;/**///".to_string();
    let mut lexer = Lexer::new(s.chars().peekable());
    assert_eq!(lexer.next(), Some(Token::LeftParens));
    assert_eq!(lexer.next(), Some(Token::RightParens));
    assert_eq!(lexer.next(), Some(Token::Comma));
    assert_eq!(lexer.next(), Some(Token::Colon));
    assert_eq!(lexer.next(), Some(Token::Newline));
    assert_eq!(lexer.next(), Some(Token::Dot));
    assert_eq!(lexer.next(), Some(Token::LineCommentStart));
    assert_eq!(lexer.next(), Some(Token::CommentStart));
    assert_eq!(lexer.next(), Some(Token::CommentEnd));
    assert_eq!(lexer.next(), Some(Token::LineCommentStart));
  }

  #[test]
  fn whitespace() {
    let s = "    \t\t  .".to_string();
    let mut lexer = Lexer::new(s.chars().peekable());
    assert_eq!(lexer.next(), Some(Token::Dot));
  }

  #[test]
  fn words() {
    let s = "alpha 1234 alpha_1234".to_string();
    let mut lexer = Lexer::new(s.chars().peekable());
    assert_eq!(lexer.next(), Some(Token::Word("alpha".to_string())));
    assert_eq!(lexer.next(), Some(Token::Word("1234".to_string())));
    assert_eq!(lexer.next(), Some(Token::Word("alpha_1234".to_string())));
  }

  #[test]
  fn underscore_first() {
    let s = "_alpha".to_string();
    let mut lexer = Lexer::new(s.chars().peekable());
    assert_eq!(lexer.next(), None);
  }

  #[test]
  fn unused_symbols() {
    let s = "+=-!@#$%^&*`~<>?'\"[]{}\\|*".to_string();
    let mut lexer = Lexer::new(s.chars().peekable());
    for _ in s.chars() {
      assert_eq!(lexer.next(), None);
    }
  }
}
