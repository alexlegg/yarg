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
      Token::Word(s) => {
        if s.starts_with("0x") {
          s[2..].chars().all(|c| c.is_digit(16))
        } else if s.ends_with('h') {
          s[..s.len() - 1].chars().all(|c| c.is_digit(16))
        } else if s.starts_with("0b") {
          s[2..].chars().all(|c| c.is_digit(2))
        } else {
          s.chars().all(|c| c.is_digit(10))
        }
      }
      _ => false,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
  iter: Peekable<CommentStripper<'a>>,
}

#[derive(Debug, Clone)]
struct CommentStripper<'a> {
  iter: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
  pub fn new(input: Chars<'a>) -> Lexer<'a> {
    Lexer {
      iter: CommentStripper::new(input).peekable(),
    }
  }
}

impl<'a> CommentStripper<'a> {
  fn new(input: Chars<'a>) -> CommentStripper<'a> {
    CommentStripper {
      iter: input.peekable(),
    }
  }
}

impl Iterator for CommentStripper<'_> {
  type Item = char;

  fn next(&mut self) -> Option<char> {
    let c = self.iter.next()?;
    if c == '/' {
      let p = self.iter.peek()?;
      if *p == '/' {
        peeking_take_while(c, |c| *c != '\n', &mut self.iter);
        self.iter.next();
        return self.next();
      } else if *p == '*' {
        loop {
          peeking_take_while(c, |c| *c != '*', &mut self.iter);
          self.iter.next();
          if self.iter.next()? == '/' {
            break;
          }
        }
        return self.iter.next();
      }
    } else if c == ';' {
      peeking_take_while(c, |c| *c != '\n', &mut self.iter);
      return self.iter.next();
    }
    Some(c)
  }
}

impl Iterator for Lexer<'_> {
  type Item = Token;

  fn next(&mut self) -> Option<Token> {
    match self.iter.next()? {
      c @ '\n' | c @ '\r' => {
        peeking_take_while(c, |c| c.is_whitespace(), &mut self.iter);
        Some(Token::Newline)
      }
      '(' => Some(Token::LeftParens),
      ')' => Some(Token::RightParens),
      ',' => Some(Token::Comma),
      '.' => Some(Token::Dot),
      ':' => Some(Token::Colon),
      c @ ' ' | c @ '\t' => {
        peeking_take_while(c, |c| *c == ' ' || *c == '\t', &mut self.iter);
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

fn peeking_take_while<F, I>(start: char, pred: F, iter: &mut Peekable<I>) -> String
where
  F: Copy + Fn(&char) -> bool,
  I: Iterator<Item = char>,
{
  let mut ret = start.to_string();
  loop {
    let take = (*iter).peek().map(pred).unwrap_or(false);
    if take {
      ret.push((*iter).next().unwrap());
    } else {
      return ret;
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn symbols() {
    let s = "(),:\n.".to_string();
    let mut lexer = Lexer::new(s.chars());
    assert_eq!(lexer.next(), Some(Token::LeftParens));
    assert_eq!(lexer.next(), Some(Token::RightParens));
    assert_eq!(lexer.next(), Some(Token::Comma));
    assert_eq!(lexer.next(), Some(Token::Colon));
    assert_eq!(lexer.next(), Some(Token::Newline));
    assert_eq!(lexer.next(), Some(Token::Dot));
    assert_eq!(lexer.next(), None);
  }

  #[test]
  fn semicolon_line_comment() {
    let s = ";comment\n".to_string();
    let mut lexer = Lexer::new(s.chars());
    assert_eq!(lexer.next(), Some(Token::Newline));
    assert_eq!(lexer.next(), None);
  }

  #[test]
  fn slashes_line_comment() {
    let s = "//comment\n".to_string();
    let mut lexer = Lexer::new(s.chars());
    assert_eq!(lexer.next(), None);
  }

  #[test]
  fn multiline_comment() {
    let s = "./*\ncomment\n*/.".to_string();
    let mut lexer = Lexer::new(s.chars());
    assert_eq!(lexer.next(), Some(Token::Dot));
    assert_eq!(lexer.next(), Some(Token::Dot));
    assert_eq!(lexer.next(), None);

    // Newline after comment.
    let s = "./*\ncomment\n*/\n.".to_string();
    let mut lexer = Lexer::new(s.chars());
    assert_eq!(lexer.next(), Some(Token::Dot));
    assert_eq!(lexer.next(), Some(Token::Newline));
    assert_eq!(lexer.next(), Some(Token::Dot));
    assert_eq!(lexer.next(), None);

    // Spaces around comment.
    let s = ". /*\ncomment\n*/ .".to_string();
    let mut lexer = Lexer::new(s.chars());
    assert_eq!(lexer.next(), Some(Token::Dot));
    assert_eq!(lexer.next(), Some(Token::Dot));
    assert_eq!(lexer.next(), None);
  }

  #[test]
  fn whitespace() {
    let s = "    \t\t  .".to_string();
    let mut lexer = Lexer::new(s.chars());
    assert_eq!(lexer.next(), Some(Token::Dot));
    assert_eq!(lexer.next(), None);
  }

  #[test]
  fn words() {
    let s = "alpha 1234 alpha_1234".to_string();
    let mut lexer = Lexer::new(s.chars());
    assert_eq!(lexer.next(), Some(Token::Word("alpha".to_string())));
    assert_eq!(lexer.next(), Some(Token::Word("1234".to_string())));
    assert_eq!(lexer.next(), Some(Token::Word("alpha_1234".to_string())));
    assert_eq!(lexer.next(), None);
  }

  #[test]
  fn underscore_first() {
    let s = "_alpha".to_string();
    let mut lexer = Lexer::new(s.chars());
    assert_eq!(lexer.next(), None);
  }

  #[test]
  fn unused_symbols() {
    let s = "+=-!@#$%^&*`~<>?'\"[]{}\\|*".to_string();
    let mut lexer = Lexer::new(s.chars());
    for _ in s.chars() {
      assert_eq!(lexer.next(), None);
    }
  }

  #[test]
  fn instructions() {
    let s = "ld a, 10\ndec a\nnop\n".to_string();
    let mut lexer = Lexer::new(s.chars());
    assert_eq!(lexer.next(), Some(Token::Word("ld".to_string())));
    assert_eq!(lexer.next(), Some(Token::Word("a".to_string())));
    assert_eq!(lexer.next(), Some(Token::Comma));
    assert_eq!(lexer.next(), Some(Token::Word("10".to_string())));
    assert_eq!(lexer.next(), Some(Token::Newline));
    assert_eq!(lexer.next(), Some(Token::Word("dec".to_string())));
    assert_eq!(lexer.next(), Some(Token::Word("a".to_string())));
    assert_eq!(lexer.next(), Some(Token::Newline));
    assert_eq!(lexer.next(), Some(Token::Word("nop".to_string())));
    assert_eq!(lexer.next(), Some(Token::Newline));
    assert_eq!(lexer.next(), None);
  }
}
