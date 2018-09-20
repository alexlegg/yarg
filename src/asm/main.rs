mod lexer;

use lexer::Lexer;
use std::env;
use std::fs;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    println!("Must specify input file")
  }
  if let Some(filename) = args.last() {
    if let Ok(input) = fs::read_to_string(filename) {
      let lexer = Lexer::new(input.chars().peekable());
    }
  }
  println!("Failed to lex");
}
