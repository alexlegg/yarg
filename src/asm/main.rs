mod lexer;
mod ll1;
mod operation;
mod parser;

#[macro_use]
extern crate lazy_static;

use lexer::Lexer;
use parser::Parser;
use std::env;
use std::fs;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    println!("Must specify input file")
  }
  if let Some(filename) = args.last() {
    if let Ok(input) = fs::read_to_string(filename) {
      let parser = Parser::new(input.chars());
      println!("{:?}", parser.parse());
    }
  }
  println!("Failed to parse");
}
