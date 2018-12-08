use yarg::asm::assembler;
use yarg::asm::parser::Parser;
use std::env;
use std::fs;
use std::io::Write;
use std::path::Path;

fn main() -> Result<(), String> {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    println!("Must specify input file")
  }
  let path = Path::new(args.last().unwrap());
  let input = fs::read_to_string(path)
    .map_err(|e| format!("Error reading {}: {}", path.to_str().unwrap(), e))?;
  let parser = Parser::new(input.chars());
  let code = assembler::assemble(parser.parse()?)?;
  let mut file = fs::File::create(path.with_extension("gb"))
    .map_err(|e| format!("Error opening file for write: {}", e))?;
  file.write_all(&code).unwrap();
  Ok(())
}
