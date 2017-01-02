#[macro_use]
mod lib;
mod ast;
mod lexer;
mod parser;
mod interpreter;

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;
use std::env;
use std::fs::File;
use std::io::Read;

fn usage() {
	println!("usage: ./interpreter FILE");
}

fn main() {
	let args: Vec<String> = env::args().collect();

	if args.len() != 2 {
		usage();
		return;
	}

	let filename = &args[1].clone();
	let mut f = File::open(filename).unwrap();
	let mut input = String::new();
	f.read_to_string(&mut input).unwrap();

	let mut lex = Lexer::new(&input);

	let mut parser = Parser::new(lex);
	let mut interpreter = Interpreter::new(parser);
	interpreter.interpret();
}