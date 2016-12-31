#[macro_use]
mod lib;
mod ast;
mod lexer;
mod parser;
mod interpreter;

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;
//use ast::{BinOpNode};

fn main() {
	//let x = BinOpNode::Empty;
	let mut lex = Lexer::new("
		{
			true;
			2+2;
			1+4*2/2+7;
			1 > 3 and (2 >= 0);
		}
	 ");

	/*while let Some(c) = lex.next_token() {
		println!("{:?}", c)
	}*/
	let mut parser = Parser::new(lex);
	//parser.parse();
	let mut interpreter = Interpreter::new(parser);
	interpreter.interpret();
}