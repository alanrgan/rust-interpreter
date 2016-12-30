#[macro_use]
mod lib;
mod ast;
mod lexer;

use lexer::Lexer;
//use ast::{BinOpNode};

fn main() {
	//let x = BinOpNode::Empty;
	let mut lex = Lexer::new("
		for i = 1..4 {
			var = 123;
		}
	 ");
	while let Some(c) = lex.next_token() {
		println!("{:?}", c)
	}

	hashmap! {"a" => 1};
}