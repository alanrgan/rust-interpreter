use ast::*;
use parser::Parser;

pub struct Interpreter<'a> {
	parser: Parser<'a>
}

impl<'a> Interpreter<'a> {
	pub fn new(parser: Parser) -> Interpreter {
		Interpreter { parser: parser }
	}

	pub fn interpret(&mut self) -> Primitive {
		self.parser.parse().visit().unwrap()
	}
}