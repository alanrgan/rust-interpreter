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

impl Visitable for Expression {
	fn visit(&self) -> Result<Primitive, String> {
		match *self {
			Expression::BinOp(ref bexpr) => {
				let left = bexpr.clone().left
							    .visit()
							    .unwrap();
				let right = bexpr.clone().right
								.visit()
								.unwrap();
				match bexpr.op {
					BinOp::Plus => Ok(left + right),
					BinOp::Minus => Ok(left - right),
					BinOp::Mult => Ok(left * right),
					BinOp::Div => Ok(left / right),
					BinOp::And => apply_logical(left, right, |first, second| first && second),
					BinOp::Or => apply_logical(left, right, |first, second| first || second),
					BinOp::GThan => apply_compare(left, right, |first, second| first > second),
					BinOp::GTEquals => apply_compare(left, right, |first, second| first >= second),
					BinOp::LThan => apply_compare(left, right, |first, second| first < second),
					BinOp::LTEquals => apply_compare(left, right, |first, second| first <= second),
					BinOp::DEquals => apply_compare(left, right, |first, second| first == second),
				}
			},
			Expression::Value(ref prim) => Ok(prim.clone()),
			_ => Ok(Primitive::Empty)
		}
	}
}

impl Visitable for Statement {
	fn visit(&self) -> Result<Primitive, String> {
		//println!("visiting {:?}", self);
		match *self {
			Statement::Compound{ref children} => {
				for child in children {
					let result = child.visit().unwrap();
					println!("{}", result);
				}
				Ok(Primitive::Empty)
			},
			Statement::Assign{ref var, ref value} => {
				Ok(Primitive::Empty)
			},
			Statement::Expr(ref expr) => {
				expr.visit()
			},
			_ => Ok(Primitive::Empty)
		}
	}
}