use ast::*;
use parser::Parser;
use std::collections::HashMap;
use std::any::{Any, TypeId};

pub struct Interpreter<'a> {
	parser: Parser<'a>,
	pub vmap: HashMap<String, Primitive> // map variable name to value
}

impl<'a> Interpreter<'a> {
	pub fn new(parser: Parser) -> Interpreter {
		Interpreter { parser: parser, vmap: HashMap::new() }
	}

	pub fn interpret(&mut self) -> Primitive {
		let parsed_input = self.parser.parse();
		self.visit(parsed_input).unwrap()
	}

	pub fn visit(&mut self, node: Box<Visitable>) -> Result<Primitive, String> {
		match node.node_type() {
			NodeType::Expression => self.visit_expr(&node.as_expression().unwrap()),
			NodeType::Statement => self.visit_statement(&node.as_statement().unwrap())
		}
	}

	// need to do visiting as part of interpeter so that a HashMap can be kept
	fn visit_expr(&mut self, expr: &Expression) -> Result<Primitive, String> {
		match *expr {
			Expression::BinOp(ref bexpr) => {
				let left = self.visit_expr(&bexpr.left).unwrap();
				let right = self.visit_expr(&bexpr.right).unwrap();
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
					BinOp::NEquals => apply_compare(left, right, |first, second| first != second)
				}
			},
			Expression::Variable(ref vname) => {
				match self.vmap.get(vname) {
					Some(val) => Ok(val.clone()),
					None => Err(format!("No variable named '{}' defined in scope", vname))
				}
			}
			Expression::Value(ref prim) => Ok(prim.clone()),
			_ => Ok(Primitive::Empty)
		}
	}

	fn visit_statement(&mut self, statement: &Statement) -> Result<Primitive, String> {
		match *statement {
			Statement::Compound{ref children} => {
				for child in children {
					let result = self.visit(Box::new(child.clone())).unwrap();
					//println!("{}", result);
				}
				Ok(Primitive::Empty)
			},
			Statement::If(ref if_stmt) => {
				match self.visit_expr(&if_stmt.pred) {
					Ok(Primitive::Bool(value)) => {
						if value {
							Ok(self.visit_statement(&if_stmt.conseq).unwrap())
						} else if if_stmt.alt.is_some() {
							let alt = if_stmt.alt.clone().unwrap();
							Ok(self.visit_statement(&alt).unwrap())
						} else {
							Ok(Primitive::Empty)
						}
					},
					Ok(_) => panic!("expected boolean expression in 'if' statement"),
					Err(e) => panic!(e)
				}
				//Ok(Primitive::Empty)
			},
			Statement::Assign{ref var, ref value} => {
				match (var, value) {
					(&Expression::Variable(ref vname), _) => {
						let val = self.visit_expr(value).unwrap();
						self.vmap.insert(vname.clone(), val.clone());
						Ok(val)
					},
					_ => unreachable!()
				}
			},
			Statement::Expr(ref expr) => self.visit_expr(expr),
			_ => Ok(Primitive::Empty)
		}
	}
}

pub enum NodeType {
	Expression,
	Statement
}

impl Visitable for Expression {
	fn node_type(&self) -> NodeType { NodeType::Expression }

	fn as_expression(self: Box<Self>) -> Option<Expression> { Some(*self) }

	fn as_statement(self: Box<Self>) -> Option<Statement> { None }
}

impl Visitable for Statement {
	fn node_type(&self) -> NodeType { NodeType::Statement }

	fn as_expression(self: Box<Self>) -> Option<Expression> { None }

	fn as_statement(self: Box<Self>) -> Option<Statement> { Some(*self) }
}

fn apply_logical<F>(left: Primitive, right: Primitive, f: F) -> Result<Primitive, String>
	where F: Fn(bool, bool) -> bool
{
	match (left, right) {
		(Primitive::Bool(first), Primitive::Bool(second)) => {
			Ok(Primitive::Bool(f(first, second)))
		},
		_ => Err(String::from("Use of undefined logical operator"))
	}
}

fn apply_compare<F>(left: Primitive, right: Primitive, f: F) -> Result<Primitive, String>
	where F: Fn(i32, i32) -> bool
{
	match (left, right) {
		(Primitive::Integer(first), Primitive::Integer(second)) => {
			Ok(Primitive::Bool(f(first, second)))
		},
		(Primitive::Bool(first), Primitive::Bool(second)) => {
			Ok(Primitive::Bool(first == second))
		}
		_ => Err(String::from("User of undefined comparison operation"))
	}
}