use ast::*;
use parser::Parser;
use std::collections::HashMap;

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
					BinOp::NEquals => apply_compare(left, right, |first, second| first != second),
				}
			},
			Expression::BrackOp(ref brackop) => {
				self.visit_brackets(brackop)
			},
			Expression::UnaryOp(ref op_expr) => {
				let val = self.visit_expr(&op_expr.val);
				match (&op_expr.op, val.clone()) {
					(&UnaryOp::Plus, Ok(Primitive::Integer(_))) => val,
					(&UnaryOp::Minus, Ok(Primitive::Integer(num))) => Ok(Primitive::Integer(-num)),
					(&UnaryOp::Not, Ok(Primitive::Bool(bool_val))) => Ok(Primitive::Bool(!bool_val)),
					_ => Err(String::from("invalid unary operation"))
				}
			},
			Expression::Variable(ref vname) => {
				match self.vmap.get(vname) {
					Some(val) => Ok(val.clone()),
					None => Err(format!("No variable named '{}' defined in scope", vname))
				}
			}
			Expression::Value(ref prim) => {
				let mut prim = prim.clone();
				if let Primitive::Array(ref mut list) = prim {
					self.expand_list(list);
				}
				Ok(prim)
			},
			_ => Ok(Primitive::Empty)
		}
	}

	fn expand_list(&mut self, list: &mut List) {
		let mut range_found = (false, vec![]);
		for elem in &mut list.values {
			let mut expression = Expression::Empty;
			match *elem {
				ListElem::Value(ref expr) => expression = expr.clone(),
				ListElem::Range{ref start, ref end, ref step} => {
					range_found = (true, self.expand_range(start, end, step));
				},
				ListElem::SubList(ref mut sublist) => self.expand_list(sublist),
			}
			if let Expression::Empty = expression {}
			else {
				*elem = ListElem::from(self.visit_expr(&expression).unwrap());
			}
		}
		if range_found.0 {
			list.values = range_found.1;
		}
	}

	fn visit_statement(&mut self, statement: &Statement) -> Result<Primitive, String> {
		match *statement {
			Statement::Compound{ref children} => {
				for child in children {
					//println!("{:?}\n", child);
					if let Statement::Term(TermToken::Break) = *child {
						return Ok(Primitive::LTerm(TermToken::Break));
					}
					let result = self.visit(Box::new(child.clone()));
					if let Ok(Primitive::LTerm(TermToken::Break)) = result {
						return result;
					}
				}
				Ok(Primitive::Empty)
			},
			Statement::If(ref if_stmt) => {
				match self.visit_expr(&if_stmt.pred) {
					Ok(Primitive::Bool(value)) => {
						if value {
							self.visit_statement(&if_stmt.conseq)
						} else if if_stmt.alt.is_some() {
							let alt = if_stmt.alt.clone().unwrap();
							self.visit_statement(&alt)
						} else {
							Ok(Primitive::Empty)
						}
					},
					Ok(_) => panic!("expected boolean expression in 'if' statement"),
					Err(e) => panic!(e)
				}
			},
			Statement::While{ref pred, ref conseq} => {
				let pred_val = self.visit_expr(pred);
				while let Ok(Primitive::Bool(value)) = self.visit_expr(pred) {
					if value {
						if let Ok(Primitive::LTerm(TermToken::Break)) = self.visit_statement(conseq) {
							break;
						}
					}
					else { break; }
				}
				if let Ok(Primitive::Bool(_)) = pred_val {} else {
					panic!("expected boolean expression in 'while' statement");
				}
				Ok(Primitive::Empty)
			},
			Statement::Assign{ref var, ref value} => {
				match (var, value) {
					(&Expression::Variable(ref vname), _) => {
						let val = self.visit_expr(value).unwrap();
						self.vmap.insert(vname.clone(), val.clone());
						Ok(val)
					},
					// eventually handle assigning to brackopexpression
					/*
					(&Expression::BrackOpExpression(..), _) => {
					},
					*/
					_ => unreachable!()
				}
			},
			Statement::Print(ref expr) => {
				match self.visit_expr(expr) {
					Ok(val) => print!("{}", val),
					Err(e) => panic!(e),
				};
				Ok(Primitive::Empty)
			},
			Statement::Expr(ref expr) => self.visit_expr(expr),
			_ => Ok(Primitive::Empty)
		}
	}

	// EVENTUALLY have this function be 'follow_brackets' and return a mutable reference
	// to the stored Primitive in the symbol table
	// i.e. visit_brackets(&mut self, expr: &BrackOpExpression) -> Result<&mut Primitive, String>
	fn visit_brackets(&mut self, expr: &BrackOpExpression) -> Result<Primitive, String> {

	  	// TODO: convert closure to macro
		let indices = expr.indices.clone()
		    .into_iter()
		    .map(|ind| match self.visit_expr(&ind) {
				Ok(Primitive::Integer(val)) => val as usize,
				Ok(other) => panic!("invalid index: expected integer, got {:?}", other),
				Err(e) => panic!(e)
			}).collect::<Vec<_>>();

		let list_elem: Vec<ListElem>;
		{
			let stored_list = try!(self.vmap.get_mut(&expr.var)
								  .ok_or("variable does not exist in this scope"));

			let elem = List::get_mut_at(stored_list, &indices);
			list_elem = elem.unwrap().clone();
		}

		let index = *indices.last().unwrap();

		match list_elem[index] {
			ListElem::Value(ref expr) => self.visit_expr(expr),
			ListElem::SubList(ref list) => Ok(Primitive::Array(list.clone())),
			ListElem::Range{..} => unreachable!()
		}
	}

	fn expand_range(&mut self, start: &Expression, end: &Expression, step: &Option<Expression>) -> Vec<ListElem> {
		// TODO: refactor below into a macro
		let start = match self.visit_expr(start) {
			Ok(Primitive::Integer(val)) => val,
			_ => panic!("expected integer value in range expression")
		};
		let end = match self.visit_expr(end) {
			Ok(Primitive::Integer(val)) => val,
			_ => panic!("expected integer value in range expression")
		};
		let step = match *step {
			Some(ref step_expr) => {
				if let Ok(Primitive::Integer(val)) = self.visit_expr(step_expr) {
					val as usize
				} else {
					panic!("expected integer value as range step")
				}
			},
			None => 1
		};

		let range: Box<Iterator<Item = i32>> = {
			if start > end { 
				Box::new((end..start+1).rev())
			}
			else { Box::new(start..end) }
		};

		// convert range into a vector of Primitives
		range.enumerate()
			 .filter(|i| i.0 % step == 0)
			 .map(|tup| ListElem::Value(Expression::Value(Primitive::Integer(tup.1))))
			 .collect::<Vec<_>>()
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