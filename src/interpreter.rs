use ast::*;
use parser::Parser;
use std::collections::{HashMap, VecDeque};

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
				self.visit_brackets(brackop, &mut VecDeque::new())
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
			Expression::Value(ref prim) => Ok(prim.clone()),
			_ => Ok(Primitive::Empty)
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

	// BinOp left: variable, BinOp right: index
	fn visit_brackets(&mut self, expr: &BrackOpExpression, index_queue: &mut VecDeque<usize>)
					  -> Result<Primitive, String> {
		let left = {
			if let Expression::BrackOp(ref brackop) = expr.left {
				self.visit_brackets(brackop, index_queue).unwrap()
			} else {
				self.visit_expr(&expr.left).unwrap()
			}
		};
		let right = self.visit_expr(&expr.right).unwrap();
		match (left, right) {
			(Primitive::Array(list), Primitive::Integer(index)) => {
				// destructure varname into a String
				let varname = expr.base_vname.clone();

				// validity of index is bracket depends on if the array represents a list or a range
				// ranges are represented as length-1 lists, but should be allowed to be accessed
				// based on its expanded value
				let range_index = index as usize;
				let index = {
					if let Some(&ListElem::Range{..}) = list.values.get(0) {
						0
					} else {
						index_queue.push_back(index as usize);
						index as usize
					}
				};

				if index >= list.values.len() {
					panic!("array index {} out of bounds", index);
				}

				let list_elem = &list.values[index];

				// once the value is fetched from the array, determine how to convert
				// into a primitive
				match *list_elem {
					ListElem::Value(ref expr) => {
						self.visit_expr(expr)
					},
					ListElem::SubList(ref list) => {
						Ok(Primitive::Array(list.clone()))
					},
					ListElem::Range{ref start, ref end, ref step} => {
						// evaluate start, end and step expressions
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
							None => 1,
						};

						// convert range into a vector of Primitives
						let rng_list = (start..end).enumerate()
												   .filter(|i| i.0 % step == 0)
												   .map(|tup| Primitive::Integer(tup.1))
												   .collect::<Vec<_>>();

						// check if requested index is valid within the range
						if !(range_index >= rng_list.len()) {
							let res = Ok(rng_list[range_index].clone());
							// convert range into a list for performance
							// this should never happen more than once per range.
							// ranges are expanded upon first evaluation
							let updated_list = rng_list.into_iter()
												  .map(|val| ListElem::Value(Expression::Value(val)))
												  .collect::<Vec<_>>();
							{
								let new_list = ListElem::SubList(List::from(updated_list.clone()));
								let stored_list = self.vmap.get_mut(&varname).unwrap();
								List::expand_range(stored_list, new_list, index_queue);
							}

							// remove all temporary arrays from hashmap
							let hmap: HashMap<String, Primitive> = self.vmap.clone().into_iter()
												    .filter(|&(ref k, _)| !k.starts_with("%tmparr"))
												    .collect();
							self.vmap = hmap;
							res
						} else {
							panic!("array index {} out of bounds", range_index)
						}
					},
				}
			},
			_ => { Err(String::from("unsupported use of brackets operator")) }
		}
	}
}

impl List {
	pub fn expand_range(nested_arr: &mut Primitive, range: ListElem, indices: &VecDeque<usize>) {
		// follow all the values in the indices vector
		if let Primitive::Array(ref mut list) = *nested_arr {
			let mut values = &mut list.values;
			List::expand(values, range, indices, 0);
		}
	}

	fn expand(some_vec: &mut Vec<ListElem>, range: ListElem, indices: &VecDeque<usize>, ind: usize) {
		if ind == indices.len()-1 {
			some_vec[indices[ind]] = range;
		} else if let ListElem::SubList(ref mut sublist) = some_vec[indices[ind]] {
			List::expand(&mut sublist.values, range, indices, ind+1);
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