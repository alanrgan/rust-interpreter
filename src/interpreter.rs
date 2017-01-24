use ast::*;
use parser::Parser;
use std::fmt::{Formatter, Debug, Error};
use std::collections::HashMap;

pub struct Interpreter<'a> {
	parser: Parser<'a>,
	// TODO: change to <String, Value>
	pub vmap: HashMap<String, Option<Value>>, // map variable name to value
	// This HashMap is used to check if requested type/class members are valid
	pub types: HashMap<String, TypedItem>,
	// scope stack, last elem is the top-most
	pub envs: Vec<Env>
}

impl<'a> Interpreter<'a> {
	pub fn new(parser: Parser) -> Interpreter {
		let mut types = HashMap::new();
		types.insert("bool".to_string(), TypedItem::from(Primitive::Bool(true)));
		types.insert("str".to_string(), TypedItem::from(Primitive::Str("".to_string())));
		types.insert("int".to_string(), TypedItem::from(Primitive::Integer(0)));
		types.insert("list".to_string(), TypedItem::from(Primitive::Array(List::from(vec![0]))));
		let evec: Vec<Env> = vec![Env::new(-1, 0)];
		Interpreter { parser: parser, vmap: HashMap::new(), types: types, envs: evec }
	}

	pub fn interpret(&mut self) -> TypedItem {
		let parsed_input = self.parser.parse();
		self.visit(parsed_input).unwrap()
	}

	pub fn visit(&mut self, node: Box<Visitable>) -> Result<TypedItem, String> {
		match node.node_type() {
			NodeType::Expression => self.visit_expr(&node.as_expression().unwrap()),
			NodeType::Statement => self.visit_statement(&node.as_statement().unwrap())
		}
	}

	// need to do visiting as part of interpeter so that a HashMap can be kept
	fn visit_expr(&mut self, expr: &Expression) -> Result<TypedItem, String> {
		match *expr {
			Expression::BinOp(ref bexpr) => {
				let left = self.visit_expr(&bexpr.left).unwrap();
				let right = self.visit_expr(&bexpr.right).unwrap();
				match bexpr.op {
					// TODO: convert the following to macros if possible
					BinOp::Plus => {
						match (left, right) {
							(TypedItem::Primitive(l), TypedItem::Primitive(r)) => {
								Ok((l + r).into())
							},
							(TypedItem::Object(l), TypedItem::Object(r)) => {
								// apply custom add func here
								Ok(TypedItem::empty())
							},
							_ => Err("Use of undefined add operator".to_string())
						}
					},
					BinOp::Minus => {
						match (left, right) {
							(TypedItem::Primitive(l), TypedItem::Primitive(r)) => {
								Ok((l-r).into())
							},
							(TypedItem::Object(l), TypedItem::Object(r)) => {
								// TODO
								Ok(TypedItem::empty())
							},
							_ => Err("Use of undefined subtraction operator".to_string())
						}
					},
					BinOp::Mult => {
						match (left, right) {
							(TypedItem::Primitive(l), TypedItem::Primitive(r)) => {
								Ok((l*r).into())
							},
							(TypedItem::Object(l), TypedItem::Object(r)) => {
								Ok(TypedItem::empty())
							},
							_ => Err("Use of undefined mult operator".to_string())
						}
					},
					BinOp::Div => {
						match (left, right) {
							(TypedItem::Primitive(l), TypedItem::Primitive(r)) => {
								Ok((l/r).into())
							},
							(TypedItem::Object(l), TypedItem::Object(r)) => {
								Ok(TypedItem::empty())
							},
							_ => Err("Use of undefined div operator".to_string())
						}
					},
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
					(&UnaryOp::Plus, Ok(TypedItem::Primitive(Primitive::Integer(_)))) => val,
					(&UnaryOp::Minus, Ok(TypedItem::Primitive(Primitive::Integer(num)))) => {
						Ok(Primitive::Integer(-num).into())
					},
					(&UnaryOp::Not, Ok(TypedItem::Primitive(Primitive::Bool(bool_val)))) => {
						Ok(Primitive::Bool(!bool_val).into())
					},
					_ => Err(String::from("invalid unary operation"))
				}
			},
			Expression::Variable(ref vname) => {
				self.vmap.get(vname)
						 .cloned()
						 .ok_or_else(|| format!("no var named {}", vname.clone()))
						 .map(|val| TypedItem::Value(Box::new(
						 	val.expect(&format!("Variable '{}' not initialized!", vname)))))
			}
			Expression::Value(ref prim) => {
				let mut prim = prim.clone();
				let _ = prim.unpack_mut::<List>().map(|list| self.expand_list(list));
				Ok(prim)
			},
			_ => Ok((Primitive::Empty).into())
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
			match expression {
				Expression::Empty => {},
				_ => *elem = ListElem::from(self.visit_expr(&expression).unwrap())
			};
		}
		if range_found.0 {
			let length = range_found.1.len();
			list.values = range_found.1;
			list.length = length;
		}
	}

	#[allow(single_match)]
	fn visit_statement(&mut self, statement: &Statement) -> Result<TypedItem, String> {
		match *statement {
			Statement::Compound{ref children} => {
				for child in children {
					//println!("{:?}\n", child);
					if let Statement::Term(TermToken::Break) = *child {
						return Ok((Primitive::LTerm(TermToken::Break)).into());
					}
					let result = self.visit(Box::new(child.clone()));
					if result.is_err() { return result }
					if let Ok(TypedItem::Primitive(Primitive::LTerm(TermToken::Break))) = result {
						return result;
					}
				}
				Ok((Primitive::Empty).into())
			},
			Statement::If(ref if_stmt) => {
				self.visit_expr(&if_stmt.pred)
					.and_then(|val| val.unpack::<bool>()
					.map_err(|_| "expected boolean expression in 'if' statement".to_string()))
					.and_then(|value| {
						if value {
							self.visit_statement(&if_stmt.conseq)
						} else {
							if_stmt.alt.clone()
							.map_or(Ok((Primitive::Empty).into()), |alt| self.visit_statement(&alt))
						}
					})
			},
			Statement::While{ref pred, ref conseq} => {
				let pred_val = self.visit_expr(pred);
				while let Ok(Primitive::Bool(value)) = self.visit_expr(pred).unwrap().into_primitive() {
					if value {
						if let Ok(Primitive::LTerm(TermToken::Break)) = self.visit_statement(conseq)
																		.unwrap()
																		.into_primitive()
						{
							break;
						}
					}
					else { break; }
				}
				pred_val.unwrap().unpack::<bool>()
					.map_err(|_| "expected boolean expression in 'while' statement".to_string())
					.unwrap();
				Ok((Primitive::Empty).into())
			},
			Statement::For(ref fs) => {
				if let Expression::Variable(ref vname) = fs.var {
					let list = self.visit_expr(&fs.range)
						.and_then(|r| r.unpack::<List>()
						.map_err(|_| "expected list in for loop".to_string()))
						.unwrap();

					for elem in list.values {
						let prim = match elem {
							ListElem::Value(ref expr) => self.visit_expr(expr).unwrap(),
							ListElem::SubList(ref list) => Primitive::Array(list.clone()).into(),
							_ => unreachable!()
						};
						let val = Value::new(vname.clone(), prim.typename(), Some(prim));
						self.vmap.insert(vname.clone(), Some(val));
						match self.visit_statement(&fs.conseq) {
							Ok(TypedItem::Primitive(Primitive::LTerm(TermToken::Break))) => break,
							_ => {}
						};
					}
					Ok((Primitive::Empty).into())
				} else {
					Err("expected variable name in for loop".to_string())
				}
			},
			Statement::Assign{ref var, ref value} => {
				let val = self.visit_expr(value).unwrap();
				match (var, value) {
					(&Expression::Variable(ref vname), _) => {
						let elem = self.vmap.get_mut(vname)
								   .expect(&format!("Variable '{}' not declared", vname));
					    let value = Value::new(vname.clone(), val.typename(), Some(val.clone()));
					    let expected_type = elem.clone().unwrap().ty_name;
					    if expected_type != val.typename() {
					    	Err(format!("mismatched types: expected {}, found {}, {:?}", expected_type, val.typename(), val))
					    } else {
							*elem = Some(value);
							Ok(val)
						}
					},
					(&Expression::BrackOp(ref brack_expr), _) => {
						let indices = brack_expr.indices.iter()
						    .map(|ind| self.visit_expr(ind)
								.and_then(|val| val.unpack::<i32>()
								.map_err(|_| "invalid index: expected integer".to_string()))
								.map(|val| val as usize)
								.unwrap()
							).collect::<Vec<_>>();

						let stored_list = try!(self.fetch_var_mut(&brack_expr.var));
						let list_elem = List::get_mut_at(stored_list, &indices)
											.expect(&format!("invalid index {}", *indices.last().unwrap()));
						*list_elem = ListElem::from(val.clone());
						Ok(val)
					},
					
					_ => unreachable!()
				}
			},
			Statement::Let(ref lst) => {
				let ty = lst.ty.clone();
				let val = Value::new(lst.vname.clone(), ty.clone(), Some(TypedItem::empty()));
				self.vmap.insert(lst.vname.clone(), val.into());
				let assigned_val = {
					if let Some(ref statement) = lst.assign {
						Some(try!(self.visit_statement(statement)))
					} else {
						None
					}
				};

				Env::set(&mut self.envs, "test".into(), Value::new("".into(), "".into(), None), true);


				if self.types.contains_key(&ty) {
					let val = Value::new(lst.vname.clone(), ty.clone(), assigned_val.clone());
					self.vmap.insert(lst.vname.clone(), val.into());
					Ok(assigned_val.unwrap_or_else(TypedItem::empty))
				} else {
					Err(format!("Type {} is not defined", ty))
				}
			},
			Statement::Define(ref obj) => {
				if self.types.contains_key(&obj.name()) {
					panic!("class '{}' is already defined", obj.name());
				}
				self.types.insert(obj.name(), TypedItem::Object(obj.clone()));
				Ok(TypedItem::empty())
			},
			Statement::Macro(ref mac) => {
				if mac.name == "fail" {
					let result = self.visit(mac.clone().arg);
					if result.is_ok() {
						return Err("fail error: statement did not panic".into())
					}
				}
				Ok(TypedItem::empty())
			}
			Statement::Print(ref expr) => {
				self.visit_expr(expr).map(|val| self.print_item(val)).unwrap();
				Ok(TypedItem::empty())
			},
			Statement::Expr(ref expr) => self.visit_expr(expr),
			_ => Ok(TypedItem::empty())
		}
	}

	fn print_item(&self, t: TypedItem) {
		// allow overriding of print function
		match t {
			TypedItem::Value(v) => {
				self.print_item(v.value.unwrap_or_else(TypedItem::empty))
			},
			_ => print!("{}", t)
		};
	}

	fn visit_brackets(&mut self, expr: &BrackOpExpression) -> Result<TypedItem, String> {
		let indices = expr.indices.iter()
		    .map(|ind| self.visit_expr(ind)
				.and_then(|val| val.unpack::<i32>()
				.map_err(|_| "invalid index: expected integer".to_string()))
				.map(|val| val as usize)
				.unwrap()
			).collect::<Vec<_>>();

		let list_elem = {
			let stored_list = try!(self.fetch_var_mut(&expr.var));
			let elem = List::get_mut_at(stored_list, &indices);
			elem.unwrap().clone()
		};

		match list_elem {
			ListElem::Value(ref expr) => self.visit_expr(expr),
			ListElem::SubList(ref list) => Ok(Primitive::Array(list.clone()).into()),
			ListElem::Range{..} => unreachable!()
		}
	}

	fn expand_range(&mut self, start: &Expression, end: &Expression, step: &Option<Expression>) -> Vec<ListElem> {
		let start = self.visit_expr(start)
						.and_then(|r| r.unpack::<i32>()
						.map_err(|_| "expected integer value in range expression".to_string()))
						.unwrap();

		let end = self.visit_expr(end)
					  .and_then(|r| r.unpack::<i32>()
					  .map_err(|_| "expected integer value in range expression".to_string()))
					  .unwrap();

		let step = step.clone().map_or(1, |step_expr|
					self.visit_expr(&step_expr)
					.and_then(|r| r.unpack::<i32>()
					.map_err(|_| "expected integer value as range step".to_string()))
					.unwrap() as usize);

		let range: Box<Iterator<Item = i32>> = {
			if start > end { 
				Box::new((end..start+1).rev())
			}
			else { Box::new(start..end) }
		};

		// convert range into a vector of Primitives
		range.enumerate()
			 .filter(|i| i.0 % step == 0)
			 .map(|tup| ListElem::Value(Expression::Value(Primitive::Integer(tup.1).into())))
			 .collect::<Vec<_>>()
	}

	fn fetch_var_mut(&mut self, vname: &str) -> Result<&mut Option<Value>, String> {
		self.vmap.get_mut(vname)
				 .ok_or("variable does not exist in this scope".to_string())
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

	fn box_clone(&self) -> Box<Visitable> { Box::new(self.clone()) }

	fn box_fmt(&self, f: &mut Formatter) -> Result<(), Error> { self.fmt(f) }
}

impl Visitable for Statement {
	fn node_type(&self) -> NodeType { NodeType::Statement }

	fn as_expression(self: Box<Self>) -> Option<Expression> { None }

	fn as_statement(self: Box<Self>) -> Option<Statement> { Some(*self) }

	fn box_clone(&self) -> Box<Visitable> { Box::new(self.clone()) }

	fn box_fmt(&self, f: &mut Formatter) -> Result<(), Error> { self.fmt(f) }
}

fn apply_logical<F>(left: TypedItem, right: TypedItem, f: F) -> Result<TypedItem, String>
	where F: Fn(bool, bool) -> bool
{
	match (left, right) {
		(TypedItem::Primitive(Primitive::Bool(first)),
		 TypedItem::Primitive(Primitive::Bool(second))) => {
			Ok(Primitive::Bool(f(first, second)).into())
		},
		_ => Err(String::from("Use of undefined logical operator"))
	}
}

fn apply_compare<F>(left: TypedItem, right: TypedItem, f: F) -> Result<TypedItem, String>
	where F: Fn(i32, i32) -> bool
{
	match (left, right) {
		(TypedItem::Primitive(Primitive::Integer(first)),
		 TypedItem::Primitive(Primitive::Integer(second))) => {
			Ok(Primitive::Bool(f(first, second)).into())
		},
		(TypedItem::Primitive(Primitive::Bool(first)),
		 TypedItem::Primitive(Primitive::Bool(second))) => {
			Ok(Primitive::Bool(first == second).into())
		}
		_ => Err(String::from("User of undefined comparison operation"))
	}
}