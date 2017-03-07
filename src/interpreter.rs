use ast::*;
use parser::Parser;
use std::fmt::{Formatter, Debug, Error};

pub struct Interpreter<'a> {
	parser: Parser<'a>,
	pub envs: ScopeList
}

impl<'a> Interpreter<'a> {
	pub fn new(parser: Parser) -> Interpreter {
		let mut slist = ScopeList::new();
		Env::set_type(&mut slist, "bool".into(), TypedItem::from(Primitive::Bool(true)), false);
		Env::set_type(&mut slist, "str".into(), TypedItem::from(Primitive::Str("".into())), false);
		Env::set_type(&mut slist, "int".into(), TypedItem::from(Primitive::Integer(0)), false);
		Env::set_type(&mut slist, "list".into(), TypedItem::from(Primitive::Array(List::from(vec![0]))), false);
		Interpreter { parser: parser, envs: slist }
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
				let left = self.visit_expr(&bexpr.left);
				let right = self.visit_expr(&bexpr.right);
				if left.is_err() { return left; }
				if right.is_err() { return right; }
				let left = left.unwrap();
				let right = right.unwrap();
				match bexpr.op {
					// TODO: convert the following to macros if possible
					// or 'lift' the operators by passing in closures
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
			Expression::Call{ref name, ref args} => {
				let mut retval = Ok(TypedItem::empty());
				let env = self.envs.current_scope().clone();
				if let Some(func) = env.get_func(name) {
					let (assigns, vnames) = func.match_args(name.clone(), args).unwrap();
					for stmt in assigns {
						let res = self.visit_statement(&stmt);
						if res.is_err() { return res; }
					}
					self.envs.alias(vnames);
					retval = self.visit_statement(&func.conseq);
					self.envs.pop();
					if retval.is_ok() {
						let rtype = retval.clone().unwrap().unwrap_ret().unwrap().typename();
						let func_rtype = func.rtype();
						if rtype != func.rtype() {
							retval = Err(format!("{}: Expected return type {}, got {}",
												 name, func_rtype, rtype));
						}
					}
				} else {
					retval = Err(format!("Function {} is not defined in this scope", name));
				}
				if retval.is_ok() {
					retval.unwrap().unwrap_ret()
				} else { retval }
			},
			Expression::Variable(ref vname) => {
				let res = self.envs.current_scope()
						 .get_var(vname)
						 .cloned()
						 .ok_or_else(|| format!("No var named '{}' found in scope", vname.clone()))
						 .map(|val| val.value);
				match res {
					Ok(Some(val)) => Ok(val),
					Ok(None) => Err(format!("Variable '{}' not initialized!", vname)),
					_ => Err(res.err().unwrap())
				}
			}
			Expression::Value(ref prim) => {
				let mut prim = prim.clone();
				let _ = prim.unpack_mut::<List>().map(|list| self.expand_list(list));
				Ok(prim)
			},
			_ => Ok(TypedItem::empty())
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
				self.envs.extend();
				for child in children {
					//println!("{:?}\n", child);
					match *child {
						Statement::Term(TermToken::Break) => {
							self.envs.pop();
							return Ok((Primitive::LTerm(TermToken::Break)).into());
						},
						Statement::Return{ ref rval } => {
							let expr = if rval.is_some() {
								Ok(TypedItem::from(self.visit_expr(rval.as_ref().unwrap())))
							} else {
								Ok(TypedItem::empty())
							};
							self.envs.pop();
							return expr;
						},
						_ => {}
					}

					let result = self.visit(Box::new(child.clone()));
					if result.is_err() { return result }

					// propagate any returns or breaks upward
					match result {
						Ok(TypedItem::Primitive(Primitive::LTerm(TermToken::Break))) 
						| Ok(TypedItem::RetVal(_)) => {
							self.envs.pop();
							return result
						},
						_ => {}
					}
				}
				self.envs.pop();
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
						Env::set(&mut self.envs, vname.clone(), val, false);
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
			Statement::Assign{ref var, ref value, ref in_func} => {
				//println!("i am in assign and scope vars is {:?}", self.envs.current_scope().vars);
				let val = self.visit_expr(value);
				if let Err(_) = val { return val; }
				let val = val.unwrap();
				match (var, value) {
					(&Expression::Variable(ref vname), _) => {
						let value = Value::new(vname.clone(), val.typename(), Some(val.clone()));
						{
							let elem = self.envs.current_scope().get_mut(vname)
									   .expect(&format!("Variable '{}' not declared", vname));
						
						    let expected_type = elem.clone().unwrap().ty_name;
						    if expected_type != val.typename() {
						    	return Err(format!("mismatched types: expected {}, found {}",
						    			expected_type, val.typename()));
						    }
						}
					    Env::set(&mut self.envs, vname.clone(), value, *in_func);
						Ok(val)
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
				let prevval = if let Some(val) = self.envs.current_scope().vars.get(&lst.vname) {
					val.clone().map(|x| x.value.unwrap())
				} else { Some(TypedItem::empty())};
				let val = Value::new(lst.vname.clone(), ty.clone(), prevval);
				Env::set(&mut self.envs, lst.vname.clone(), val.into(), lst.in_func);
				let assigned_val = {
					if let Some(ref statement) = lst.assign {
						Some(try!(self.visit_statement(statement)))
					} else {
						None
					}
				};

				if self.envs.current_scope().has_type(&ty) {
					let val = Value::new(lst.vname.clone(), ty.clone(), assigned_val.clone());
					Env::set(&mut self.envs, lst.vname.clone(), val.into(), lst.in_func);
					Ok(assigned_val.unwrap_or_else(TypedItem::empty))
				} else {
					Err(format!("Type {} is not defined", ty))
				}
			},
			Statement::Define(ref obj) => {
				if self.envs.current_scope().has_type(&obj.name()) {
					panic!("class '{}' is already defined", obj.name());
				}
				Env::set_type(&mut self.envs, obj.name(), TypedItem::Object(obj.clone()), false);
				Ok(TypedItem::empty())
			},
			Statement::FuncDef{ref name, ref func} => {
				use std::collections::HashSet;

				let fun = *func.clone();
				if let Some(ref param_list) = fun.params {
					let has_dup_param = {
						param_list.iter().fold((false, HashSet::new()), |acc, x| {
							let mut hset = acc.1.clone();
							let mut res = acc.0;
							if let Parameter::Full{ ref varname, ..} = *x {
								res = res || acc.1.contains(varname);
								hset.insert(varname);
							}
							(res, hset)
						}).0
					};
					if has_dup_param {
						return Err(format!("function definition for {} has duplicate parameter names", name));
					}
				}
				self.envs.current_scope()
						 .def_func(name.clone(), fun);
				Ok(TypedItem::empty())
			},
			Statement::FuncCall(ref call) => {
				self.visit_expr(call)
			},
			Statement::Return{ref rval} => {
				println!("Visiting return... {:?}\n", self.envs.current_scope().vars);
				rval.as_ref().map_or(Ok(TypedItem::empty()), |expr| self.visit_expr(expr))
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
				let res = self.visit_expr(expr);
				let _ = res.clone().map(|val| self.print_item(val));
				res
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
			 .map(|tup| Primitive::Integer(tup.1).into())
			 .collect::<Vec<_>>()
	}

	fn fetch_var_mut(&mut self, vname: &str) -> Result<&mut Option<Value>, String> {
		self.envs.current_scope().get_mut(vname)
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
	let ll = {
		if let TypedItem::Value(ref val) = left.clone() {
			if let Some(ref v) = val.value {
				v.clone()
			} else { left }
		} else { left }
	};

	let rr = {
		if let TypedItem::Value(ref val) = right.clone() {
			if let Some(ref v) = val.value {
				v.clone()
			} else { right }
		} else { right }
	};

	match (ll, rr) {
		(TypedItem::Primitive(Primitive::Integer(first)),
		 TypedItem::Primitive(Primitive::Integer(second))) => {
			Ok(Primitive::Bool(f(first, second)).into())
		},
		(TypedItem::Primitive(Primitive::Bool(first)),
		 TypedItem::Primitive(Primitive::Bool(second))) => {
			Ok(Primitive::Bool(first == second).into())
		}
		_ => Err(String::from("Use of undefined comparison operation"))
	}
}