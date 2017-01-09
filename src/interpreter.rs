use ast::*;
use parser::Parser;
use std::collections::HashMap;
use std::cell::RefCell;

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
				self.visit_brackets(brackop, &mut Vec::new())
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
	fn visit_brackets(&mut self, expr: &BrackOpExpression, index_queue: &mut Vec<usize>)
					  -> Result<Primitive, String> {

		println!("selfvmap is {:?}", self.vmap);
	  	// TODO: convert closure to macro
		let indices = expr.indices.clone()
		    .into_iter()
		    .map(|ind| match self.visit_expr(&ind) {
				Ok(Primitive::Integer(val)) => val as usize,
				Ok(other) => panic!("invalid index: expected integer, got {:?}", other),
				Err(e) => panic!(e)
			}).collect::<Vec<_>>();

		//let mut mymap = self.vmap.clone();
		let list_elem: Option<ListElem>;
		{
			let stored_list = try!(self.vmap.get_mut(&expr.var)
								  .ok_or("variable does not exist in this scope"));

			let elem = try!(List::get_mut_at(stored_list, &indices));
			list_elem = Some(elem.clone());
		}

		match list_elem.unwrap() {
			ListElem::Value(ref expr) => {
				self.visit_expr(expr)
			},
			ListElem::SubList(ref list) => {
				Ok(Primitive::Array(list.clone()))
			},
			// semi-lazy evaluation of range expressions
			ListElem::Range{ref start, ref end, ref step} => {
				// TODO: convert the following to a separate function 
				// given a mutable reference to the range to expand,
				// replace it with the appropriate expanded range
				// fn expand_range(&mut ListElem)
				// that way, in List::set() won't have to traverse through nested
				// structure more than once (get mut reference, expand if necessary),
				// then set

				// test that out of bounds array access is still detected

				let rng_list = self.expand_range(start, end, step);
				//if !(range_index >= rng_list.len()) {
					let res = Ok(rng_list[*indices.last().unwrap()].clone());
					let updated_list = rng_list.into_iter()
									   .map(|val| ListElem::Value(Expression::Value(val)))
									   .collect::<Vec<_>>();

				    let stored_list = try!(self.vmap.get_mut(&expr.var)
							  .ok_or("variable does not exist in this scope"));

					let list_elem = try!(List::get_mut_at(stored_list, &indices));
					*list_elem = ListElem::SubList(List::from(updated_list));

					res
				//} else {
				//	panic!("array index {} out of bounds", range_index)
				//}
			}
		}

		//let left = self.visit_expr(&expr.var).unwrap();

		//let right = self.visit_expr(&expr.right).unwrap();
		/*match (left, right) {
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
					} else { index as usize }
				};

				if index >= list.values.len() {
					panic!("array index {} out of bounds", index);
				}

				index_queue.push(index);

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
					// semi-lazy evaluation of range expressions
					ListElem::Range{ref start, ref end, ref step} => {
						// TODO: convert the following to a separate function 
						// given a mutable reference to the range to expand,
						// replace it with the appropriate expanded range
						// fn expand_range(&mut ListElem)
						// that way, in List::set() won't have to traverse through nested
						// structure more than once (get mut reference, expand if necessary),
						// then set

						let rng_list = self.expand_range(start, end, step);
						if !(range_index >= rng_list.len()) {
							let res = Ok(rng_list[range_index].clone());
							let updated_list = rng_list.into_iter()
											   .map(|val| ListElem::Value(Expression::Value(val)))
											   .collect::<Vec<_>>();

							let stored_list = self.vmap.get_mut(&varname).unwrap();
							//let vec_elem = try!(List::get_mut_at(stored_list, index_queue));
							*vec_elem = ListElem::SubList(List::from(updated_list));

							res
						} else {
							panic!("array index {} out of bounds", range_index)
						}
					},
				}
			},
			//_ => { Err(String::from("unsupported use of brackets operator")) }
		}*/
	}

	//fn get_mut_at<'b>(&mut self, indices: &Vec<usize>) -> Result<&'b mut ListElem, String> {
	//	Err("test".to_string())
	//}

	fn expand_range(&mut self, start: &Expression, end: &Expression, step: &Option<Expression>) -> Vec<Primitive> {
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
			None => 1,
		};

		let range: Box<Iterator<Item = i32>> = {
			if start > end { 
				Box::new((end..start+1).rev())
			}
			else { Box::new(start..end) }
		};

		// convert range into a vector of Primitives
		let rng_list = range.enumerate()
							.filter(|i| i.0 % step == 0)
							.map(|tup| Primitive::Integer(tup.1))
							.collect::<Vec<_>>();
		rng_list
	}
}

impl List {
	pub fn get_mut_at<'b>(nested_arr: &'b mut Primitive, 
						  indices: &Vec<usize>) -> Result<&'b mut ListElem, String> {
		// follow all the values in the indices vector
		if let Primitive::Array(ref mut list) = *nested_arr {
			let mut values = &mut list.values;
			return List::get_mut_helper(values, indices, 0);
		}
		Err(format!("Expected an array, got a {:?}", nested_arr))
	}

	fn get_mut_helper<'a>(some_vec: &'a mut Vec<ListElem>,
						  indices: &Vec<usize>, ind: usize) -> Result<&'a mut ListElem, String> {
		if some_vec.len() == 1 {
			let temp = some_vec.get_mut(0);
			if let Some(&mut ListElem::SubList(..)) = temp {
				//if let ListElem::Range{..} = some_vec[0] {
					return temp.ok_or("error".to_string());
				//}
			}
		}
		if indices.is_empty() { return Err(String::from("Array indices cannot be empty")); }
		else if ind == indices.len()-1 {
			//println!("vec is {:?}, veclen is {}, ind is {}, indices is {:?}", some_vec, some_vec.len(), ind, indices);
			if some_vec.len() == 1 {
				if let ListElem::Range{..} = some_vec[0] {
					println!("yo");
					return some_vec.get_mut(0).ok_or("error".to_string());
				}
			}
			some_vec.get_mut(indices[ind])
						   .ok_or(format!("invalid array index {}", indices[ind]))
		} else {
			let a = some_vec.get_mut(indices[ind]);
			if let Some(&mut ListElem::SubList(ref mut sublist)) = a {
				/*println!("sublist is {:?}", sublist);
				if sublist.values.len() == 1 {
					if let ListElem::Range{..} = sublist.values[0] {
						return a.ok_or("error".to_string());
					}
				}*/
				List::get_mut_helper(&mut sublist.values, indices, ind+1)
			} else {
				Err(format!("Requested indices are deeper than array, found {:?}", a))
			}
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