use lexer::Lexer;
use ast::*;

pub struct Parser<'a> {
	lexer: Lexer<'a>,
	prev_token: Option<Token>,
	current_token: Option<Token>
}

impl<'a> Parser<'a> {
	pub fn new(lexer: Lexer) -> Parser {
		let mut parser = Parser {
			lexer: lexer,
			prev_token: None,
			current_token: None,
		};

		parser.current_token = parser.lexer.next_token();
		parser
	}

	fn error(&self, expect: Token) {
		panic!("Invalid syntax: expected {:?}, got {:?}", expect, self.current_token);
	}

	fn eat(&mut self, tok: Token) {
		if let Some(ref token) = self.current_token.clone() {
			if Token::equals(token, &tok) {
				self.eat_current();
			} else {
				self.error(tok);
			}
		} else {
			self.error(tok);
		}
	}

	fn eat_current(&mut self) {
		let current_token = self.current_token.clone();
		//println!("Eating {:?}", current_token);
		self.prev_token = current_token;
		self.current_token = self.lexer.next_token();
	}

	pub fn parse(&mut self) -> Box<Visitable> {
		//self.eat(Token::Plus);
		Box::new(self.compound_statement())
		//Box::new(self.factor())
		//Box::new(self.factor());
		//println!("yoyo");
		//Box::new(self.factor())
		//Box::new(Expression::Empty)
	}

	fn statement(&mut self) -> Statement {
		match self.current_token.clone() {
			Some(Token::LCurl) => self.compound_statement(),
			Some(Token::If) => self.conditional(),
			Some(Token::For) => self.for_loop(),
			Some(Token::While) => self.while_loop(),
			Some(Token::Ident(_)) => {
				let var = self.variable();

				match self.current_token {
					Some(Token::Equals) => self.assignment(var),
					Some(Token::MEquals) => self.op_assignment(var, Token::Minus),
					Some(Token::PEquals) => self.op_assignment(var, Token::Plus),
					Some(Token::MultEquals) => self.op_assignment(var, Token::Mult),
					Some(Token::DivEquals) => self.op_assignment(var, Token::Div),
					_ => Statement::Empty
				}
			},
			Some(Token::Break) => {
				self.eat(Token::Break);
				Statement::Term(TermToken::Break) 
			},
			Some(Token::Print) => self.print_statement(),
			Some(_) => { Statement::Expr(self.expr(0)) },
			None => Statement::Empty
		}
	}

	fn statement_list(&mut self) -> Statement {
		let mut nodes: Vec<Statement> = vec![self.statement()];
		while let Some(ref tok) = self.current_token.clone() {
			match tok {
				&Token::Semi => {
					self.eat(Token::Semi);
				},
				&Token::Comment => {
					self.eat(Token::Comment);
					// TO COMPLETE
				},
				&Token::RCurl => break,
				_ => {}
			}
			if let Some(Token::RCurl) = self.current_token { break; }
			let statement = self.statement();
			nodes.push(statement);
		}
		Statement::Compound{children: nodes}
	}

	fn compound_statement(&mut self) -> Statement {
		if let Some(ref tok) = self.current_token.clone() {
			if Token::equals(tok, &Token::LCurl) {
				self.eat(Token::LCurl);
				let statement = self.statement_list();
				self.eat(Token::RCurl);
				return statement;
			}
		}
		Statement::Empty
	}

	fn conditional(&mut self) -> Statement {
		self.eat(Token::If);
		let pred = self.expr(0);
		let conseq = self.compound_statement();
		let mut alt = None;
		match self.current_token {
			Some(Token::Else) => {
				self.eat(Token::Else);
				alt = Some(self.compound_statement());
			},
			_ => {}
		}
		Statement::If(Box::new(IfStatement::new(pred, conseq, alt)))
	}

	fn assignment(&mut self, var: Expression) -> Statement {
		self.eat(Token::Equals);
		match var {
			Expression::Variable(_) => {
				Statement::Assign {
					var: var.clone(),
					value: self.expr(0)
				}
			},
			_ => panic!("Expected a variable on assignment")
		}
	}

	fn for_loop(&mut self) -> Statement {
		Statement::Empty
	}

	fn while_loop(&mut self) -> Statement {
		self.eat(Token::While);
		let pred = self.expr(0);
		let conseq = self.compound_statement();
		Statement::While{ pred: pred, conseq: Box::new(conseq) }
	}

	fn op_assignment(&mut self, var: Expression, op: Token) -> Statement {
		self.eat_current();
		match var {
			Expression::Variable(_) => {
				Statement::Assign {
					var: var.clone(),
					value: Expression::new_binop(op, var, self.expr(0))
				}
			},
			_ => panic!("Expected a variable on assignment")
		}
	}

	fn print_statement(&mut self) -> Statement {
		self.eat(Token::Print);
		self.eat(Token::LParen);
		let node = Statement::Print(self.expr(0));
		self.eat(Token::RParen);
		node
	}

	fn factor(&mut self) -> Expression {
		let token = self.current_token.clone().unwrap();
		match token {
			Token::Plus | Token::Minus | Token::Not => {
				self.eat(token.clone());
				Expression::UnaryOp(Box::new(UnaryOpExpression::new(token, self.factor())))
			},
			Token::Integer(val) => {
				self.eat(token.clone());
				Expression::Value(Primitive::Integer(val))
			},
			Token::Bool(val) => {
				self.eat(token.clone());
				Expression::Value(Primitive::Bool(val))
			},
			Token::Quot => {
				let string = self.string();
				Expression::Value(Primitive::Str(string))
			},
			Token::LBrace => {
				self.array()
			},
			Token::LParen => {
				self.eat(Token::LParen);
				let expr = self.expr(0);
				self.eat(Token::RParen);
				expr
			},
			Token::Ident(..) => { 
				let var = self.variable();
				if let Some(Token::LBrace) = self.current_token {
					self.eat_current();
					let index = self.expr(0);
					self.eat(Token::RBrace);
					return Expression::new_binop(Token::LBrace, var, index);
				}
				var
			},
			_ => { panic!("found unexpected token {:?}", token) }
		}
	}

	fn array(&mut self) -> Expression {
		self.eat_current();
		let mut list_stack: Vec<List> = vec![];
		list_stack.push(List::new());
		loop {
			match self.current_token {
				Some(Token::RBrace) => {
					self.eat(Token::RBrace);
					if let Some(list) = list_stack.pop() {
						//println!("list stack: {:?}, list: {:?}", list_stack, list);
						if let Some(mut lower_list) = list_stack.last_mut() {
							lower_list.push(ListElem::SubList(list));
						} else {
							//println!("returning here");
							return Expression::from(list);
						}
					} else { unreachable!(); }
				},
				Some(Token::LBrace) => {
					self.eat_current();
					// create a new sublist
					let mut sublist = List::new();
					// we are either parsing a range or a sublist
					let elem = self.try_parse_range();
					match elem {
						ListElem::Range{..} => {
							if let Some(Token::RBrace) = self.current_token {}
							else { panic!("unexpected token"); }
						},
						_ => {}
					};
					sublist.push(elem);
					list_stack.push(sublist);
				},
				Some(_) => {
					//println!("curtok is {:?}", self.current_token);
					let top = list_stack.last_mut().unwrap();
					// only accept factor-level values in array
					// push value onto top-level list in stack
					top.push(self.try_parse_range());
				},
				None => panic!("error, reached none")
			};
			match self.current_token {
				Some(Token::Comma) => { self.eat_current(); },
				Some(Token::RBrace) => {},
				_ => { panic!("Unexpected token {:?}", self.current_token); }
			};
		}
		Expression::Empty
	}

	// returns ListElem::Range if range is parsed,
	// otherwise returns some other ListElem
	fn try_parse_range(&mut self) -> ListElem {
		let factor = self.factor();
		// check if the token is now DotRange('..') or otherwise
		match self.current_token {
			Some(Token::DotRange) => {
				self.eat_current();
				let end = self.factor();
				let mut step = None;
				match self.current_token {
					Some(Token::Semi) => {
						self.eat_current();
						step = Some(self.factor());
					},
					_ => {}
				};
				//self.eat(Token::RBrace);
				ListElem::Range{start: factor, end: end, step: step }
			},
			// otherwise do nothing and continue
			_ => { ListElem::Value(factor) }
		}
	}

	fn string(&mut self) -> String {
		let s: String = self.lexer.consume_until(|c| c == '"')
			.into_iter()
			.collect();
		self.eat(Token::Quot);
		self.current_token = self.lexer.next_token();
		s
	}

	fn variable(&mut self) -> Expression {
		let tok = self.current_token.clone().unwrap();
		if let Token::Ident(val) = tok {
			self.eat(Token::Ident(String::from("")));
			Expression::Variable(val)
		} else {
			panic!("expected variable name, got {:?}", tok);
		}
	}

	fn expr(&mut self, precedence: u8) -> Expression 
	{
		//println!("curtok is {:?}", self.current_token);
		// parse expression prefix as defined in factor() function
		let mut expr = self.factor();
		// continue parsing chained binary operators
		while let Some(token) = self.current_token.clone() {
			let next_precedence = {
				if token.is_binop() {
					token.get_precedence()
				} else {
					break;
				}
			};

			if precedence >= next_precedence {
				break;
			}
			expr = self.infix_expr(expr, next_precedence);
		}
		expr
	}

	fn infix_expr(&mut self, left: Expression, precedence: u8) -> Expression {
		match self.current_token.clone() {
			Some(tok) => {
				if tok.is_binop() {
					self.eat(tok.clone());
					let right = self.expr(precedence);
					Expression::new_binop(tok, left, right)
				} else {
					panic!("Unexpected token {:?}", tok);
				}
			},
			_ => panic!("Err: no more tokens")
		}
	}
}