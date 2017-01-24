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
		if let Some(Token::BlockStart) = self.current_token {
			self.block_comment();
		}
	}

	pub fn parse(&mut self) -> Box<Visitable> {
		Box::new(self.statement_list())
	}

	fn statement(&mut self) -> Statement {
		match self.current_token.clone() {
			Some(Token::LCurl) => self.compound_statement(),
			Some(Token::If) => self.conditional(),
			Some(Token::For) => self.for_loop(),
			Some(Token::While) => self.while_loop(),
			Some(Token::Ident(vname)) => {
				let mut var = self.variable();
				//println!("var {:?}", var);
				//println!("curtok: {:?}", self.current_token);
				var = self.parse_brackets(vname).unwrap_or(var);

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
			Some(Token::Macro(macro_name)) => {
				self.eat_current();
				self.eat(Token::LParen);
				let mstmt = Box::new(self.statement());
				self.eat(Token::RParen);
				Statement::Macro(Box::new(Macro::new(macro_name, mstmt)))
			},
			Some(Token::Print) => self.print_statement(),
			Some(Token::Def) => self.define(),
			Some(Token::Let) => self.parse_let(),
			Some(Token::Comment) | Some(Token::BlockStart) | None => Statement::Empty,
			Some(_) => { Statement::Expr(self.expr(0)) }
		}
	}

	fn statement_list(&mut self) -> Statement {
		let mut nodes: Vec<Statement> = vec![self.statement()];
		while let Some(ref tok) = self.current_token.clone() {
			match *tok {
				Token::Semi => self.eat(Token::Semi),
				Token::Comment => {
					self.comment();
				},
				Token::BlockStart => {
					self.block_comment();
					//continue;
				},
				Token::RCurl => break,
				_ => {
					match *nodes.last().unwrap() {
						Statement::If(_) | Statement::For(_) |
						Statement::While{..} => {},
						_ => panic!("expected semicolon, got {:?}", tok)
					}
				}
			}
			match self.current_token {
				Some(Token::RCurl) | None => break,
				_ => {}
			};
			nodes.push(self.statement());
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
		if let Some(Token::Else) = self.current_token {
			self.eat(Token::Else);
			alt = Some(self.compound_statement());
		}
		Statement::If(Box::new(IfStatement::new(pred, conseq, alt)))
	}

	fn comment(&mut self) {
		self.eat(Token::Comment);
		self.lexer.consume_until(|c| c == '\n');
		self.lexer.iter.next();
		self.current_token = self.lexer.next_token();
	}

	fn block_comment(&mut self) {
		loop {
			self.eat_current();
			if let Some(Token::BlockEnd) = self.current_token {
				self.eat_current();
				break;
			}
		}
	}

	fn parse_let(&mut self) -> Statement {
		self.eat(Token::Let);
		let var = self.variable();
		self.eat(Token::Colon);
		let tyname = match self.current_token {
			Some(Token::Ident(ref tname)) => tname.clone(),
			_ => panic!("expected type specification in variable declaration, got {:?}",
						 self.current_token)
		};
		self.eat_current();
		let assign = match self.current_token.clone() {
			Some(Token::Equals) => Some(self.assignment(var.clone())),
			_ => None
		};
		Statement::Let(Box::new(
			LetStatement { vname: var.as_variable().unwrap(),
		 				   ty: tyname,
		 				   assign: assign
		 				 }))
	}

	fn assignment(&mut self, var: Expression) -> Statement {
		self.eat(Token::Equals);
		match var {
			Expression::Variable(_) | Expression::BrackOp(_) => {
				Statement::Assign {
					var: var.clone(),
					value: self.expr(0)
				}
			},
			_ => panic!("Expected a variable on assignment")
		}
	}

	fn for_loop(&mut self) -> Statement {
		self.eat(Token::For);
		let var = self.variable();
		self.eat(Token::In);
		let iterable_expr = self.expr(0);
		let conseq = self.compound_statement();
		Statement::For(Box::new(ForStatement{var: var, range: iterable_expr, conseq: conseq}))
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

	fn define(&mut self) -> Statement {
		self.eat(Token::Def);
		self.eat(Token::Class);
		let rval: Statement;
		if let Some(Token::Ident(ref tname)) = self.current_token {
			rval = Statement::Define(Object::new(tname.clone()));
		} else {
			rval = Statement::Empty;
		}
		self.eat_current();
		rval
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
				Expression::from(Primitive::Integer(val))
			},
			Token::Bool(val) => {
				self.eat(token.clone());
				Expression::from(Primitive::Bool(val))
			},
			Token::Quot => {
				let string = self.string();
				Expression::from(Primitive::Str(string))
			},
			Token::LBrace => self.array(),
			Token::LParen => {
				self.eat(Token::LParen);
				let expr = self.expr(0);
				self.eat(Token::RParen);
				expr
			},
			Token::Ident(vname) => { 
				let var = self.variable();
				self.parse_brackets(vname).unwrap_or(var)
			},
			_ => { panic!("found unexpected token {:?}", token) }
		}
	}

	fn parse_brackets(&mut self, var: String) -> Option<Expression> {
		if let Some(Token::LBrace) = self.current_token {
			let mut brackop = BrackOpExpression::new(var);
			while let Some(Token::LBrace) = self.current_token {
				self.eat_current();
				let index = self.expr(0);
				self.eat(Token::RBrace);
				brackop.indices.push(index);
			}
			Some(Expression::BrackOp(Box::new(brackop)))
		} else {
			None
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
							return Expression::from(list);
						}
					} else { unreachable!(); }
				},
				Some(Token::LBrace) => {
					// we are either parsing a range or a sublist
					// allow nested sublists
					while let Some(Token::LBrace) = self.current_token {
						self.eat_current();
						// create a new sublist
						let sublist = List::new();
						list_stack.push(sublist);
					}
					let elem = self.try_parse_range();
					if let ListElem::Range{..} = elem {
						if let Some(Token::RBrace) = self.current_token {}
						else { panic!("unexpected token"); }
					}
					let inner_list = list_stack.last_mut().unwrap();
					inner_list.push(elem);
				},
				Some(_) => {
					let top = list_stack.last_mut().unwrap();
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
	}

	// returns ListElem::Range if range is parsed,
	// otherwise returns some other ListElem
	fn try_parse_range(&mut self) -> ListElem {
		let factor = self.expr(0);
		// check if the token is now DotRange('..') or otherwise
		match self.current_token {
			Some(Token::DotRange) => {
				self.eat_current();
				let end = self.factor();
				let mut step = None;
				if let Some(Token::Semi) = self.current_token {
					self.eat_current();
					step = Some(self.factor());
				}
				ListElem::Range{start: factor, end: end, step: step }
			},
			// otherwise do nothing and continue
			_ => ListElem::Value(factor)
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
			self.eat_current();
			Expression::Variable(val)
		} else {
			panic!("expected variable name, got {:?}", tok);
		}
	}

	fn expr(&mut self, precedence: u8) -> Expression 
	{
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
					self.eat_current();
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