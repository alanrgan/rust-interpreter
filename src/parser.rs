use lexer::Lexer;
use ast::*;
use std::collections::HashMap;

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
				//println!("Eating {:?}", token);
				self.prev_token = Some(token.clone());
				self.current_token = self.lexer.next_token();
			} else {
				self.error(tok);
			}
		} else {
			self.error(tok);
		}
	}

	fn eat_current(&mut self) {
		let current_token = self.current_token.clone();
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
			Some(Token::Ident(varname)) => {
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

	fn factor(&mut self) -> Expression {
		let token = self.current_token.clone().unwrap();
		match token {
			Token::Plus | Token::Minus | Token::Not => {
				self.eat(token.clone());
				return Expression::UnaryOp(Box::new(UnaryOpExpression::new(token, self.factor())));
			},
			Token::Integer(val) => {
				self.eat(token.clone());
				return Expression::Value(Primitive::Integer(val));
			},
			Token::Bool(val) => {
				self.eat(token.clone());
				return Expression::Value(Primitive::Bool(val));
			},
			Token::Quot => {
				let string = self.string();
				return Expression::Value(Primitive::Str(string));
			},
			Token::LParen => {
				self.eat(Token::LParen);
				let expr = self.expr(0);
				self.eat(Token::RParen);
				return expr;
			},
			Token::Ident(..) => { return self.variable(); },
			_ => return Expression::Empty
		}
		Expression::Empty
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
		let mut expr = self.factor();
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