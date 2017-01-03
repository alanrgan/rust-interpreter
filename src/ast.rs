#[macro_use]
use std::collections::HashMap;
use std::convert::From;
use std::ops::{Add, Sub, Mul, Div};
use std::fmt;
use interpreter::NodeType;

pub trait Visitable {
	fn node_type(&self) -> NodeType;
	fn as_statement(self: Box<Self>) -> Option<Statement>;
	fn as_expression(self: Box<Self>) -> Option<Expression>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
	Integer(i32),
	Bool(bool),
	Ident(String),
	Equals,
	DEquals,
	Mult,
	Div,
	Plus,
	Minus,
	And,
	Or,
	Not,
	PEquals,
	MEquals,
	NEquals,
	MultEquals,
	DivEquals,
	LParen,
	RParen,
	Semi,
	Comma,
	Dot,
	DotRange,
	RCurl, // }
	LCurl, // {
	LBrace,
	RBrace,
	DColon,
	GTEquals,
	GThan,
	LTEquals,
	LThan,
	Quot,
	Comment,
	BlockStart,
	BlockEnd,
	If,
	Else,
	While,
	For,
	Continue,
	Return,
	Break
}

impl Token {
	pub fn equals(a: &Token, b: &Token) -> bool {
		match (a.clone(), b.clone()) {
			(Token::Integer(..), Token::Integer(..)) => true,
			(Token::Bool(..), Token::Bool(..)) => true,
			(Token::Ident(..), Token::Ident(..)) => true,
			_ => { a == b }
		}
	}

	pub fn is_binop(&self) -> bool {
		match *self {
			Token::Plus | Token::Minus | Token::Mult |
			Token::Div | Token::GThan | Token::LThan |
			Token::DEquals | Token::LTEquals | Token::GTEquals |
			Token::NEquals | Token::And | Token::Or => {
				true
			},
			_ => false,
		}
	}

	pub fn get_precedence(&self) -> u8 {
		match *self {
			Token::And | Token::Or => 10,
			Token::DEquals | Token::NEquals => 20,
			Token::GThan | Token::LThan |
			Token::GTEquals | Token::LTEquals => 30,
			Token::Plus | Token::Minus => 40,
			Token::Mult | Token::Div => 50,
			_ => panic!("Token has no precedence value")
		}
	}
}

impl<'a> From<&'a str> for Token {
	fn from(s: &'a str) -> Token {
		Token::Ident(s.to_string())
	}
}

#[test]
fn test_tok_equals() {
	assert!(!Token::equals(&Token::If, &Token::Else));
	assert!(Token::equals(&Token::from("hello"), &Token::from("bye")));
}

#[derive(Debug, Clone)]
pub enum Primitive {
	Bool(bool),
	Integer(i32),
	Str(String),
	Empty
}

impl fmt::Display for Primitive {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Primitive::Bool(ref val) => write!(f, "{}", val),
			Primitive::Integer(ref val) => write!(f, "{}", val),
			Primitive::Str(ref val) => write!(f, "{}", val),
			Primitive::Empty => write!(f, "")
		}
	}
}

impl Add for Primitive {
	type Output = Primitive;
	fn add(self, other: Primitive) -> Primitive {
		match (self, other) {
			(Primitive::Integer(first), Primitive::Integer(second)) => {
				Primitive::Integer(first + second)
			},
			(Primitive::Str(first), Primitive::Str(second)) => {
				Primitive::Str(first+&second[..])
			},
			_ => panic!("Use of undefined add operation")
		}
	}
}

impl Sub for Primitive {
	type Output = Primitive;
	fn sub(self, other: Primitive) -> Primitive {
		match (self, other) {
			(Primitive::Integer(first), Primitive::Integer(second)) => {
				Primitive::Integer(first - second)
			},
			_ => panic!("Use of undefined sub operation")
		}
	}
}

impl Mul for Primitive {
	type Output = Primitive;
	fn mul(self, other: Primitive) -> Primitive {
		match (self, other) {
			(Primitive::Integer(first), Primitive::Integer(second)) => {
				Primitive::Integer(first * second)
			},
			_ => panic!("Use of undefined mul operation")
		}
	}
}

impl Div for Primitive {
	type Output = Primitive;
	fn div(self, other: Primitive) -> Primitive {
		match (self, other) {
			(Primitive::Integer(first), Primitive::Integer(second)) => {
				Primitive::Integer(first / second)
			},
			_ => panic!("Use of undefined div operation")
		}
	}
}

#[derive(PartialEq)]
pub struct KeywordBank {
	kwords: HashMap<&'static str, Token>
}

impl KeywordBank {
	pub fn new() -> KeywordBank {
		KeywordBank {
			kwords: hashmap! {
						"if" => Token::If,
						"else" => Token::Else,
						"while" => Token::While,
						"for" => Token::For,
						"true" => Token::Bool(true),
						"false" => Token::Bool(false),
						"continue" => Token::Continue,
						"return" => Token::Return,
						"break" => Token::Break,
						"and" => Token::And,
						"or" => Token::Or,
						"not" => Token::Not
 					}
		}
	}

	pub fn try_fetch(&mut self, needle: String, fallback: Token) -> Option<Token> {
		if let Some(&ref tok) = self.kwords.get(&*needle) {
			Some(tok.clone())
		} else {
			Some(fallback)
		}
	}
}

#[derive(Debug, Clone)]
pub enum Statement {
	For(Box<ForStatement>),
	Compound { children: Vec<Statement> },
	Assign { var: Expression, value: Expression },
	// temporary
	Expr(Expression),
	Empty
}

#[derive(Debug, Clone)]
pub struct ForStatement {
	assign: Statement, // must be an assign statement
	//range:
	cond: Expression,
	post: Option<Expression>,
	conseq: Statement,
	var: Token
}

#[derive(Debug, Clone)]
pub enum BinOp {
	Mult,
	Div,
	Plus,
	Minus,
	And,
	Or,
	GThan,
	LThan,
	GTEquals,
	LTEquals,
	DEquals
}

#[derive(Debug, Clone)]
pub struct BinOpExpression {
	pub op: BinOp,
	pub left: Expression,
	pub right: Expression
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
	Plus,
	Minus,
	Not
}

#[derive(Debug, Clone)]
pub struct UnaryOpExpression {
	op: UnaryOp,
	val: Expression
}

impl UnaryOpExpression {
	pub fn new(tok: Token, exp: Expression) -> UnaryOpExpression {
		UnaryOpExpression {
			op: match tok {
				Token::Plus => UnaryOp::Plus,
				Token::Minus => UnaryOp::Minus,
				_ => panic!("invalid unary op")
			},
			val: exp
		}
	}
}

#[derive(Debug, Clone)]
pub enum Expression {
	BinOp(Box<BinOpExpression>),
	Value(Primitive),
	Variable(String),
	Empty,
	UnaryOp(Box<UnaryOpExpression>)
}

impl Expression {
	pub fn new_binop(t: Token, left: Expression, right: Expression) -> Expression {
		let op = match t {
			Token::Mult => BinOp::Mult,
			Token::Plus => BinOp::Plus,
			Token::Div => BinOp::Div,
			Token::Minus => BinOp::Minus,
			Token::And => BinOp::And,
			Token::Or => BinOp::Or,
			Token::LThan => BinOp::LThan,
			Token::GThan => BinOp::GThan,
			Token::GTEquals => BinOp::GTEquals,
			Token::LTEquals => BinOp::LTEquals,
			Token::DEquals => BinOp::DEquals,
			_ => panic!("Invalid BinOp token type: {:?}", t)
		};
		let bexpr = BinOpExpression { op: op, left: left, right: right };
		Expression::BinOp(Box::new(bexpr))
	}
}