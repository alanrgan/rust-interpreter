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
	Break,
	Print
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
	Array(List),
	LTerm(TermToken),
	Empty
}

impl fmt::Display for Primitive {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Primitive::Bool(ref val) => write!(f, "{}", val),
			Primitive::Integer(ref val) => write!(f, "{}", val),
			Primitive::Str(ref val) => write!(f, "{}", val),
			_ => write!(f, "")
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
			(Primitive::Str(first), second) => {
				Primitive::Str(format!("{}{}", first, second))
				//Primitive::Str(first+&second[..])
			},
			(first, Primitive::Str(second)) => {
				Primitive::Str(format!("{}{}", first, second))
				//Primitive::Str(first+&second[..])
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

/*impl From<Token> for Primitive {
	fn from(token: Token) -> Primitive {
		match token {
			Token::Bool(val) => Primitive::Bool(val),
			Token::Ident(val) => Primitive::Ident(val),
			Token::Integer(val) => Primitive::Integer(val),
			_ => panic!("cannot convert {:?} to primitive", token)
		}
	}
}*/

#[derive(Debug, Clone)]
pub enum TermToken {
	Break,
	Continue,
	Return { retval: Box<Expression> }
}

#[derive(Debug, Clone)]
pub struct List {
	values: Vec<ListElem>
}

#[derive(Debug, Clone)]
pub enum ListElem {
	SubList(List),
	Value(Expression)
}

impl List {
	pub fn new() -> List {
		List { values: vec![] }
	}

	pub fn push(&mut self, elem: ListElem) {
		self.values.push(elem);
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
						"not" => Token::Not,
						"print" => Token::Print
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
	While { pred: Expression, conseq: Box<Statement> },
	Compound { children: Vec<Statement> },
	Assign { var: Expression, value: Expression },
	If(Box<IfStatement>),
	Print(Expression),
	// temporary
	Expr(Expression),
	Term(TermToken),
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
pub struct IfStatement {
	pub pred: Expression,
	pub conseq: Statement,
	pub alt: Option<Statement>
}

impl IfStatement {
	pub fn new(pred: Expression, conseq: Statement, alt: Option<Statement>) -> IfStatement {
		IfStatement {
			pred: pred,
			conseq: conseq,
			alt: alt
		}
	}
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
	DEquals,
	NEquals
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
	pub op: UnaryOp,
	pub val: Expression
}

impl UnaryOpExpression {
	pub fn new(tok: Token, exp: Expression) -> UnaryOpExpression {
		UnaryOpExpression {
			op: match tok {
				Token::Plus => UnaryOp::Plus,
				Token::Minus => UnaryOp::Minus,
				Token::Not => UnaryOp::Not,
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
			Token::NEquals => BinOp::NEquals,
			_ => panic!("Invalid BinOp token type: {:?}", t)
		};
		let bexpr = BinOpExpression { op: op, left: left, right: right };
		Expression::BinOp(Box::new(bexpr))
	}
}

impl From<List> for Expression {
	fn from(some: List) -> Expression {
		Expression::Value(Primitive::Array(some))
	}
}