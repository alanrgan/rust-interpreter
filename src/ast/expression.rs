use super::ast::*;
use super::token::*;
use super::types::*;
use super::list::*;
use super::func::{ArgList,Function};

pub trait MapIfNone<T> {
    //type Item;
    fn map_if_none(&self, other: Option<T>) -> Option<T>;
}

#[derive(Debug, Clone)]
pub enum Expression {
	BinOp(Box<BinOpExpression>),
	BrackOp(Box<BrackOpExpression>),
	DotOp(Box<DotOpExpression>),
	Call { left: Box<Expression>, alias: String, args: Option<ArgList> },
	Closure(Box<Function>),
	Value(TypedItem),
	Variable(String),
	Empty,
	UnaryOp(Box<UnaryOpExpression>)
}

impl MapIfNone<Expression> for Option<Expression> {
	fn map_if_none(&self, other: Option<Expression>) -> Option<Expression> {
		if self.is_none() {
			other
		} else {
			self.clone()
		}
	}
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

	pub fn as_variable(&self) -> Option<String> {
		if let Expression::Variable(ref vname) = *self {
			Some(vname.clone())
		} else { None }
	}
}

impl From<List> for Expression {
	fn from(some: List) -> Expression {
		Expression::Value(Primitive::Array(some).into())
	}
}

impl From<Primitive> for Expression {
	fn from(some: Primitive) -> Expression {
		Expression::Value(some.into())
	}
}

impl From<Function> for Expression {
	fn from(some: Function) -> Expression {
		Expression::Value(some.into())
	}
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
pub struct BinOpExpression {
	pub op: BinOp,
	pub left: Expression,
	pub right: Expression
}

/*
	left: Expression::Value(..)
	right: String
*/
#[derive(Debug, Clone)]
pub struct DotOpExpression {
	pub left: Expression,
	pub right: String
}

#[derive(Debug, Clone)]
pub struct BrackOpExpression {
	pub var: String,
	pub indices: Vec<Expression>
}

impl BrackOpExpression {
	pub fn new(var: String) -> BrackOpExpression {
		BrackOpExpression { var: var, indices: vec![] }
   }
}