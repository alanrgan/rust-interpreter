use interpreter::NodeType;

use super::statement::*;
use super::expression::*;

pub trait Visitable {
	fn node_type(&self) -> NodeType;
	fn as_statement(self: Box<Self>) -> Option<Statement>;
	fn as_expression(self: Box<Self>) -> Option<Expression>;
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
	NEquals,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
	Plus,
	Minus,
	Not
}