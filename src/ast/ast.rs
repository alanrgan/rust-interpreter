use interpreter::NodeType;
use std::fmt::{Debug, Formatter, Error};

use super::statement::*;
use super::expression::*;

pub trait Visitable {
	fn node_type(&self) -> NodeType;
	fn as_statement(self: Box<Self>) -> Option<Statement>;
	fn as_expression(self: Box<Self>) -> Option<Expression>;
	fn box_clone(&self) -> Box<Visitable>;
	fn box_fmt(&self, &mut Formatter) -> Result<(), Error>;
}

impl Clone for Box<Visitable> {
	fn clone(&self) -> Box<Visitable> {
		self.box_clone()
	}
}

impl Debug for Box<Visitable> {
	fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
		self.box_fmt(f)
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
	NEquals,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
	Plus,
	Minus,
	Not
}