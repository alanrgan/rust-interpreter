use super::expression::*;
use super::token::*;
use super::types::*;
use super::func::*;
use super::ast::Visitable;

#[derive(Debug, Clone)]
pub enum Statement {
	For(Box<ForStatement>),
	While { pred: Expression, conseq: Box<Statement> },
	Compound { children: Vec<Statement> },
	Assign { var: Expression, value: Expression, in_func: bool },
	If(Box<IfStatement>),
	Print(Expression),
	Define(Object),
	FuncDef { name: String, func: Box<Function> },
	FuncCall(Expression),
	Let(Box<LetStatement>),
	Macro(Box<Macro>),
	// temporary
	Expr(Expression),
	Term(TermToken),
	Empty
}

#[derive(Debug, Clone)]
pub struct Macro {
	pub name: String,
	pub arg: Box<Visitable>
}

#[derive(Debug, Clone)]
pub struct LetStatement {
	pub vname: String,
	pub ty: String,
	pub assign: Option<Statement>,
	// used to distinguish assigning variables within function params vs
	// regular
	pub in_func: bool
}

impl Statement {
	pub fn new_let(vname: String, ty: String, assign: Option<Statement>, in_func: bool) -> Statement {
		let s = LetStatement {
			vname: vname,
			ty: ty,
			assign: assign,
			in_func: in_func
		};
		Statement::Let(Box::new(s))
	}
}

#[derive(Debug, Clone)]
pub struct ForStatement {
	pub var: Expression,
	pub range: Expression,
	pub conseq: Statement,
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

impl Macro {
	pub fn new(name: String, arg: Box<Visitable>) -> Macro {
		Macro { name: name, arg: arg }
	}
}