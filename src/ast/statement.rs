use super::expression::*;
use super::token::*;

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