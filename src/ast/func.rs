use super::types::TypedItem;
use super::statement::Statement;
use super::ast::Visitable;

#[derive(Debug, Clone)]
pub struct Function {
	// TODO: MAYBE, scopes are specific to the call
	// pub env: Env,
	pub params: Option<Vec<Parameter>>,
	// Compound statement
	pub conseq: Statement,
	// string representation of return value type
	pub retval: Option<String>
}

#[derive(Debug, Clone)]
pub enum Parameter {
	Full { varname: String, typename: String },
	Partial
}

pub struct ArgList {
	pub args: Vec<Box<Visitable>>
}

pub struct FnArg {
	pub varname: String,
	pub typename: String
}

impl Function {
	// get return value type of Function
	//pub fn rtype(&self) -> Option<String> {
	//	self.retval.as_ref().map(|v| v.typename())
	//}
	pub fn new(params: Option<Vec<Parameter>>, conseq: Statement, rtype: Option<String>) -> Function {
		Function { params: params, conseq: conseq, retval: rtype }
	}
}

/*

For functions, take vector of FnArgs.

*/