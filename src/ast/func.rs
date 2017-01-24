use super::types::TypedItem;
use super::statement::Statement;

#[derive(Debug, Clone)]
pub struct Function {
	// TODO: MAYBE, scopes are specific to the call
	// pub env: Env,
	pub params: Option<Vec<Parameter>>,
	// Compound statement
	pub conseq: Statement,
	pub retval: Option<TypedItem>
}

#[derive(Debug, Clone)]
pub struct Parameter {

}