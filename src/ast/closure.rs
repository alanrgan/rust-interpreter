use super::statement::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Closure {
	args: ClosureArgs,
	body: Box<Statement>
}

#[derive(Clone, Debug)]
pub struct ClosureArgs(HashMap<String, AnonType>);

#[derive(Clone, Debug)]
struct AnonParam {
	name: String,
	ty: Option<String>
}

#[derive(Clone, Debug)]
pub enum AnonType {
	Typed(String),
	Nothing
}

impl Closure {
	pub fn new() -> Closure {
		Closure { args: ClosureArgs(HashMap::new()), body: Box::new(Statement::Empty) }
	}

	pub fn add_param(&mut self, name: String) {
		self.args.0.insert(name, AnonType::Nothing);
	}
}

impl Default for Closure {
	fn default() -> Self {
		Self::new()
	}
}