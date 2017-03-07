use super::statement::Statement;
use super::expression::Expression;

type NameArgPair = (Vec<Statement>, Vec<String>);

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

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Parameter {
	Full { varname: String, typename: String },
	Partial
}

#[derive(Debug, Clone)]
pub struct ArgList(pub Vec<Expression>);

impl Function {
	pub fn new(params: Option<Vec<Parameter>>, conseq: Statement, rtype: Option<String>) -> Function {
		Function { params: params, conseq: conseq, retval: rtype }
	}

	pub fn rtype(&self) -> String {
		if self.retval.is_some() { self.retval.clone().unwrap() }
		else { "".to_string() }
	}

	/*
	*  If number of arguments provided matches the number of parameters, then
	*  a vector of assign statements are returned, otherwise an error
	*/
	pub fn match_args(&self, fname: String, args: &Option<ArgList>) -> Result<NameArgPair, String> {
		let params = if self.params.is_none() { vec![] } else { self.params.clone().unwrap() };
		let args = if args.is_none() { vec![] } else { args.clone().unwrap().0 };
		let nparams = params.len();
		if nparams == args.len() {
			let mut assigns: Vec<Statement> = vec![];
			let mut vnames: Vec<String> = vec![];
			for ppair in params.iter().zip(args.iter()) {
				if let Parameter::Full{ref varname, ref typename} = *ppair.0 {
					let v = Expression::Variable(varname.clone());
					let expr = Statement::Assign{var: v, value: ppair.1.clone(), in_func: true};
					vnames.push(varname.clone());
					assigns.push(Statement::new_let(varname.clone(), typename.clone(), Some(expr), true));
				} else {
					vnames.push("".into());
					assigns.push(Statement::Empty);
				}
			}
			Ok((assigns, vnames))
		} else {
			Err(format!("Expected {} arguments to {}, got {}", nparams, fname, args.len()))
		}
	}
}