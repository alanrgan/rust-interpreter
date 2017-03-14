use super::statement::Statement;
use super::expression::Expression;
use regex::Regex;

type NameArgPair = (Vec<Statement>, Vec<String>);

#[derive(Debug, Clone)]
pub struct Function {
	// TODO: MAYBE, scopes are specific to the call
	// pub env: Env,
	pub params: Option<Vec<Parameter>>,
	// Compound statement
	pub conseq: Statement,
	// string representation of return value type
	pub retval: Option<String>,
	pub ty: String,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Parameter {
	Full { varname: String, typename: String },
	Partial
}

impl Parameter {
	pub fn typename(&self) -> &str {
		if let Parameter::Full{ ref varname, ref typename } = *self {
			typename
		} else {
			"?"
		}
	}
}

#[derive(Debug, Clone)]
pub struct ArgList(pub Vec<Expression>);

impl Function {
	pub fn new(params: Option<Vec<Parameter>>, conseq: Statement, rtype: Option<String>) -> Function {
		let ftype = Function::construct_type(&params, &rtype);
		Function { params: params, conseq: conseq, retval: rtype, ty: ftype }
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

	fn construct_type(params: &Option<Vec<Parameter>>, retval: &Option<String>) -> String {
		let mut ftype = "Func<".to_string();
		if let Some(ref pvec) = *params {
			match pvec.len() {
				0 => ftype.push_str("_"),
				1 => ftype.push_str(pvec[0].typename()),
				_ => {
					ftype.push_str("(");
					for (i,p) in pvec.iter().enumerate() {
						ftype.push_str(p.typename());
						if i != pvec.len() - 1 {
							ftype.push_str(",");
						}
					}
					ftype.push_str(")");
				}
			}
		} else {
			ftype.push_str("_");
		}
		ftype.push_str(",");
		if let Some(ref rval) = *retval {
			ftype.push_str(rval);
		} else {
			ftype.push_str("_");
		}
		ftype.push_str(">");
		ftype
	}

	pub fn check_valid_type(ty: &str) -> bool {
		let re = Regex::new(r"^(Func<)(_{1}|(\(([A-Za-z]+, *)+([A-Za-z]+)\)|[A-Za-z]+), *([A-Za-z]+|_)>)").unwrap();
		re.is_match(ty)
	}
}