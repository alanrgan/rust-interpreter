use super::statement::Statement;
use super::expression::Expression;
use std::collections::HashSet;
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

pub struct FuncBuilder {
	func: Function
}

impl FuncBuilder {
	pub fn new() -> FuncBuilder {
		FuncBuilder{func: Function::new(None, Statement::Empty, None)}
	}

	pub fn param(&mut self, name: &str, ty: &str) -> FuncBuilder {
		let param = Parameter::Full{varname: name.to_string(), typename: ty.to_string()};
		if let Some(ref mut v) = self.func.params {
			v.push(param);
		} else {
			self.func.params = Some(vec![param]);
		}
		FuncBuilder{func: self.func.clone()}
	}

	pub fn conseq(&mut self, stmt: Statement) -> FuncBuilder {
		self.func.conseq = stmt;
		FuncBuilder{func: self.func.clone()}
	}

	pub fn retval(&mut self, ty: &str) -> FuncBuilder {
		self.func.retval = Some(ty.to_string());
		FuncBuilder{func: self.func.clone()}
	}

	pub fn done(mut self) -> Function {
		self.func.ty = Function::construct_type(&self.func.params, &self.func.retval);
		self.func
	}
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

#[derive(Debug, Clone)]
pub struct FnPtr {
	pub fname: String,
	pub ftype: String,
	pub is_def: bool,
	pub def: Option<Box<Function>>
}

impl FnPtr {
	pub fn new(fname: String, ftype: String, isdef: bool, def: Option<Function>) -> FnPtr {
		FnPtr{fname: fname, ftype: ftype, is_def: isdef, def: def.map(Box::new)}
	}
}

impl Function {
	pub fn new(params: Option<Vec<Parameter>>, conseq: Statement, rtype: Option<String>) -> Function {
		let ftype = Function::construct_type(&params, &rtype);
		Function { params: params, conseq: conseq, retval: rtype, ty: ftype }
	}

	pub fn rtype(&self) -> String {
		if self.retval.is_some() { self.retval.clone().unwrap() }
		else { "_".to_string() }
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
					assigns.push(Statement::new_let(varname.clone(),
													Some(typename.clone()),
													Some(expr), true));
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

	// returns the number of successive return values that are functions
	pub fn chain_depth(&self) -> i32 {
		match self.retval {
			Some(ref rval) if rval.starts_with("Func<") => {
				let mut depth = 2;
				let mut next_rval = parse_fn_rval(rval);
				while next_rval.starts_with("Func<") {
					depth += 1;
					next_rval = parse_fn_rval(&next_rval);
				}
				depth
			},
			_ => 1
		}
	}

	pub fn check_valid_type(ty: &str) -> bool {
		let re = Regex::new(r"^(Func<)(_{1}|(\(([A-Za-z]+, *)+([A-Za-z]+)\)|[A-Za-z]+)),( *([A-Za-z]+|_)>)").unwrap();
		re.is_match(ty)
	}

	pub fn check_dup_param(param_list: Option<Vec<Parameter>>, name: String) -> Result<(),String> {
		if param_list.is_none() { return Ok(()); }
		let param_list = param_list.unwrap();
		let has_dup_param = {
			param_list.iter().fold((false, HashSet::new()), |acc, x| {
				let mut hset = acc.1.clone();
				let mut res = acc.0;
				if let Parameter::Full{ ref varname, ..} = *x {
					res = res || acc.1.contains(varname);
					hset.insert(varname);
				}
				(res, hset)
			}).0
		};
		if has_dup_param {
			Err(format!("function definition for {} has duplicate parameter names", name))
		} else {
			Ok(())
		}
	}
}

#[derive(Default)]
struct Index {
	ind: i32,
	ltct: i32,
	parenct: i32,
	sat: bool
}

impl Index {
	fn new(ind: i32, ltct: i32, parenct: i32, sat: bool) -> Index {
		Index{ind: ind,
			  ltct: ltct,
			  parenct: parenct,
			  sat: sat}
	}
}

pub fn parse_fn_rval(s: &str) -> String {
	let ind = s.chars().fold(Index{..Default::default()},
		|acc, c| {
			if acc.sat {
				acc
			} else if acc.ltct == 1 && c == ',' && acc.parenct == 0 {
				Index::new(acc.ind+1, acc.ltct, 0, true)
			} else if c == '<' {
				Index::new(acc.ind+1, acc.ltct+1, acc.parenct, false)
			} else if c == '>' {
				Index::new(acc.ind+1, acc.ltct-1, acc.parenct, false)
			} else if c == '(' {
				Index::new(acc.ind+1, acc.ltct, acc.parenct+1, false)
			} else if c == ')' {
				Index::new(acc.ind+1, acc.ltct, acc.parenct-1, false)
			} else {
				Index::new(acc.ind+1, acc.ltct, acc.parenct, false)
			}
		}).ind;
	s[(ind as usize)..(s.len()-1 as usize)].into()
}
