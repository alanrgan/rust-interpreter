use super::statement::Statement;
use super::expression::Expression;
use super::env::Env;
use std::collections::HashSet;
use regex::Regex;

#[derive(Debug, Clone)]
pub struct Function {
	pub env: Env,
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
	Half(String),
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

#[derive(Default)]
pub struct FAssigns {
	pub p_assigns: Vec<Statement>,
	pub subp_assigns: Vec<Statement>,
	pub vnames: Vec<String>,
	pub vals: Vec<String>
}

impl FAssigns {
	pub fn new(p: Vec<Statement>, subp: Vec<Statement>, vns: Vec<String>, vals: Vec<String>) -> FAssigns {
		FAssigns{p_assigns: p, subp_assigns: subp, vnames: vns, vals: vals}
	}
}

impl FnPtr {
	pub fn new(fname: String, ftype: String, isdef: bool, def: Option<Function>) -> FnPtr {
		FnPtr{fname: fname, ftype: ftype, is_def: isdef, def: def.map(Box::new)}
	}
}

impl Function {
	pub fn new(params: Option<Vec<Parameter>>, conseq: Statement, rtype: Option<String>) -> Function {
		let ftype = Function::construct_type(&params, &rtype);
		Function { env: Env::new(), 
				   params: params,
				   conseq: conseq,
				   retval: rtype,
				   ty: ftype }
	}

	pub fn rtype(&self) -> String {
		if self.retval.is_some() { self.retval.clone().unwrap() }
		else { "_".to_string() }
	}

	pub fn set_type(&mut self, ftype: &str) {
		let (params, rval) = Function::parse_type(ftype);
		if params.is_some() && self.params.is_some() {
			let myparams = self.params.as_mut().unwrap().iter_mut();
			let ps = params.unwrap().into_iter();
			for (param, ty) in myparams.zip(ps) {
				let name = if let Parameter::Half(vname) = param.clone() {
					vname
				} else { "".to_string() };
				*param = Parameter::Full{varname: name, typename: ty}
			}
		} else if params.is_none() {
			self.params = None;
		}
		self.retval = rval;
		self.ty = Function::construct_type(&self.params, &self.retval);
	}

	/*
	*  If number of arguments provided matches the number of parameters, then
	*  a vector of assign statements are returned, otherwise an error
	*/
	pub fn match_args(&self, fname: &str, args: &Option<ArgList>) -> Result<FAssigns, String> {
		let params = if self.params.is_none() { vec![] } else { self.params.clone().unwrap() };
		let args = if args.is_none() { vec![] } else { args.clone().unwrap().0 };
		let nparams = params.len();
		if nparams == args.len() {
			let mut fassigns = FAssigns{ ..Default::default() };
			for (i,ppair) in params.iter().zip(args.iter()).enumerate() {
				if let Parameter::Full{ref varname, ref typename} = *ppair.0 {
					let v = Expression::Variable(varname.clone());
					let param_name = format!("@{}:param_{}", fname, i);
					let pvar = Expression::Variable(param_name.clone());
					let subpexpr = Statement::Assign{var: pvar, value: ppair.1.clone(), in_func: true};
					fassigns.vnames.push(varname.clone());
					fassigns.vals.push(param_name.clone());
					fassigns.p_assigns.push(Statement::new_let(param_name.clone(),
													Some(typename.clone()),
													Some(subpexpr), true));
				} else {
					fassigns.vnames.push("".into());
					fassigns.p_assigns.push(Statement::Empty);
				}
			}
			Ok(fassigns)
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

	pub fn parse_type(ty: &str) -> (Option<Vec<String>>, Option<String>) {
		let pty = parse_fn_param(ty);
		let rval = parse_fn_rval(ty);
		let mut param_tys = None;
		let mut ret_ty = None;
		if pty != "_" {
			if pty.starts_with('(') {
				let mut v = vec![];
				let mut s = "".to_string();
				let mut ltct = 0;
				for c in pty.trim_matches(|x| x == '(' || x == ')').chars() {
					match c {
						',' if ltct == 0 => {
							v.push(s.clone());
							s = "".to_string();
							continue
						},
						'<' => ltct += 1,
						'>' => ltct -= 1,
						_ => {}
					}
					s.push_str(&c.to_string());
				}
				v.push(s);
				param_tys = Some(v);
			} else {
				param_tys = Some(vec![pty]);
			}
		}
		if rval != "_" {
			ret_ty = Some(rval)
		}

		(param_tys, ret_ty)
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
				match *x {
					Parameter::Full{ ref varname, ..} | Parameter::Half(ref varname) => {
						res = res || acc.1.contains(varname);
						hset.insert(varname);
					},
					_ => {}
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

pub fn parse_fn_param(s: &str) -> String {
	let start = s.find('<').expect("should not fail");
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
	s[(start+1 as usize)..(ind as usize - 1)].into()
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
