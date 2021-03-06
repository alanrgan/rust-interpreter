use std::collections::HashMap;
use super::value::*;
use super::func::*;
use super::types::TypedItem;

type VarMap = HashMap<String, Option<Value>>;

#[derive(Clone, Debug, Default)]
pub struct ScopeList {
	pub envs: Vec<Env>,
	aliases: Vec<VarMap>
}

impl ScopeList {
	pub fn new() -> ScopeList {
		ScopeList { envs: vec![ Env::new() ], ..Default::default() }
	}

	pub fn current_scope(&mut self) -> &mut Env {
		self.envs.last_mut().expect("")
	}

	pub fn extend(&mut self) {
		let scope = self.current_scope().extend();
		self.envs.push(scope);
	}

	pub fn extend_with(&mut self, env: Env) {
		self.envs.push(env);
	}

	pub fn push(&mut self, env: Env) {
		self.envs.push(env);
	}

	pub fn pop(&mut self) -> Option<Env> {
		self.envs.pop()
	}

	// vnames is a vector of names to import from the previous scope (i.e. the argnames)
	pub fn alias(&mut self, vnames: Vec<String>) {
		let scope = self.current_scope().alias(vnames);
		self.envs.push(scope);
	}

	pub fn remove_all(&mut self, vnames: &[String]) {
		self.current_scope().remove_all(vnames);
	}

	pub fn global_scope(&mut self) -> &mut Env {
		self.envs.first_mut().expect("")
	}
}

#[derive(Clone, Debug, Default)]
pub struct Env {
	pub vars: VarMap,
	pub types: HashMap<String, TypedItem>,
	pub funcs: HashMap<String, Function>,
	pub in_call: bool
}

impl Env {
	pub fn new() -> Env {
		Env { ..Default::default() }
	}

	pub fn extend(&mut self) -> Env {
		let mut e = self.clone();
		e.in_call = false;
		e
	}

	pub fn alias(&mut self, vnames: Vec<String>) -> Env {
		let mut e = self.clone();
		e.vars = HashMap::new();

		// copy all function pointers over
		for (key, val) in &self.vars {
			if let Some(TypedItem::FnPtr(ref fptr)) = val.as_ref().unwrap().value {
				if fptr.is_def {
					e.vars.insert(key.clone(), val.clone());
				}
			}
		}

		for name in vnames {
			let value = self.vars.get(&name);
			if value.is_some() {
				e.vars.insert(name, value.unwrap().clone());
			}
		}
		e
	}

	pub fn remove_all(&mut self, vnames: &[String]) {
		for vn in vnames {
			self.vars.remove(vn);
		}
	}

	pub fn fetch_and_set(&mut self, other: &Env, vnames: &[String], values: &[String]) {
		for (key, val) in &other.vars {
			if let Some(TypedItem::FnPtr(ref fptr)) = val.as_ref().unwrap().value {
				if fptr.is_def {
					self.vars.insert(key.clone(), val.clone());
				}
			}
		}

		for (name,val) in vnames.into_iter().zip(values.into_iter()) {
			let value = other.vars.get(val);
			if value.is_some() {
				self.vars.insert(name.clone(), value.unwrap().clone());
			}
		}

		self.funcs = other.funcs.clone();
	}

	// get all vars and fn ptrs
	pub fn fetch_vars(&mut self, vnames: Vec<String>) -> Vec<(String, Option<Value>)> {
		let mut v = vec![];
		for (key, val) in &self.vars {
			if let Some(TypedItem::FnPtr(ref fptr)) = val.as_ref().unwrap().value {
				if fptr.is_def {
					v.push((key.clone(), val.clone()));
				}
			}
		}

		for name in vnames {
			let value = self.vars.get(&name);
			if value.is_some() {
				v.push((name, value.unwrap().clone()));
			}
		}
		v
	}

	pub fn add_all(&mut self, pairs: Vec<(String, Option<Value>)>) {
		for tup in pairs {
			self.vars.insert(tup.0, tup.1);
		}
 	}

	/*
	*	passbyval should be set to true in the case when you're setting a function
	* 	argument. Functions should have their own isolated scope on each call.
	*	That way, argument names do not interfere with global scope names.
	*/
	pub fn set(envs: &mut ScopeList, name: String, value: Value, passbyval: bool) {
		let envs = &mut envs.envs;
		let mut idx = (envs.len()) as i32 - 1;
		{
			let env = &envs[idx as usize];
			if env.funcs.contains_key(&name) {
				panic!("{} is already defined as a function", name);
			}
		}
		let len = envs.len() as i32 - 1;
		while let Some(e) = envs.get_mut(idx as usize) {
			if e.vars.contains_key(&name) || idx == len {
				e.vars.insert(name.clone(), Some(value.clone()));
			}
			// if function call, do not update any other scopes except the current
			if passbyval || e.in_call { break; }
			idx -= 1;
		}
	}

	pub fn set_type(envs: &mut ScopeList, name: String, value: TypedItem, is_func: bool) {
		let envs = &mut envs.envs;
		let mut idx = (envs.len()) as i32 - 1;
		{
			let env = &envs[idx as usize];
			if env.funcs.contains_key(&name) {
				panic!("{} is already defined as a function", name);
			}
		}
		let len = envs.len() as i32 - 1;
		while let Some(e) = envs.get_mut(idx as usize) {
			if e.types.contains_key(&name) || idx == len {
				e.types.insert(name.clone(), value.clone());
			}
			// if function call, do not update any other scopes except the current
			if is_func { break; }
			idx -= 1;
		}
	}

	pub fn has_type(&self, name: &str) -> bool {
		self.types.contains_key(name)
	}

	pub fn get_func(&self, name: &str) -> Option<&Function> {
		self.funcs.get(name)
	}

	pub fn get_func_mut(&mut self, name: &str) -> Option<&mut Function> {
		self.funcs.get_mut(name)
	}

	pub fn get_var(&self, name: &str) -> Option<&Value> {
		let var = self.vars.get(name);
		if let Some(val) = var {
			val.as_ref()
		} else {
			None
		}
	}

	pub fn get_mut(&mut self, name: &str) -> Option<&mut Option<Value>> {
		self.vars.get_mut(name)
	}

	pub fn def_func(&mut self, name: String, func: Function) {
		self.funcs.entry(name).or_insert(func);
	}

	pub fn def_var(&mut self, name: String, val: Value) {
		self.vars.entry(name).or_insert(Some(val));
	}
}