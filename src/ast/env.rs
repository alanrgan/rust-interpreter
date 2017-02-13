use std::collections::HashMap;
use super::value::*;
use super::func::*;
use super::types::TypedItem;

type VarMap = HashMap<String, Option<Value>>;

#[derive(Clone, Debug)]
pub struct ScopeList {
	pub envs: Vec<Env>,
	aliases: Vec<VarMap>
}

impl ScopeList {
	pub fn new() -> ScopeList {
		ScopeList { envs: vec![ Env::new() ], aliases: vec![] }
	}

	pub fn current_scope(&mut self) -> &mut Env {
		self.envs.last_mut().expect("")
	}

	pub fn extend(&mut self) {
		let scope = self.current_scope().extend();
		self.envs.push(scope);
	}

	pub fn pop(&mut self) {
		self.envs.pop();
	}

	pub fn alias(&mut self) {
		let scope = self.current_scope().alias();
		self.envs.push(scope);
	}

	pub fn global_scope(&mut self) -> &mut Env {
		self.envs.first_mut().expect("")
	}
}

#[derive(Clone, Debug)]
pub struct Env {
	pub vars: VarMap,
	pub types: HashMap<String, TypedItem>,
	pub funcs: HashMap<String, Function>
}

impl Env {
	pub fn new() -> Env {
		Env {
			vars: HashMap::new(),
			types: HashMap::new(),
			funcs: HashMap::new()
		}
	}

	pub fn extend(&mut self) -> Env {
		self.clone()
	}

	pub fn alias(&mut self) -> Env {
		let mut e = self.clone();
		e.vars = HashMap::new();
		e
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
			if passbyval { break; }
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