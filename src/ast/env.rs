use std::collections::HashMap;
use super::value::*;
use super::func::*;

#[derive(Clone, Debug)]
pub struct Env {
	// parent will be idx of env in interpreter's env vector
	// value of -1 signifies root
	pub parent: i32,
	pub idx: i32,
	pub vars: HashMap<String, Option<Value>>,
	pub funcs: HashMap<String, Function>
}

impl Env {
	pub fn new(parent: i32, idx: i32) -> Env {
		Env {
			parent: parent,
			idx: idx,
			vars: HashMap::new(),
			funcs: HashMap::new()
		}
	}

	pub fn extend(&mut self) -> Env {
		self.clone()
	}

	/*
	*	passbyval should be set to true in the case when you're setting a function
	* 	argument. Functions should have their own isolated scope on each call.
	*	That way, argument names do not interfere with global scope names.
	*/
	pub fn set(envs: &mut Vec<Env>, name: String, value: Value, passbyval: bool) {
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

	pub fn def_func(&mut self, name: String, func: Function) {
		self.funcs.entry(name).or_insert(func);
	}

	pub fn def_var(&mut self, name: String, val: Value) {
		self.vars.entry(name).or_insert(Some(val));
	}
}