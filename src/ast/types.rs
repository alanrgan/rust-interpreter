use std::ops::{Add, Sub, Mul, Div};
use std::fmt;
use std::marker::Sized;
use std::collections::HashMap;

use super::token::*;
use super::list::*;
use super::closure::*;
use super::func::Function;
use super::value::Value;

pub trait Unpacker<T> {
	fn unpack(&T) -> Result<Self, ()> where Self: Sized;
	fn unpack_mut(&mut T) -> Result<&mut Self, ()> where Self: Sized;
}

// user-defined types, essentially
#[derive(Clone, Debug)]
pub struct Object {
	pub typename: String,
	attrs: HashMap<String, TypedItem>,
	funcs: HashMap<String, Function>
}

impl Object {
	pub fn new(t: String) -> Object {
		Object{ typename: t, attrs: HashMap::new(), funcs: HashMap::new() }
	}

	pub fn add_attr(&mut self, name: String, ty: TypedItem) -> Result<(), String> {
		self.attrs.insert(name, ty);
		Ok(())
	}

	pub fn get_func(&self, name: &str) -> Option<&Function> {
		self.funcs.get(name)
	}

	pub fn name(&self) -> String {
		self.typename.clone()
	}
}

#[derive(Debug, Clone)]
pub enum Primitive {
	Bool(bool),
	Integer(i32),
	Str(String),
	Array(List),
	LTerm(TermToken),
	Empty
}

#[derive(Clone, Debug)]
pub enum TypedItem {
	Primitive(Primitive),
	Object(Object),
	Closure(Closure),
	Value(Box<Value>) // essentially a named reference
}

impl TypedItem {
	pub fn typename(&self) -> String {
		match *self {
			TypedItem::Object(ref obj) => obj.name(),
			TypedItem::Primitive(Primitive::Bool(_)) => "bool".to_string(),
			TypedItem::Primitive(Primitive::Integer(_)) => "int".to_string(),
			TypedItem::Primitive(Primitive::Str(_)) => "str".to_string(),
			TypedItem::Primitive(Primitive::Array(_)) => "list".to_string(),
			_ => "".to_string()
		}
	}

	pub fn into_primitive(self) -> Result<Primitive, ()> {
		match self {
			TypedItem::Primitive(some) => Ok(some),
			_ => Err(())
		}
	}

	pub fn unpack<T>(&self) -> Result<T, ()> where T: Unpacker<Primitive> {
		match *self {
			TypedItem::Primitive(ref prim) => T::unpack(prim),
			_ => Err(())
		}
	}

	pub fn unpack_mut<T>(&mut self) -> Result<&mut T, ()> where T: Unpacker<Primitive> {
		match *self {
			TypedItem::Primitive(ref mut prim) => T::unpack_mut(prim),
			_ => Err(())
		}
	}

	pub fn empty() -> TypedItem {
		TypedItem::from(Primitive::Empty)
	}
}

impl fmt::Display for TypedItem {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			TypedItem::Primitive(Primitive::Bool(ref val)) => write!(f, "{}", val),
			TypedItem::Primitive(Primitive::Integer(ref val)) => write!(f, "{}", val),
			TypedItem::Primitive(Primitive::Str(ref val)) => write!(f, "{}", val),
			TypedItem::Value(ref val) => {
				write!(f, "'{}': {}", val.name.clone(),
					   val.value.clone()
						  .unwrap_or(TypedItem::from(Primitive::Str("None".to_string()))))
			},
			_ => write!(f, "")
		}
	}
}

impl From<Primitive> for TypedItem {
	fn from(some: Primitive) -> TypedItem {
		TypedItem::Primitive(some)
	}
}

impl Unpacker<Primitive> for bool {
	fn unpack(p: &Primitive) -> Result<bool, ()> {
		if let Primitive::Bool(ref v) = *p { Ok(*v) } else { Err(()) }
	}

	fn unpack_mut(p: &mut Primitive) -> Result<&mut bool, ()> {
		if let Primitive::Bool(ref mut v) = *p { Ok(v) } else { Err(()) }
	}
}

impl Unpacker<Primitive> for i32 {
	fn unpack(p: &Primitive) -> Result<i32, ()> {
		if let Primitive::Integer(ref i) = *p { Ok(*i) } else { Err(()) }
	}

	fn unpack_mut(p: &mut Primitive) -> Result<&mut i32, ()> {
		if let Primitive::Integer(ref mut i) = *p { Ok(i) } else { Err(()) }
	}
}

impl Unpacker<Primitive> for String {
	fn unpack(p: &Primitive) -> Result<String, ()> {
		if let Primitive::Str(ref s) = *p { Ok(s.clone()) } else { Err(()) }
	}

	fn unpack_mut(p: &mut Primitive) -> Result<&mut String, ()> {
		if let Primitive::Str(ref mut s) = *p { Ok(s) } else { Err(()) }
	}
}

impl Unpacker<Primitive> for List {
	fn unpack(p: &Primitive) -> Result<List, ()> {
		if let Primitive::Array(ref l) = *p { Ok(l.clone()) } else { Err(()) }
	}

	fn unpack_mut(p: &mut Primitive) -> Result<&mut List, ()> {
		if let Primitive::Array(ref mut l) = *p { Ok(l) } else { Err(()) }
	}
}

impl Primitive {
	pub fn unpack<T>(&self) -> Result<T, ()> where T: Unpacker<Primitive> {
		T::unpack(self)
	}

	pub fn unpack_mut<T>(&mut self) -> Result<&mut T, ()> where T: Unpacker<Primitive> {
		T::unpack_mut(self)
	}
}

impl fmt::Display for Primitive {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Primitive::Bool(ref val) => write!(f, "{}", val),
			Primitive::Integer(ref val) => write!(f, "{}", val),
			Primitive::Str(ref val) => write!(f, "{}", val),
			_ => write!(f, "")
		}
	}
}

impl Add for Primitive {
	type Output = Primitive;
	fn add(self, other: Primitive) -> Primitive {
		match (self, other) {
			(Primitive::Integer(first), Primitive::Integer(second)) => {
				Primitive::Integer(first + second)
			},
			(Primitive::Str(first), second) => {
				Primitive::Str(format!("{}{}", first, second))
			},
			(first, Primitive::Str(second)) => {
				Primitive::Str(format!("{}{}", first, second))
			},
			_ => panic!("Use of undefined add operation")
		}
	}
}

impl Sub for Primitive {
	type Output = Primitive;
	fn sub(self, other: Primitive) -> Primitive {
		match (self, other) {
			(Primitive::Integer(first), Primitive::Integer(second)) => {
				Primitive::Integer(first - second)
			},
			_ => panic!("Use of undefined sub operation")
		}
	}
}

impl Mul for Primitive {
	type Output = Primitive;
	fn mul(self, other: Primitive) -> Primitive {
		match (self, other) {
			(Primitive::Integer(first), Primitive::Integer(second)) => {
				Primitive::Integer(first * second)
			},
			_ => panic!("Use of undefined mul operation")
		}
	}
}

impl Div for Primitive {
	type Output = Primitive;
	fn div(self, other: Primitive) -> Primitive {
		match (self, other) {
			(Primitive::Integer(first), Primitive::Integer(second)) => {
				Primitive::Integer(first / second)
			},
			_ => panic!("Use of undefined div operation")
		}
	}
}