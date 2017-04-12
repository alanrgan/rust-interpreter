use std::ops::{Add, Sub, Mul, Div};
use std::fmt;
use std::marker::Sized;
use std::collections::HashMap;

use super::token::*;
use super::list::*;
use super::func::{Function, FnPtr, FuncBuilder};
use super::value::Value;
use super::statement::*;
use super::expression::Expression;

pub type VisitResult = Result<TypedItem, String>;

pub trait Unpacker<T> {
	fn unpack(&T) -> Result<Self, ()> where Self: Sized;
	fn unpack_mut(&mut T) -> Result<&mut Self, ()> where Self: Sized;
}

// user-defined types, essentially
#[derive(Clone, Debug)]
pub struct Object {
	pub typename: String,
	pub attrs: HashMap<String, TypedItem>,
	pub funcs: HashMap<String, Function>
}

// '.' operator trait
// ex: let tup = (0,4);
//     tup.0; <- do operator
// possibly take '&mut Env' as a parameter
pub trait DotOp {
	fn dot_val(&self, right: &str) -> Result<TypedItem, &str> {
		Err("Undefined dot_val")
	}
	fn dot_val_ref(&self, right: &str) -> Result<&TypedItem, &str> {
		Err("Undefined dot_val_ref")
	}
	fn dot_val_mref(&mut self, right: &str) -> Result<&mut TypedItem, &str> {
		Err("Undefined dot_val_mref")
	}
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

// TODO: replace all instances of 'ty: String' with 'ty: Ty'
#[derive(Debug, Clone)]
pub enum Ty {
	TVar(String), // plain types; e.g. str, bool, int, etc.
	// parameterized types: Array<T>, Tuple<T,K>, Func<A,B>, etc
	Param{name: String, inner_types: Vec<Ty>}
}

impl Ty {
	pub fn to_string(&self) -> String {
		let mut s = "".to_string();
		match *self {
			Ty::TVar(ref name) => s.push_str(name),
			Ty::Param{ref name, ref inner_types} => {
				s.push_str(&format!("{}<", name));
				for (i,t) in inner_types.iter().enumerate() {
					s.push_str(&t.to_string());
					if i < inner_types.len() - 1 {
						s.push_str(",");
					}
				}
				s.push_str(">");
			}
		}
		s
	}
}

impl From<String> for Ty {
	fn from(some: String) -> Ty {
		Ty::TVar(some)
	}
}

#[derive(Debug, Clone)]
pub struct Tuple {
	pub body: Vec<TypedItem>,
	pub exprs: Vec<Expression>,
	pub ty: Vec<Ty>
}

impl Tuple {
	pub fn new(first: Expression, second: Expression) -> Tuple {
		Tuple{body: vec![], exprs: vec![first, second], ty: vec![]}
	}

	pub fn check_valid(ty: &str) -> bool {
		true
	}
}

impl DotOp for Tuple {
	fn dot_val_ref(&self, right: &str) -> Result<&TypedItem, &str> {
		match right {
			"zero" => self.body.get(0).ok_or("Invalid index zero for tuple"),
			"one" => self.body.get(1).ok_or("Invalid index one for tuple"),
			_ => Err("Undefined dot op for tuple")
		}
	}

	fn dot_val_mref(&mut self, right: &str) -> Result<&mut TypedItem, &str> {
		match right {
			"zero" => self.body.get_mut(0).ok_or("Invalid index zero for tuple"),
			"one" => self.body.get_mut(1).ok_or("Invalid index one for tuple"),
			_ => Err("Undefined dot op for tuple")
		}
	}
}

// TODO: instead of creating a new Function object every time 'len' or w.e. is called
// create a lookup table and see if a definition already exists.
// if not, then return function
impl DotOp for String {
	fn dot_val(&self, right: &str) -> Result<TypedItem, &str> {
		match right {
			"len" => {
				let l = Expression::from(Primitive::Integer(self.len() as i32));
				let conseq = Statement::Return{rval: Some(l)};
				let func = FuncBuilder::new().conseq(conseq).retval("int").done();
				Ok(TypedItem::from(func))
			},
			"chars" => {
				let s = self.clone().chars()
							.map(|x| {
								let p = Primitive::Str(x.to_string());
								ListElem::from(p)
							}).collect::<Vec<_>>();
				let l = Expression::from(Primitive::Array(List::from(s)));
				let conseq = Statement::Return{rval: Some(l)};
				let func = FuncBuilder::new().conseq(conseq).retval("list").done();
				Ok(TypedItem::from(func))
			},
			_ => Err("Undefined op for str")
		}
	}
}

impl DotOp for List {
	fn dot_val(&self, right: &str) -> Result<TypedItem, &str> {
		match right {
			"len" => {
				let l = Expression::from(Primitive::Integer(self.length as i32));
				let conseq = Statement::Return{rval: Some(l)};
				let func = FuncBuilder::new().conseq(conseq).retval("int").done();
				Ok(TypedItem::from(func))
			},
			_ => Err("Undefined op for list")
		}
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

// in the future: maybe rewrite this as a trait?
// pub trait Typed {
// 		fn typename() -> String
// }
#[derive(Clone, Debug)]
pub enum TypedItem {
	Primitive(Primitive),
	Tuple(Tuple),
	Object(Object),
	Closure(Box<Function>),
	FnPtr(FnPtr),
	Value(Box<Value>), // essentially a named reference
	RetVal(Box<VisitResult>)
}

impl TypedItem {
	pub fn typename(&self) -> String {
		match *self {
			TypedItem::Object(ref obj) => obj.name(),
			TypedItem::Closure(ref b) => b.clone().ty,
			TypedItem::Primitive(Primitive::Bool(_)) => "bool".to_string(),
			TypedItem::Primitive(Primitive::Integer(_)) => "int".to_string(),
			TypedItem::Primitive(Primitive::Str(_)) => "str".to_string(),
			TypedItem::Primitive(Primitive::Array(_)) => "list".to_string(),
			TypedItem::FnPtr(ref fptr) => fptr.ftype.clone(),
			TypedItem::Tuple(ref tup) => {
				let mut s = "(".to_string();
				for (i,t) in tup.ty.iter().enumerate() {
					s.push_str(&t.to_string());
					if i < tup.ty.len() - 1 {
						s.push_str(",");
					}
				}
				s.push_str(")");
				s
			}
			_ => "_".to_string()
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

	pub fn unwrap_ret(self) -> VisitResult {
		if let TypedItem::RetVal(ref rval) = self {
			*rval.clone()
		} else { Ok(self) }
	}
}

impl fmt::Display for TypedItem {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			TypedItem::Primitive(ref p) => write!(f, "{}", p.to_string()),
			TypedItem::Tuple(ref tup) => {
				let mut s = "(".to_string();
				for (i,titem) in tup.body.iter().enumerate() {
					s.push_str(&titem.to_string());
					if i < tup.body.len() - 1 {
						s.push_str(",");
					}
				}
				s.push_str(")");
				write!(f, "{}", s)
			},
			TypedItem::Value(ref val) => {
				write!(f, "'{}': {}", val.name.clone(),
					   val.value.clone()
						  .unwrap_or(TypedItem::from(Primitive::Str("None".to_string()))))
			},
			_ => write!(f, "")
		}
	}
}

impl From<bool> for TypedItem {
	fn from(some: bool) -> TypedItem {
		TypedItem::Primitive(Primitive::Bool(some))
	}
}

impl From<Primitive> for TypedItem {
	fn from(some: Primitive) -> TypedItem {
		TypedItem::Primitive(some)
	}
}

impl From<VisitResult> for TypedItem {
	fn from(some: VisitResult) -> TypedItem {
		TypedItem::RetVal(Box::new(some))
	}
}

impl From<Function> for TypedItem {
	fn from(some: Function) -> TypedItem {
		TypedItem::Closure(Box::new(some))
	}
}

impl From<Tuple> for TypedItem {
	fn from(some: Tuple) -> TypedItem {
		TypedItem::Tuple(some)
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

impl Add for TypedItem {
	type Output = TypedItem;
	fn add(self, other: TypedItem) -> TypedItem {
		match (self, other) {
			(v, TypedItem::Primitive(Primitive::Str(s))) => {
				Primitive::Str(format!("{}{}", v.to_string(), s)).into()
			},
			(TypedItem::Primitive(Primitive::Str(s)), v) => {
				Primitive::Str(format!("{}{}", s, v.to_string())).into()
			},
			(TypedItem::Primitive(a), TypedItem::Primitive(b)) => (a+b).into(),
			_ => panic!("Use of undefined add operation")
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
		match (self.clone(), other.clone()) {
			(Primitive::Integer(first), Primitive::Integer(second)) => {
				Primitive::Integer(first - second)
			},
			_ => panic!("Use of undefined sub operation for {:?} and {:?}", self, other)
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