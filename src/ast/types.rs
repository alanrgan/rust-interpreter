use std::ops::{Add, Sub, Mul, Div};
use std::fmt;
use std::marker::Sized;

use super::token::*;
use super::list::*;

pub trait Unpacker {
	fn unpack(&Primitive) -> Result<Self, ()> where Self: Sized;
	fn unpack_mut(&mut Primitive) -> Result<&mut Self, ()> where Self: Sized;
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

impl Unpacker for bool {
	fn unpack(p: &Primitive) -> Result<bool, ()> {
		if let Primitive::Bool(ref v) = *p { Ok(*v) } else { Err(()) }
	}

	fn unpack_mut(p: &mut Primitive) -> Result<&mut bool, ()> {
		if let Primitive::Bool(ref mut v) = *p { Ok(v) } else { Err(()) }
	}
}

impl Unpacker for i32 {
	fn unpack(p: &Primitive) -> Result<i32, ()> {
		if let Primitive::Integer(ref i) = *p { Ok(*i) } else { Err(()) }
	}

	fn unpack_mut(p: &mut Primitive) -> Result<&mut i32, ()> {
		if let Primitive::Integer(ref mut i) = *p { Ok(i) } else { Err(()) }
	}
}

impl Unpacker for String {
	fn unpack(p: &Primitive) -> Result<String, ()> {
		if let Primitive::Str(ref s) = *p { Ok(s.clone()) } else { Err(()) }
	}

	fn unpack_mut(p: &mut Primitive) -> Result<&mut String, ()> {
		if let Primitive::Str(ref mut s) = *p { Ok(s) } else { Err(()) }
	}
}

impl Unpacker for List {
	fn unpack(p: &Primitive) -> Result<List, ()> {
		if let Primitive::Array(ref l) = *p { Ok(l.clone()) } else { Err(()) }
	}

	fn unpack_mut(p: &mut Primitive) -> Result<&mut List, ()> {
		if let Primitive::Array(ref mut l) = *p { Ok(l) } else { Err(()) }
	}
}

impl Primitive {
	pub fn unpack<T>(&self) -> Result<T, ()> where T: Unpacker {
		T::unpack(self)
	}

	pub fn unpack_mut<T>(&mut self) -> Result<&mut T, ()> where T: Unpacker {
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