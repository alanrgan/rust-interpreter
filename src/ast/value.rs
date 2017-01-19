use super::types::TypedItem;

#[derive(Clone, Debug)]
pub struct Value {
	pub name: String,
	pub ty_name: String,
	pub value: Option<TypedItem>
}

impl Value {
	pub fn new(name: String, ty_name: String, value: Option<TypedItem>) -> Value {
		Value{ name: name, ty_name: ty_name, value: value }
	}
}