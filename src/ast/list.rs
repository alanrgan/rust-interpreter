use super::expression::*;
use super::types::*;
use super::value::Value;

#[derive(Debug, Clone, Default)]
pub struct List {
	pub values: Vec<ListElem>,
	pub length: usize
}

#[derive(Debug, Clone)]
pub enum ListElem {
	Value(Expression),
	SubList(List),
	Range {start: Expression, end: Expression, step: Option<Expression> }
}

impl List {
	pub fn new() -> List {
		List { values: vec![], length: 0 }
	}

	pub fn push(&mut self, elem: ListElem) {
		self.values.push(elem);
		self.length += 1;
	}
}

impl From<Vec<i32>> for List {
	fn from(some: Vec<i32>) -> List {
		let values = some.into_iter()
						.map(|v| ListElem::Value(Expression::from(Primitive::Integer(v))))
						.collect::<Vec<_>>();
		let len = values.len();
		List { values: values, length: len }
	}
}

impl From<Vec<ListElem>> for List {
	fn from(some: Vec<ListElem>) -> List {
		let len = some.len();
		List { values: some, length: len }
	}
}

impl From<Primitive> for ListElem {
	fn from(some: Primitive) -> ListElem {
		if let Primitive::Array(list) = some {
			ListElem::SubList(list)
		} else {
			ListElem::Value(Expression::from(some))
		}
	}
}

impl From<TypedItem> for ListElem {
	fn from(some: TypedItem) -> ListElem {
		if let TypedItem::Primitive(prim) = some {
			ListElem::from(prim)
		} else {
			ListElem::Value(Expression::Value(some))
		}
	}
}

impl List {
	// returns reference to mutable elem
	pub fn get_mut_at<'b>(nested_arr: &'b mut Option<Value>, 
						  indices: &[usize]) -> Option<&'b mut ListElem> {
		let nested_arr = {
			if let Some(TypedItem::Primitive(ref mut prim)) = nested_arr.as_mut().unwrap().value {
				prim
			} else {
				panic!("Expected list, not object")
			}
		};
		// follow all the values in the indices vector
		if let Primitive::Array(ref mut list) = *nested_arr {
			let mut values = &mut list.values;
			return List::get_mut_helper(values, indices, 0);
		}
		panic!("Expected an array, got a {:?}", nested_arr)
	}

	fn get_mut_helper<'a>(some_vec: &'a mut Vec<ListElem>,
						  indices: &[usize], ind: usize) -> Option<&'a mut ListElem> {
		if indices.is_empty() { panic!("indices may not be empty"); }
		if ind == indices.len()-1 {
			return some_vec.get_mut(indices[ind]);
		} else if let ListElem::SubList(ref mut sublist) = some_vec[indices[ind]] {
			return List::get_mut_helper(&mut sublist.values, indices, ind+1);
		}
		panic!("mismatched array dimensions");
	}
}