use std::collections::HashMap;
use super::expression::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
	Integer(i32),
	Bool(bool),
	Ident(String),
	Class,
	Def,
	Equals,
	DEquals,
	Mult,
	Div,
	Plus,
	Minus,
	And,
	Or,
	Not,
	PEquals,
	MEquals,
	NEquals,
	MultEquals,
	DivEquals,
	LParen,
	RParen,
	Semi,
	Comma,
	Dot,
	DotRange,
	RCurl, // }
	LCurl, // {
	LBrace,
	RBrace,
	Colon,
	DColon,
	GTEquals,
	GThan,
	LTEquals,
	LThan,
	Quot,
	Comment,
	BlockStart,
	BlockEnd,
	If,
	Else,
	While,
	For,
	In,
	Continue,
	Return,
	Break,
	Print
}

impl Token {
	pub fn equals(a: &Token, b: &Token) -> bool {
		match (a.clone(), b.clone()) {
			(Token::Integer(..), Token::Integer(..)) |
			(Token::Bool(..), Token::Bool(..)) |
			(Token::Ident(..), Token::Ident(..)) => true,
			_ => { a == b }
		}
	}

	pub fn is_binop(&self) -> bool {
		match *self {
			Token::Plus | Token::Minus | Token::Mult |
			Token::Div | Token::GThan | Token::LThan |
			Token::DEquals | Token::LTEquals | Token::GTEquals |
			Token::NEquals | Token::And | Token::Or | Token::LBrace => {
				true
			},
			_ => false,
		}
	}

	pub fn get_precedence(&self) -> u8 {
		match *self {
			Token::LBrace => 5,
			Token::And | Token::Or => 10,
			Token::DEquals | Token::NEquals => 20,
			Token::GThan | Token::LThan |
			Token::GTEquals | Token::LTEquals => 30,
			Token::Plus | Token::Minus => 40,
			Token::Mult | Token::Div => 50,
			_ => panic!("Token has no precedence value")
		}
	}
}

impl<'a> From<&'a str> for Token {
	fn from(s: &'a str) -> Token {
		Token::Ident(s.to_string())
	}
}

#[test]
fn test_tok_equals() {
	assert!(!Token::equals(&Token::If, &Token::Else));
	assert!(Token::equals(&Token::from("hello"), &Token::from("bye")));
}

#[derive(Debug, Clone)]
pub enum TermToken {
	Break,
	Continue,
	Return { retval: Box<Expression> }
}

#[derive(PartialEq)]
pub struct KeywordBank {
	kwords: HashMap<&'static str, Token>
}

impl KeywordBank {
	pub fn new() -> KeywordBank {
		KeywordBank {
			kwords: hashmap! {
						"if" => Token::If,
						"else" => Token::Else,
						"while" => Token::While,
						"for" => Token::For,
						"true" => Token::Bool(true),
						"false" => Token::Bool(false),
						"continue" => Token::Continue,
						"return" => Token::Return,
						"break" => Token::Break,
						"and" => Token::And,
						"or" => Token::Or,
						"not" => Token::Not,
						"print" => Token::Print,
						"in" => Token::In,
						"def" => Token::Def,
						"class" => Token::Class
 					}
		}
	}

	pub fn try_fetch(&mut self, needle: String, fallback: Token) -> Option<Token> {
		if let Some(&ref tok) = self.kwords.get(&*needle) {
			Some(tok.clone())
		} else {
			Some(fallback)
		}
	}
}