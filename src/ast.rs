#[macro_use]
use std::collections::HashMap;

pub struct AST {
	token: Token,
	children: Vec<AST>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
	Integer(i32),
	Bool(bool),
	Ident(String),
	Equals,
	DEquals,
	Mult,
	Div,
	Plus,
	Minus,
	And,
	Or,
	PEquals,
	MEquals,
	MultEquals,
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
	Continue,
	Return,
	Break
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
						"break" => Token::Break
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

pub enum BinOp {
	Mult,
	Div,
	Plus,
	Minus,
	And,
	Or,
	GThan,
	LThan,
	GTEquals,
	LTEquals,
	Equals
}

pub enum Expression {
	BinOp(Box<BinOpExpression>),
	Assign(Box<Assign>)
}

pub struct Assign {

}

pub struct BinOpExpression {
	op: BinOp,
	left: Expression,
	right: Expression
}