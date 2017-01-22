extern crate itertools;

use std::str::Chars;
use std::iter::Peekable;
use self::itertools::multipeek;

use ast::{Token, KeywordBank};

pub struct Lexer<'a> {
	pub iter: Peekable<Chars<'a>>,
	keywords: KeywordBank
}

impl<'a> Lexer<'a> {
	pub fn new(text: &str) -> Lexer {
		Lexer {
			iter: text.chars().peekable(),
			keywords: KeywordBank::new()
		}
	}

	pub fn next_token(&mut self) -> Option<Token> {
		while let Some(&ch) = self.iter.peek() {
			if ch.is_whitespace() {
				self.pass_while(|c| c.is_whitespace());
				continue;
			} else {
				match ch {
					'a' ... 'z' | 'A' ... 'Z' | '_' => {
						let var: String = self.consume_while(|c| c.is_alphanumeric() || c == '_')
								.into_iter()
								.collect();
						let mut iter = self.iter.clone();
						if let Some('!') = iter.next() {
							if let Some('(') = iter.next() {
								self.iter.next();
								return Some(Token::Macro(var));
							}
						}
						return self.keywords.try_fetch(var.clone(), Token::Ident(var));
					},
					'0' ... '9' => {
						let num: String = self.consume_while(|c| c.is_numeric())
								.into_iter()
								.collect();
						return Some(Token::Integer(num.parse::<i32>().unwrap()));
					},
					'+' => {
						self.iter.next();
						return self.peek_choose(Token::Plus, Token::PEquals, |c| c == '=');
					},
					'-' => {
						self.iter.next();
						return self.peek_choose(Token::Minus, Token::MEquals, |c| c == '=');
					},
					'*' => {
						self.iter.next();
						if let Some(&c) = self.iter.peek() {
							match c {
								'=' => {
									self.iter.next();
									return Some(Token::MultEquals);
								},
								'/' => {
									self.iter.next();
									return Some(Token::BlockEnd);
								},
								_ => {}
							}
						}
						return Some(Token::Mult);
					},
					'/' => {
						self.iter.next();
						if let Some(&c) = self.iter.peek() {
							match c {
								'/' => {
									self.iter.next();
									return Some(Token::Comment);
								},
								'=' => {
									self.iter.next();
									return Some(Token::DivEquals);
								},
								'*' => {
									self.iter.next();
									return Some(Token::BlockStart);
								}
								_ => {}
							}
						}
						return Some(Token::Div);
					},
					'!' => {
						self.iter.next();
						return self.peek_choose(Token::Not, Token::NEquals, |c| c == '=');
					}
					'.' => {
						self.iter.next();
						return self.peek_choose(Token::Dot, Token::DotRange, |c| c == '.');
					},
					';' => {
						self.iter.next();
						return Some(Token::Semi);
					},
					':' => {
						self.iter.next();
						return self.peek_choose(Token::Colon, Token::DColon, |c| c == ':');
					},
					'=' => {
						self.iter.next();
						return self.peek_choose(Token::Equals, Token::DEquals, |c| c == '=');
					},
					'>' => {
						self.iter.next();
						return self.peek_choose(Token::GThan, Token::GTEquals, |c| c == '=');
					},
					'<' => {
						self.iter.next();
						return self.peek_choose(Token::LThan, Token::LTEquals, |c| c == '=');
					},
					'(' => {
						self.iter.next();
						return Some(Token::LParen);
					},
					')' => {
						self.iter.next();
						return Some(Token::RParen);
					},
					'{' => {
						self.iter.next();
						return Some(Token::LCurl);
					},
					'}' => {
						self.iter.next();
						return Some(Token::RCurl);
					},
					'[' => {
						self.iter.next();
						return Some(Token::LBrace);
					},
					']' => {
						self.iter.next();
						return Some(Token::RBrace);
					},
					',' => {
						self.iter.next();
						return Some(Token::Comma);
					},
					'"' => {
						self.iter.next();
						return Some(Token::Quot);
					},
					_ => return None
				}
			}
		}
		None
	}

	fn pass_while<F>(&mut self, f: F)
		where F: Fn(char) -> bool {
		while let Some(&ch) = self.iter.peek() {
			if f(ch) {
				self.iter.next();
			} else {
				break;
			}
		}
	}

	pub fn consume_while<F>(&mut self, f: F) -> Vec<char>
		where F: Fn(char) -> bool {

		let mut v: Vec<char> = vec![];

		while let Some(&ch) = self.iter.peek() {
			if f(ch) {
				self.iter.next();
				v.push(ch);
			} else {
				break;
			}
		}

		v
	}

	pub fn consume_until<F>(&mut self, f: F) -> Vec<char> 
		where F: Fn(char) -> bool {
			let mut v: Vec<char> = vec![];

		while let Some(&ch) = self.iter.peek() {
			if !f(ch) {
				self.iter.next();
				if let Some(esc_char) = self.peek_escape_char(ch) {
					v.push(esc_char);
				} else {
					v.push(ch);
				}
			} else {
				break;
			}
		}
		v	
	}

	fn peek_escape_char(&mut self, curchar: char) -> Option<char> {
		if curchar != '\\' { return None; }
		let result = match self.iter.peek() {
			Some(&'n') => Some('\n'),
			Some(&'t') => Some('\t'),
			Some(&'\\') => Some('\\'),
			Some(&'"') => Some('"'),
			_ => None 
		};
		self.iter.next();
		result
	}

	fn peek_choose<F>(&mut self, opt1: Token, opt2: Token, f: F) -> Option<Token>
		where F: Fn(char) -> bool {

		if let Some(&c) = self.iter.peek() {
			if f(c) {
				self.iter.next();
				return Some(opt2);
			}
		}
		Some(opt1)
	}
}