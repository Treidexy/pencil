#[derive(Debug, PartialEq)]
pub enum TokenKind {
	Error,
	Float(f64),
	Name(String),

	And,
	Or,

	Plus,
	Minus,
	Slash,
	Star,
	Percent,
	Pipe,
	Carot,
	Bang,

	LParen,
	RParen,
	LBrace,
	RBrace,
	LBracket,
	RBracket,
	LAngle,
	RAngle,

	Arrow,
	Hash,

	Equal,
	Comma,
	Colon,
	SemiColon,
	Dot,
	DotDot,
	Eof,
}

#[derive(Debug)]
pub struct Token {
	pub span: Span,
	pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
	pub start: usize,
	pub end: usize,
}

#[derive(Debug)]
pub enum LexErrorKind {
	Unexpected(char),
}

#[derive(Debug)]
pub struct LexError {
	span: Span,
	kind: LexErrorKind,
}

pub struct Lexer<'src, 'error_list> {
	src: &'src [u8],
	pos: usize,
	error_list: &'error_list mut Vec<LexError>,
}

impl<'src, 'error_list> Lexer<'src, 'error_list> {
	pub fn lex(src: &'src str, error_list: &'error_list mut Vec<LexError>) -> Vec<Token> {
		let mut lexer = Lexer {
			src: src.as_bytes(),
			pos: 0,
			error_list,
		};

		let mut tokens = Vec::<Token>::new();

		lexer.skip_whitespace();
		while !lexer.at_eof() {
			tokens.push(lexer.lex_next());
			lexer.skip_whitespace();
		}

		tokens.push(Token {
			span: Span {
				start: src.len(),
				end: src.len(),
			},
			kind: TokenKind::Eof,
		});

		tokens
	}

	fn lex_next(&mut self) -> Token {
		match self.peek(0) {
			b'+' => {
				self.const_token(TokenKind::Plus, 1)
			},
			b'-' => {
				if self.peek(1) == b'>' {
					self.const_token(TokenKind::Arrow, 2)
				} else {
					self.const_token(TokenKind::Minus, 1)
				}
			},
			b'/' => {
				self.const_token(TokenKind::Slash, 1)
			},
			b'*' => {
				self.const_token(TokenKind::Star, 1)
			},
			b'%' => {
				self.const_token(TokenKind::Percent, 1)
			},
			b'|' => {
				self.const_token(TokenKind::Pipe, 1)
			},
			b'^' => {
				self.const_token(TokenKind::Carot, 1)
			},
			b'!' => {
				self.const_token(TokenKind::Bang, 1)
			},
			b'(' => {
				self.const_token(TokenKind::LParen, 1)
			},
			b')' => {
				self.const_token(TokenKind::RParen, 1)
			},
			b'{' => {
				self.const_token(TokenKind::LBrace, 1)
			},
			b'}' => {
				self.const_token(TokenKind::RBrace, 1)
			},
			b'[' => {
				self.const_token(TokenKind::LBracket, 1)
			},
			b']' => {
				self.const_token(TokenKind::RBracket, 1)
			},
			b'<' => {
				self.const_token(TokenKind::LAngle, 1)
			},
			b'>' => {
				self.const_token(TokenKind::RAngle, 1)
			},
			b'#' => {
				self.const_token(TokenKind::Hash, 1)
			},
			b'=' => {
				self.const_token(TokenKind::Equal, 1)
			},
			b',' => {
				self.const_token(TokenKind::Comma, 1)
			},
			b':' => {
				self.const_token(TokenKind::Colon, 1)
			},
			b';' => {
				self.const_token(TokenKind::SemiColon, 1)
			},
			b'.' => {
				if self.peek(1) == b'.' {
					self.const_token(TokenKind::DotDot, 2)
				} else {
					self.const_token(TokenKind::Dot, 1)
				}
			},
			_ => {
				if self.peek(0).is_ascii_digit() || self.peek(0) == b'.' {
					self.lex_float()
				} else if self.peek(0).is_ascii_alphanumeric() {
					self.lex_name()
				} else {
					let c = self.next();
					self.report_unexpected_token(c as char)
				}
			},
		}
	}

	fn lex_float(&mut self) -> Token {
		let start = self.pos;
		let mut str = String::new();
		while self.peek(0).is_ascii_digit() || self.peek(0) == b'.' {
			if self.peek(0) == b'.' && self.peek(1) == b'.' {
				break;
			}

			str.push(self.next() as char);
		}

		let span = Span {
			start,
			end: self.pos,
		};

		Token {
			span,
			kind: TokenKind::Float(str.parse::<f64>().expect(format!("{}", str).as_str())),
		}
	}

	fn lex_name(&mut self) -> Token {
		let start = self.pos;
		let mut name = String::new();
		while self.peek(0).is_ascii_alphanumeric() {
			name.push(self.next() as char);
		}

		let span = Span {
			start,
			end: self.pos,
		};

		match name.as_str() {
			"and" => {
				self.span_token(TokenKind::And, span)
			},
			"or" => {
				self.span_token(TokenKind::Or, span)
			},
			_ => {
				Token {
					span,
					kind: TokenKind::Name(name),
				}
			}
		}	
	}

	fn const_token(&mut self, kind: TokenKind, len: usize) -> Token {
		let tok = Token {
			span: Span {
				start: self.pos,
				end: self.pos + len,
			},
			kind,
		};

		self.pos += len;

		tok
	}

	fn span_token(&self, kind: TokenKind, span: Span) -> Token {
		let tok = Token {
			span,
			kind,
		};

		tok
	}

	fn report_unexpected_token(&mut self, c: char) -> Token {
		let span = Span {
			start: self.pos,
			end: self.pos + 1,
		};

		self.error_list.push(LexError {
			span,
			kind: LexErrorKind::Unexpected(c),
		});

		let tok = Token {
			span,
			kind: TokenKind::Error,
		};

		self.pos += 1;

		tok
	}

	fn skip_whitespace(&mut self) {
		while !self.at_eof() && self.peek(0).is_ascii_whitespace() || self.peek(0) == b'\n' || (self.peek(0) == b'/' && self.peek(1) == b'/') {
			if self.peek(0) == b'/' && self.peek(1) == b'/' {
				while self.peek(0) != b'\n' && !self.at_eof() {
					self.next();
				}
			}

			self.pos += 1;
		}
	}

	fn peek(&self, offset: usize) -> u8 {
		let pos = self.pos + offset;
		if pos < 0 || pos >= self.src.len() {
			b'\0'
		} else {
			self.src[pos]
		}
	}

	fn next(&mut self) -> u8 {
		let c = self.peek(0);
		self.pos += 1;
		c
	}

	fn at_eof(&self) -> bool {
		self.pos >= self.src.len()
	}
}