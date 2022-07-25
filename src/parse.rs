use crate::lex::{ Span, Token, TokenKind };

#[derive(Debug)]
pub enum TypeKind {
	Float,
	Struct(Vec<(String, Type)>),
	List(Box<Type>),
}

#[derive(Debug)]
pub struct Type {
	span: Span,
	kind: TypeKind,
}

#[derive(Debug)]
pub enum ConditionKind {
	Eq(Expr, Expr),
	Ne(Expr, Expr),
	Lt(Expr, Expr),
	Le(Expr, Expr),
	Gt(Expr, Expr),
	Ge(Expr, Expr),

	And(Box<Condition>, Box<Condition>),
	Or(Box<Condition>, Box<Condition>),
	Not(Box<Condition>),
}

#[derive(Debug)]
pub struct Condition {
	pub span: Span,
	pub kind: ConditionKind,
}

#[derive(Debug)]
pub struct Expr {
	pub span: Span,
	pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
	Literal(f64),
	Add(Box<Expr>, Box<Expr>),
	Sub(Box<Expr>, Box<Expr>),
	Mul(Box<Expr>, Box<Expr>),
	Div(Box<Expr>, Box<Expr>),
	Mod(Box<Expr>, Box<Expr>),
	Join(Box<Expr>, Box<Expr>),
	Pow(Box<Expr>, Box<Expr>),
	Neg(Box<Expr>),
	Array(Vec<Expr>),
	Name(String),
	Call(String, Vec<Expr>),
	Run(String, Vec<Expr>),
	PeiceWise(Vec<(Condition, Expr)>, Box<Expr>),
}

#[derive(Debug)]
pub enum ParseErrorKind {
	
}

#[derive(Debug)]
pub struct ParseError {
	span: Span,
	kind: ParseErrorKind,
}

pub struct Parser<'toks, 'error_list> {
	toks: &'toks [Token],
	pos: usize,
	error_list: &'error_list mut Vec<ParseError>,
}

static EOF_TOKEN: Token = Token {
	span: Span {
		start: 0,
		end: 0,
	},
	kind: TokenKind::Eof,
};

type Precedence = u8;
impl<'toks, 'error_list> Parser<'toks, 'error_list> {
	pub fn parse_expr(toks: &'toks [Token], error_list: &'error_list mut Vec<ParseError>) -> Expr {
		let mut parser = Parser {
			toks,
			pos: 0,
			error_list,
		};

		parser.parse_bin_expr(0)
	}

	fn parse_bin_expr(&mut self, precedence: Precedence) -> Expr {
		let mut left = self.parse_unary_expr();

		while self.peek(0).kind != TokenKind::Eof && precedence < self.peek(0).kind.precedence() {
			let op = &self.next().kind;
			let right = self.parse_bin_expr(op.precedence());
			left = Expr {
				span: Span {
					start: left.span.start,
					end: right.span.end,
				},
				kind: match op {
					TokenKind::Plus => ExprKind::Add(Box::new(left), Box::new(right)),
					TokenKind::Minus => ExprKind::Sub(Box::new(left), Box::new(right)),
					TokenKind::Star => ExprKind::Mul(Box::new(left), Box::new(right)),
					TokenKind::Slash => ExprKind::Div(Box::new(left), Box::new(right)),
					TokenKind::Percent => ExprKind::Mod(Box::new(left), Box::new(right)),
					TokenKind::Pipe => ExprKind::Join(Box::new(left), Box::new(right)),
					TokenKind::Carot => ExprKind::Pow(Box::new(left), Box::new(right)),
					_ => unimplemented!("unexpected token kind"),
				},
			};
		}

		left
	}

	fn parse_unary_expr(&mut self) -> Expr {
		match self.peek(0).kind {
			TokenKind::Minus => {
				let tok = self.next();
				Expr {
					span: tok.span,
					kind: ExprKind::Neg(Box::new(self.parse_unary_expr())),
				}
			},
			_ => {
				self.parse_primary_expr()
			},
		}
	}

	fn parse_primary_expr(&mut self) -> Expr {
		let tok = self.next();
		match tok.kind {
			TokenKind::Float(val) => {
				Expr {
					span: tok.span,
					kind: ExprKind::Literal(val),
				}
			},
			TokenKind::LBracket => {
				let mut exprs = Vec::new();

				self.next();
				while self.peek(0).kind != TokenKind::RBracket {
					exprs.push(Parser::parse_expr(&self.toks[self.pos..], self.error_list));
					if self.peek(0).kind == TokenKind::Comma {
						self.next();
					} else {
						todo!("err")
					}
				}

				self.next();

				Expr {
					span: tok.span,
					kind: ExprKind::Array(exprs),
				}
			},
			_ => {
				todo!()
			}
		}
	}
}

impl<'toks, 'error_list> Parser<'toks, 'error_list> {
	pub fn parse_condition(toks: &'toks [Token], error_list: &'error_list mut Vec<ParseError>) -> Condition {
		todo!()
	}
}

impl<'toks, 'error_list> Parser<'toks, 'error_list> {
	fn peek(&self, offset: usize) -> &'toks Token {
		let pos = self.pos + offset;
		if pos < 0 || pos >= self.toks.len() {
			&EOF_TOKEN
		} else {
			&self.toks[pos]
		}
	}

	fn next(&mut self) -> &'toks Token {
		let tok = self.peek(0);
		self.pos += 1;
		tok
	}
}

trait Precedenceable {
	fn precedence(&self) -> Precedence;
}

impl Precedenceable for TokenKind {
	fn precedence(&self) -> Precedence {
		match self {
			TokenKind::Plus | TokenKind::Minus => 1,
			TokenKind::Star | TokenKind::Slash | TokenKind::Percent => 2,
			TokenKind::Pipe => 3,
			TokenKind::Carot => 4,
			_ => 0,
		}
	}
}