pub(crate) use crate::lex::{ Span, Token, TokenKind };


#[derive(Debug)]
pub struct Param {
	pub span: Span,
	pub name: String,
	pub ty: Type,
}

#[derive(Debug)]
pub enum TypeKind {
	Float,
	Struct(Vec<Param>),
	Array(Box<Type>, usize),
	List(Box<Type>),
}

#[derive(Debug)]
pub struct Type {
	pub span: Span,
	pub kind: TypeKind,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Condition {
	pub span: Span,
	pub kind: ConditionKind,
}

#[derive(Debug, Clone)]
pub struct Expr {
	pub span: Span,
	pub kind: ExprKind,
}

#[derive(Debug, Clone)]
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
	pub span: Span,
	pub kind: ParseErrorKind,
}

#[derive(Debug)]
pub struct Var {
	pub span: Span,
	pub name: String,
	pub ty: Type,
	pub init: Expr,
}

#[derive(Debug)]
pub struct Func {
	pub span: Span,
	pub name: String,
	pub params: Vec<Param>,
	pub ret: Type,
	pub body: Expr,
}

#[derive(Debug)]
pub struct Mutation {
	pub span: Span,
	pub name: String,
	pub expr: Expr,
}

#[derive(Debug)]
pub struct Action {
	pub span: Span,
	pub name: String,
	pub params: Vec<Param>,
	pub body: Vec<Mutation>,
}

#[derive(Debug)]
pub enum Stmt {
	Var(Var),
	Expr(Expr),
	Func(Func),
	Action(Action),
}

pub struct Parser<'toks, 'error_list> {
	toks: &'toks [Token],
	pos: usize,
	error_list: &'error_list mut Vec<ParseError>,
}

const EOF_TOKEN: Token = Token {
	span: Span {
		start: 0,
		end: 0,
	},
	kind: TokenKind::Eof,
};

const DEFAULT_TYPE: Type = Type {
	span: Span {
		start: 0,
		end: 0,
	},
	kind: TypeKind::Float,
};

type Precedence = u8;
impl<'toks, 'error_list> Parser<'toks, 'error_list> {
	fn parse_expr(&mut self) -> Expr {
		self.parse_bin_expr(0)
	}

	fn parse_bin_expr(&mut self, precedence: Precedence) -> Expr {
		let mut left = self.parse_unary_expr();

		while self.peek(0).kind != TokenKind::Eof && self.peek(0).kind.precedence() > 0 && precedence < self.peek(0).kind.precedence() {
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

				while self.peek(0).kind != TokenKind::RBracket && self.peek(0).kind != TokenKind::SemiColon && self.peek(0).kind != TokenKind::Eof {
					exprs.push(self.parse_expr());
					if self.peek(0).kind == TokenKind::Comma {
						self.next();
					} else if self.peek(0).kind != TokenKind::RBracket && self.peek(0).kind != TokenKind::SemiColon {
						todo!("err")
					}
				}

				if self.peek(0).kind == TokenKind::SemiColon {
					self.next();
					let len = if let TokenKind::Float(len) = self.next().kind {
						len as usize
					} else {
						exprs.len()
					};

					if exprs.len() > len {
						todo!("err")
					}

					while len > exprs.len() {
						exprs.push(exprs.last().unwrap().clone());
					}

					let rbrace = self.next();
					if rbrace.kind != TokenKind::RBracket {
						todo!()
					}

					Expr {
						span: Span {
							start: tok.span.start,
							end: rbrace.span.end,
						},
						kind: ExprKind::Array(exprs),
					}
				} else {
					let rbrace = self.next();
					if rbrace.kind != TokenKind::RBracket {
						todo!()
					}
	
					Expr {
						span: Span {
							start: tok.span.start,
							end: rbrace.span.end,
						},
						kind: ExprKind::Array(exprs),
					}
				}
			},
			TokenKind::Name(ref name) => {
				if self.peek(0).kind == TokenKind::LParen {
					self.parse_call_expr(name.clone(), tok.span)
				} else {
					Expr {
						span: tok.span,
						kind: ExprKind::Name(name.clone()),
					}
				}
			},
			_ => {
				todo!("{:?}", tok)
			}
		}
	}

	fn parse_call_expr(&mut self, name: String, name_span: Span) -> Expr {
		let lparen = self.next();
		if lparen.kind != TokenKind::LParen {
			todo!()
		}

		let mut args = Vec::new();
		while self.peek(0).kind != TokenKind::RParen && self.peek(0).kind != TokenKind::Eof {
			args.push(self.parse_expr());
			if self.peek(0).kind != TokenKind::Comma {
				if self.peek(0).kind != TokenKind::RParen {
					self.next();
				}
			} else {
				todo!()
			}
		}

		let rparen = self.next();
		if rparen.kind != TokenKind::RParen {
			todo!()
		}
		Expr {
			span: Span {
				start: name_span.start,
				end: rparen.span.end,
			},
			kind: ExprKind::Call(name, args),
		}
	}
}

impl<'toks, 'error_list> Parser<'toks, 'error_list> {
	fn parse_condition(&mut self) -> Condition {
		todo!()
	}
}

impl<'toks, 'error_list> Parser<'toks, 'error_list> {
	fn parse_mutation(&mut self) -> Mutation {
		let name_tok = self.next();
		let name = if let TokenKind::Name(name) = &name_tok.kind {
			name.clone()
		} else {
			todo!()
		};

		let arrow = self.next();
		if arrow.kind != TokenKind::Arrow {
			todo!()
		}

		let expr = self.parse_expr();

		Mutation {
			span: Span {
				start: name_tok.span.start,
				end: expr.span.end,
			},
			name,
			expr,
		}
	}
}

impl<'toks, 'error_list> Parser<'toks, 'error_list> {
	fn parse_type(&mut self) -> Type {
		match self.peek(0).kind {
			TokenKind::LBracket => {
				self.next();
				let inner_ty = if self.peek(0).kind != TokenKind::RBracket && self.peek(0).kind != TokenKind::SemiColon {
					self.parse_type()
				} else {
					DEFAULT_TYPE
				};

				if self.peek(0).kind == TokenKind::SemiColon {
					self.next();
					
					let len = if let TokenKind::Float(val) = self.next().kind {
						val as usize
					} else {
						todo!()
					};

					if self.next().kind != TokenKind::RBracket {
						todo!()
					}

					Type {
						span: Span {
							start: self.pos,
							end: self.pos,
						},
						kind: TypeKind::Array(Box::new(inner_ty), len),
					}
				} else {
					if self.next().kind != TokenKind::RBracket {
						todo!()
					}

					Type {
						span: Span {
							start: self.peek(0).span.start,
							end: self.peek(0).span.end,
						},
						kind: TypeKind::List(Box::new(inner_ty)),
					}
				}
			},
			_ => {
				todo!()
			},
		}
	}
}

impl<'toks, 'error_list> Parser<'toks, 'error_list> {
	pub fn parse(toks: &'toks [Token], error_list: &'error_list mut Vec<ParseError>) -> Vec<Stmt> {
		let mut parser = Parser {
			toks,
			pos: 0,
			error_list,
		};

		let mut stmts = Vec::new();
		while parser.peek(0).kind != TokenKind::Eof {
			stmts.push(parser.parse_stmt());
		}

		stmts
	}

	fn parse_stmt(&mut self) -> Stmt {
		match self.peek(0).kind {
			TokenKind::Name(_) => {
				if self.peek(1).kind == TokenKind::LParen {
					Stmt::Func(self.parse_func())
				} else if self.peek(1).kind == TokenKind::Bang {
					Stmt::Action(self.parse_action())
				} else {
					Stmt::Var(self.parse_var())
				}
			},
			_ => {
				Stmt::Expr(self.parse_expr())
			},
		}
	}

	fn parse_func(&mut self) -> Func {
		let name_tok = self.next();
		let name = if let TokenKind::Name(name) = &name_tok.kind {
			name.clone()
		} else {
			todo!()
		};

		let lparen = self.next();
		if lparen.kind != TokenKind::LParen {
			todo!()
		}

		let mut params = Vec::new();
		while self.peek(0).kind != TokenKind::RParen {
			let name_tok = self.next();
			let name = if let TokenKind::Name(name) = &name_tok.kind {
				name.clone()
			} else {
				todo!("{:?}", name_tok)
			};

			if self.peek(0).kind == TokenKind::Colon {
				self.next();
				let ty = self.parse_type();
				params.push(Param {
					span: Span {
						start: name_tok.span.start,
						end: ty.span.end,
					},
					name,
					ty,
				});
			} else {
				params.push(Param {
					span: name_tok.span,
					name,
					ty: DEFAULT_TYPE,
				});
			}
			
			if self.peek(0).kind != TokenKind::Comma {
				if self.peek(0).kind != TokenKind::RParen {
					todo!()
				}
			} else {
				self.next();
			}
		}

		let rparen = self.next();
		if rparen.kind != TokenKind::RParen {
			todo!()
		}

		let ret = if self.peek(0).kind == TokenKind::Arrow {
			self.next();
			self.parse_type()
		} else {
			DEFAULT_TYPE
		};

		let equal = self.next();
		if equal.kind != TokenKind::Equal {
			todo!()
		}

		let body = self.parse_expr();

		Func {
			span: Span {
				start: name_tok.span.start,
				end: body.span.end,
			},
			name,
			params,
			ret,
			body,
		}
	}

	fn parse_action(&mut self) -> Action {
		let name_tok = self.next();
		let name = if let TokenKind::Name(name) = &name_tok.kind {
			name.clone()
		} else {
			todo!()
		};

		let bang = self.next();
		if bang.kind != TokenKind::Bang {
			todo!()
		}

		let lparen = self.next();
		if lparen.kind != TokenKind::LParen {
			todo!()
		}

		let mut params = Vec::new();
		while self.peek(0).kind != TokenKind::RParen {
			let name_tok = self.next();
			let name = if let TokenKind::Name(name) = &name_tok.kind {
				name.clone()
			} else {
				todo!("{:?}", name_tok)
			};

			if self.peek(0).kind == TokenKind::Colon {
				self.next();
				let ty = self.parse_type();
				params.push(Param {
					span: Span {
						start: name_tok.span.start,
						end: ty.span.end,
					},
					name,
					ty,
				});
			} else {
				params.push(Param {
					span: name_tok.span,
					name,
					ty: DEFAULT_TYPE,
				});
			}
			
			if self.peek(0).kind != TokenKind::Comma {
				if self.peek(0).kind != TokenKind::RParen {
					todo!()
				}
			} else {
				self.next();
			}
		}

		let rparen = self.next();
		if rparen.kind != TokenKind::RParen {
			todo!()
		}

		let equal = self.next();
		if equal.kind != TokenKind::Equal {
			todo!()
		}

		let mut body = Vec::new();
		body.push(self.parse_mutation());
		while self.peek(0).kind == TokenKind::Comma {
			self.next();
			body.push(self.parse_mutation());
		}

		Action {
			span: Span {
				start: name_tok.span.start,
				end: body.last().unwrap().span.end,
			},
			name,
			params,
			body,
		}
	}

	fn parse_var(&mut self) -> Var {
		let name_tok = self.next();
		let name = if let TokenKind::Name(name) = &name_tok.kind {
			name.clone()
		} else {
			todo!()
		};

		let ty = if self.peek(0).kind == TokenKind::Colon {
			self.next();
			self.parse_type()
		} else {
			DEFAULT_TYPE
		};

		if self.next().kind != TokenKind::Equal {
			todo!()
		}

		let init = self.parse_expr();

		Var {
			span: Span {
				start: name_tok.span.start,
				end: init.span.end,
			},
			name,
			ty,
			init,
		}
	}
}

impl<'toks, 'error_list> Parser<'toks, 'error_list> {
	pub(crate) fn peek(&self, offset: usize) -> &'toks Token {
		let pos = self.pos + offset;
		if pos < 0 || pos >= self.toks.len() {
			&EOF_TOKEN
		} else {
			&self.toks[pos]
		}
	}

	pub(crate) fn next(&mut self) -> &'toks Token {
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