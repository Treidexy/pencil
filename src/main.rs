mod lex;
use std::ptr::null_mut;

use lex::*;

mod parse;
use parse::*;

mod emit;
use emit::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;

fn main() {
	let src = std::fs::read_to_string("./samples/test.pencil").unwrap();
	let mut lex_errors = Vec::<LexError>::new();
	let tokens = Lexer::lex(&src.as_str(), &mut lex_errors);
	if lex_errors.len() > 0 {
		println!("{:?}", lex_errors);
		return;
	} else {
		// println!("{:?}", tokens);
	}

	let mut parse_errors = Vec::<ParseError>::new();
	let ast = Parser::parse_data(&tokens, &mut parse_errors);
	if parse_errors.len() > 0 {
		println!("{:?}", parse_errors);
		return;
	} else {
		// println!("{:?}", ast);
	}

	let mut emitter = Emitter::new();
	emitter.emit_data(&ast);
	emitter.print();
}