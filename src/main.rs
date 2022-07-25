mod lex;
use std::ptr::null_mut;

use lex::*;

mod parse;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use parse::*;

mod emit;
use emit::*;

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
	let ast = Parser::parse_expr(&tokens, &mut parse_errors);
	if parse_errors.len() > 0 {
		println!("{:?}", parse_errors);
		return;
	} else {
		// println!("{:?}", ast);
	}

	unsafe {
		let ctx = LLVMContextCreate();
		let module = LLVMModuleCreateWithNameInContext(cstr!("test"), ctx);
		let builder = LLVMCreateBuilderInContext(ctx);
		
		let ty = LLVMFunctionType(LLVMVoidTypeInContext(ctx), null_mut(), 0, 0);
		let func = LLVMAddFunction(module, cstr!("main"), ty);
		let entry = LLVMAppendBasicBlockInContext(ctx, func, cstr!("entry"));
		LLVMPositionBuilderAtEnd(builder, entry);

		let expr = emit_expr(ctx, module, builder, &ast);
		LLVMBuildRet(builder, expr);

		let out = LLVMPrintModuleToString(module);
		println!("{}", std::ffi::CString::from_raw(out).to_str().unwrap());
	}
}