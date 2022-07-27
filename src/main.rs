mod lex;
use std::mem::size_of;
use std::ptr::null_mut;

use lex::*;

mod parse;
use parse::*;

mod emit;
use emit::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::execution_engine::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::LLVMCodeModel::*;

macro_rules! from_cstr {
	($s:expr) => {
		::std::ffi::CString::from_raw($s).to_str().unwrap()
	};
}

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
	let stmts = Parser::parse(&tokens, &mut parse_errors);
	if parse_errors.len() > 0 {
		println!("{:?}", parse_errors);
		return;
	} else {
		// println!("{:?}", stmts);
	}

	let mut emitter = Emitter::new();
	emitter.emit(&stmts);
	// emitter.print_ir();

	

	unsafe {
		LLVMLinkInMCJIT();
		LLVM_InitializeNativeAsmPrinter();
		let mut engine: LLVMExecutionEngineRef = null_mut();
		let mut options = LLVMMCJITCompilerOptions {
			OptLevel: 0,
			CodeModel: LLVMCodeModelDefault,
			NoFramePointerElim: 1,
			EnableFastISel: 0,
			MCJMM: null_mut(),
		};

		let err: *mut i8 = null_mut();
		if LLVMCreateMCJITCompilerForModule(&mut engine as *mut LLVMExecutionEngineRef, emitter.module,
			&mut options as *mut LLVMMCJITCompilerOptions, size_of::<LLVMMCJITCompilerOptions>(), &mut err as *mut *mut i8) != 0 {
			panic!("{}", from_cstr!(err));
		}

		let actions = &emitter.actions;
		for (name, action) in actions {
			let ptr = LLVMGetPointerToGlobal(engine, action);
			println!("{}!", name);
		}
	}
}