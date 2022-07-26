mod lex;
use std::mem::size_of;
use std::ptr::null_mut;

use lex::*;

mod parse;
use llvm_sys::execution_engine::LLVMCreateMCJITCompilerForModule;
use llvm_sys::execution_engine::LLVMExecutionEngineRef;
use llvm_sys::execution_engine::LLVMLinkInMCJIT;
use llvm_sys::execution_engine::LLVMMCJITCompilerOptions;
use llvm_sys::target::LLVM_InitializeNativeAsmPrinter;
use llvm_sys::target_machine::LLVMCodeModel::*;
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

	let module = emitter.module;
	unsafe {
		LLVMLinkInMCJIT();
		LLVM_InitializeNativeAsmPrinter();
		LLVM_InitializeNativeAsmPrinter();

		let mut engine: LLVMExecutionEngineRef = null_mut();
		let mut options = LLVMMCJITCompilerOptions {
			OptLevel: 0,
			CodeModel: LLVMCodeModelDefault,
			NoFramePointerElim: 1,
			EnableFastISel: 0,
			MCJMM: null_mut(),
		};
		let mut err = null_mut::<i8>();
		if LLVMCreateMCJITCompilerForModule(&mut engine as *mut LLVMExecutionEngineRef, module,
			&mut options as *mut LLVMMCJITCompilerOptions, size_of::<LLVMMCJITCompilerOptions>(), &mut err as *mut *mut i8) == 1 {
			panic!("Error creating JIT compiler: {}", std::ffi::CString::from_raw(err).to_str().unwrap());
		}
	}

}