use std::collections::HashMap;
use std::ptr::null_mut;

use llvm_sys::LLVMTypeKind::*;
use llvm_sys::analysis::LLVMVerifierFailureAction::*;
use llvm_sys::analysis::*;
use llvm_sys::bit_writer::LLVMWriteBitcodeToFile;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::target_machine::LLVMCodeGenOptLevel::*;
use llvm_sys::target_machine::LLVMRelocMode::*;
use llvm_sys::target_machine::LLVMCodeModel::*;
use llvm_sys::target_machine::LLVMCodeGenFileType::*;
use crate::parse::Mutation;
use crate::parse::{ Expr, ExprKind, Type, TypeKind, Var, Func, Action, Stmt };

macro_rules! cstr {
	($s:expr) => {
		::std::ffi::CString::new($s).unwrap().as_ptr()
	};
	() => {
		::std::ffi::CString::new("").unwrap().as_ptr()
	};
}

macro_rules! from_cstr {
	($s:expr) => {
		::std::ffi::CString::from_raw($s).to_str().unwrap()
	};
}

pub(crate) use cstr;

pub struct Emitter {
	ctx: LLVMContextRef,
	module: LLVMModuleRef,
	builder: LLVMBuilderRef,
	vars: HashMap<String, LLVMValueRef>,
	funcs: HashMap<String, LLVMValueRef>,
	actions: HashMap<String, LLVMValueRef>,
	params: HashMap<String, LLVMValueRef>,
	param_ptrs: HashMap<String, LLVMValueRef>,
}

impl Emitter {
	pub fn new() -> Self {
		unsafe {
			let ctx = LLVMContextCreate();
			let module = LLVMModuleCreateWithNameInContext(cstr!(), ctx);
			let builder = LLVMCreateBuilderInContext(ctx);
		
			Emitter {
				ctx,
				module,
				builder,
				vars: HashMap::new(),
				funcs: HashMap::new(),
				actions: HashMap::new(),
				params: HashMap::new(),
				param_ptrs: HashMap::new(),
			}
		}
	}

	pub fn drop(&mut self) {
		unsafe {
			LLVMContextDispose(LLVMGetModuleContext(self.module));
			LLVMDisposeBuilder(self.builder);
			LLVMDisposeModule(self.module);
		}
	}

	pub fn print_ir(&self) {
		unsafe {
			let out = LLVMPrintModuleToString(self.module);
			println!("{}", std::ffi::CString::from_raw(out).to_str().unwrap());
		}
	}
}

impl Emitter {
	unsafe fn emit_expr(&mut self, expr: &Expr) -> LLVMValueRef {
		match &expr.kind {
			ExprKind::Add(left, right) => {
				let left = self.emit_expr(left.as_ref());
				let right = self.emit_expr(right.as_ref());
				LLVMBuildFAdd(self.builder, left, right, cstr!())
			},
			ExprKind::Sub(left, right) => {
				let left = self.emit_expr(left.as_ref());
				let right = self.emit_expr(right.as_ref());
				LLVMBuildFSub(self.builder, left, right, cstr!())
			},
			ExprKind::Mul(left, right) => {
				let left = self.emit_expr(left.as_ref());
				let right = self.emit_expr(right.as_ref());
				LLVMBuildFMul(self.builder, left, right, cstr!())
			},
			ExprKind::Div(left, right) => {
				let left = self.emit_expr(left.as_ref());
				let right = self.emit_expr(right.as_ref());
				LLVMBuildFDiv(self.builder, left, right, cstr!())
			},
			ExprKind::Mod(left, right) => {
				let left = self.emit_expr(left.as_ref());
				let right = self.emit_expr(right.as_ref());
				LLVMBuildFRem(self.builder, left, right, cstr!())
			},
			ExprKind::Pow(left, right) => {
				let left = self.emit_expr(left.as_ref());
				let right = self.emit_expr(right.as_ref());

				let ty = LLVMFunctionType(self.fp_ty(),
				&mut [ self.fp_ty(), self.fp_ty() ] as *mut LLVMTypeRef, 2, 0);
				let func = LLVMAddFunction(self.module, cstr!("pow"), ty);
				LLVMBuildCall2(self.builder, ty, func, &mut [ left, right ] as *mut LLVMValueRef, 2, cstr!("pow"))
			},
			ExprKind::Join(left, right) => {
				let left = self.emit_expr(left.as_ref());
				let right = self.emit_expr(right.as_ref());
				self.emit_join(left, right)
			},
			ExprKind::Neg(expr) => {
				let expr = self.emit_expr(expr.as_ref());
				LLVMBuildFNeg(self.builder, expr, cstr!())
			},
			ExprKind::Literal(val) => {
				LLVMConstReal(self.fp_ty(), *val)
			},
			ExprKind::Name(name) => {
				if let Some(param) = self.params.get(name) {
					*param
				} else if let Some(var) = self.vars.get(name) {
					LLVMBuildLoad2(self.builder, LLVMGetElementType(LLVMTypeOf(*var)), *var, cstr!())
				} else {
					todo!("{}", name);
				}
			},
			ExprKind::Array(exprs) => {
				let exprs = exprs.iter().map(|expr| self.emit_expr(expr)).collect::<Vec<_>>();
				let ty = if exprs.len() > 0 {
					LLVMTypeOf(exprs[0])
				} else {
					LLVMVoidTypeInContext(self.ctx)
				};

				let len_expr = LLVMConstInt(self.len_ty(), exprs.len() as u64, 0);
				let array = LLVMBuildArrayMalloc(self.builder, ty, len_expr, cstr!());
				for (i, expr) in exprs.iter().enumerate() {
					let ptr = LLVMBuildInBoundsGEP2(self.builder, ty, array,
						&mut [ LLVMConstInt(self.len_ty(), i as u64, 0) ] as *mut LLVMValueRef, 1, cstr!());
					LLVMBuildStore(self.builder, *expr, ptr);
				}

				let ptr = LLVMBuildBitCast(self.builder, array, LLVMPointerType(ty, 0), cstr!("s"));
				let list = LLVMBuildAlloca(self.builder, self.emit_list_ty(ty), cstr!());
				let ele_ptr = LLVMBuildInBoundsGEP2(self.builder, LLVMTypeOf(ptr), list,
				&mut [ LLVMConstInt(self.len_ty(), 0, 0) ] as *mut LLVMValueRef, 1, cstr!());
				LLVMBuildStore(self.builder, ptr, ele_ptr);
				let len_ptr = LLVMBuildInBoundsGEP2(self.builder, LLVMTypeOf(len_expr), list,
				&mut [ LLVMConstInt(self.len_ty(), 1, 0) ] as *mut LLVMValueRef, 1, cstr!());
				LLVMBuildStore(self.builder, len_expr, len_ptr);
				
				LLVMBuildLoad2(self.builder, LLVMGetElementType(LLVMTypeOf(list)), list, cstr!())
			},
			ExprKind::Call(name, args) => {
				let mut args = args.iter().map(|arg| self.emit_expr(arg)).collect::<Vec<_>>();
				let func = if let Some(func) = self.funcs.get(name) {
					*func
				} else {
					todo!();
				};
				
				LLVMBuildCall2(self.builder, LLVMGetReturnType(LLVMTypeOf(func)), func, args.as_mut_ptr(), args.len() as u32, cstr!())
			},
			_ => {
				todo!()
			},
		}
	}

	unsafe fn emit_join(&mut self, left: LLVMValueRef, right: LLVMValueRef) -> LLVMValueRef {
		let left_struct = LLVMBuildAlloca(self.builder, LLVMTypeOf(left), cstr!("left_struct"));
		let right_struct = LLVMBuildAlloca(self.builder, LLVMTypeOf(left), cstr!("right_struct"));

		LLVMBuildStore(self.builder, left, left_struct);
		LLVMBuildStore(self.builder, right, right_struct);

		let list_ty = LLVMTypeOf(left);
		let ele_ty = LLVMGetElementType(LLVMStructGetTypeAtIndex(list_ty, 0));
		let ptr_ty = LLVMPointerType(ele_ty, 0);

		let left_len = LLVMBuildStructGEP2(self.builder, list_ty, left_struct, 1, cstr!());
		let left_len = LLVMBuildLoad2(self.builder, self.len_ty(), left_len, cstr!("left_len"));
		let right_len = LLVMBuildStructGEP2(self.builder, list_ty, right_struct, 1, cstr!());
		let right_len = LLVMBuildLoad2(self.builder, self.len_ty(), right_len, cstr!("right_len"));
		let len = LLVMBuildAdd(self.builder, left_len, right_len, cstr!("len"));

		let ptr = LLVMBuildArrayMalloc(self.builder, ele_ty, len, cstr!("ptr"));
		let left_ptr = LLVMBuildStructGEP2(self.builder, list_ty, left_struct, 0, cstr!());
		let left_ptr = LLVMBuildLoad2(self.builder, ptr_ty, left_ptr, cstr!("left_ptr"));
		let right_ptr = LLVMBuildStructGEP2(self.builder, list_ty, right_struct, 0, cstr!());
		let right_ptr = LLVMBuildLoad2(self.builder, ptr_ty, right_ptr, cstr!("right_ptr"));

		let cpy_size = LLVMBuildMul(self.builder, left_len, LLVMSizeOf(ele_ty), cstr!("left_cpy_size"));
		LLVMBuildMemCpy(self.builder, ptr, 8, left_ptr, 8, cpy_size);

		let ptr_as_len = LLVMBuildPtrToInt(self.builder, ptr, self.len_ty(), cstr!());
		let offsetted_ptr = LLVMBuildAdd(self.builder, ptr_as_len, cpy_size, cstr!());
		let offsetted_ptr = LLVMBuildIntToPtr(self.builder, offsetted_ptr, LLVMTypeOf(ptr), cstr!("offsetted_ptr"));

		let cpy_size = LLVMBuildMul(self.builder, right_len, LLVMSizeOf(ele_ty), cstr!("right_cpy_size"));
		LLVMBuildMemCpy(self.builder, offsetted_ptr, 8, right_ptr, 8, cpy_size);

		let list = LLVMBuildAlloca(self.builder, list_ty, cstr!("list"));
		let list_ptr = LLVMBuildStructGEP2(self.builder, list_ty, list, 0, cstr!());
		let list_len = LLVMBuildStructGEP2(self.builder, list_ty, list, 1, cstr!());
		LLVMBuildStore(self.builder, ptr, list_ptr);
		LLVMBuildStore(self.builder, len, list_len);

		LLVMBuildLoad2(self.builder, LLVMGetElementType(LLVMTypeOf(list)), list, cstr!())
	}
}

impl Emitter {
	unsafe fn emit_ty(&mut self, ty: &TypeKind) -> LLVMTypeRef {
		match *ty {
			TypeKind::Float => self.fp_ty(),
			TypeKind::Array(ref inner_ty, len) => {
				let inner_ty = self.emit_ty(&inner_ty.as_ref().kind);
				LLVMArrayType(inner_ty, len as u32)
			},
			TypeKind::List(ref inner_ty) => {
				let inner_ty = self.emit_ty(&inner_ty.as_ref().kind);
				self.emit_list_ty(inner_ty)
			},
			_ => {
				todo!()
			},
		}
	}

	unsafe fn emit_list_ty(&mut self, inner_ty: LLVMTypeRef) -> LLVMTypeRef {
		let ptr = LLVMPointerType(inner_ty, 0);
		LLVMStructTypeInContext(self.ctx,
			&mut [ ptr, self.len_ty() ] as *mut LLVMTypeRef, 2, 0)
	}

	unsafe fn fp_ty(&mut self) -> LLVMTypeRef {
		LLVMDoubleTypeInContext(self.ctx)
	}

	unsafe fn len_ty(&mut self) -> LLVMTypeRef {
		LLVMInt64TypeInContext(self.ctx)
	}
}

impl Emitter {
	unsafe fn emit_mutation(&mut self, mutation: &Mutation) {
		let ptr = if let Some(param_ptr) = self.param_ptrs.get(&mutation.name) {
			*param_ptr
		} else if let Some(var) = self.vars.get(&mutation.name) {
			*var
		} else {
			todo!();
		};
		
		let expr = self.emit_expr(&mutation.expr);
		self.emit_destroy(ptr);
		LLVMBuildStore(self.builder, expr, ptr);
	}

	unsafe fn emit_destroy(&mut self, ptr: LLVMValueRef) {
		let ty = LLVMGetElementType(LLVMTypeOf(ptr));
		let kind = LLVMGetTypeKind(ty);
		match kind {
			LLVMDoubleTypeKind => {},
			LLVMStructTypeKind => {
				let len = LLVMCountStructElementTypes(ty);
				let mut ele_tys = Vec::with_capacity(len as usize);
				ele_tys.resize(len as usize, null_mut());
				LLVMGetStructElementTypes(ty, ele_tys.as_mut_ptr());
				if ele_tys.len() == 2 && LLVMGetTypeKind(ele_tys[0]) == LLVMPointerTypeKind && ele_tys[1] == self.len_ty() {
					let ele_ptr = LLVMBuildStructGEP2(self.builder, ty, ptr, 0, cstr!());
					let ele = LLVMBuildLoad2(self.builder, ele_tys[0], ele_ptr, cstr!());
					LLVMBuildFree(self.builder, ele);
				} else {
					for (i, ele_ty) in ele_tys.iter().enumerate() {
						let ele_ptr = LLVMBuildStructGEP2(self.builder, ty, ptr, i as u32, cstr!());
						let ele = LLVMBuildLoad2(self.builder, *ele_ty, ele_ptr, cstr!());
						self.emit_destroy(ele)
					}
				}
			},
			_ => {
				todo!()
			},
		}
	}
}

impl Emitter {
	pub fn emit(&mut self, stmts: &Vec<Stmt>) {
		unsafe {
			{
				let ty = LLVMFunctionType(LLVMVoidTypeInContext(self.ctx), null_mut(), 0, 0);
				let func = LLVMAddFunction(self.module, cstr!(".init"), ty);
				let entry = LLVMAppendBasicBlockInContext(self.ctx, func, cstr!("entry"));
				LLVMPositionBuilderAtEnd(self.builder, entry);
			}

			for stmt in stmts {
				self.emit_stmt(stmt);
			}

			LLVMBuildRetVoid(self.builder);

			let mut err_msg: *mut i8 = null_mut();
			if LLVMPrintModuleToFile(self.module, cstr!("bin/test.ll"), &mut err_msg as *mut *mut i8) != 0 {
				panic!("{}", from_cstr!(err_msg));
			}

			if LLVMVerifyModule(self.module, LLVMPrintMessageAction, &mut err_msg as *mut *mut i8) != 0 {
				panic!();
			}
			
			if LLVMWriteBitcodeToFile(self.module, cstr!("bin/test.bc")) != 0 {
				panic!();
			}
			

			LLVM_InitializeAllTargetInfos();
			LLVM_InitializeAllTargets();
			LLVM_InitializeAllTargetMCs();
			LLVM_InitializeAllAsmParsers();
			LLVM_InitializeAllAsmPrinters();

			let triple = LLVMGetDefaultTargetTriple();
			let mut target: LLVMTargetRef = null_mut();
			if LLVMGetTargetFromTriple(triple, &mut target as *mut LLVMTargetRef, &mut err_msg as *mut *mut i8) != 0 {
				panic!("{}", from_cstr!(err_msg));
			}

			let target_machine = LLVMCreateTargetMachine(target, triple, cstr!("generic"), cstr!(""),
				LLVMCodeGenLevelAggressive, LLVMRelocDefault, LLVMCodeModelDefault);
				if LLVMTargetMachineEmitToFile(target_machine, self.module, cstr!("bin/test.o") as *mut i8,
						LLVMObjectFile, &mut err_msg as *mut *mut i8) != 0 {
					panic!("{}", from_cstr!(err_msg));
				}
				if LLVMTargetMachineEmitToFile(target_machine, self.module, cstr!("bin/test.s") as *mut i8,
						LLVMAssemblyFile, &mut err_msg as *mut *mut i8) != 0 {
					panic!("{}", from_cstr!(err_msg));
				}
		}
	}

	unsafe fn emit_stmt(&mut self, stmt: &Stmt) -> LLVMValueRef {
		match stmt {
			Stmt::Var(var) => {
				self.emit_var(var)
			},
			Stmt::Func(func) => {
				self.emit_func(func)
			},
			Stmt::Action(action) => {
				self.emit_action(action)
			},
			_ => {
				todo!()
			},
		}
	}

	unsafe fn emit_var(&mut self, var: &Var) -> LLVMValueRef {
		let ty = self.emit_ty(&var.ty.kind);
		
		let ptr = LLVMAddGlobal(self.module, ty, cstr!(var.name.as_str()));
		LLVMSetInitializer(ptr, LLVMConstNull(ty));
		let init = self.emit_expr(&var.init);
		LLVMBuildStore(self.builder, init, ptr);

		self.vars.insert(var.name.clone(), ptr);
		ptr
	}

	unsafe fn emit_func(&mut self, func: &Func) -> LLVMValueRef {
		let prev_block = LLVMGetInsertBlock(self.builder);

		let ret = self.emit_ty(&func.ret.kind);
		let mut params = func.params.iter().map(|param| self.emit_ty(&param.ty.kind)).collect::<Vec<_>>();
		let ty = LLVMFunctionType(ret, params.as_mut_ptr(), params.len() as u32, 0);
		let llfunc = LLVMAddFunction(self.module, cstr!(func.name.as_str()), ty);
		self.funcs.insert(func.name.clone(), llfunc);

		let entry = LLVMAppendBasicBlockInContext(self.ctx, llfunc, cstr!("entry"));
		LLVMPositionBuilderAtEnd(self.builder, entry);

		self.params.clear();
		for (i, param) in func.params.iter().enumerate() {
			let ptr = LLVMGetParam(llfunc, i as u32);
			self.params.insert(param.name.clone(), ptr);
		}

		let body = self.emit_expr(&func.body);
		LLVMBuildRet(self.builder, body);

		LLVMPositionBuilderAtEnd(self.builder, prev_block);

		llfunc
	}

	unsafe fn emit_action(&mut self, action: &Action) -> LLVMValueRef {
		let prev_block = LLVMGetInsertBlock(self.builder);

		let ret = LLVMVoidTypeInContext(self.ctx);
		let mut params = action.params.iter().map(|param| LLVMPointerType(self.emit_ty(&param.ty.kind), 0)).collect::<Vec<_>>();
		let ty = LLVMFunctionType(ret, params.as_mut_ptr(), params.len() as u32, 0);
		let llfunc = LLVMAddFunction(self.module, cstr!(format!("{}!", action.name).as_str()), ty);
		self.actions.insert(action.name.clone(), llfunc);
		let entry = LLVMAppendBasicBlockInContext(self.ctx, llfunc, cstr!("entry"));
		LLVMPositionBuilderAtEnd(self.builder, entry);

		self.params.clear();
		for (i, param) in action.params.iter().enumerate() {
			let ptr = LLVMGetParam(llfunc, i as u32);
			self.param_ptrs.insert(param.name.clone(), ptr);
			let val = LLVMBuildLoad2(self.builder, LLVMGetElementType(LLVMTypeOf(ptr)), ptr, cstr!());
			self.params.insert(param.name.clone(), val);
		}

		for mutation in action.body.iter() {
			self.emit_mutation(mutation);
		}

		LLVMBuildRetVoid(self.builder);

		LLVMPositionBuilderAtEnd(self.builder, prev_block);

		llfunc
	}
}