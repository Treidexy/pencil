use std::collections::HashMap;
use std::ptr::null_mut;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use LLVMDoubleTypeInContext as fp_type;
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

	pub fn print(&self) {
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

				let ty = LLVMFunctionType(fp_type(self.ctx),
				&mut [ fp_type(self.ctx), fp_type(self.ctx) ] as *mut LLVMTypeRef, 2, 0);
				let func = LLVMAddFunction(self.module, cstr!("pow"), ty);
				LLVMBuildCall2(self.builder, ty, func, &mut [ left, right ] as *mut LLVMValueRef, 2, cstr!("pow"))
			},
			ExprKind::Neg(expr) => {
				let expr = self.emit_expr(expr.as_ref());
				LLVMBuildFNeg(self.builder, expr, cstr!())
			},
			ExprKind::Literal(val) => {
				LLVMConstReal(fp_type(self.ctx), *val)
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

				let len_expr = LLVMConstInt(LLVMInt64TypeInContext(self.ctx), exprs.len() as u64, 0);
				let array = LLVMBuildArrayMalloc(self.builder, ty, len_expr, cstr!());
				for (i, expr) in exprs.iter().enumerate() {
					let ptr = LLVMBuildGEP2(self.builder, ty, array,
						&mut [ LLVMConstInt(LLVMInt64TypeInContext(self.ctx), i as u64, 0) ] as *mut LLVMValueRef, 1, cstr!());
					LLVMBuildStore(self.builder, *expr, ptr);
				}

				let ptr = LLVMBuildBitCast(self.builder, array, LLVMPointerType(ty, 0), cstr!("s"));
				let list = LLVMBuildAlloca(self.builder, self.emit_list_ty(ty), cstr!());
				let ele_ptr = LLVMBuildGEP2(self.builder, LLVMTypeOf(ptr), list,
				&mut [ LLVMConstInt(LLVMInt64TypeInContext(self.ctx), 0, 0) ] as *mut LLVMValueRef, 1, cstr!());
				LLVMBuildStore(self.builder, ptr, ele_ptr);
				let len_ptr = LLVMBuildGEP2(self.builder, LLVMTypeOf(len_expr), list,
				&mut [ LLVMConstInt(LLVMInt64TypeInContext(self.ctx), 1, 0) ] as *mut LLVMValueRef, 1, cstr!());
				LLVMBuildStore(self.builder, len_expr, len_ptr);
				list
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
}

impl Emitter {
	unsafe fn emit_ty(&mut self, ty: &TypeKind) -> LLVMTypeRef {
		match *ty {
			TypeKind::Float => fp_type(self.ctx),
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
			&mut [ ptr, LLVMInt64TypeInContext(self.ctx), LLVMInt64TypeInContext(self.ctx) ] as *mut LLVMTypeRef, 2, 0)
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
		LLVMBuildStore(self.builder, expr, ptr);
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