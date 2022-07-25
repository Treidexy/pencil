use std::collections::HashMap;
use std::ptr::null_mut;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use LLVMDoubleTypeInContext as fp_type;
use crate::parse::{ Expr, ExprKind, Type, TypeKind, DataAst, Var };

macro_rules! cstr {
	($s:expr) => {
		::std::ffi::CString::new($s).unwrap().as_ptr()
	};
	() => {
		::std::ffi::CString::new("").unwrap().as_ptr()
	}
}

pub(crate) use cstr;

macro_rules! as_ptr {
	($obj:expr, $other:ty) => {
		&mut $obj as *mut $other
	};
}

pub struct Emitter {
	ctx: LLVMContextRef,
	module: LLVMModuleRef,
	builder: LLVMBuilderRef,
	datas: HashMap<String, LLVMValueRef>,
}

impl Emitter {
	pub fn new() -> Self {
		unsafe {
			let ctx = LLVMContextCreate();
			let module = LLVMModuleCreateWithNameInContext(cstr!("test"), ctx);
			let builder = LLVMCreateBuilderInContext(ctx);
		
			Emitter {
				ctx,
				module,
				builder,
				datas: HashMap::new(),
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

				let ty = LLVMFunctionType(fp_type(self.ctx), as_ptr!([ fp_type(self.ctx), fp_type(self.ctx) ], LLVMTypeRef), 2, 0);
				let func = LLVMAddFunction(self.module, cstr!("pow"), ty);
				LLVMBuildCall2(self.builder, ty, func, as_ptr!([ left, right ], LLVMValueRef), 2, cstr!("pow"))
			},
			ExprKind::Neg(expr) => {
				let expr = self.emit_expr(expr.as_ref());
				LLVMBuildFNeg(self.builder, expr, cstr!())
			},
			ExprKind::Literal(val) => {
				LLVMConstReal(fp_type(self.ctx), *val)
			},
			ExprKind::Name(val) => {
				let var = *self.datas.get(val).unwrap();
				LLVMBuildLoad2(self.builder, LLVMTypeOf(var), var, cstr!())
			},
			_ => {
				todo!()
			},
		}
	}
}

impl Emitter {
	pub fn emit_data(&mut self, ast: &DataAst) {
		unsafe {
			{
				let ty = LLVMFunctionType(LLVMVoidTypeInContext(self.ctx), null_mut(), 0, 0);
				let func = LLVMAddFunction(self.module, cstr!(".init"), ty);
				let entry = LLVMAppendBasicBlockInContext(self.ctx, func, cstr!("entry"));
				LLVMPositionBuilderAtEnd(self.builder, entry);
			}

			for var in &ast.vars {
				let ty = match var.ty.kind {
					TypeKind::Float => fp_type(self.ctx),
					_ => todo!(),
				};
				
				let ptr = LLVMAddGlobal(self.module, ty, cstr!(var.name.as_str()));
				let init = self.emit_expr(&var.init);
				LLVMBuildStore(self.builder, init, ptr);

				self.datas.insert(var.name.clone(), ptr);
			}

			LLVMBuildRetVoid(self.builder);
		}
	}
}