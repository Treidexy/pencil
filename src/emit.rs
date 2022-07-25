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
	};
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
				LLVMBuildLoad2(self.builder, LLVMGetElementType(LLVMTypeOf(var)), var, cstr!())
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
					let ptr = LLVMBuildGEP2(self.builder, ty, array, as_ptr!([ LLVMConstInt(LLVMInt64TypeInContext(self.ctx), i as u64, 0) ], LLVMValueRef), 1, cstr!());
					LLVMBuildStore(self.builder, *expr, ptr);
				}

				let ptr = LLVMBuildBitCast(self.builder, array, LLVMPointerType(ty, 0), cstr!("s"));
				let list = LLVMBuildAlloca(self.builder, self.emit_list_ty(ty), cstr!());
				let ele_ptr = LLVMBuildGEP2(self.builder, LLVMTypeOf(ptr), list, as_ptr!([ LLVMConstInt(LLVMInt64TypeInContext(self.ctx), 0, 0) ], LLVMValueRef), 1, cstr!());
				LLVMBuildStore(self.builder, ptr, ele_ptr);
				let len_ptr = LLVMBuildGEP2(self.builder, LLVMTypeOf(len_expr), list, as_ptr!([ LLVMConstInt(LLVMInt64TypeInContext(self.ctx), 1, 0) ], LLVMValueRef), 1, cstr!());
				LLVMBuildStore(self.builder, len_expr, len_ptr);
				list
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
		let mut x = [ ptr, LLVMInt64TypeInContext(self.ctx), LLVMInt64TypeInContext(self.ctx) ];
		LLVMStructTypeInContext(self.ctx, as_ptr!(x, LLVMTypeRef), 2, 0)
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
				let ty = self.emit_ty(&var.ty.kind);
				
				let ptr = LLVMAddGlobal(self.module, ty, cstr!(var.name.as_str()));
				let init = self.emit_expr(&var.init);
				LLVMBuildStore(self.builder, init, ptr);

				self.datas.insert(var.name.clone(), ptr);
			}

			LLVMBuildRetVoid(self.builder);
		}
	}
}