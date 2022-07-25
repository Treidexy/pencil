use std::ptr::null_mut;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use LLVMDoubleTypeInContext as fp_type;
use crate::parse::{ Expr, ExprKind };

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

pub fn emit_expr(ctx: LLVMContextRef, module: LLVMModuleRef, builder: LLVMBuilderRef, expr: &Expr) -> LLVMValueRef {
	unsafe {
		match &expr.kind {
			ExprKind::Add(left, right) => {
				let left = emit_expr(ctx, module, builder, left.as_ref());
				let right = emit_expr(ctx, module, builder, right.as_ref());
				LLVMBuildFAdd(builder, left, right, cstr!())
			},
			ExprKind::Sub(left, right) => {
				let left = emit_expr(ctx, module, builder, left.as_ref());
				let right = emit_expr(ctx, module, builder, right.as_ref());
				LLVMBuildFSub(builder, left, right, cstr!())
			},
			ExprKind::Mul(left, right) => {
				let left = emit_expr(ctx, module, builder, left.as_ref());
				let right = emit_expr(ctx, module, builder, right.as_ref());
				LLVMBuildFMul(builder, left, right, cstr!())
			},
			ExprKind::Div(left, right) => {
				let left = emit_expr(ctx, module, builder, left.as_ref());
				let right = emit_expr(ctx, module, builder, right.as_ref());
				LLVMBuildFDiv(builder, left, right, cstr!())
			},
			ExprKind::Mod(left, right) => {
				let left = emit_expr(ctx, module, builder, left.as_ref());
				let right = emit_expr(ctx, module, builder, right.as_ref());
				LLVMBuildFRem(builder, left, right, cstr!())
			},
			ExprKind::Pow(left, right) => {
				let left = emit_expr(ctx, module, builder, left.as_ref());
				let right = emit_expr(ctx, module, builder, right.as_ref());

				let ty = LLVMFunctionType(fp_type(ctx), as_ptr!([ fp_type(ctx), fp_type(ctx) ], LLVMTypeRef), 2, 0);
				let func = LLVMAddFunction(module, cstr!("pow"), ty);
				LLVMBuildCall2(builder, ty, func, as_ptr!([ left, right ], LLVMValueRef), 2, cstr!("pow"))
			},
			ExprKind::Literal(val) => {
				LLVMConstReal(fp_type(ctx), *val)
			},
			_ => {
				todo!()
			},
		}
	}
}