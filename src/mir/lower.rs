use crate::ir::{IRConstant, IRFunction, IRProgram, IRStatement, IRStatic, IRType, IRVariable};
use crate::mir::{
    MIRConstant, MIRExpression, MIRFunction, MIRProgram, MIRStatement, MIRStatic, MIRType,
    MIRVariable,
};

/// Converts a MIRProgram into an IRProgram.
pub fn mir_to_ir<'a>(program: &MIRProgram<'a>) -> IRProgram<'a> {
    let ir_constants = program
        .constants
        .iter()
        .map(|(name, mir_const)| (*name, lower_constant(mir_const)))
        .collect();

    let ir_statics = program
        .statics
        .iter()
        .map(|(name, mir_static)| (*name, lower_static(mir_static)))
        .collect();

    let ir_functions = program
        .functions
        .iter()
        .map(|(name, mir_func)| (*name, lower_function(mir_func)))
        .collect();

    IRProgram {
        constants: ir_constants,
        statics: ir_statics,
        functions: ir_functions,
    }
}

/// Converts MIRType to IRType.
fn lower_type(mir_type: &MIRType) -> IRType {
    match mir_type {
        MIRType::U32 => IRType::U32,
        MIRType::Unit => unreachable!("Unit type does not exist in IR!"),
    }
}

/// Converts MIRVariable to IRVariable.
fn lower_variable<'a>(mir_variable: &MIRVariable<'a>) -> IRVariable<'a> {
    IRVariable {
        name: mir_variable.name,
        ty: lower_type(&mir_variable.ty.ty),
    }
}

/// Converts MIRStatement to IRStatement.
fn lower_statement<'a>(mir_statement: &MIRStatement<'a>) -> IRStatement<'a> {
    match mir_statement {
        MIRStatement::CreateVariable(mir_var, ..) => {
            IRStatement::CreateVariable(lower_variable(mir_var))
        }
        MIRStatement::DropVariable(name, ..) => IRStatement::DropVariable(name),
        MIRStatement::SetVariable {
            name,
            value: MIRExpression::Number(num, ..),
            ..
        } => IRStatement::SetVariableNum { name, value: *num },
        other => panic!("Unhandled statement during MIR lowering: {other:?}"),
    }
}

/// Converts MIRConstant to IRConstant.
fn lower_constant<'a>(mir_constant: &MIRConstant<'a>) -> IRConstant<'a> {
    let MIRExpression::Number(num, ..) = mir_constant.value else {
        panic!("Non-numeric expression during MIR lowering: {mir_constant:?}");
    };

    IRConstant {
        name: mir_constant.name,
        ty: lower_type(&mir_constant.ty.ty),
        value: num,
    }
}

/// Converts MIRStatic to IRStatic.
fn lower_static<'a>(mir_static: &MIRStatic<'a>) -> IRStatic<'a> {
    let MIRExpression::Number(num, ..) = mir_static.value else {
        panic!("Non-numeric expression during MIR lowering: {mir_static:?}");
    };

    IRStatic {
        name: mir_static.name,
        ty: lower_type(&mir_static.ty.ty),
        value: num,
    }
}

/// Converts MIRFunction to IRFunction.
fn lower_function<'a>(mir_function: &MIRFunction<'a>) -> IRFunction<'a> {
    // Unit means no return.
    let ir_ret_ty = match mir_function.ret_ty.ty {
        MIRType::Unit => None,
        MIRType::U32 => Some(IRType::U32),
    };

    let ir_args = mir_function.args.iter().map(lower_variable).collect();

    let ir_body = mir_function.body.iter().map(lower_statement).collect();

    IRFunction {
        name: mir_function.name,
        ret_ty: ir_ret_ty,
        args: ir_args,
        body: ir_body,
    }
}
