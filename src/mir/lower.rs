use crate::ir::{IRConstant, IRFunction, IRProgram, IRStatement, IRStatic, IRType, IRVariable};
use crate::mir::{
    MIRConstant, MIRExpression, MIRExpressionInner, MIRFunction, MIRProgram, MIRStatement,
    MIRStatic, MIRTypeInner, MIRVariable,
};
use std::borrow::Cow;

/// Converts a MIRProgram into an IRProgram.
pub fn mir_to_ir<'a>(program: &MIRProgram<'a>) -> IRProgram<'a> {
    let ir_constants = program
        .constants
        .iter()
        .map(|(name, mir_const)| (name.clone(), lower_constant(mir_const)))
        .collect();

    let ir_statics = program
        .statics
        .iter()
        .map(|(name, mir_static)| (name.clone(), lower_static(mir_static)))
        .collect();

    let ir_functions = program
        .functions
        .iter()
        .map(|(name, mir_func)| (name.clone(), lower_function(mir_func)))
        .collect();

    IRProgram {
        constants: ir_constants,
        statics: ir_statics,
        functions: ir_functions,
    }
}

/// Converts MIRType to IRType.
fn lower_type<'a>(mir_type: &MIRTypeInner<'a>) -> IRType<'a> {
    match mir_type {
        MIRTypeInner::U32 => IRType::U32,
        MIRTypeInner::Bool => IRType::Bool,
        MIRTypeInner::Unit => unreachable!("Unit type does not exist in IR!"),
        MIRTypeInner::Named(val) => IRType::Named(val.clone()),
    }
}

/// Converts MIRVariable to IRVariable.
fn lower_variable<'a>(mir_variable: &MIRVariable<'a>) -> IRVariable<'a> {
    IRVariable {
        name: mir_variable.name.clone(),
        ty: lower_type(&mir_variable.ty.ty),
    }
}

/// Converts MIRStatement to IRStatement.
fn lower_statement<'a>(mir_statement: &MIRStatement<'a>) -> IRStatement<'a> {
    match mir_statement {
        MIRStatement::CreateVariable(mir_var, ..) => {
            IRStatement::CreateVariable(lower_variable(mir_var))
        }
        MIRStatement::DropVariable(name, ..) => IRStatement::DropVariable(name.clone()),
        MIRStatement::SetVariable { name, value, .. } => lower_set_variable(name.clone(), &value),
        other => panic!("Unhandled statement during MIR lowering: {other:?}"),
    }
}

/// Lowers MIRStatement::SetVariable.
fn lower_set_variable<'a>(name: Cow<'a, str>, value: &MIRExpression<'a>) -> IRStatement<'a> {
    match value {
        MIRExpression {
            inner: MIRExpressionInner::Number(num, ..),
            ..
        } => IRStatement::SetVariableNum {
            name: name.clone(),
            value: *num,
        },
        MIRExpression {
            inner: MIRExpressionInner::Bool(val, ..),
            ..
        } => IRStatement::SetVariableNum {
            name: name.clone(),
            value: if *val { 1 } else { 0 },
        },
        MIRExpression {
            inner: MIRExpressionInner::Variable(var, ..),
            ..
        } => IRStatement::SetVariableVariable {
            name: name.clone(),
            value: var.clone(),
        },
        MIRExpression {
            inner:
                MIRExpressionInner::Add(
                    box MIRExpression {
                        inner: MIRExpressionInner::Number(num),
                        ..
                    },
                    box MIRExpression {
                        inner: MIRExpressionInner::Variable(var),
                        ..
                    },
                )
                | MIRExpressionInner::Add(
                    box MIRExpression {
                        inner: MIRExpressionInner::Variable(var),
                        ..
                    },
                    box MIRExpression {
                        inner: MIRExpressionInner::Number(num),
                        ..
                    },
                ),
            ..
        } => IRStatement::SetVariableAddNumVariable {
            name: name.clone(),
            value: (*num, var.clone()),
        },
        // No need to handle num num as const eval
        // removes it.
        MIRExpression {
            inner:
                MIRExpressionInner::Add(
                    box MIRExpression {
                        inner: MIRExpressionInner::Variable(var1),
                        ..
                    },
                    box MIRExpression {
                        inner: MIRExpressionInner::Variable(var2),
                        ..
                    },
                ),
            ..
        } => IRStatement::SetVariableAddVariableVariable {
            name: name.clone(),
            value: (var1.clone(), var2.clone()),
        },
        other => panic!("Unhandled statement during MIR lowering: {other:?}"),
    }
}

/// Converts MIRConstant to IRConstant.
fn lower_constant<'a>(mir_constant: &MIRConstant<'a>) -> IRConstant<'a> {
    let MIRExpressionInner::Number(num, ..) = mir_constant.value.inner else {
        panic!("Non-numeric expression during MIR lowering: {mir_constant:?}");
    };

    IRConstant {
        name: mir_constant.name.clone(),
        ty: lower_type(&mir_constant.ty.ty),
        value: num,
    }
}

/// Converts MIRStatic to IRStatic.
fn lower_static<'a>(mir_static: &MIRStatic<'a>) -> IRStatic<'a> {
    let MIRExpressionInner::Number(num, ..) = mir_static.value.inner else {
        panic!("Non-numeric expression during MIR lowering: {mir_static:?}");
    };

    IRStatic {
        name: mir_static.name.clone(),
        ty: lower_type(&mir_static.ty.ty),
        value: num,
    }
}

/// Converts MIRFunction to IRFunction.
fn lower_function<'a>(mir_function: &MIRFunction<'a>) -> IRFunction<'a> {
    // Unit means no return.
    let ir_ret_ty = match &mir_function.ret_ty.ty {
        MIRTypeInner::Unit => None,
        other => Some(lower_type(other)),
    };

    let ir_args = mir_function.args.iter().map(lower_variable).collect();

    let ir_body = mir_function.body.iter().map(lower_statement).collect();

    IRFunction {
        name: mir_function.name.clone(),
        ret_ty: ir_ret_ty,
        args: ir_args,
        body: ir_body,
    }
}
