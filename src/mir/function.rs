use crate::mir::scope::{StatementExplorer, explore_expression_mut};
use crate::mir::{
    MIRContext, MIRExpression, MIRExpressionInner, MIRFnSource, MIRFunction, MIRStatement, MIRType,
    MIRTypeInner,
};

/// Changes direct calls to indirect calls
/// when a variable with the name is available.
/// This needs to run before type checking, so
/// that type checking can accurately understand
/// a function's source.
pub fn resolve_fn_to_vars<'a>(ctx: &mut MIRContext<'_>) -> bool {
    let mut functions = ctx.program.functions.clone();

    for function in functions.values_mut() {
        if !<StatementExplorer>::explore_block_mut(
            &mut function.body,
            &|statement, scope| {
                match statement {
                    // No expressions.
                    MIRStatement::CreateVariable(_, ..) => {}
                    MIRStatement::DropVariable(_, ..) => {}
                    MIRStatement::Goto { .. } => {}
                    MIRStatement::Label { .. } => {}
                    MIRStatement::ContinueStatement { .. } => {}
                    MIRStatement::BreakStatement { .. } => {}
                    MIRStatement::LoopStatement { .. } => {}

                    MIRStatement::SetVariable { value, .. }
                    | MIRStatement::IfStatement {
                        condition: value, ..
                    }
                    | MIRStatement::GotoNotEqual {
                        condition: value, ..
                    } => {
                        resolve_expr_fn_to_vars(ctx, value);
                    }

                    MIRStatement::FunctionCall(fn_call) => {
                        for arg in &mut fn_call.args {
                            resolve_expr_fn_to_vars(ctx, arg);
                        }

                        if let MIRFnSource::Direct(name, span) = &fn_call.source {
                            // Direct function calls are only valid
                            // if the name points to a function.
                            // If name points to a variable, then
                            // it needs to be turned into indirect.
                            if scope.get_variable(name).is_some() {
                                fn_call.source = MIRFnSource::Indirect(MIRExpression {
                                    inner: MIRExpressionInner::Variable(name.clone()),
                                    span: span.clone(),
                                    ty: None,
                                });
                            }
                        }
                    }
                }

                true
            },
            &|_, _| true,
            &|_, _| true,
        ) {
            return false;
        }
    }

    true
}

/// Changes direct function calls to indirect
/// when it points to a variable.
fn resolve_expr_fn_to_vars<'a>(ctx: &MIRContext<'a>, expr: &mut MIRExpression<'a>) -> bool {
    explore_expression_mut(expr, &mut |expr| {
        // TODO: Handle function call expressions here.

        true
    })
}

/// Gets the type for a function as a function pointer.
pub fn get_fn_type<'a>(fn_data: &MIRFunction<'a>) -> MIRType<'a> {
    MIRType {
        ty: MIRTypeInner::FunctionPtr(
            fn_data.args.iter().map(|arg| arg.ty.ty.clone()).collect(),
            Box::new(fn_data.ret_ty.ty.clone()),
        ),
        span: Some(fn_data.span.clone()),
    }
}
