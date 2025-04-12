use crate::mir::scope::Scope;
use crate::mir::{
    MIRConstant, MIRContext, MIRExpression, MIRExpressionInner, MIRStatement, MIRVariable,
};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU32, Ordering};

/// Attempts to evaluate all constants and statics, returning
/// whether it was successful.
pub fn const_eval(ctx: &mut MIRContext<'_>) -> bool {
    let mut current_evals = HashSet::new();
    let mut done_evals = HashSet::new();

    let const_names = ctx.program.constants.keys().cloned().collect::<Vec<_>>();
    let static_names = ctx.program.statics.keys().cloned().collect::<Vec<_>>();

    for constant in const_names {
        if !eval_constant(ctx, constant, &mut current_evals, &mut done_evals) {
            return false;
        }
    }

    for static_name in static_names {
        if !eval_static(ctx, static_name) {
            return false;
        }
    }

    true
}

/// Attempts to optimize all expressions
/// in every function, returning
/// whether it was successful.
/// This MUST occur after const evaluation.
pub fn const_optimize_expr(ctx: &mut MIRContext<'_>) -> bool {
    for function in ctx.program.functions.values_mut() {
        for statement in function.body.iter_mut() {
            match statement {
                // No expressions.
                MIRStatement::CreateVariable(_, ..) => {}
                MIRStatement::DropVariable(_, ..) => {}
                MIRStatement::SetVariable { value, .. } => {
                    *value = reduce_expr_simple(&ctx.program.constants, &value.clone());
                }
            }
        }
    }

    true
}

/// Attempts to evaluate a constant, returning
/// whether it was successful.
/// Evaluation means that it's reduced to a primitive.
fn eval_constant<'a>(
    ctx: &mut MIRContext<'a>,
    constant_name: Cow<'a, str>,
    current_evals: &mut HashSet<Cow<'a, str>>,
    done_evals: &mut HashSet<Cow<'a, str>>,
) -> bool {
    if done_evals.contains(&constant_name) {
        // Already done.
        return true;
    }

    if current_evals.contains(&constant_name) {
        // Eval loop.
        eprintln!("Constant loop detected: {current_evals:?}");
        return false;
    }

    current_evals.insert(constant_name.clone());

    let old_expr = ctx.program.constants[&constant_name].value.clone();
    let reduced = reduce_expr(&old_expr, &mut |name| {
        // Ensure the constant exists.
        if !ctx.program.constants.contains_key(&name) {
            return None;
        }

        // Ensure the constant is evaluated.
        if !eval_constant(ctx, name.clone(), current_evals, done_evals) {
            return None;
        }

        // No need to validate that this is a primitive here,
        // since eval_constant already does that.
        Some(ctx.program.constants[&name].value.clone())
    });

    // Constants must be fully reduced.
    if !matches!(reduced.inner, MIRExpressionInner::Number(_, ..)) {
        eprintln!("Failed to reduce constant to a number: {:?}", &old_expr);
        return false;
    }

    ctx.program.constants.get_mut(&constant_name).unwrap().value = reduced;

    current_evals.remove(&constant_name);
    done_evals.insert(constant_name);

    true
}

/// Attempts to evaluate a static, returning
/// whether it was successful.
/// This MUST occur after constant evaluation.
/// Evaluation means that it's reduced to a primitive.
fn eval_static<'a>(ctx: &mut MIRContext<'a>, constant_name: Cow<'a, str>) -> bool {
    let old_expr = ctx.program.statics[&constant_name].value.clone();
    let reduced = reduce_expr_simple(&ctx.program.constants, &old_expr);

    // Statics must be fully reduced.
    if !matches!(reduced.inner, MIRExpressionInner::Number(_, ..)) {
        eprintln!("Failed to reduce static to a number: {:?}", &old_expr);
        return false;
    }

    ctx.program.statics.get_mut(&constant_name).unwrap().value = reduced;

    true
}

/// Expression reduction that uses
/// the values inside constants.
/// This MUST be run after constant evaluation.
fn reduce_expr_simple<'a>(
    constants: &HashMap<Cow<'a, str>, MIRConstant<'a>>,
    expr: &MIRExpression<'a>,
) -> MIRExpression<'a> {
    reduce_expr(&expr, &mut |name| {
        // Ensure the constant exists.
        if !constants.contains_key(&name) {
            return None;
        }

        // Constants are guaranteed to already be evaluated.

        // No need to validate that this is a primitive here,
        // since eval_constant already does that.
        Some(constants[&name].value.clone())
    })
}

/// Attempts to reduce an expression
/// using simple constant evaluation.
fn reduce_expr<'a>(
    expr: &MIRExpression<'a>,
    get_const: &mut impl FnMut(Cow<'a, str>) -> Option<MIRExpression<'a>>,
) -> MIRExpression<'a> {
    let new_expr = (|| match &expr.inner {
        MIRExpressionInner::Add(left, right) => {
            let left = reduce_expr(left, get_const);
            let right = reduce_expr(right, get_const);

            if let MIRExpressionInner::Number(left, ..) = left.inner {
                if let MIRExpressionInner::Number(right, ..) = right.inner {
                    return MIRExpressionInner::Number(left + right);
                }
            }

            MIRExpressionInner::Add(Box::new(left), Box::new(right))
        }
        MIRExpressionInner::Sub(left, right) => {
            let left = reduce_expr(left, get_const);
            let right = reduce_expr(right, get_const);

            if let MIRExpressionInner::Number(left, ..) = left.inner {
                if let MIRExpressionInner::Number(right, ..) = right.inner {
                    return MIRExpressionInner::Number(left - right);
                }
            }

            MIRExpressionInner::Sub(Box::new(left), Box::new(right))
        }
        MIRExpressionInner::Mul(left, right) => {
            let left = reduce_expr(left, get_const);
            let right = reduce_expr(right, get_const);

            if let MIRExpressionInner::Number(left, ..) = left.inner {
                if let MIRExpressionInner::Number(right, ..) = right.inner {
                    return MIRExpressionInner::Number(left * right);
                }
            }

            MIRExpressionInner::Mul(Box::new(left), Box::new(right))
        }
        MIRExpressionInner::Div(left, right) => {
            let left = reduce_expr(left, get_const);
            let right = reduce_expr(right, get_const);

            if let MIRExpressionInner::Number(left, ..) = left.inner {
                if let MIRExpressionInner::Number(right, ..) = right.inner {
                    return MIRExpressionInner::Number(left / right);
                }
            }

            MIRExpressionInner::Div(Box::new(left), Box::new(right))
        }
        MIRExpressionInner::Number(val) => MIRExpressionInner::Number(*val),
        MIRExpressionInner::Variable(name) => get_const(name.clone())
            .map(|v| v.inner)
            .unwrap_or(MIRExpressionInner::Variable(name.clone())),
    })();

    MIRExpression {
        inner: new_expr,
        ty: expr.ty.clone(),
        span: expr.span.clone(),
    }
}
