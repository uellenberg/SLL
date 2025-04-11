use crate::mir::{MIRConstant, MIRExpression, MIRProgram, MIRStatement};
use std::collections::{HashMap, HashSet};

/// Attempts to evaluate all constants and statics, returning
/// whether it was successful.
pub fn const_eval(program: &mut MIRProgram<'_>) -> bool {
    let mut current_evals = HashSet::new();
    let mut done_evals = HashSet::new();

    let const_names = program.constants.keys().cloned().collect::<Vec<_>>();
    let static_names = program.statics.keys().cloned().collect::<Vec<_>>();

    for constant in const_names {
        if !eval_constant(program, constant, &mut current_evals, &mut done_evals) {
            return false;
        }
    }

    for static_name in static_names {
        if !eval_static(program, static_name) {
            return false;
        }
    }

    true
}

/// Attempts to optimize all expressions
/// in every function, returning
/// whether it was successful.
/// This MUST occur after const evaluation.
pub fn const_optimize_expr(program: &mut MIRProgram<'_>) -> bool {
    for function in program.functions.values_mut() {
        for statement in function.body.iter_mut() {
            match statement {
                // No expressions.
                MIRStatement::CreateVariable(_) => {}
                MIRStatement::DropVariable(_) => {}
                MIRStatement::SetVariable { value, .. } => {
                    *value = reduce_expr_simple(&program.constants, &value.clone());
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
    program: &mut MIRProgram<'a>,
    constant_name: &'a str,
    current_evals: &mut HashSet<&'a str>,
    done_evals: &mut HashSet<&'a str>,
) -> bool {
    if done_evals.contains(constant_name) {
        // Already done.
        return true;
    }

    if current_evals.contains(constant_name) {
        // Eval loop.
        eprintln!("Constant loop detected: {current_evals:?}");
        return false;
    }

    current_evals.insert(constant_name);

    let old_expr = program.constants[constant_name].value.clone();
    let reduced = reduce_expr(&old_expr, &mut |name| {
        // Ensure the constant exists.
        if !program.constants.contains_key(name) {
            return None;
        }

        // Ensure the constant is evaluated.
        if !eval_constant(program, name, current_evals, done_evals) {
            return None;
        }

        // No need to validate that this is a primitive here,
        // since eval_constant already does that.
        Some(program.constants[name].value.clone())
    });

    // Constants must be fully reduced.
    if !matches!(reduced, MIRExpression::Number(_)) {
        eprintln!("Failed to reduce constant to a number: {:?}", &old_expr);
        return false;
    }

    program.constants.get_mut(constant_name).unwrap().value = reduced;

    current_evals.remove(constant_name);
    done_evals.insert(constant_name);

    true
}

/// Attempts to evaluate a static, returning
/// whether it was successful.
/// This MUST occur after constant evaluation.
/// Evaluation means that it's reduced to a primitive.
fn eval_static<'a>(program: &mut MIRProgram<'a>, constant_name: &'a str) -> bool {
    let old_expr = program.statics[constant_name].value.clone();
    let reduced = reduce_expr_simple(&program.constants, &old_expr);

    // Statics must be fully reduced.
    if !matches!(reduced, MIRExpression::Number(_)) {
        eprintln!("Failed to reduce static to a number: {:?}", &old_expr);
        return false;
    }

    program.statics.get_mut(constant_name).unwrap().value = reduced;

    true
}

/// Expression reduction that uses
/// the values inside constants.
/// This MUST be run after constant evaluation.
fn reduce_expr_simple<'a>(
    constants: &HashMap<&'a str, MIRConstant<'a>>,
    expr: &MIRExpression<'a>,
) -> MIRExpression<'a> {
    reduce_expr(&expr, &mut |name| {
        // Ensure the constant exists.
        if !constants.contains_key(name) {
            return None;
        }

        // Constants are guaranteed to already be evaluated.

        // No need to validate that this is a primitive here,
        // since eval_constant already does that.
        Some(constants[name].value.clone())
    })
}

/// Attempts to reduce an expression
/// using simple constant evaluation.
fn reduce_expr<'a>(
    expr: &MIRExpression<'a>,
    get_const: &mut impl FnMut(&'a str) -> Option<MIRExpression<'a>>,
) -> MIRExpression<'a> {
    match expr {
        MIRExpression::Add(left, right) => {
            let left = reduce_expr(left, get_const);
            let right = reduce_expr(right, get_const);

            if let MIRExpression::Number(left) = left {
                if let MIRExpression::Number(right) = right {
                    return MIRExpression::Number(left + right);
                }
            }

            MIRExpression::Add(Box::new(left), Box::new(right))
        }
        MIRExpression::Sub(left, right) => {
            let left = reduce_expr(left, get_const);
            let right = reduce_expr(right, get_const);

            if let MIRExpression::Number(left) = left {
                if let MIRExpression::Number(right) = right {
                    return MIRExpression::Number(left - right);
                }
            }

            MIRExpression::Sub(Box::new(left), Box::new(right))
        }
        MIRExpression::Mul(left, right) => {
            let left = reduce_expr(left, get_const);
            let right = reduce_expr(right, get_const);

            if let MIRExpression::Number(left) = left {
                if let MIRExpression::Number(right) = right {
                    return MIRExpression::Number(left * right);
                }
            }

            MIRExpression::Mul(Box::new(left), Box::new(right))
        }
        MIRExpression::Div(left, right) => {
            let left = reduce_expr(left, get_const);
            let right = reduce_expr(right, get_const);

            if let MIRExpression::Number(left) = left {
                if let MIRExpression::Number(right) = right {
                    return MIRExpression::Number(left / right);
                }
            }

            MIRExpression::Div(Box::new(left), Box::new(right))
        }
        MIRExpression::Number(val) => MIRExpression::Number(*val),
        MIRExpression::Variable(name) => get_const(name).unwrap_or(MIRExpression::Variable(name)),
    }
}
