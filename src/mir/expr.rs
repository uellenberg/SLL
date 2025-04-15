use crate::mir::scope::StatementExplorer;
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
        let res = <StatementExplorer>::explore_block_mut(
            &mut function.body,
            &|statement, _scope| {
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
                        *value = reduce_expr_simple(&ctx.program.constants, &value.clone());
                    }
                }

                true
            },
            &|_, _| true,
            &|_, _| true,
        );

        if !res {
            return false;
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
    macro_rules! simple_binary {
        ($left:expr, $right:expr, $($red_i:path)|+, $red_o:path, $op:tt, $ret:path) => {{
            use MIRExpressionInner::*;

            let left = reduce_expr($left, get_const);
            let right = reduce_expr($right, get_const);

            $(if let $red_i(left, ..) = left.inner {
                if let $red_i(right, ..) = right.inner {
                    return $red_o(left $op right);
                }
            })+

            $ret(Box::new(left), Box::new(right))
        }};
    }

    let new_expr = (|| match &expr.inner {
        MIRExpressionInner::Add(left, right) => {
            simple_binary!(left, right, Number, Number, +, Add)
        }
        MIRExpressionInner::Sub(left, right) => {
            simple_binary!(left, right, Number, Number, -, Sub)
        }
        MIRExpressionInner::Mul(left, right) => {
            simple_binary!(left, right, Number, Number, *, Mul)
        }
        MIRExpressionInner::Div(left, right) => {
            simple_binary!(left, right, Number, Number, /, Div)
        }
        MIRExpressionInner::Equal(left, right) => {
            simple_binary!(left, right, Number | Bool, Bool, ==, Equal)
        }
        MIRExpressionInner::NotEqual(left, right) => {
            simple_binary!(left, right, Number | Bool, Bool, !=, NotEqual)
        }
        MIRExpressionInner::Greater(left, right) => {
            simple_binary!(left, right, Number | Bool, Bool, >, Greater)
        }
        MIRExpressionInner::Less(left, right) => {
            simple_binary!(left, right, Number | Bool, Bool, <, Less)
        }
        MIRExpressionInner::GreaterEq(left, right) => {
            simple_binary!(left, right, Number | Bool, Bool, >=, GreaterEq)
        }
        MIRExpressionInner::LessEq(left, right) => {
            simple_binary!(left, right, Number | Bool, Bool, <=, LessEq)
        }
        MIRExpressionInner::BoolAnd(left, right) => {
            simple_binary!(left, right, Bool, Bool, &&, BoolAnd)
        }
        MIRExpressionInner::BoolOr(left, right) => {
            simple_binary!(left, right, Bool, Bool, ||, BoolOr)
        }
        MIRExpressionInner::Number(val) => MIRExpressionInner::Number(*val),
        MIRExpressionInner::Bool(val) => MIRExpressionInner::Bool(*val),
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

/// Splits every expression into locals,
/// which IR can process.
/// This MUST run after type checking.
pub fn split_exprs_to_locals(ctx: &mut MIRContext) {
    for function in ctx.program.functions.values_mut() {
        let local_idx = AtomicU32::new(0);

        let mut pre = vec![];
        let mut post = vec![];

        if !<StatementExplorer>::rewrite_block(
            &mut function.body,
            &mut |statement, scope, block| {
                let new_statement = match statement {
                    // No expressions.
                    MIRStatement::CreateVariable(..)
                    | MIRStatement::DropVariable(..)
                    | MIRStatement::Goto { .. }
                    | MIRStatement::Label { .. }
                    | MIRStatement::BreakStatement { .. }
                    | MIRStatement::ContinueStatement { .. }
                    | MIRStatement::LoopStatement { .. } => {
                        block.push(statement);
                        return true;
                    }

                    MIRStatement::IfStatement {
                        condition,
                        on_true,
                        on_false,
                        span,
                    } => {
                        let new_condition =
                            split_expr_to_locals(&condition, &mut pre, &mut post, &local_idx, true);

                        MIRStatement::IfStatement {
                            condition: new_condition,
                            on_true,
                            on_false,
                            span,
                        }
                    }

                    MIRStatement::GotoNotEqual {
                        name,
                        condition,
                        span,
                    } => {
                        let new_condition =
                            split_expr_to_locals(&condition, &mut pre, &mut post, &local_idx, true);

                        MIRStatement::GotoNotEqual {
                            name,
                            condition: new_condition,
                            span,
                        }
                    }

                    MIRStatement::SetVariable { value, name, span } => {
                        let new_expr =
                            split_expr_to_locals(&value, &mut pre, &mut post, &local_idx, true);

                        MIRStatement::SetVariable {
                            value: new_expr,
                            name,
                            span,
                        }
                    }
                };

                block.append(&mut pre);

                block.push(new_statement);

                // This needs to be applied in reverse
                // order, since that's how it's
                // constructed.
                post.reverse();
                block.append(&mut post);

                true
            },
            &mut |_, _| true,
            &mut |_, _, _| true,
        ) {
            panic!("split_exprs_to_locals returned false!");
        }
    }
}

/// Post is in reverse order.
/// When it appears in the code,
/// it needs to be reversed.
///
/// head is used to designate
/// the top-level expression.
/// This shouldn't be split into
/// a local, since it won't be
/// used in a more complex expression.
fn split_expr_to_locals<'a>(
    expr: &MIRExpression<'a>,
    pre: &mut Vec<MIRStatement<'a>>,
    post: &mut Vec<MIRStatement<'a>>,
    local_idx: &AtomicU32,
    head: bool,
) -> MIRExpression<'a> {
    let Some(expression_ty) = &expr.ty else {
        panic!("Expression splitting requires type information!");
    };

    let mut child_pre = vec![];
    let mut child_post = vec![];

    macro_rules! recurse {
        ($val:expr) => {
            split_expr_to_locals($val, &mut child_pre, &mut child_post, local_idx, false)
        };
    }

    // Simple binary expressions don't
    // need a variable if they're the head,
    // since these are already primitive operations.
    let simple_binary_needs_var = !head;

    macro_rules! simple_binary {
        ($left:expr, $right:expr, $name:path) => {{
            let left = recurse!($left);
            let right = recurse!($right);

            (
                $name(Box::new(left), Box::new(right)),
                simple_binary_needs_var,
            )
        }};
    }

    let (new_expr, needs_var) = match &expr.inner {
        MIRExpressionInner::Add(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::Add)
        }
        MIRExpressionInner::Sub(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::Sub)
        }
        MIRExpressionInner::Mul(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::Mul)
        }
        MIRExpressionInner::Div(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::Div)
        }
        MIRExpressionInner::Equal(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::Equal)
        }
        MIRExpressionInner::NotEqual(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::NotEqual)
        }
        MIRExpressionInner::Greater(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::Greater)
        }
        MIRExpressionInner::Less(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::Less)
        }
        MIRExpressionInner::GreaterEq(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::GreaterEq)
        }
        MIRExpressionInner::LessEq(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::LessEq)
        }
        // TODO: Add a compile pass before this to translate these into ifs for short circuiting.
        MIRExpressionInner::BoolAnd(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::BoolAnd)
        }
        MIRExpressionInner::BoolOr(left, right) => {
            simple_binary!(left, right, MIRExpressionInner::BoolOr)
        }
        // Primitive expressions don't need variables.
        MIRExpressionInner::Variable(val) => (MIRExpressionInner::Variable(val.clone()), false),
        MIRExpressionInner::Number(val) => (MIRExpressionInner::Number(*val), false),
        MIRExpressionInner::Bool(val) => (MIRExpressionInner::Bool(*val), false),
    };

    // Add span and type information back to
    // the expression.
    let new_expr = MIRExpression {
        inner: new_expr,
        ty: expr.ty.clone(),
        span: expr.span.clone(),
    };

    pre.append(&mut child_pre);

    let final_expr;

    if needs_var {
        let local_idx = local_idx.fetch_add(1, Ordering::Relaxed);
        let local_name = format!("$local_{local_idx}");

        pre.push(MIRStatement::CreateVariable(
            MIRVariable {
                name: Cow::Owned(local_name.clone()),
                ty: expression_ty.clone(),
                // This span isn't correct, but good enough.
                span: expr.span.clone(),
            },
            // This span isn't correct, but good enough.
            expr.span.clone(),
        ));

        pre.push(MIRStatement::SetVariable {
            value: new_expr,
            // This span isn't correct, but good enough.
            span: expr.span.clone(),
            name: Cow::Owned(local_name.clone()),
        });

        post.push(MIRStatement::DropVariable(
            Cow::Owned(local_name.clone()),
            // This span isn't correct, but good enough.
            expr.span.clone(),
        ));

        final_expr = MIRExpression {
            inner: MIRExpressionInner::Variable(Cow::Owned(local_name.clone())),
            ty: Some(expression_ty.clone()),
            span: expr.span.clone(),
        };

        // This needs to be applied in reverse
        // order, since that's how it's
        // constructed.
        child_post.reverse();
        pre.append(&mut child_post);
    } else {
        final_expr = new_expr;

        // Because we have no variable, we
        // can't apply child_post right now
        // or else we might drop something before
        // it gets used.
        // Instead, we'll give it to the parent.
        // No reverse here, since that'll get done
        // when everything is applied in the end.
        post.append(&mut child_post);
    }

    final_expr
}
