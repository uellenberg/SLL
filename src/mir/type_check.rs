use crate::mir::scope::{Scope, explore_block};
use crate::mir::{
    MIRConstant, MIRContext, MIRExpression, MIRFunction, MIRStatement, MIRStatic, MIRType,
    MIRTypeLiteral, Span,
};
use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind};
use std::os::linux::raw::stat;

/// Finds and reports type errors, returning
/// whether type check succeeded.
pub fn type_check(ctx: &MIRContext<'_>) -> bool {
    for constant in ctx.program.constants.values() {
        if !check_constant(ctx, constant) {
            return false;
        }
    }

    for static_data in ctx.program.statics.values() {
        if !check_static(ctx, static_data) {
            return false;
        }
    }

    for function in ctx.program.functions.values() {
        if !check_function(ctx, function) {
            return false;
        }
    }

    true
}

/// Prints an error for when an expression
/// returns an unexpected type.
fn print_unexpected_expr_ty(
    ctx: &MIRContext<'_>,
    expected_ty: MIRTypeLiteral<'_>,
    actual_ty: MIRTypeLiteral<'_>,
    error_expr_span: Span<'_>,
) {
    let mut colors = ColorGenerator::new();

    let expected = colors.next();
    let actual = colors.next();

    let expected_ty_str: &str = expected_ty.ty.clone().into();
    let expected_ty_str = expected_ty_str.fg(expected);

    let actual_ty_str: &str = actual_ty.ty.clone().into();
    let actual_ty_str = actual_ty_str.fg(actual);

    let mut report = Report::build(ReportKind::Error, error_expr_span.clone()).with_message(
        format!("Expected type {expected_ty_str}, found {actual_ty_str}"),
    );

    if let Some(expected_ty_span) = &expected_ty.span {
        report = report.with_label(
            Label::new(expected_ty_span.clone())
                .with_message(format!("Expected {expected_ty_str} because of this"))
                .with_color(expected),
        )
    }

    report
        .with_label(
            Label::new(error_expr_span)
                .with_message(format!("This expression returns {actual_ty_str}"))
                .with_color(actual),
        )
        .finish()
        .eprint(ctx.file_cache.clone())
        .unwrap();
}

/// Checks whether the given constant is valid.
fn check_constant(ctx: &MIRContext<'_>, constant: &MIRConstant<'_>) -> bool {
    let Some(expr_type) = check_expression(ctx, &constant.value, None) else {
        return false;
    };

    if expr_type.ty != constant.ty.ty {
        print_unexpected_expr_ty(
            ctx,
            constant.ty.clone(),
            expr_type,
            constant.value.span().clone(),
        );

        return false;
    }

    true
}

/// Checks whether the given static is valid.
fn check_static(ctx: &MIRContext<'_>, static_data: &MIRStatic<'_>) -> bool {
    let Some(expr_type) = check_expression(ctx, &static_data.value, None) else {
        return false;
    };

    if expr_type.ty != static_data.ty.ty {
        print_unexpected_expr_ty(
            ctx,
            static_data.ty.clone(),
            expr_type,
            static_data.value.span().clone(),
        );

        return false;
    }

    true
}

/// Checks whether the given function is valid.
fn check_function(ctx: &MIRContext<'_>, function: &MIRFunction<'_>) -> bool {
    // TODO: Check return types.

    let res = explore_block(
        &function.body,
        &|statement, scope| {
            match statement {
                // No expressions.
                MIRStatement::CreateVariable(..) => {}
                MIRStatement::DropVariable(..) => {}
                MIRStatement::SetVariable { value, name, .. } => {
                    let var_ty;

                    if let Some(var) = scope.variables.get(name) {
                        var_ty = var.ty.clone();
                    } else if let Some(var) = ctx.program.constants.get(name) {
                        var_ty = var.ty.clone();
                    } else if let Some(var) = ctx.program.statics.get(name) {
                        var_ty = var.ty.clone();
                    } else {
                        eprintln!("Cannot set variable {name}: variable does not exist in scope!");
                        return false;
                    }

                    let Some(ty) = check_expression(ctx, value, Some(scope)) else {
                        return false;
                    };

                    if ty.ty != var_ty.ty {
                        print_unexpected_expr_ty(ctx, var_ty, ty, value.span().clone());

                        return false;
                    }
                }
            }

            true
        },
        &|_, _| true,
    );

    if !res {
        return false;
    }

    true
}

/// Checks whether the expression is valid.
/// If it isn't, errors are reported.
/// If it is, the expression's type is returned.
fn check_expression<'a>(
    ctx: &MIRContext<'a>,
    expr: &MIRExpression<'a>,
    scope: Option<&Scope<'a>>,
) -> Option<MIRTypeLiteral<'a>> {
    let ty = (|| {
        match expr {
            MIRExpression::Add(left, right, ..) => {
                let t_left = check_expression(ctx, left, scope)?;
                let t_right = check_expression(ctx, right, scope)?;

                if t_left.ty != t_right.ty {
                    eprintln!("Addition failed: left type != right: {expr:?}");
                    return None;
                }

                Some(t_left)
            }
            MIRExpression::Sub(left, right, ..) => {
                let t_left = check_expression(ctx, left, scope)?;
                let t_right = check_expression(ctx, right, scope)?;

                if t_left.ty != t_right.ty {
                    eprintln!("Subtraction failed: left type != right: {expr:?}");
                    return None;
                }

                Some(t_left)
            }
            MIRExpression::Mul(left, right, ..) => {
                let t_left = check_expression(ctx, left, scope)?;
                let t_right = check_expression(ctx, right, scope)?;

                if t_left.ty != t_right.ty {
                    eprintln!("Multiplication failed: left type != right: {expr:?}");
                    return None;
                }

                Some(t_left)
            }
            MIRExpression::Div(left, right, ..) => {
                let t_left = check_expression(ctx, left, scope)?;
                let t_right = check_expression(ctx, right, scope)?;

                if t_left.ty != t_right.ty {
                    eprintln!("Division failed: left type != right: {expr:?}");
                    return None;
                }

                Some(t_left)
            }
            MIRExpression::Variable(name, ..) => {
                if let Some(scope) = scope {
                    if let Some(var) = scope.variables.get(name) {
                        return Some(var.ty.clone());
                    }
                }

                if let Some(var) = ctx.program.constants.get(name) {
                    return Some(var.ty.clone());
                }

                if let Some(var) = ctx.program.statics.get(name) {
                    return Some(var.ty.clone());
                }

                if ctx.program.functions.get(name).is_some() {
                    eprintln!(
                        "Cannot directly access function as value (use a reference): {expr:?}"
                    );
                    return None;
                }

                eprintln!("Variable doesn't exist: {expr:?}");
                None
            }
            MIRExpression::Number(num, span) => {
                // TODO: Handle negatives.
                assert!(*num >= 0);

                Some(MIRTypeLiteral {
                    ty: MIRType::U32,
                    span: Some(span.clone()),
                })
            }
        }
    })()?;

    // Ensure that we return a type
    // whose span covers the entire expression.
    Some(MIRTypeLiteral {
        ty: ty.ty,
        span: Some(expr.span().clone()),
    })
}
