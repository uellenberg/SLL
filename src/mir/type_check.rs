use crate::mir::scope::{Scope, explore_block_mut};
use crate::mir::{
    MIRConstant, MIRContext, MIRExpression, MIRExpressionInner, MIRFunction, MIRStatement,
    MIRStatic, MIRType, MIRTypeInner, Span,
};
use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind};
use std::borrow::Cow;

/// Finds and reports type errors, returning
/// whether type check succeeded.
/// Also modifies the MIR to contain
/// type information.
pub fn type_check(ctx: &mut MIRContext<'_>) -> bool {
    let mut constants = ctx.program.constants.clone();
    let mut statics = ctx.program.statics.clone();
    let mut functions = ctx.program.functions.clone();

    for constant in constants.values_mut() {
        if !check_constant(ctx, constant) {
            return false;
        }
    }

    for static_data in statics.values_mut() {
        if !check_static(ctx, static_data) {
            return false;
        }
    }

    for function in functions.values_mut() {
        if !check_function(ctx, function) {
            return false;
        }
    }

    ctx.program.constants = constants;
    ctx.program.statics = statics;
    ctx.program.functions = functions;

    true
}

/// Prints an error for when an expression
/// returns an unexpected type.
fn print_unexpected_expr_ty(
    ctx: &MIRContext<'_>,
    expected_ty: MIRType<'_>,
    actual_ty: MIRType<'_>,
    error_expr_span: Span<'_>,
) {
    let mut colors = ColorGenerator::new();

    let expected = colors.next();
    let actual = colors.next();

    let expected_ty_str: Cow<str> = expected_ty.ty.clone().into();
    let expected_ty_str = expected_ty_str.fg(expected);

    let actual_ty_str: Cow<str> = actual_ty.ty.clone().into();
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

/// Checks whether the given constant is valid,
/// and modifies its type information to match.
fn check_constant<'a>(ctx: &MIRContext<'a>, constant: &mut MIRConstant<'a>) -> bool {
    let Some(expr_type) = check_expression(ctx, &mut constant.value, None) else {
        return false;
    };

    if expr_type.ty != constant.ty.ty {
        print_unexpected_expr_ty(
            ctx,
            constant.ty.clone(),
            expr_type,
            constant.value.span.clone(),
        );

        return false;
    }

    true
}

/// Checks whether the given static is valid,
/// and modifies its type information to match.
fn check_static<'a>(ctx: &MIRContext<'a>, static_data: &mut MIRStatic<'a>) -> bool {
    let Some(expr_type) = check_expression(ctx, &mut static_data.value, None) else {
        return false;
    };

    if expr_type.ty != static_data.ty.ty {
        print_unexpected_expr_ty(
            ctx,
            static_data.ty.clone(),
            expr_type,
            static_data.value.span.clone(),
        );

        return false;
    }

    true
}

/// Checks whether the given function is valid,
/// and modifies its type information to match.
fn check_function<'a>(ctx: &MIRContext<'a>, function: &mut MIRFunction<'a>) -> bool {
    // TODO: Check return types.

    let res = explore_block_mut(
        &mut function.body,
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
                        print_unexpected_expr_ty(ctx, var_ty, ty, value.span.clone());

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

/// Checks whether the expression is valid,
/// and modifies its type information to match.
/// If it isn't, errors are reported.
/// If it is, the expression's type is returned.
fn check_expression<'a>(
    ctx: &MIRContext<'a>,
    expr: &mut MIRExpression<'a>,
    scope: Option<&Scope<'a>>,
) -> Option<MIRType<'a>> {
    macro_rules! simple_binary {
        ($left:expr, $right:expr, $name:literal, internal) => {{
            let t_left = check_expression(ctx, $left, scope)?;
            let t_right = check_expression(ctx, $right, scope)?;

            if t_left.ty != t_right.ty {
                eprintln!("{} failed: left type != right: {expr:?}", $name);
                return None;
            }

            Some(t_left)
        }};
        ($left:expr, $right:expr, $name:literal) => {
            simple_binary!($left, $right, $name, internal)
        };
        ($left:expr, $right:expr, $name:literal, $ty:expr) => {{
            simple_binary!($left, $right, $name, internal);

            Some(MIRType {
                ty: $ty,
                // Span will get set below.
                span: None,
            })
        }};
    }

    let mut ty = (|| {
        match &mut expr.inner {
            MIRExpressionInner::Add(left, right, ..) => {
                simple_binary!(left, right, "Addition")
            }
            MIRExpressionInner::Sub(left, right, ..) => {
                simple_binary!(left, right, "Subtraction")
            }
            MIRExpressionInner::Mul(left, right, ..) => {
                simple_binary!(left, right, "Multiplication")
            }
            MIRExpressionInner::Div(left, right, ..) => {
                simple_binary!(left, right, "Division")
            }
            MIRExpressionInner::Equal(left, right, ..) => {
                simple_binary!(left, right, "Equals", MIRTypeInner::Bool)
            }
            MIRExpressionInner::NotEqual(left, right, ..) => {
                simple_binary!(left, right, "Not equals", MIRTypeInner::Bool)
            }
            MIRExpressionInner::Less(left, right, ..) => {
                simple_binary!(left, right, "Less than", MIRTypeInner::Bool)
            }
            MIRExpressionInner::Greater(left, right, ..) => {
                simple_binary!(left, right, "Greater than", MIRTypeInner::Bool)
            }
            MIRExpressionInner::LessEq(left, right, ..) => {
                simple_binary!(left, right, "Less than or equals", MIRTypeInner::Bool)
            }
            MIRExpressionInner::GreaterEq(left, right, ..) => {
                simple_binary!(left, right, "Greater than or equals", MIRTypeInner::Bool)
            }
            MIRExpressionInner::BoolAnd(left, right, ..) => {
                simple_binary!(left, right, "Binary and", MIRTypeInner::Bool)
            }
            MIRExpressionInner::BoolOr(left, right, ..) => {
                simple_binary!(left, right, "Binary or", MIRTypeInner::Bool)
            }
            MIRExpressionInner::Variable(name, ..) => {
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
            MIRExpressionInner::Number(num) => {
                // TODO: Handle negatives.
                assert!(*num >= 0);

                Some(MIRType {
                    ty: MIRTypeInner::U32,
                    // Span is added after.
                    span: None,
                })
            }
            MIRExpressionInner::Bool(_) => Some(MIRType {
                ty: MIRTypeInner::Bool,
                // Span is added after.
                span: None,
            }),
        }
    })()?;

    // Ensure the type covers the
    // whole span.
    ty.span = Some(expr.span.clone());

    // Save the type for later
    // phases.
    expr.ty = Some(ty.clone());

    // Ensure that we return a type
    // whose span covers the entire expression.
    expr.ty.clone()
}
