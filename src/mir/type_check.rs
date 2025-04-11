use crate::mir::scope::{Scope, explore_block};
use crate::mir::{
    MIRConstant, MIRExpression, MIRFunction, MIRProgram, MIRStatement, MIRStatic, MIRType,
};

/// Finds and reports type errors, returning
/// whether type check succeeded.
pub fn type_check(program: &MIRProgram<'_>) -> bool {
    for constant in program.constants.values() {
        if !check_constant(program, constant) {
            return false;
        }
    }

    for static_data in program.statics.values() {
        if !check_static(program, static_data) {
            return false;
        }
    }

    for function in program.functions.values() {
        if !check_function(program, function) {
            return false;
        }
    }

    true
}

/// Checks whether the given constant is valid.
fn check_constant(program: &MIRProgram<'_>, constant: &MIRConstant<'_>) -> bool {
    let Some(expr_type) = check_expression(program, &constant.value, None) else {
        return false;
    };

    if expr_type != constant.ty {
        eprintln!("Constant type != expression type: {constant:?}");
        return false;
    }

    true
}

/// Checks whether the given static is valid.
fn check_static(program: &MIRProgram<'_>, static_data: &MIRStatic<'_>) -> bool {
    let Some(expr_type) = check_expression(program, &static_data.value, None) else {
        return false;
    };

    if expr_type != static_data.ty {
        eprintln!("Static type != expression type: {static_data:?}");
        return false;
    }

    true
}

/// Checks whether the given function is valid.
fn check_function(program: &MIRProgram, function: &MIRFunction<'_>) -> bool {
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
                    } else if let Some(var) = program.constants.get(name) {
                        var_ty = var.ty.clone();
                    } else if let Some(var) = program.statics.get(name) {
                        var_ty = var.ty.clone();
                    } else {
                        eprintln!("Cannot set variable {name}: variable does not exist in scope!");
                        return false;
                    }

                    let Some(ty) = check_expression(program, value, Some(scope)) else {
                        return false;
                    };

                    if ty != var_ty {
                        eprintln!(
                            "Expression returns {ty:?} but variable is of type {:?}",
                            var_ty
                        );
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
fn check_expression(
    program: &MIRProgram<'_>,
    expr: &MIRExpression<'_>,
    scope: Option<&Scope<'_>>,
) -> Option<MIRType> {
    match expr {
        MIRExpression::Add(left, right, ..) => {
            let t_left = check_expression(program, left, scope)?;
            let t_right = check_expression(program, right, scope)?;

            if t_left != t_right {
                eprintln!("Addition failed: left type != right: {expr:?}");
                return None;
            }

            Some(t_left)
        }
        MIRExpression::Sub(left, right, ..) => {
            let t_left = check_expression(program, left, scope)?;
            let t_right = check_expression(program, right, scope)?;

            if t_left != t_right {
                eprintln!("Subtraction failed: left type != right: {expr:?}");
                return None;
            }

            Some(t_left)
        }
        MIRExpression::Mul(left, right, ..) => {
            let t_left = check_expression(program, left, scope)?;
            let t_right = check_expression(program, right, scope)?;

            if t_left != t_right {
                eprintln!("Multiplication failed: left type != right: {expr:?}");
                return None;
            }

            Some(t_left)
        }
        MIRExpression::Div(left, right, ..) => {
            let t_left = check_expression(program, left, scope)?;
            let t_right = check_expression(program, right, scope)?;

            if t_left != t_right {
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

            if let Some(var) = program.constants.get(name) {
                return Some(var.ty.clone());
            }

            if let Some(var) = program.statics.get(name) {
                return Some(var.ty.clone());
            }

            if program.functions.get(name).is_some() {
                eprintln!("Cannot directly access function as value (use a reference): {expr:?}");
                return None;
            }

            eprintln!("Variable doesn't exist: {expr:?}");
            None
        }
        MIRExpression::Number(num, ..) => {
            // TODO: Handle negatives.
            assert!(*num >= 0);

            Some(MIRType::U32)
        }
    }
}
