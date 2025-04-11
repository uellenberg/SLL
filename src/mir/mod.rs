mod expr;
pub mod lower;
mod scope;
mod type_check;

use crate::mir::expr::{const_eval, const_optimize_expr};
use crate::mir::type_check::type_check;
use pest::Span;
use std::collections::HashMap;

/// Applies all MIR phases and
/// optimizations, returning
/// whether it was successful.
pub fn visit_mir(program: &mut MIRProgram<'_>) -> bool {
    if !type_check(program) {
        return false;
    }

    if !const_eval(program) {
        return false;
    }

    if !const_optimize_expr(program) {
        return false;
    }

    true
}

/// An entire program.
/// Every name must be unique
/// among all named items contained
/// inside here.
#[derive(Debug)]
pub struct MIRProgram<'a> {
    /// A list of constants in the program.
    /// Name -> Constant data.
    pub constants: HashMap<&'a str, MIRConstant<'a>>,

    /// A list of statics in the program.
    /// Name -> Static data.
    pub statics: HashMap<&'a str, MIRStatic<'a>>,

    /// A list of functions in the program.
    /// Name -> Function data.
    pub functions: HashMap<&'a str, MIRFunction<'a>>,
}

/// A constant variable.
/// These cannot be modified, and can only
/// be initialized with simple expressions.
#[derive(Debug)]
pub struct MIRConstant<'a> {
    /// The variable's name.
    pub name: &'a str,

    /// The constant's type.
    pub ty: MIRType,

    /// The constant's value.
    pub value: MIRExpression<'a>,

    /// The code that created
    /// this item.
    pub span: Span<'a>,
}

/// A static variable.
/// These can be modified and can only
/// be initialized with simple expressions.
#[derive(Debug)]
pub struct MIRStatic<'a> {
    /// The variable's name.
    pub name: &'a str,

    /// The constant's type.
    pub ty: MIRType,

    /// The constant's value.
    pub value: MIRExpression<'a>,

    /// The code that created
    /// this item.
    pub span: Span<'a>,
}

/// A function.
#[derive(Debug)]
pub struct MIRFunction<'a> {
    /// The function's name.
    pub name: &'a str,

    /// The function's return type.
    pub ret_ty: MIRType,

    /// A list of the arguments that
    /// the function takes in.
    pub args: Vec<MIRVariable<'a>>,

    /// A list of statements
    /// that will be executed
    /// when the function runs.
    pub body: Vec<MIRStatement<'a>>,

    /// The code that created
    /// this item.
    pub span: Span<'a>,
}

/// A variable inside a function.
#[derive(Debug, Clone)]
pub struct MIRVariable<'a> {
    /// The variable's name.
    pub name: &'a str,

    /// The type of the data stored
    /// inside the variable.
    pub ty: MIRType,

    /// The code that created
    /// this item.
    pub span: Span<'a>,
}

/// A statement inside a function's
/// body.
#[derive(Debug)]
pub enum MIRStatement<'a> {
    /// Creates a new variable.
    CreateVariable(MIRVariable<'a>, Span<'a>),

    /// Drops the value stored
    /// inside a variable and
    /// invalidates it.
    DropVariable(&'a str, Span<'a>),

    /// Sets a variable to a certain value.
    SetVariable {
        /// Is the variable's name.
        name: &'a str,

        /// Is the expression to set it to.
        value: MIRExpression<'a>,

        /// The code that created
        /// this item.
        span: Span<'a>,
    },
}

/// An expression that evaluates to some
/// value.
#[derive(Debug, Clone)]
pub enum MIRExpression<'a> {
    /// Addition.
    Add(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>, Span<'a>),

    /// Subtraction.
    Sub(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>, Span<'a>),

    /// Multiplication.
    Mul(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>, Span<'a>),

    /// Division.
    Div(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>, Span<'a>),

    /// Number literal.
    Number(i64, Span<'a>),

    /// Variable access.
    Variable(&'a str, Span<'a>),
}

/// The type of data a variable represents.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MIRType {
    /// Unsigned 32-bit integer.
    U32,

    /// Unit type (void).
    Unit,
}
