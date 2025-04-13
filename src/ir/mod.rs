mod alloc;
pub mod arm32;

use std::borrow::Cow;
use std::collections::HashMap;

/// An entire program.
/// Every name must be unique
/// among all named items contained
/// inside here.
#[derive(Debug)]
pub struct IRProgram<'a> {
    /// A list of constants in the program.
    /// Name -> Constant data.
    pub constants: HashMap<Cow<'a, str>, IRConstant<'a>>,

    /// A list of statics in the program.
    /// Name -> Static data.
    pub statics: HashMap<Cow<'a, str>, IRStatic<'a>>,

    /// A list of functions in the program.
    /// Name -> Function data.
    pub functions: HashMap<Cow<'a, str>, IRFunction<'a>>,
}

/// A constant variable.
/// These cannot be modified, and can only
/// be initialized with simple expressions.
#[derive(Debug)]
pub struct IRConstant<'a> {
    /// The variable's name.
    pub name: Cow<'a, str>,

    /// The constant's type.
    pub ty: IRType<'a>,

    /// The constant's value.
    pub value: i64,
}

/// A static variable.
/// These can be modified and can only
/// be initialized with simple expressions.
#[derive(Debug)]
pub struct IRStatic<'a> {
    /// The variable's name.
    pub name: Cow<'a, str>,

    /// The constant's type.
    pub ty: IRType<'a>,

    /// The constant's value.
    pub value: i64,
}

/// A function.
#[derive(Debug)]
pub struct IRFunction<'a> {
    /// The function's name.
    pub name: Cow<'a, str>,

    /// The function's return type,
    /// if it returns anything.
    pub ret_ty: Option<IRType<'a>>,

    /// A list of the arguments that
    /// the function takes in.
    pub args: Vec<IRVariable<'a>>,

    /// A list of statements
    /// that will be executed
    /// when the function runs.
    pub body: Vec<IRStatement<'a>>,
}

/// A variable inside a function.
#[derive(Debug)]
pub struct IRVariable<'a> {
    /// The variable's name.
    pub name: Cow<'a, str>,

    /// The type of the data stored
    /// inside the variable.
    pub ty: IRType<'a>,
}

/// A statement inside a function's
/// body.
#[derive(Debug)]
pub enum IRStatement<'a> {
    /// Creates a new variable.
    CreateVariable(IRVariable<'a>),

    /// Drops the value stored
    /// inside a variable and
    /// invalidates it.
    DropVariable(Cow<'a, str>),

    /// Sets a variable to a certain constant number.
    SetVariableNum {
        /// Is the variable's name.
        name: Cow<'a, str>,

        /// Is the value to set the variable to.
        value: i64,
    },

    /// Sets a variable to another variable.
    SetVariableVariable {
        /// Is the variable's name.
        name: Cow<'a, str>,

        /// Is the value to set the variable to.
        value: Cow<'a, str>,
    },

    /// Sets a variable to another variable.
    SetVariableOpNumVariable {
        /// Is the variable's name.
        name: Cow<'a, str>,

        /// Is the value to set the variable to.
        value: (i64, Cow<'a, str>),

        /// Is the operation to perform.
        op: IRBinaryOperation,
    },

    /// Sets a variable to another variable.
    SetVariableOpVariableVariable {
        /// Is the variable's name.
        name: Cow<'a, str>,

        /// Is the value to set the variable to.
        value: (Cow<'a, str>, Cow<'a, str>),

        /// Is the operation to perform.
        op: IRBinaryOperation,
    },
}

/// An operation that can be performed
/// on two inputs.
/// Any operation must work for a lower
/// bit size.
/// For example, Add32 must work for
/// 8-bit numbers.
#[derive(Debug)]
pub enum IRBinaryOperation {
    /// Add two 32-bit ints.
    Add32,

    /// Subtract two 32-bit ints.
    Sub32,

    /// Multiply two 32-bit ints.
    Mul32,

    /// Divide two 32-bit ints.
    Div32,

    /// Check if two 32-bit ints are equal.
    Equal32,

    /// Check if two 32-bit ints are not equal.
    NotEqual32,

    /// Check if the left 32-bit int is greater than the right.
    Greater32,

    /// Check if the left 32-bit int is less than the right.
    Less32,

    /// Check if the left 32-bit int is greater than or equal to the right.
    GreaterEq32,

    /// Check if the left 32-bit int is less than or equal to the right.
    LessEq32,

    /// Bitwise and on two 32-bit integers.
    And32,

    /// Bitwise or on two 32-bit integers.
    Or32,
}

/// The type of data a variable represents.
#[derive(Debug, Eq, PartialEq)]
pub enum IRType<'a> {
    /// Unsigned 32-bit integer.
    U32,

    /// Boolean value.
    Bool,

    /// A named type (struct).
    Named(Cow<'a, str>),
}
