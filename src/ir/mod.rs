use std::collections::HashMap;

/// An entire program.
/// Every name must be unique
/// among all named items contained
/// inside here.
#[derive(Debug)]
pub struct IRProgram<'a> {
    /// A list of constants in the program.
    /// Name -> Constant data.
    pub constants: HashMap<&'a str, IRConstant<'a>>,

    /// A list of statics in the program.
    /// Name -> Static data.
    pub statics: HashMap<&'a str, IRStatic<'a>>,

    /// A list of functions in the program.
    /// Name -> Function data.
    pub functions: HashMap<&'a str, IRFunction<'a>>,
}

/// A constant variable.
/// These cannot be modified, and can only
/// be initialized with simple expressions.
#[derive(Debug)]
pub struct IRConstant<'a> {
    /// The variable's name.
    pub name: &'a str,

    /// The constant's type.
    pub ty: IRType,

    /// The constant's value.
    pub value: i64,
}

/// A static variable.
/// These can be modified and can only
/// be initialized with simple expressions.
#[derive(Debug)]
pub struct IRStatic<'a> {
    /// The variable's name.
    pub name: &'a str,

    /// The constant's type.
    pub ty: IRType,

    /// The constant's value.
    pub value: i64,
}

/// A function.
#[derive(Debug)]
pub struct IRFunction<'a> {
    /// The function's name.
    pub name: &'a str,

    /// The function's return type,
    /// if it returns anything.
    pub ret_ty: Option<IRType>,

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
    pub name: &'a str,

    /// The type of the data stored
    /// inside the variable.
    pub ty: IRType,
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
    DropVariable(&'a str),

    /// Sets a variable to a certain constant number.
    SetVariableNum {
        /// Is the variable's name.
        name: &'a str,

        /// Is the number to set it to.
        value: i64,
    },
}

/// The type of data a variable represents.
#[derive(Debug, Eq, PartialEq)]
pub enum IRType {
    /// Unsigned 32-bit integer.
    U32,
}
