use std::collections::HashMap;

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

    // TODO: Add expression handling.
    /// The constant's value.
    pub value: i64,
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

    // TODO: Add expression handling.
    /// The constant's value.
    pub value: i64,
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
}

/// A variable inside a function.
#[derive(Debug)]
pub struct MIRVariable<'a> {
    /// The variable's name.
    pub name: &'a str,

    /// The type of the data stored
    /// inside the variable.
    pub ty: MIRType,
}

/// A statement inside a function's
/// body.
#[derive(Debug)]
pub enum MIRStatement<'a> {
    /// Creates a new variable.
    CreateVariable(MIRVariable<'a>),

    /// Drops the value stored
    /// inside a variable and
    /// invalidates it.
    DropVariable(&'a str),

    /// Sets a variable to a certain value.
    SetVariable {
        /// Is the variable's name.
        name: &'a str,

        // TODO: Make this an actual expression.
        /// Is the expression to set it to.
        value: i64,
    },
}

/// The type of data a variable represents.
#[derive(Debug)]
pub enum MIRType {
    /// Unsigned 32-bit integer.
    U32,

    /// Unit type (void).
    Unit,
}
