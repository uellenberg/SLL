mod alloc;
pub mod arm32;
mod display;

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

    /// Sets a variable to a new value.
    SetVariable {
        /// Is the variable's name.
        name: Cow<'a, str>,

        /// Is the value to set the variable to.
        value: IRLoadOp<'a>,
    },

    /// Calls a function, ignoring its return value.
    FunctionCall(IRFnCall<'a>),

    /// Returns from the function, along with
    /// an optional return value.
    Return(Option<IRLoadOp<'a>>),

    /// A label that can be jumped to.
    Label {
        /// Is the label's name.
        /// This MUST be unique across
        /// the entire program.
        name: Cow<'a, str>,
    },

    /// Jumps to a label.
    Goto {
        /// The name of the label to jump to.
        name: Cow<'a, str>,
    },

    /// Jumps to a label if
    /// the condition is false.
    GotoNotEqual {
        /// The name of the label to jump to.
        name: Cow<'a, str>,

        /// Is the condition to check.
        condition: IRLoadOp<'a>,
    },
}

/// Performs a load and an optional
/// operation.
#[derive(Debug, Clone)]
pub enum IRLoadOp<'a> {
    /// Performs a unary operation.
    Unary(IRLoadUnary<'a>),

    /// Performs a binary operation.
    Binary(IRBinaryOperation, IRLoadBinary<'a>),

    /// Calls a function, loading its return value.
    FunctionCall(Box<IRFnCall<'a>>),
}

/// A single piece of data that's
/// loaded in for an operation.
#[derive(Debug, Clone)]
pub enum IRLoadUnary<'a> {
    /// A variable.
    Variable(Cow<'a, str>),

    /// A reference to a variable.
    Reference(Cow<'a, str>),

    /// A number literal.
    Num(i64),
}

/// A single piece of data that's
/// loaded in for an operation.
#[derive(Debug, Clone)]
pub enum IRLoadBinary<'a> {
    /// Two variables.
    VariableVariable(Cow<'a, str>, Cow<'a, str>),

    // TODO: Remove this, since it isn't needed
    //       and doesn't correctly translate to asm.
    //       Instead, flip the arguments and change
    //       opcode to get VariableNum.
    /// A number literal and a variable.
    NumVariable(i64, Cow<'a, str>),

    /// A number literal and a variable.
    VariableNum(i64, Cow<'a, str>),
}

/// An operation that can be performed
/// on two inputs.
/// Any operation must work for a lower
/// bit size.
/// For example, Add32 must work for
/// 8-bit numbers.
#[derive(Debug, Clone)]
pub enum IRBinaryOperation {
    /// Add two 32-bit ints.
    Add32,

    /// Subtract two 32-bit ints.
    Sub32,

    /// Multiply two 32-bit ints.
    Mul32,

    /// Divide two unsigned 32-bit ints.
    UDiv32,

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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IRType<'a> {
    /// Unsigned 32-bit integer.
    U32,

    /// Boolean value.
    Bool,

    /// A function pointer, args -> return value.
    FunctionPtr(Vec<IRType<'a>>, Box<IRType<'a>>),

    /// A pointer to a type.
    Ptr(Box<IRType<'a>>),

    /// A named type (struct).
    Named(Cow<'a, str>),
}

/// A function call.
#[derive(Debug, Clone)]
pub struct IRFnCall<'a> {
    /// The source for the function (name or ptr).
    pub source: IRFnSource<'a>,

    /// The function's arguments as (value, arg)[].
    pub args: Vec<(IRLoadOp<'a>, IRType<'a>)>,

    /// The function's return type.
    /// None if it doesn't return
    /// anything.
    pub ret_ty: Option<IRType<'a>>,
}

/// The source for a function pointer
/// when performing a function call.
#[derive(Debug, Clone)]
pub enum IRFnSource<'a> {
    /// A direct function call, containing
    /// the name of the function to call.
    Direct(Cow<'a, str>),

    /// An indirect function call, meaning
    /// the function pointer is stored in
    /// a variable.
    /// Contains the name of the variable
    /// storing the pointer.
    Indirect(IRLoadOp<'a>),
}
