mod drop;
mod expr;
mod if_statement;
mod label;
mod loop_statement;
pub mod lower;
mod scope;
mod type_check;

use crate::mir::drop::drop_at_scope_end;
use crate::mir::expr::{const_eval, const_optimize_expr, split_exprs_to_locals};
use crate::mir::if_statement::flatten_ifs;
use crate::mir::label::rename_labels;
use crate::mir::loop_statement::flatten_loops;
use crate::mir::type_check::type_check;
use crate::parser::file_cache::FileCache;
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::Range;
use std::path::Path;

/// Context that can be used
/// throughout the MIR processing.
#[derive(Debug, Default)]
pub struct MIRContext<'a> {
    /// The current program.
    pub program: MIRProgram<'a>,

    /// A cache of files that have been loaded.
    pub file_cache: FileCache,
}

/// Applies all MIR phases and
/// optimizations, returning
/// whether it was successful.
pub fn visit_mir(ctx: &mut MIRContext<'_>) -> bool {
    if !type_check(ctx) {
        return false;
    }

    // Type information now exists.

    if !const_eval(ctx) {
        return false;
    }

    // Constants are now only literals.

    if !const_optimize_expr(ctx) {
        return false;
    }

    // Expressions no longer contain references
    // to constants.

    drop_at_scope_end(ctx);

    // All variables are now dropped.

    flatten_loops(ctx);

    // Loops no longer exist
    // in MIR.

    // This needs to happen after
    // scope drop is added because
    // it erases scope.
    flatten_ifs(ctx);

    // If statements no longer exist
    // in MIR.

    // This needs to happen after all
    // operations that create labels.
    rename_labels(ctx);

    // Labels are now unique and ready
    // to be processed in asm.

    split_exprs_to_locals(ctx);

    // Expressions are now split
    // into primitives and ready to
    // be processed in asm.

    true
}

#[derive(Clone, Debug)]
pub struct Span<'a>(&'a Path, Range<usize>);

/// Converts a pest Span to an ariadne Span.
pub fn to_span<'a>(file: &'a Path, span: pest::Span<'_>) -> Span<'a> {
    Span(file, span.start()..span.end())
}

impl<'a> ariadne::Span for Span<'a> {
    type SourceId = Path;

    fn source(&self) -> &Self::SourceId {
        self.0
    }

    fn start(&self) -> usize {
        self.1.start
    }

    fn end(&self) -> usize {
        self.1.end
    }
}

/// An entire program.
/// Every name must be unique
/// among all named items contained
/// inside here.
#[derive(Debug, Default)]
pub struct MIRProgram<'a> {
    /// A list of constants in the program.
    /// Name -> Constant data.
    pub constants: HashMap<Cow<'a, str>, MIRConstant<'a>>,

    /// A list of statics in the program.
    /// Name -> Static data.
    pub statics: HashMap<Cow<'a, str>, MIRStatic<'a>>,

    /// A list of functions in the program.
    /// Name -> Function data.
    pub functions: HashMap<Cow<'a, str>, MIRFunction<'a>>,
}

/// A constant variable.
/// These cannot be modified, and can only
/// be initialized with simple expressions.
#[derive(Debug, Clone)]
pub struct MIRConstant<'a> {
    /// The variable's name.
    pub name: Cow<'a, str>,

    /// The constant's type.
    pub ty: MIRType<'a>,

    /// The constant's value.
    pub value: MIRExpression<'a>,

    /// The code that created
    /// this item.
    pub span: Span<'a>,
}

/// A static variable.
/// These can be modified and can only
/// be initialized with simple expressions.
#[derive(Debug, Clone)]
pub struct MIRStatic<'a> {
    /// The variable's name.
    pub name: Cow<'a, str>,

    /// The constant's type.
    pub ty: MIRType<'a>,

    /// The constant's value.
    pub value: MIRExpression<'a>,

    /// The code that created
    /// this item.
    pub span: Span<'a>,
}

/// A function.
#[derive(Debug, Clone)]
pub struct MIRFunction<'a> {
    /// The function's name.
    pub name: Cow<'a, str>,

    /// The function's return type.
    pub ret_ty: MIRType<'a>,

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
    pub name: Cow<'a, str>,

    /// The type of the data stored
    /// inside the variable.
    pub ty: MIRType<'a>,

    /// The code that created
    /// this item.
    pub span: Span<'a>,
}

/// A statement inside a function's
/// body.
#[derive(Debug, Clone)]
pub enum MIRStatement<'a> {
    /// Creates a new variable.
    CreateVariable(MIRVariable<'a>, Span<'a>),

    /// Drops the value stored
    /// inside a variable and
    /// invalidates it.
    DropVariable(Cow<'a, str>, Span<'a>),

    /// Sets a variable to a certain value.
    SetVariable {
        /// Is the variable's name.
        name: Cow<'a, str>,

        /// Is the expression to set it to.
        value: MIRExpression<'a>,

        /// The code that created
        /// this item.
        span: Span<'a>,
    },

    /// A label that can be jumped to.
    Label {
        /// The label's name.
        name: Cow<'a, str>,

        /// The code that created
        /// this item.
        span: Span<'a>,
    },

    /// Jumps to the specified label.
    Goto {
        /// The name of the label to jump to.
        name: Cow<'a, str>,

        /// The code that created
        /// this item.
        span: Span<'a>,
    },

    /// Jumps to the specified label if the
    /// given condition is not true.
    GotoNotEqual {
        /// The name of the label to jump to.
        name: Cow<'a, str>,

        /// The condition to check.
        /// If it's false, we'll jump.
        condition: MIRExpression<'a>,

        /// The code that created
        /// this item.
        span: Span<'a>,
    },

    /// An if statement.
    IfStatement {
        /// The if statement's condition.
        condition: MIRExpression<'a>,

        /// Code that runs on the true case.
        on_true: Vec<MIRStatement<'a>>,

        /// Code that runs on the false case.
        on_false: Vec<MIRStatement<'a>>,

        /// The code that created
        /// this item.
        span: Span<'a>,
    },

    /// An infinite loop.
    LoopStatement {
        /// Code that runs inside the loop.
        body: Vec<MIRStatement<'a>>,

        /// The code that created
        /// this item.
        span: Span<'a>,
    },

    /// Goes to the top of the parent
    /// loop.
    ContinueStatement {
        /// The code that created
        /// this item.
        span: Span<'a>,
    },

    /// Immediately exits the parent loop.
    BreakStatement {
        /// The code that created
        /// this item.
        span: Span<'a>,
    },
}

/// An expression that evaluates to some
/// value.
#[derive(Debug, Clone)]
pub struct MIRExpression<'a> {
    /// The expression.
    pub inner: MIRExpressionInner<'a>,

    /// The expression's type.
    /// This is only available
    /// after type checking and inference.
    pub ty: Option<MIRType<'a>>,

    /// The expression's span.
    pub span: Span<'a>,
}

/// An expression that evaluates to some
/// value.
#[derive(Debug, Clone)]
pub enum MIRExpressionInner<'a> {
    /// Addition.
    Add(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Subtraction.
    Sub(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Multiplication.
    Mul(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Division.
    Div(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Equals.
    Equal(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Not equal.
    NotEqual(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Less than.
    Less(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Greater than.
    Greater(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Less than or equal.
    LessEq(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Greater than or equal.
    GreaterEq(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Logical (boolean) and.
    BoolAnd(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Logical (boolean) or.
    BoolOr(Box<MIRExpression<'a>>, Box<MIRExpression<'a>>),

    /// Number literal.
    Number(i64),

    /// Bool literal.
    Bool(bool),

    /// Variable access.
    Variable(Cow<'a, str>),
}

/// A type written out as text.
#[derive(Debug, Clone)]
pub struct MIRType<'a> {
    /// The type represented by the literal.
    pub ty: MIRTypeInner<'a>,

    /// The literal's span.
    /// This type is sometimes
    /// inferred.
    /// In that case, it will be
    /// placed at the inference site,
    /// if possible, or else None.
    pub span: Option<Span<'a>>,
}

/// The type of data a variable represents.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MIRTypeInner<'a> {
    /// Unsigned 32-bit integer.
    U32,

    /// Boolean value.
    Bool,

    /// Unit type (void).
    Unit,

    /// A named type (struct).
    Named(Cow<'a, str>),
}

impl<'a> From<MIRTypeInner<'a>> for Cow<'a, str> {
    fn from(value: MIRTypeInner<'a>) -> Self {
        match value {
            MIRTypeInner::U32 => Cow::Borrowed("u32"),
            MIRTypeInner::Unit => Cow::Borrowed("()"),
            MIRTypeInner::Bool => Cow::Borrowed("bool"),
            MIRTypeInner::Named(val) => val,
        }
    }
}
