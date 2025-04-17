use crate::ir::{
    IRBinaryOperation, IRConstant, IRFunction, IRLoadBinary, IRLoadOp, IRLoadUnary, IRProgram,
    IRStatement, IRStatic, IRType,
};
use std::fmt::{Display, Formatter};

impl<'a> Display for IRProgram<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "// Constants: \n\n")?;

        for (_name, constant) in &self.constants {
            constant.fmt(f)?;
            write!(f, "\n")?;
        }

        if !self.constants.is_empty() {
            write!(f, "\n")?;
        }

        write!(f, "// Statics: \n\n")?;

        for (_name, static_data) in &self.statics {
            static_data.fmt(f)?;
            write!(f, "\n")?;
        }

        if !self.statics.is_empty() {
            write!(f, "\n")?;
        }

        write!(f, "// Functions: \n\n")?;

        for (_name, function) in &self.functions {
            function.fmt(f)?;
            write!(f, "\n")?;
        }

        Ok(())
    }
}

impl<'a> Display for IRConstant<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "const {}: {} = {};", &self.name, &self.ty, &self.value)
    }
}

impl<'a> Display for IRStatic<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "static {}: {} = {};", &self.name, &self.ty, &self.value)
    }
}

impl<'a> Display for IRFunction<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "function {}(", &self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", &arg.name, &arg.ty)?;
        }
        write!(f, ")")?;

        // Optional return type
        if let Some(ret_ty) = &self.ret_ty {
            write!(f, " : {}", ret_ty)?;
        }

        write!(f, " {{\n")?;

        for stmt in &self.body {
            write_indented(f, stmt, "    ")?;
            write!(f, "\n")?;
        }

        write!(f, "}}")
    }
}

impl<'a> Display for IRStatement<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IRStatement::CreateVariable(var) => {
                write!(f, "let {}: {};", &var.name, &var.ty)
            }
            IRStatement::DropVariable(name) => {
                write!(f, "drop {};", name)
            }
            IRStatement::SetVariable { name, value } => {
                write!(f, "{} = {};", name, value)
            }
            IRStatement::Label { name } => {
                write!(f, "label {}:", name)
            }
            IRStatement::Goto { name } => {
                write!(f, "goto {};", name)
            }
            IRStatement::GotoNotEqual { name, condition } => {
                write!(f, "goto_ne({}) {};", condition, name)
            }
        }
    }
}

impl<'a> Display for IRLoadOp<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IRLoadOp::Unary(unary) => {
                write!(f, "{}", unary)
            }
            IRLoadOp::Binary(op, binary) => {
                write!(f, "{} ({})", op, binary)
            }
        }
    }
}

impl<'a> Display for IRLoadUnary<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IRLoadUnary::Variable(name) => write!(f, "{}", name),
            IRLoadUnary::Num(val) => write!(f, "{}", val),
        }
    }
}

impl<'a> Display for IRLoadBinary<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IRLoadBinary::VariableVariable(v1, v2) => write!(f, "{}, {}", v1, v2),
            IRLoadBinary::NumVariable(n, v) => write!(f, "{}, {}", n, v),
            IRLoadBinary::VariableNum(v, n) => write!(f, "{}, {}", v, n),
        }
    }
}

impl Display for IRBinaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IRBinaryOperation::Add32 => write!(f, "add32"),
            IRBinaryOperation::Sub32 => write!(f, "sub32"),
            IRBinaryOperation::Mul32 => write!(f, "mul32"),
            IRBinaryOperation::Div32 => write!(f, "div32"),
            IRBinaryOperation::Equal32 => write!(f, "eq32"),
            IRBinaryOperation::NotEqual32 => write!(f, "neq32"),
            IRBinaryOperation::Greater32 => write!(f, "gt32"),
            IRBinaryOperation::Less32 => write!(f, "lt32"),
            IRBinaryOperation::GreaterEq32 => write!(f, "gte32"),
            IRBinaryOperation::LessEq32 => write!(f, "lte32"),
            IRBinaryOperation::And32 => write!(f, "and32"),
            IRBinaryOperation::Or32 => write!(f, "or32"),
        }
    }
}

impl<'a> Display for IRType<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IRType::U32 => write!(f, "u32"),
            IRType::Bool => write!(f, "bool"),
            IRType::Named(name) => write!(f, "{}", name),
        }
    }
}

/// Adds indentation to an item that implements Display,
/// then writes it.
fn write_indented(f: &mut Formatter<'_>, item: &impl Display, indent: &str) -> std::fmt::Result {
    let mut first = true;

    let fmt = if f.alternate() {
        format!("{:#}", item)
    } else {
        format!("{}", item)
    };

    for line in fmt.lines() {
        if !first {
            write!(f, "\n")?;
        } else {
            first = false;
        }

        write!(f, "{}{}", indent, line)?;
    }
    Ok(())
}
