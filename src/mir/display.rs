use crate::mir::{
    MIRConstant, MIRExpression, MIRExpressionInner, MIRFunction, MIRProgram, MIRStatement,
    MIRStatic, MIRType, MIRTypeInner,
};
use std::borrow::Cow;
use std::fmt::{Display, Formatter, Pointer};

impl<'a> Display for MIRProgram<'a> {
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

impl<'a> Display for MIRConstant<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "const {}: {} = ", &self.name, &self.ty)?;

        // Preserve formatting alternate mode.
        self.value.fmt(f)?;

        write!(f, ";")?;

        Ok(())
    }
}

impl<'a> Display for MIRStatic<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "static {}: {} = ", &self.name, &self.ty)?;

        // Preserve formatting alternate mode.
        self.value.fmt(f)?;

        write!(f, ";")?;

        Ok(())
    }
}

impl<'a> Display for MIRFunction<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "function {}(", &self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", &arg.name, &arg.ty)?;
        }
        write!(f, ") : {} {{\n", &self.ret_ty)?;

        for stmt in &self.body {
            write_indented(f, stmt, "    ")?;
            write!(f, "\n")?;
        }

        write!(f, "}}")
    }
}

impl<'a> Display for MIRStatement<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRStatement::CreateVariable(var, span) => {
                write!(f, "let {}: {};", &var.name, &var.ty)?;

                if f.alternate() {
                    writeln!(f, " /* {span} */")?;
                }
            }
            MIRStatement::DropVariable(name, span) => {
                write!(f, "drop {};", name)?;

                if f.alternate() {
                    writeln!(f, " /* {span} */")?;
                }
            }
            MIRStatement::SetVariable {
                name, value, span, ..
            } => {
                write!(f, "{} = {};", name, value)?;

                if f.alternate() {
                    writeln!(f, " /* {span} */")?;
                }
            }
            MIRStatement::Label { name, span, .. } => {
                write!(f, "label {}:", name)?;

                if f.alternate() {
                    writeln!(f, " /* {span} */")?;
                }
            }
            MIRStatement::Goto { name, span, .. } => {
                write!(f, "goto {};", name)?;

                if f.alternate() {
                    writeln!(f, " /* {span} */")?;
                }
            }
            MIRStatement::GotoNotEqual {
                name,
                condition,
                span,
                ..
            } => {
                write!(f, "goto_ne({}) {};", condition, name)?;

                if f.alternate() {
                    writeln!(f, " /* {span} */")?;
                }
            }
            MIRStatement::IfStatement {
                condition,
                on_true,
                on_false,
                span,
                ..
            } => {
                write!(f, "if {} {{\n", condition)?;
                for stmt in on_true {
                    write_indented(f, stmt, "    ")?;
                    write!(f, "\n")?;
                }
                write!(f, "}}")?;

                if !on_false.is_empty() {
                    write!(f, " else {{\n")?;
                    for stmt in on_false {
                        write_indented(f, stmt, "    ")?;
                        write!(f, "\n")?;
                    }
                    write!(f, "}}")?;
                }

                if f.alternate() {
                    writeln!(f, " /* {span} */")?;
                }
            }
            MIRStatement::LoopStatement { body, span, .. } => {
                write!(f, "loop {{\n")?;

                for stmt in body {
                    write_indented(f, stmt, "    ")?;
                    write!(f, "\n")?;
                }

                write!(f, "}}")?;

                if f.alternate() {
                    writeln!(f, " /* {span} */")?;
                }
            }
            MIRStatement::ContinueStatement { span, .. } => {
                write!(f, "continue;")?;

                if f.alternate() {
                    writeln!(f, " /* {span} */")?;
                }
            }
            MIRStatement::BreakStatement { span, .. } => {
                write!(f, "break;")?;

                if f.alternate() {
                    writeln!(f, " /* {span} */")?;
                }
            }
        }

        Ok(())
    }
}

impl<'a> Display for MIRExpression<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.inner)
    }
}

impl<'a> Display for MIRExpressionInner<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Use parentheses for binary operations to maintain precedence
        match self {
            MIRExpressionInner::Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            MIRExpressionInner::Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            MIRExpressionInner::Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            MIRExpressionInner::Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
            MIRExpressionInner::Equal(lhs, rhs) => write!(f, "({} == {})", lhs, rhs),
            MIRExpressionInner::NotEqual(lhs, rhs) => write!(f, "({} != {})", lhs, rhs),
            MIRExpressionInner::Less(lhs, rhs) => write!(f, "({} < {})", lhs, rhs),
            MIRExpressionInner::Greater(lhs, rhs) => write!(f, "({} > {})", lhs, rhs),
            MIRExpressionInner::LessEq(lhs, rhs) => write!(f, "({} <= {})", lhs, rhs),
            MIRExpressionInner::GreaterEq(lhs, rhs) => write!(f, "({} >= {})", lhs, rhs),
            MIRExpressionInner::BoolAnd(lhs, rhs) => write!(f, "({} && {})", lhs, rhs),
            MIRExpressionInner::BoolOr(lhs, rhs) => write!(f, "({} || {})", lhs, rhs),
            MIRExpressionInner::Number(val) => write!(f, "{}", val),
            MIRExpressionInner::Bool(val) => write!(f, "{}", val),
            MIRExpressionInner::Variable(name) => write!(f, "{}", name),
        }
    }
}

impl<'a> Display for MIRType<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.ty)
    }
}

impl<'a> Display for MIRTypeInner<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let type_name: Cow<'_, str> = self.clone().into();
        write!(f, "{}", type_name)
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
