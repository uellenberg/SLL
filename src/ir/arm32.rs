use crate::ir::alloc::{StackAllocator, TypeData};
use crate::ir::{
    IRBinaryOperation, IRConstant, IRFunction, IRLoadBinary, IRLoadOp, IRLoadUnary, IRProgram,
    IRStatement, IRStatic, IRType,
};
use std::fmt::Write as _;

#[derive(Default)]
struct Arm32Context {
    /// The current output string.
    data: String,
}

impl Arm32Context {
    /// Writes an instruction.
    fn push_instruction(&mut self, code: String, data: String) {
        write!(&mut self.data, "\t{code}\t{data}\n").expect("Push instruction failed!");
    }

    /// Writes a labeled instruction.
    fn push_labeled_instruction(&mut self, label: String, code: String, data: String) {
        write!(&mut self.data, "{label}:\n\t{code}\t{data}\n")
            .expect("Push labeled instruction failed!");
    }

    /// Writes a label.
    fn push_label(&mut self, name: String) {
        write!(&mut self.data, "{name}:\n").expect("Push label failed!");
    }
}

/// Converts an IRProgram into Arm32.
pub fn ir_to_arm32<'a>(program: &IRProgram<'a>) -> String {
    let mut ctx = Arm32Context::default();

    ctx.push_instruction(".syntax".into(), "unified".into());
    ctx.push_instruction(".arm".into(), "".into());

    // TODO: Not all const / static names (e.g., "b") are valid.
    for constant in &program.constants {
        lower_constant(&mut ctx, constant.1);
    }

    ctx.push_instruction(".section".into(), ".data".into());

    for static_data in &program.statics {
        lower_static(&mut ctx, static_data.1);
    }

    ctx.push_instruction(".section".into(), ".text".into());
    ctx.push_instruction(".global".into(), ".main".into());

    for function in &program.functions {
        lower_function(&mut ctx, function.1);
    }

    ctx.data
}

/// Converts IRType to its size and alignment.
fn lower_type(ir_type: &IRType) -> TypeData {
    match ir_type {
        IRType::U32 => TypeData { size: 4, align: 4 },
        IRType::Bool => TypeData { size: 1, align: 1 },
        IRType::Named(val) => todo!(),
    }
}

/// Converts IRConstant to Arm32.
fn lower_constant<'a>(ctx: &mut Arm32Context, ir_constant: &IRConstant<'a>) {
    let IRType::U32 = ir_constant.ty else {
        panic!("Non-numeric const type during IR lowering: {ir_constant:?}");
    };

    ctx.push_instruction(
        ".equ".into(),
        format!("{}, {}", ir_constant.name, ir_constant.value.to_string()),
    );
}

/// Converts IRStatic to Arm32.
fn lower_static<'a>(ctx: &mut Arm32Context, ir_static: &IRStatic<'a>) {
    let IRType::U32 = ir_static.ty else {
        panic!("Non-numeric static type during IR lowering: {ir_static:?}");
    };

    ctx.push_labeled_instruction(
        ir_static.name.to_string(),
        ".word".into(),
        ir_static.value.to_string(),
    );
}

/// Converts IRFunction to Arm32.
fn lower_function<'a>(ctx: &mut Arm32Context, ir_function: &IRFunction<'a>) {
    if ir_function.args.len() > 0 {
        todo!();
    }

    if ir_function.ret_ty.is_some() {
        todo!();
    }

    // Function names are unique.
    ctx.push_label(format!(".{}", ir_function.name));

    // Save calling data.
    ctx.push_instruction("PUSH".into(), "{FP, LR}".into());

    // Setup new FP.
    ctx.push_instruction("MOV".into(), "FP, SP".into());

    // Stack requires 8-byte alignment, and an offset
    // of 4 for the old FP (currently, FP points to the old FP).
    let mut stack_alloc = StackAllocator::new(8, 4);

    // We need to give the stack allocator data
    // before it can continue.
    for statement in &ir_function.body {
        match statement {
            IRStatement::CreateVariable(var) => {
                let ty_info = lower_type(&var.ty);

                stack_alloc.create(var.name.clone(), ty_info);
            }
            IRStatement::DropVariable(name) => {
                stack_alloc.drop(name);
            }
            // We only care about variables here.
            _ => {}
        }
    }

    // Set stack directly to maximum instead of push/pop.
    ctx.push_instruction(
        "SUB".into(),
        format!("SP, SP, #{}", stack_alloc.stack_size().to_string()),
    );

    for statement in &ir_function.body {
        lower_statement(ctx, &mut stack_alloc, statement);
    }

    // Function names are unique.
    ctx.push_label(format!(".{}_ret", ir_function.name));

    // Restore stack.
    ctx.push_instruction("MOV".into(), "SP, FP".into());

    // Return.
    ctx.push_instruction("POP".into(), "{FP, PC}".into());
}

/// Converts IRStatement to Arm32.
/// The StackAllocator must have already
/// ran through every statement.
fn lower_statement<'a>(
    ctx: &mut Arm32Context,
    stack_alloc: &mut StackAllocator<'a>,
    ir_statement: &IRStatement<'a>,
) {
    // TODO: Handle statics and constants.
    macro_rules! load_var {
        ($var_data:expr, $reg:expr) => {
            if $var_data.1.size == 4 {
                ctx.push_instruction("LDR".into(), format!("{}, [FP, #-{}]", $reg, $var_data.0));
            } else if $var_data.1.size == 1 {
                ctx.push_instruction("LDRB".into(), format!("{}, [FP, #-{}]", $reg, $var_data.0));
            } else {
                todo!()
            }
        };
    }

    macro_rules! binary_op {
        ($op:expr, ($left_reg:expr, $right_reg:expr) => $store_reg:expr) => {
            match $op {
                IRBinaryOperation::Add32 => {
                    ctx.push_instruction(
                        "ADD".into(),
                        format!("{}, {}, {}", $store_reg, $left_reg, $right_reg),
                    );
                }
                IRBinaryOperation::Sub32 => {
                    ctx.push_instruction(
                        "SUB".into(),
                        format!("{}, {}, {}", $store_reg, $left_reg, $right_reg),
                    );
                }
                IRBinaryOperation::Mul32 => {
                    ctx.push_instruction(
                        "MUL".into(),
                        format!("{}, {}, {}", $store_reg, $left_reg, $right_reg),
                    );
                }
                IRBinaryOperation::Div32 => {
                    ctx.push_instruction(
                        "DIV".into(),
                        format!("{}, {}, {}", $store_reg, $left_reg, $right_reg),
                    );
                }
                IRBinaryOperation::Equal32 => {
                    ctx.push_instruction("CMP".into(), format!("{}, {}", &$left_reg, &$right_reg));
                    ctx.push_instruction("MOV".into(), format!("{}, #0", &$store_reg));
                    ctx.push_instruction("MOVEQ".into(), format!("{}, #1", $store_reg));
                }
                IRBinaryOperation::NotEqual32 => {
                    ctx.push_instruction("CMP".into(), format!("{}, {}", &$left_reg, &$right_reg));
                    ctx.push_instruction("MOV".into(), format!("{}, #0", &$store_reg));
                    ctx.push_instruction("MOVNE".into(), format!("{}, #1", $store_reg));
                }
                IRBinaryOperation::Less32 => {
                    ctx.push_instruction("CMP".into(), format!("{}, {}", &$left_reg, &$right_reg));
                    ctx.push_instruction("MOV".into(), format!("{}, #0", &$store_reg));
                    ctx.push_instruction("MOVLT".into(), format!("{}, #1", $store_reg));
                }
                IRBinaryOperation::Greater32 => {
                    ctx.push_instruction("CMP".into(), format!("{}, {}", &$left_reg, &$right_reg));
                    ctx.push_instruction("MOV".into(), format!("{}, #0", &$store_reg));
                    ctx.push_instruction("MOVGT".into(), format!("{}, #1", $store_reg));
                }
                IRBinaryOperation::LessEq32 => {
                    ctx.push_instruction("CMP".into(), format!("{}, {}", &$left_reg, &$right_reg));
                    ctx.push_instruction("MOV".into(), format!("{}, #0", &$store_reg));
                    ctx.push_instruction("MOVLE".into(), format!("{}, #1", $store_reg));
                }
                IRBinaryOperation::GreaterEq32 => {
                    ctx.push_instruction("CMP".into(), format!("{}, {}", &$left_reg, &$right_reg));
                    ctx.push_instruction("MOV".into(), format!("{}, #0", &$store_reg));
                    ctx.push_instruction("MOVGE".into(), format!("{}, #1", $store_reg));
                }
                _ => todo!(),
            }
        };
    }

    macro_rules! store_var {
        ($var_data:expr, $reg:expr) => {
            if $var_data.1.size == 4 {
                ctx.push_instruction("STR".into(), format!("{}, [FP, #-{}]", $reg, $var_data.0));
            } else if $var_data.1.size == 1 {
                ctx.push_instruction("STRB".into(), format!("{}, [FP, #-{}]", $reg, $var_data.0));
            } else {
                todo!()
            }
        };
    }

    macro_rules! load_op {
        ($value:expr, $output_size:expr, $out_reg:expr) => {
            match $value {
                IRLoadOp::Unary(value) => {
                    match value {
                        IRLoadUnary::Num(value) => {
                            if value >= &0 && value <= &65535 {
                                ctx.push_instruction(
                                    "MOV".into(),
                                    format!("{}, #{}", &$out_reg, value.to_string()),
                                );
                            } else {
                                todo!();
                            }
                        }
                        IRLoadUnary::Variable(value) => {
                            let var_data = stack_alloc.get(value);

                            // The output type can't expect more
                            // data than the input can give it.
                            assert!($output_size.size <= var_data.1.size);

                            load_var!(var_data, &$out_reg);
                        }
                    }
                }
                IRLoadOp::Binary(op, value) => {
                    match value {
                        IRLoadBinary::VariableVariable(var1, var2) => {
                            let var1_data = stack_alloc.get(var1);
                            let var2_data = stack_alloc.get(var2);

                            // We can't perform binary operations
                            // on different types.
                            assert_eq!(var1_data.1, var2_data.1);

                            // The output type can't expect more
                            // data than the input can give it.
                            assert!($output_size.size <= var1_data.1.size);

                            load_var!(var1_data, "R0");
                            load_var!(var2_data, "R1");
                        }
                        IRLoadBinary::NumVariable(num, var) => {
                            let var_data = stack_alloc.get(var);

                            // The output type can't expect more
                            // data than the input can give it.
                            assert!($output_size.size <= var_data.1.size);

                            if num >= &0 && num <= &65535 {
                                ctx.push_instruction(
                                    "MOV".into(),
                                    format!("R0, #{}", num.to_string()),
                                );
                            } else {
                                todo!();
                            }

                            load_var!(var_data, "R1");
                        }
                        IRLoadBinary::VariableNum(num, var) => {
                            let var_data = stack_alloc.get(var);

                            // The output type can't expect more
                            // data than the input can give it.
                            assert!($output_size.size <= var_data.1.size);

                            load_var!(var_data, "R0");

                            if num >= &0 && num <= &65535 {
                                ctx.push_instruction(
                                    "MOV".into(),
                                    format!("R1, #{}", num.to_string()),
                                );
                            } else {
                                todo!();
                            }
                        }
                    }

                    binary_op!(op, ("R0", "R1") => &$out_reg);
                }
            }
        }
    }

    match ir_statement {
        IRStatement::CreateVariable(var) => {
            let ty_info = lower_type(&var.ty);

            stack_alloc.create(var.name.clone(), ty_info);
        }
        IRStatement::DropVariable(name) => {
            stack_alloc.drop(name);
        }
        IRStatement::SetVariable { name, value } => {
            let output_data = stack_alloc.get(name);

            load_op!(value, output_data.1, "R0");

            store_var!(output_data, "R0");
        }
        IRStatement::Label { name } => {
            // Label names are unique.
            ctx.push_label(format!(".{}", name));
        }
        IRStatement::Goto { name } => ctx.push_instruction("B".into(), format!(".{}", name)),
        IRStatement::GotoNotEqual { name, condition } => {
            load_op!(condition, lower_type(&IRType::Bool), "R0");

            ctx.push_instruction("CMP".into(), "R0, #1".into());
            ctx.push_instruction("BNE".into(), format!(".{}", name));
        }
    }
}
