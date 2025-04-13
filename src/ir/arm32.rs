use crate::ir::alloc::{StackAllocator, TypeData};
use crate::ir::{
    IRBinaryOperation, IRConstant, IRFunction, IRProgram, IRStatement, IRStatic, IRType,
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

    ctx.push_label(ir_function.name.to_string());

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

    ctx.push_label(format!("{}_ret", ir_function.name));

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

    match ir_statement {
        IRStatement::CreateVariable(var) => {
            let ty_info = lower_type(&var.ty);

            stack_alloc.create(var.name.clone(), ty_info);
        }
        IRStatement::DropVariable(name) => {
            stack_alloc.drop(name);
        }
        IRStatement::SetVariableNum { name, value } => {
            let output_data = stack_alloc.get(name);

            if value >= &0 && value <= &65535 {
                ctx.push_instruction("MOV".into(), format!("R0, #{}", value.to_string()));
            } else {
                todo!();
            }

            if output_data.1.size == 4 {
                ctx.push_instruction("STR".into(), format!("R0, [FP, #-{}]", output_data.0));
            } else if output_data.1.size == 1 {
                ctx.push_instruction("STRB".into(), format!("R0, [FP, #-{}]", output_data.0));
            }
        }
        IRStatement::SetVariableVariable { name, value } => {
            let output_data = stack_alloc.get(name);
            let var_data = stack_alloc.get(value);

            // The output type can't expect more
            // data than the input can give it.
            assert!(output_data.1.size <= var_data.1.size);

            if var_data.1.size == 4 {
                ctx.push_instruction("LDR".into(), format!("R1, [FP, #-{}]", var_data.0));
            } else if var_data.1.size == 1 {
                ctx.push_instruction("LDRB".into(), format!("R1, [FP, #-{}]", var_data.0));
            } else {
                todo!()
            }

            if output_data.1.size == 4 {
                ctx.push_instruction("STR".into(), format!("R0, [FP, #-{}]", output_data.0));
            } else if output_data.1.size == 1 {
                ctx.push_instruction("STRB".into(), format!("R0, [FP, #-{}]", output_data.0));
            }
        }
        IRStatement::SetVariableOpVariableVariable {
            name,
            value: (var1, var2),
            op,
        } => {
            let output_data = stack_alloc.get(name);
            let var1_data = stack_alloc.get(var1);
            let var2_data = stack_alloc.get(var2);

            // We can't perform binary operations
            // on different types.
            assert_eq!(var1_data.1, var2_data.1);

            // The output type can't expect more
            // data than the input can give it.
            assert!(output_data.1.size <= var1_data.1.size);

            if var1_data.1.size == 4 {
                ctx.push_instruction("LDR".into(), format!("R0, [FP, #-{}]", var1_data.0));
                ctx.push_instruction("LDR".into(), format!("R1, [FP, #-{}]", var2_data.0));
            } else if var1_data.1.size == 1 {
                ctx.push_instruction("LDRB".into(), format!("R0, [FP, #-{}]", var1_data.0));
                ctx.push_instruction("LDRB".into(), format!("R1, [FP, #-{}]", var2_data.0));
            } else {
                todo!()
            }

            match op {
                IRBinaryOperation::Add32 => {
                    ctx.push_instruction("ADD".into(), "R0, R0, R1".into());
                }
                IRBinaryOperation::Equal32 => {
                    ctx.push_instruction("CMP".into(), "R0, R1".into());
                    ctx.push_instruction("MOV".into(), "R0, #0".into());
                    ctx.push_instruction("MOVEQ".into(), "R0, #1".into());
                }
                _ => todo!(),
            }

            if output_data.1.size == 4 {
                ctx.push_instruction("STR".into(), format!("R0, [FP, #-{}]", output_data.0));
            } else if output_data.1.size == 1 {
                ctx.push_instruction("STRB".into(), format!("R0, [FP, #-{}]", output_data.0));
            }
        }
        IRStatement::SetVariableOpNumVariable {
            name,
            value: (num, var),
            op,
        } => {
            let output_data = stack_alloc.get(name);
            let var_data = stack_alloc.get(var);

            // The output type can't expect more
            // data than the input can give it.
            assert!(output_data.1.size <= var_data.1.size);

            if num >= &0 && num <= &65535 {
                ctx.push_instruction("MOV".into(), format!("R0, #{}", num.to_string()));
            } else {
                todo!();
            }

            if var_data.1.size == 4 {
                ctx.push_instruction("LDR".into(), format!("R1, [FP, #-{}]", var_data.0));
            } else if var_data.1.size == 1 {
                ctx.push_instruction("LDRB".into(), format!("R1, [FP, #-{}]", var_data.0));
            } else {
                todo!()
            }

            match op {
                IRBinaryOperation::Add32 => {
                    ctx.push_instruction("ADD".into(), "R0, R0, R1".into());
                }
                IRBinaryOperation::Equal32 => {
                    ctx.push_instruction("CMP".into(), "R0, R1".into());
                    ctx.push_instruction("MOV".into(), "R0, #0".into());
                    ctx.push_instruction("MOVEQ".into(), "R0, #1".into());
                }
                _ => todo!(),
            }

            if output_data.1.size == 4 {
                ctx.push_instruction("STR".into(), format!("R0, [FP, #-{}]", output_data.0));
            } else if output_data.1.size == 1 {
                ctx.push_instruction("STRB".into(), format!("R0, [FP, #-{}]", output_data.0));
            }
        }
    }
}
