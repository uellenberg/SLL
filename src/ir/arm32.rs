use crate::ir::alloc::{
    RegMaybeTemporary, RegisterAllocator, StackAllocator, TemporaryRegister, TypeData,
    UnifiedAllocator,
};
use crate::ir::{
    IRBinaryOperation, IRConstant, IRFnSource, IRFunction, IRLoadBinary, IRLoadOp, IRLoadUnary,
    IRProgram, IRStatement, IRStatic, IRType, IRVariable,
};
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Write as _;

#[derive(Default, Clone)]
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

/// Used to allocate variables in a function
/// context.
struct Arm32Allocator<'a, 'b> {
    /// Allocates locals onto registers.
    reg_alloc: RegisterAllocator<'a>,

    /// Allocates locals on the stack.
    stack_alloc: StackAllocator<'a>,

    /// A list of statics that the
    /// function has access to.
    statics: &'b HashMap<Cow<'a, str>, TypeData>,
}

impl<'a, 'b> UnifiedAllocator<'a> for Arm32Allocator<'a, 'b> {
    type Ctx = Arm32Context;

    type Read1 = RegMaybeTemporary<1>;
    type Read4 = RegMaybeTemporary<1>;
    type Read8 = RegMaybeTemporary<2>;

    type Read1Direct = [&'static str; 1];
    type Read4Direct = [&'static str; 1];
    type Read8Direct = [&'static str; 2];

    fn alloc_variable(&mut self, name: Cow<'a, str>, ty: TypeData) {
        if self.reg_alloc.try_alloc(name.clone(), ty).is_some() {
            return;
        }

        self.stack_alloc.create(name, ty);
    }

    fn drop_variable(&mut self, name: &Cow<'a, str>) {
        // Variable names are always unique, so this
        // can't cause conflicts.
        if self.reg_alloc.drop(name) {
            return;
        }

        if self.stack_alloc.drop(name) {
            return;
        }

        panic!("Failed to drop variable in Arm32 unified allocator!");
    }

    fn alloc_temporary(&mut self, size: u32) -> TemporaryRegister {
        self.reg_alloc.alloc_temporary(size)
    }

    fn drop_temporary(&mut self, temporary: TemporaryRegister) {
        self.reg_alloc.drop_temporary(temporary);
    }

    fn get(&self, name: &Cow<'a, str>) -> TypeData {
        if let Some(data) = self.reg_alloc.get(name) {
            return data.ty();
        }

        if let Some(data) = self.stack_alloc.get(name) {
            return data.1;
        }

        if let Some(data) = self.statics.get(name) {
            return *data;
        }

        panic!("Failed to get variable info!");
    }

    fn read_1(&mut self, ctx: &mut Self::Ctx, name: &Cow<'a, str>, offset: u32) -> Self::Read1 {
        if let Some(reg) = self.reg_alloc.get(name) {
            let (reg_name, reg_offset, bytes) = reg.try_read(offset).expect("Read1 failed!");

            // 1 byte cannot be spread across multiple registers.
            assert!(bytes >= 1);

            // If this is stored in a larger type,
            // it means that the rest of the data
            // in the register is valid, and therefore
            // we need a temporary.
            if reg.ty().size != 1 {
                // Minimum register 4 bytes.
                let temp = self.alloc_temporary(4);

                // Shift into the right position in the temporary.
                // This will still include junk data.
                match reg_offset {
                    // Already in position.
                    0 => {}
                    1 => ctx.push_instruction(
                        "LSR".into(),
                        format!("{}, {}, #8", temp.name(), reg_name),
                    ),
                    2 => ctx.push_instruction(
                        "LSR".into(),
                        format!("{}, {}, #16", temp.name(), reg_name),
                    ),
                    3 => ctx.push_instruction(
                        "LSR".into(),
                        format!("{}, {}, #24", temp.name(), reg_name),
                    ),
                    _ => panic!("Unexpected offset for read1!"),
                }

                // Remove junk data by only keeping the last byte.
                match reg_offset {
                    0 | 1 | 2 => ctx.push_instruction(
                        "AND".into(),
                        format!("AND {}, {}, #0xFF", temp.name(), temp.name()),
                    ),
                    // No junk, since our data was the last part
                    // of the original register.
                    3 => {}
                    _ => panic!("Unexpected offset for read1!"),
                }

                return RegMaybeTemporary::Temporary([temp]);
            }

            // The only data in this register is
            // the one we want, so we can just
            // return it directly.
            // In this case, offset should be 0.
            assert_eq!(reg_offset, 0);

            return RegMaybeTemporary::Register([reg_name]);
        }

        if let Some(stack) = self.stack_alloc.get(name) {
            // Minimum register 4 bytes.
            let temp = self.alloc_temporary(4);

            ctx.push_instruction(
                "LDRB".into(),
                format!("{}, [FP, #-{}]", temp.name(), offset + stack.0),
            );

            return RegMaybeTemporary::Temporary([temp]);
        }

        if let Some(_static_data) = self.statics.get(name) {
            // Minimum register 4 bytes.
            let temp = self.alloc_temporary(4);

            ctx.push_instruction("LDR".into(), format!("{}, ={}", temp.name(), name));

            if offset == 0 {
                ctx.push_instruction("LDRB".into(), format!("{}, [{}]", temp.name(), temp.name()));
            } else {
                // TODO: Should this be positive?
                ctx.push_instruction(
                    "LDRB".into(),
                    format!("{}, [{}, #-{}]", temp.name(), temp.name(), offset),
                );
            }

            return RegMaybeTemporary::Temporary([temp]);
        }

        panic!("Tried to read1 non-existent variable!");
    }

    fn read_4(&mut self, ctx: &mut Self::Ctx, name: &Cow<'a, str>, offset: u32) -> Self::Read4 {
        // Only aligned reads are allowed.
        assert_eq!(offset % 4, 0);

        if let Some(reg) = self.reg_alloc.get(name) {
            let (reg_name, reg_offset, bytes) = reg.try_read(offset).expect("Read4 failed!");

            // Due to alignment, if a 4-byte value is placed in a struct,
            // it will be aligned to 4 bytes.
            // Since our registers are only 4-bytes, it's impossible
            // for a 4-byte value to be spread across multiple registers.
            assert!(bytes >= 4);
            assert_eq!(reg_offset, 0);

            return RegMaybeTemporary::Register([reg_name]);
        }

        if let Some(stack) = self.stack_alloc.get(name) {
            // Minimum register 4 bytes.
            let temp = self.alloc_temporary(4);

            ctx.push_instruction(
                "LDR".into(),
                format!("{}, [FP, #-{}]", temp.name(), offset + stack.0),
            );

            return RegMaybeTemporary::Temporary([temp]);
        }

        if let Some(_static_data) = self.statics.get(name) {
            // Minimum register 4 bytes.
            let temp = self.alloc_temporary(4);

            ctx.push_instruction("LDR".into(), format!("{}, ={}", temp.name(), name));

            if offset == 0 {
                ctx.push_instruction("LDR".into(), format!("{}, [{}]", temp.name(), temp.name()));
            } else {
                // TODO: Should this be positive?
                ctx.push_instruction(
                    "LDR".into(),
                    format!("{}, [{}, #-{}]", temp.name(), temp.name(), offset),
                );
            }

            return RegMaybeTemporary::Temporary([temp]);
        }

        panic!("Tried to read4 non-existent variable!");
    }

    fn read_8(&mut self, ctx: &mut Self::Ctx, name: &Cow<'a, str>, offset: u32) -> Self::Read8 {
        todo!()
    }

    fn read_1_direct(&self, name: &Cow<'a, str>, offset: u32) -> Option<Self::Read1Direct> {
        if let Some(reg) = self.reg_alloc.get(name) {
            let (reg_name, reg_offset, bytes) = reg.try_read(offset).expect("Read1 failed!");

            // 1 byte cannot be spread across multiple registers.
            assert!(bytes >= 1);

            // If this is stored in a larger type,
            // it means that the rest of the data
            // in the register is valid, and therefore
            // we need a temporary.
            if reg.ty().size != 1 {
                // This requires a temporary.
                return None;
            }

            // The only data in this register is
            // the one we want, so we can just
            // return it directly.
            // In this case, offset should be 0.
            assert_eq!(reg_offset, 0);

            return Some([reg_name]);
        }

        if let Some(_stack) = self.stack_alloc.get(name) {
            // This requires a temporary.
            return None;
        }

        if let Some(_static_data) = self.statics.get(name) {
            // This requires a temporary.
            return None;
        }

        panic!("Tried to read1_direct non-existent variable!");
    }

    fn read_4_direct(&self, name: &Cow<'a, str>, offset: u32) -> Option<Self::Read4Direct> {
        // Only aligned reads are allowed.
        assert_eq!(offset % 4, 0);

        if let Some(reg) = self.reg_alloc.get(name) {
            let (reg_name, reg_offset, bytes) = reg.try_read(offset).expect("Read4 failed!");

            // Due to alignment, if a 4-byte value is placed in a struct,
            // it will be aligned to 4 bytes.
            // Since our registers are only 4-bytes, it's impossible
            // for a 4-byte value to be spread across multiple registers.
            assert!(bytes >= 4);
            assert_eq!(reg_offset, 0);

            return Some([reg_name]);
        }

        if let Some(_stack) = self.stack_alloc.get(name) {
            // This requires a temporary.
            return None;
        }

        if let Some(_static_data) = self.statics.get(name) {
            // This requires a temporary.
            return None;
        }

        panic!("Tried to read4_direct non-existent variable!");
    }

    fn read_8_direct(&self, name: &Cow<'a, str>, offset: u32) -> Option<Self::Read8Direct> {
        todo!()
    }

    fn write_1(
        &mut self,
        ctx: &mut Self::Ctx,
        reg: &Self::Read1,
        name: &Cow<'a, str>,
        offset: u32,
    ) {
        let from_reg_name = reg.names()[0];

        if let Some(reg) = self.reg_alloc.get(name) {
            let (reg_name, reg_offset, bytes) = reg.try_read(offset).expect("Write1 failed!");

            // 1 byte cannot be spread across multiple registers.
            assert!(bytes >= 1);

            // If this is stored in a larger type,
            // it means that the rest of the data
            // in the register is valid, and therefore
            // we need a temporary.
            if reg.ty().size != 1 {
                // Shift into the right position in the temporary.
                // This will still include junk data.
                match reg_offset {
                    // Already in position.
                    0 => ctx.push_instruction(
                        "BFI".into(),
                        format!("{reg_name}, {from_reg_name}, #0, #8"),
                    ),
                    1 => ctx.push_instruction(
                        "BFI".into(),
                        format!("{reg_name}, {from_reg_name}, #8, #8"),
                    ),
                    2 => ctx.push_instruction(
                        "BFI".into(),
                        format!("{reg_name}, {from_reg_name}, #16, #8"),
                    ),
                    3 => ctx.push_instruction(
                        "BFI".into(),
                        format!("{reg_name}, {from_reg_name}, #24, #8"),
                    ),
                    _ => panic!("Unexpected offset for write1!"),
                }

                return;
            }

            // The only data in this register is
            // the one we want, so we can just
            // store it directly.
            // In this case, offset should be 0.
            assert_eq!(reg_offset, 0);

            ctx.push_instruction("MOV".into(), format!("{}, {}", reg_name, from_reg_name));

            return;
        }

        if let Some(stack) = self.stack_alloc.get(name) {
            ctx.push_instruction(
                "STRB".into(),
                format!("{}, [FP, #-{}]", from_reg_name, offset + stack.0),
            );

            return;
        }

        if let Some(_static_data) = self.statics.get(name) {
            // Minimum register 4 bytes.
            let temp = self.alloc_temporary(4);

            ctx.push_instruction("LDR".into(), format!("{}, ={}", temp.name(), name));

            if offset == 0 {
                ctx.push_instruction(
                    "STRB".into(),
                    format!("{}, [{}]", from_reg_name, temp.name()),
                );
            } else {
                // TODO: Should this be positive?
                ctx.push_instruction(
                    "STRB".into(),
                    format!("{}, [{}, #-{}]", from_reg_name, temp.name(), offset),
                );
            }

            self.drop_temporary(temp);

            return;
        }

        panic!("Tried to write1 non-existent variable!");
    }

    fn write_4(
        &mut self,
        ctx: &mut Self::Ctx,
        reg: &Self::Read1,
        name: &Cow<'a, str>,
        offset: u32,
    ) {
        let from_reg_name = reg.names()[0];

        // Only aligned writes are allowed.
        assert_eq!(offset % 4, 0);

        if let Some(reg) = self.reg_alloc.get(name) {
            let (reg_name, reg_offset, bytes) = reg.try_read(offset).expect("Write4 failed!");

            // Due to alignment, if a 4-byte value is placed in a struct,
            // it will be aligned to 4 bytes.
            // Since our registers are only 4-bytes, it's impossible
            // for a 4-byte value to be spread across multiple registers.
            assert!(bytes >= 4);
            assert_eq!(reg_offset, 0);

            ctx.push_instruction("MOV".into(), format!("{}, {}", reg_name, from_reg_name));

            return;
        }

        if let Some(stack) = self.stack_alloc.get(name) {
            ctx.push_instruction(
                "STR".into(),
                format!("{}, [FP, #-{}]", from_reg_name, offset + stack.0),
            );

            return;
        }

        if let Some(_static_data) = self.statics.get(name) {
            // Minimum register 4 bytes.
            let temp = self.alloc_temporary(4);

            ctx.push_instruction("LDR".into(), format!("{}, ={}", temp.name(), name));

            if offset == 0 {
                ctx.push_instruction(
                    "STR".into(),
                    format!("{}, [{}]", from_reg_name, temp.name()),
                );
            } else {
                // TODO: Should this be positive?
                ctx.push_instruction(
                    "STR".into(),
                    format!("{}, [{}, #-{}]", from_reg_name, temp.name(), offset),
                );
            }

            self.drop_temporary(temp);

            return;
        }

        panic!("Tried to write4 non-existent variable!");
    }

    fn write_8(
        &mut self,
        ctx: &mut Self::Ctx,
        reg: &Self::Read1,
        name: &Cow<'a, str>,
        offset: u32,
    ) {
        todo!()
    }

    fn stack_size(&self) -> u32 {
        self.stack_alloc.stack_size()
    }

    fn used_regs(&self) -> impl Iterator<Item = &'static str> {
        self.reg_alloc.used_regs()
    }
}

/// Converts an IRProgram into Arm32.
pub fn ir_to_arm32<'a>(program: &IRProgram<'a>) -> String {
    let mut ctx = Arm32Context::default();

    ctx.push_instruction(".syntax".into(), "unified".into());
    ctx.push_instruction(".thumb".into(), "".into());

    // TODO: Not all const / static names (e.g., "b") are valid.
    for constant in &program.constants {
        lower_constant(&mut ctx, constant.1);
    }

    ctx.push_instruction(".section".into(), ".data".into());

    let mut statics = HashMap::new();

    for static_data in &program.statics {
        lower_static(&mut ctx, static_data.1);

        if statics
            .insert(static_data.0.clone(), lower_type(&static_data.1.ty))
            .is_some()
        {
            panic!("Overrode static!");
        }
    }

    ctx.push_instruction(".section".into(), ".text".into());
    ctx.push_instruction(".global".into(), "main".into());

    for function in &program.functions {
        lower_function(&mut ctx, &statics, function.1);
    }

    ctx.data
}

/// Converts IRType to its size and alignment.
fn lower_type(ir_type: &IRType) -> TypeData {
    match ir_type {
        IRType::U32 => TypeData { size: 4, align: 4 },
        IRType::Bool => TypeData { size: 1, align: 1 },
        IRType::FunctionPtr(_, _) => TypeData { size: 4, align: 4 },
        IRType::Ptr(_) => TypeData { size: 4, align: 4 },
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

/// Sets up a stack allocator with
/// function arguments.
fn alloc_args<'a>(alloc: &mut Arm32Allocator<'a, '_>, args: &[IRVariable<'a>]) {
    let regs: &'static [&'static str] = &["R0", "R1", "R2", "R3"];
    let mut cur_reg = 0;

    for arg in args {
        if !matches!(arg.ty, IRType::U32) {
            todo!();
        }

        if cur_reg >= regs.len() {
            // todo!();
        }

        let count = alloc.reg_alloc.take_registers([regs[cur_reg]]).len();
        assert_eq!(count, 0);

        alloc
            .reg_alloc
            .register_variable(arg.name.clone(), [regs[cur_reg]], lower_type(&arg.ty));
        cur_reg += 1;
    }
}

/// Converts IRFunction to Arm32.
fn lower_function<'a>(
    ctx: &mut Arm32Context,
    statics: &HashMap<Cow<'a, str>, TypeData>,
    ir_function: &IRFunction<'a>,
) {
    if ir_function.ret_ty.is_some() {
        todo!();
    }

    // TODO: Restrict function arg registers.
    let registers = [
        ("R4", 4),
        ("R5", 4),
        ("R6", 4),
        ("R7", 4),
        ("R8", 4),
        ("R9", 4),
        ("R10", 4),
        // These are last because they're used
        // for function calls, and if we use
        // them, we have to push/pop the stack
        // to save them.
        // TODO: Implement an actual priority system for this.
        ("R0", 4),
        ("R1", 4),
        ("R2", 4),
        ("R3", 4),
    ];

    // We need a new context for the inner statements.
    // The reason is that our function prefix and postfix
    // depends on data derived from running the statements
    // inside it.
    // So, we'll use this as a dummy context before
    // running it on the real context.
    let mut dummy_ctx = ctx.clone();

    let mut alloc = Arm32Allocator {
        // Stack requires 8-byte alignment, and an offset
        // of 4 for the old FP (currently, FP points to the old FP).
        stack_alloc: StackAllocator::new(8, 4),
        reg_alloc: RegisterAllocator::new(registers.clone(), 3),
        statics,
    };

    alloc_args(&mut alloc, &ir_function.args);

    for statement in &ir_function.body {
        lower_statement(&mut dummy_ctx, &mut alloc, statement);
    }

    let saved_regs = alloc.used_regs().intersperse(", ").collect::<String>();

    let mut push = "FP, LR".to_string();
    let mut pop = "FP, PC".to_string();

    if saved_regs.len() != 0 {
        push += ", ";
        push += &saved_regs;

        pop += ", ";
        pop += &saved_regs;
    }

    // Function names are unique.
    ctx.push_label(format!("{}", ir_function.name));

    // Save calling data.
    ctx.push_instruction("PUSH".into(), format!("{{{}}}", push));

    // Setup new FP.
    ctx.push_instruction("MOV".into(), "FP, SP".into());

    // Set stack directly to maximum instead of push/pop.
    ctx.push_instruction(
        "SUB".into(),
        format!("SP, SP, #{}", alloc.stack_alloc.stack_size().to_string()),
    );

    // The reason this is done twice is that
    // args are dropped when calling lower_statement,
    // so need to be re-added.
    alloc_args(&mut alloc, &ir_function.args);

    for statement in &ir_function.body {
        lower_statement(ctx, &mut alloc, statement);
    }

    // Function names are unique.
    ctx.push_label(format!(".{}_ret", ir_function.name));

    // Restore stack.
    ctx.push_instruction("MOV".into(), "SP, FP".into());

    // Return.
    ctx.push_instruction("POP".into(), format!("{{{}}}", pop));
}

/// Lowers a 32-bit binary operation
/// into assembly.
fn lower_op_binary_32(
    ctx: &mut Arm32Context,
    op: &IRBinaryOperation,
    store_reg: &str,
    left_reg: &str,
    right_reg: &str,
) {
    match op {
        IRBinaryOperation::Add32 => {
            ctx.push_instruction(
                "ADD".into(),
                format!("{}, {}, {}", store_reg, left_reg, right_reg),
            );
        }
        IRBinaryOperation::Sub32 => {
            ctx.push_instruction(
                "SUB".into(),
                format!("{}, {}, {}", store_reg, left_reg, right_reg),
            );
        }
        IRBinaryOperation::Mul32 => {
            ctx.push_instruction(
                "MUL".into(),
                format!("{}, {}, {}", store_reg, left_reg, right_reg),
            );
        }
        IRBinaryOperation::Div32 => {
            ctx.push_instruction(
                "DIV".into(),
                format!("{}, {}, {}", store_reg, left_reg, right_reg),
            );
        }
        IRBinaryOperation::Equal32 => {
            ctx.push_instruction("CMP".into(), format!("{}, {}", &left_reg, &right_reg));
            ctx.push_instruction("MOV".into(), format!("{}, #0", &store_reg));
            ctx.push_instruction("IT".into(), "EQ".into());
            ctx.push_instruction("MOVEQ".into(), format!("{}, #1", store_reg));
        }
        IRBinaryOperation::NotEqual32 => {
            ctx.push_instruction("CMP".into(), format!("{}, {}", &left_reg, &right_reg));
            ctx.push_instruction("MOV".into(), format!("{}, #0", &store_reg));
            ctx.push_instruction("IT".into(), "NE".into());
            ctx.push_instruction("MOVNE".into(), format!("{}, #1", store_reg));
        }
        IRBinaryOperation::Less32 => {
            ctx.push_instruction("CMP".into(), format!("{}, {}", &left_reg, &right_reg));
            ctx.push_instruction("MOV".into(), format!("{}, #0", &store_reg));
            ctx.push_instruction("IT".into(), "LT".into());
            ctx.push_instruction("MOVLT".into(), format!("{}, #1", store_reg));
        }
        IRBinaryOperation::Greater32 => {
            ctx.push_instruction("CMP".into(), format!("{}, {}", &left_reg, &right_reg));
            ctx.push_instruction("MOV".into(), format!("{}, #0", &store_reg));
            ctx.push_instruction("IT".into(), "GT".into());
            ctx.push_instruction("MOVGT".into(), format!("{}, #1", store_reg));
        }
        IRBinaryOperation::LessEq32 => {
            ctx.push_instruction("CMP".into(), format!("{}, {}", &left_reg, &right_reg));
            ctx.push_instruction("MOV".into(), format!("{}, #0", &store_reg));
            ctx.push_instruction("IT".into(), "LE".into());
            ctx.push_instruction("MOVLE".into(), format!("{}, #1", store_reg));
        }
        IRBinaryOperation::GreaterEq32 => {
            ctx.push_instruction("CMP".into(), format!("{}, {}", &left_reg, &right_reg));
            ctx.push_instruction("MOV".into(), format!("{}, #0", &store_reg));
            ctx.push_instruction("IT".into(), "GE".into());
            ctx.push_instruction("MOVGE".into(), format!("{}, #1", store_reg));
        }
        _ => todo!(),
    }
}

/// Converts IRLoadOp into a
/// register that can be loaded from.
/// Size is the number of output registers
/// needed.
///
/// If output_reg is provided, this will
/// try to load into it, although it might
/// fail.
/// In that case, it will return the temporary
/// that it loaded into.
fn lower_load<'a, const Size: usize>(
    ctx: &mut Arm32Context,
    alloc: &mut Arm32Allocator<'a, '_>,
    ir_load: &IRLoadOp<'a>,
    output_size: TypeData,
    output_reg: Option<[&'static str; Size]>,
) -> Option<RegMaybeTemporary<Size>> {
    let get_temporary_output = |alloc: &mut Arm32Allocator<'a, '_>| {
        if let Some(output_reg) = output_reg {
            RegMaybeTemporary::Register(output_reg)
        } else {
            let temporary_size = match Size {
                1 => 4,
                2 => todo!(),
                _ => panic!("Unexpected Size in lower_load!"),
            };

            match Size {
                1 => RegMaybeTemporary::Temporary([alloc.alloc_temporary(temporary_size)])
                    .try_conv()
                    .unwrap(),
                2 => todo!(),
                _ => panic!("Unexpected Size in lower_load!"),
            }
        }
    };

    let get_return = |temporary: RegMaybeTemporary<Size>| {
        // If we used the output register
        // as a temporary, we shouldn't
        // return it.
        // This will tell the caller that
        // there's no need for a move and drop.

        // Of course, if there was no output
        // register to begin with, we'll always
        // return the temporary.
        let Some(output_reg) = output_reg else {
            return Some(temporary);
        };

        if temporary == RegMaybeTemporary::Register(output_reg) {
            // We used the output.
            None
        } else {
            // We made a new temporary.
            Some(temporary)
        }
    };

    match ir_load {
        IRLoadOp::Unary(value) => {
            match value {
                IRLoadUnary::Num(value) => {
                    if value >= &0 && value <= &65535 {
                        let temp = get_temporary_output(alloc);

                        match Size {
                            1 => {
                                ctx.push_instruction(
                                    "MOV".into(),
                                    format!("{}, #{}", temp.names()[0], value.to_string()),
                                );

                                get_return(temp)
                            }
                            2 => todo!(),
                            _ => panic!("Unexpected Size in lower_load!"),
                        }
                    } else {
                        todo!();
                    }
                }
                IRLoadUnary::Variable(value) => {
                    let var_data = alloc.get(value);

                    // The output type can't expect more
                    // data than the input can give it.
                    assert!(output_size.size <= var_data.size);

                    // TODO: Avoid a temporary here when reading stack -> register.
                    // When you look at this in the future, we can't always read out
                    // to the output register, so SetVariable will always need to contain
                    // a MOV.
                    // The reason is because read_1 might be direct, and will just return
                    // a register.
                    match var_data.size {
                        1 => get_return(alloc.read_1(ctx, value, 0).try_conv().unwrap()),
                        4 => get_return(alloc.read_4(ctx, value, 0).try_conv().unwrap()),
                        8 => todo!(),
                        _ => panic!("Unexpected Size in lower_load!"),
                    }
                }
                IRLoadUnary::Reference(value) => {
                    // TODO: References need to force stack allocation.
                    let var_data = alloc
                        .stack_alloc
                        .get(value)
                        .expect("References need to force stack allocation!");

                    // Pointer length is 4 bytes.
                    assert_eq!(output_size.size, 4);
                    assert_eq!(Size, 1);

                    match output_reg {
                        Some(reg) => {
                            ctx.push_instruction(
                                "ADD".into(),
                                format!("{}, SP, #{}", reg[0], var_data.0),
                            );
                            None
                        }
                        None => {
                            let temp = alloc.alloc_temporary(4);
                            ctx.push_instruction(
                                "ADD".into(),
                                format!("{}, SP, #{}", temp.name(), var_data.0),
                            );
                            Some(RegMaybeTemporary::Temporary([temp]).try_conv().unwrap())
                        }
                    }
                }
            }
        }
        IRLoadOp::Binary(op, value) => {
            match value {
                IRLoadBinary::VariableVariable(var1, var2) => {
                    let var1_data = alloc.get(var1);
                    let var2_data = alloc.get(var2);

                    // We can't perform binary operations
                    // on different types.
                    assert_eq!(var1_data, var2_data);

                    // The output type can't expect more
                    // data than the input can give it.
                    assert!(output_size.size <= var1_data.size);

                    let temp = get_temporary_output(alloc);

                    let left_reg: RegMaybeTemporary<Size> = match var1_data.size {
                        1 => alloc.read_1(ctx, var1, 0).try_conv().unwrap(),
                        4 => alloc.read_4(ctx, var1, 0).try_conv().unwrap(),
                        8 => todo!(),
                        _ => panic!("Unexpected Size in lower_load!"),
                    };

                    let right_reg: RegMaybeTemporary<Size> = match var2_data.size {
                        1 => alloc.read_1(ctx, var2, 0).try_conv().unwrap(),
                        4 => alloc.read_4(ctx, var2, 0).try_conv().unwrap(),
                        8 => todo!(),
                        _ => panic!("Unexpected Size in lower_load!"),
                    };

                    lower_op_binary_32(
                        ctx,
                        op,
                        temp.names()[0],
                        left_reg.names()[0],
                        right_reg.names()[0],
                    );

                    alloc.drop_maybe_temporary(left_reg);
                    alloc.drop_maybe_temporary(right_reg);

                    get_return(temp)
                }
                IRLoadBinary::NumVariable(num, var) => {
                    let var_data = alloc.get(var);

                    // The output type can't expect more
                    // data than the input can give it.
                    assert!(output_size.size <= var_data.size);

                    let temp = get_temporary_output(alloc);

                    let left_num = if num >= &0 && num <= &65535 {
                        format!("#{}", num.to_string())
                    } else {
                        todo!();
                    };

                    let right_reg: RegMaybeTemporary<Size> = match var_data.size {
                        1 => alloc.read_1(ctx, var, 0).try_conv().unwrap(),
                        4 => alloc.read_4(ctx, var, 0).try_conv().unwrap(),
                        8 => todo!(),
                        _ => panic!("Unexpected Size in lower_load!"),
                    };

                    lower_op_binary_32(ctx, op, temp.names()[0], &left_num, right_reg.names()[0]);

                    alloc.drop_maybe_temporary(right_reg);

                    get_return(temp)
                }
                IRLoadBinary::VariableNum(num, var) => {
                    let var_data = alloc.get(var);

                    // The output type can't expect more
                    // data than the input can give it.
                    assert!(output_size.size <= var_data.size);

                    let temp = get_temporary_output(alloc);

                    let left_reg: RegMaybeTemporary<Size> = match var_data.size {
                        1 => alloc.read_1(ctx, var, 0).try_conv().unwrap(),
                        4 => alloc.read_4(ctx, var, 0).try_conv().unwrap(),
                        8 => todo!(),
                        _ => panic!("Unexpected Size in lower_load!"),
                    };

                    let right_num = if num >= &0 && num <= &65535 {
                        format!("#{}", num.to_string())
                    } else {
                        todo!();
                    };

                    lower_op_binary_32(ctx, op, temp.names()[0], left_reg.names()[0], &right_num);

                    alloc.drop_maybe_temporary(left_reg);

                    get_return(temp)
                }
            }
        }
    }
}

/// Sets the given variable to the load operation.
fn lower_set_variable<'a>(
    ctx: &mut Arm32Context,
    alloc: &mut Arm32Allocator<'a, '_>,
    name: &Cow<'a, str>,
    value: &IRLoadOp<'a>,
) {
    let size = alloc.get(name);
    match size.size {
        1 => {
            let output = alloc.read_1_direct(name, 0);

            let load = lower_load(ctx, alloc, value, size, output);

            // If load returns None, it means it wrote
            // directly into our output variable.
            // Otherwise, it gave a temporary that we
            // need to move over to the output.
            if let Some(load) = load {
                alloc.write_1(ctx, &load, name, 0);

                alloc.drop_maybe_temporary(load);
            }
        }
        4 => {
            let output = alloc.read_4_direct(name, 0);

            let load = lower_load(ctx, alloc, value, size, output);

            // If load returns None, it means it wrote
            // directly into our output variable.
            // Otherwise, it gave a temporary that we
            // need to move over to the output.
            if let Some(load) = load {
                alloc.write_4(ctx, &load, name, 0);

                alloc.drop_maybe_temporary(load);
            }
        }
        8 => todo!(),
        _ => panic!("Can only set variables with 1, 4, or 8 bytes!"),
    }
}

/// Converts IRStatement to Arm32.
/// The StackAllocator must have already
/// ran through every statement.
fn lower_statement<'a>(
    ctx: &mut Arm32Context,
    alloc: &mut Arm32Allocator<'a, '_>,
    ir_statement: &IRStatement<'a>,
) {
    // TODO: Handle statics and constants.

    match ir_statement {
        IRStatement::CreateVariable(var) => {
            let ty_info = lower_type(&var.ty);

            alloc.alloc_variable(var.name.clone(), ty_info);
        }
        IRStatement::DropVariable(name) => {
            alloc.drop_variable(name);
        }
        IRStatement::SetVariable { name, value } => {
            lower_set_variable(ctx, alloc, name, value);
        }
        IRStatement::FunctionCall(fn_data) => {
            const ALLOWED_REGS: &'static [&'static str] = &["R0", "R1", "R2", "R3"];

            // Next reg to use for arg passing.
            let mut cur_reg = 0;

            // Used to create temporary stack vars when passing large composites.
            let mut arg_temp_idx = 0;

            // Arguments that need to be placed into registers.
            let mut reg_args = vec![];

            // Arguments that need to be placed into the stack.
            // These are inserted in argument order, but need
            // to be placed in reverse order.
            let mut stack_args = vec![];

            // Variables that need to be dropped after
            // the function call is over.
            let mut stack_temporaries = vec![];

            for arg in &fn_data.args {
                // cur_reg may be > ALLOWED_REGS.len()
                let regs_left = ALLOWED_REGS.len().checked_sub(cur_reg).unwrap_or(0);

                // Register size is 4 bytes, so
                // round up to get the number needed.
                let regs_needed = lower_type(&arg.1).size.div_ceil(4) as usize;

                if regs_needed > regs_left {
                    // Primitive types can be passed
                    // directly on the stack.
                    // However, structs and similar
                    // can only be passed directly on
                    // the stack if they have <= 4
                    // bytes.
                    // If we exceed this amount, we need
                    // to allocate them somewhere in memory,
                    // in this case in an arbitrary stack
                    // location, and pass a pointer to it
                    // as the argument.
                    let ty_info = lower_type(&arg.1);
                    if ty_info.size <= 4 {
                        stack_args.push(arg.clone());
                    } else {
                        match arg.1 {
                            IRType::U32
                            | IRType::Bool
                            | IRType::FunctionPtr(..)
                            | IRType::Ptr(..) => {
                                // Primitive.
                                stack_args.push(arg.clone());
                            }
                            IRType::Named(_) => {
                                // Composite.

                                // This won't conflict with anything, since
                                // the variables are only created within this context.
                                // Nested function calls are split into locals.
                                let temp_name: Cow<'a, str> =
                                    Cow::Owned(format!("$fn_arg_{arg_temp_idx}"));
                                alloc.stack_alloc.create(temp_name.clone(), ty_info);
                                arg_temp_idx += 1;

                                stack_temporaries.push(temp_name.clone());
                                stack_args.push((
                                    IRLoadOp::Unary(IRLoadUnary::Reference(temp_name)),
                                    IRType::Ptr(Box::new(arg.1.clone())),
                                ));
                            }
                        }
                    }
                } else {
                    reg_args.push((arg.clone(), &ALLOWED_REGS[cur_reg..(cur_reg + regs_needed)]));
                    cur_reg += regs_needed;
                }
            }

            // Lock all the registers that we need
            // to use, and which haven't already been used.
            // The ones that have been used already will be
            // offloaded to the stack.
            let need_to_lock = reg_args
                .iter()
                .map(|arg| arg.1)
                .flatten()
                .copied()
                .collect::<Vec<_>>();
            let vars_to_offload = alloc.reg_alloc.take_registers(need_to_lock.clone());

            // Take a byte-by-byte copy of each variable.
            for var in &vars_to_offload {
                let var_ty = alloc.reg_alloc.get(var).unwrap().clone();
                alloc.reg_alloc.drop(var);

                let mut write_start = alloc.stack_alloc.create(var.clone(), var_ty.ty());

                for (reg, size) in var_ty.regs() {
                    ctx.push_instruction("STR".into(), format!("{}, [FP, #-{}]", reg, write_start));
                    write_start += size;
                }
            }

            // Now that the registers have been offloaded,
            // we can lock the rest.
            let final_take_size = alloc.reg_alloc.take_registers(need_to_lock.clone()).len();
            // Ensure that there's nothing left.
            assert_eq!(final_take_size, 0);

            // A list of all the register variables used
            // for argument passing that need to be dropped.
            let mut reg_temporaries = vec![];

            // Create variables for each register, then
            // set them with the correct values.
            for arg in &reg_args {
                // This won't conflict with anything, since
                // the variables are only created within this context.
                // Nested function calls are split into locals.
                let temp_name: Cow<'a, str> = Cow::Owned(format!("$fn_arg_{arg_temp_idx}"));
                alloc.reg_alloc.register_variable(
                    temp_name.clone(),
                    arg.1.iter().cloned(),
                    lower_type(&arg.0.1),
                );
                arg_temp_idx += 1;

                reg_temporaries.push(temp_name.clone());
                lower_set_variable(ctx, alloc, &temp_name, &arg.0.0);
            }

            // TODO: Implement pass by stack.
            //       Make sure padding is added and
            //       args are added in reverse order.
            if stack_args.len() > 0 || stack_temporaries.len() > 0 {
                todo!();
            }

            match &fn_data.source {
                IRFnSource::Direct(name) => {
                    ctx.push_instruction("BL".into(), name.to_string());
                }
                IRFnSource::Indirect(expr) => todo!(),
            }

            // Free used resources.
            for to_drop in stack_temporaries {
                alloc.stack_alloc.drop(&to_drop);
            }

            for to_drop in reg_temporaries {
                alloc.reg_alloc.drop(&to_drop);
            }

            // Restore copied variables back to registers (if possible).
            for var in &vars_to_offload {
                let var_ty = alloc.stack_alloc.get(var).unwrap().clone();

                let Some(reg) = alloc.reg_alloc.try_alloc(var.clone(), var_ty.1.clone()) else {
                    continue;
                };

                // Once we've confirmed a spot on the registers,
                // drop the old stack space.
                alloc.stack_alloc.drop(var);

                let mut read_start = var_ty.0;

                for (reg, size) in reg.regs() {
                    ctx.push_instruction("LDR".into(), format!("{}, [FP, #-{}]", reg, read_start));
                    read_start += size;
                }
            }

            // What has to be implemented:
            // - Offloading registers to the stack.
            // - Allocating specific registers for args.
            // - Ensuring that a BLX lives in a register.
            // - Restoring variables after the call.
        }
        IRStatement::Label { name } => {
            // Label names are unique.
            ctx.push_label(format!(".{}", name));
        }
        IRStatement::Goto { name } => ctx.push_instruction("B".into(), format!(".{}", name)),
        IRStatement::GotoNotEqual { name, condition } => {
            let load = lower_load::<1>(ctx, alloc, condition, lower_type(&IRType::Bool), None)
                .expect("lower_load returned None for GotoNotEqual!");
            let reg_name = load.names()[0];

            ctx.push_instruction("CMP".into(), format!("{reg_name}, #1"));
            ctx.push_instruction("BNE".into(), format!(".{}", name));

            alloc.drop_maybe_temporary(load);
        }
    }
}
