#![feature(box_patterns)]
#![feature(iter_intersperse)]

use crate::ir::arm32::ir_to_arm32;
use crate::mir::lower::mir_to_ir;
use crate::mir::{MIRContext, visit_mir};
use crate::parser::parse_file;

mod ir;
mod mir;
mod parser;

fn main() {
    let mut mir_ctx = MIRContext::default();

    if !parse_file("./test/simple.sll".as_ref(), &mut mir_ctx) {
        return;
    }

    println!("{}", mir_ctx.program);

    if !visit_mir(&mut mir_ctx) {
        return;
    }

    println!("{:#}", mir_ctx.program);

    let ir = mir_to_ir(&mir_ctx.program);
    println!("{ir}");

    let asm = ir_to_arm32(&ir);

    println!("{asm}");
}
