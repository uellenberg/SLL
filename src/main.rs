#![feature(box_patterns)]

use crate::ir::arm32::ir_to_arm32;
use crate::mir::lower::mir_to_ir;
use crate::mir::{MIRContext, visit_mir};
use crate::parser::parse_file;

mod ir;
mod mir;
mod parser;

fn main() {
    let mut mir_ctx = MIRContext::default();

    if !parse_file("./test/if.sll".as_ref(), &mut mir_ctx) {
        return;
    }

    if !visit_mir(&mut mir_ctx) {
        return;
    }

    let ir = mir_to_ir(&mir_ctx.program);
    println!("{ir:#?}");

    let asm = ir_to_arm32(&ir);

    println!("{asm}");
}
