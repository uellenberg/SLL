use crate::mir::lower::mir_to_ir;
use crate::mir::visit_mir;
use crate::parser::parse_string;
use std::fs;

mod ir;
mod mir;
mod parser;

fn main() {
    let content = fs::read_to_string("./test/simple.sll").unwrap();

    let mut mir = parse_string(&content).unwrap();
    if !visit_mir(&mut mir) {
        return;
    }

    let ir = mir_to_ir(&mir);

    println!("{ir:?}");
}
