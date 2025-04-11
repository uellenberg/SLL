use crate::mir::lower::mir_to_ir;
use crate::mir::{MIRProgram, visit_mir};
use crate::parser::parse_file;
use ariadne::FileCache;

mod ir;
mod mir;
mod parser;

fn main() {
    let mut mir = MIRProgram::default();
    let mut cache = FileCache::default();

    parse_file("./test/simple.sll".as_ref(), &mut mir, &mut cache).unwrap();
    if !visit_mir(&mut mir) {
        return;
    }

    let ir = mir_to_ir(&mir);

    println!("{ir:?}");
}
