use crate::mir::lower::mir_to_ir;
use crate::parser::parse_string;
use std::fs;

mod ir;
mod mir;
mod parser;

fn main() {
    let content = fs::read_to_string("./test/simple.sll").unwrap();
    println!("{:?}", mir_to_ir(&parse_string(&content).unwrap()));
    println!("Hello, world!");
}
