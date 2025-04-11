use crate::parser::parse_string;
use std::fs;

mod ir;
mod mir;
mod parser;

fn main() {
    let content = fs::read_to_string("./test/simple.sll").unwrap();
    println!("{:?}", parse_string(&content));
    println!("Hello, world!");
}
