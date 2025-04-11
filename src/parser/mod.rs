use crate::mir::{
    MIRConstant, MIRFunction, MIRProgram, MIRStatement, MIRStatic, MIRType, MIRVariable,
};
use pest::Parser;
use pest::error::Error;
use pest::iterators::Pair;
use pest_derive::Parser;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "parser/program.pest"]
struct SLLParser;

/// Parses a file into MIR.
pub fn parse_string(content: &str) -> Result<MIRProgram, Error<Rule>> {
    let ast = SLLParser::parse(Rule::program, content)?;

    let mut output = MIRProgram {
        constants: HashMap::new(),
        statics: HashMap::new(),
        functions: HashMap::new(),
    };

    for pair in ast {
        match pair.as_rule() {
            Rule::constDeclaration => {
                let constant = parse_constant(pair);
                verify_no_duplicates(&output, constant.name);

                output.constants.insert(constant.name, constant);
            }
            Rule::staticDeclaration => {
                let static_data = parse_static(pair);
                verify_no_duplicates(&output, static_data.name);

                output.statics.insert(static_data.name, static_data);
            }
            Rule::functionDeclaration => {
                let function_data = parse_function(pair);
                verify_no_duplicates(&output, function_data.name);

                output.functions.insert(function_data.name, function_data);
            }
            Rule::EOI => {}
            _ => unreachable!(),
        }
    }

    todo!()
}

fn verify_no_duplicates(program: &MIRProgram, name: &str) {
    if !program.statics.contains_key(name)
        && !program.constants.contains_key(name)
        && !program.functions.contains_key(name)
    {
        return;
    }

    panic!("Duplicate identifier: {}", name);
}

fn parse_static(value: Pair<Rule>) -> MIRStatic {
    assert_eq!(value.as_rule(), Rule::staticDeclaration);

    let mut data = value.into_inner();

    let identifier = data.next().unwrap().as_str();
    let ty = parse_type(data.next().unwrap());
    let number = parse_number(data.next().unwrap());

    MIRStatic {
        name: identifier,
        ty,
        value: number,
    }
}

fn parse_constant(value: Pair<Rule>) -> MIRConstant {
    assert_eq!(value.as_rule(), Rule::constDeclaration);

    let mut data = value.into_inner();

    let identifier = data.next().unwrap().as_str();
    let ty = parse_type(data.next().unwrap());
    let number = parse_number(data.next().unwrap());

    MIRConstant {
        name: identifier,
        ty,
        value: number,
    }
}

fn parse_function(value: Pair<Rule>) -> MIRFunction {
    assert_eq!(value.as_rule(), Rule::functionDeclaration);

    let mut data = value.into_inner();

    let identifier = data.next().unwrap().as_str();
    let mut args = vec![];
    let mut ret = MIRType::Unit;

    for pair in data {
        match pair.as_rule() {
            Rule::functionArgs => {
                args = parse_function_args(pair);
            }
            Rule::functionReturn => {
                // functionReturn([type])
                ret = parse_type(pair.into_inner().next().unwrap());
            }
            Rule::functionBody => {
                // Function body is the last item.
                return MIRFunction {
                    name: identifier,
                    args,
                    ret_ty: ret,
                    body: parse_function_body(pair),
                };
            }
            _ => unreachable!(),
        }
    }

    // No function body.
    unreachable!();
}

fn parse_function_body(value: Pair<Rule>) -> Vec<MIRStatement> {
    assert_eq!(value.as_rule(), Rule::functionBody);

    let mut body = vec![];

    for pair in value.into_inner() {
        match pair.as_rule() {
            Rule::createVariable => {
                let mut data = pair.into_inner();

                let identifier = data.next().unwrap().as_str();
                let ty = parse_type(data.next().unwrap());

                body.push(MIRStatement::CreateVariable(MIRVariable {
                    name: identifier,
                    ty,
                }));
            }
            Rule::setVariable => {
                let mut data = pair.into_inner();

                let identifier = data.next().unwrap().as_str();
                let value = parse_number(data.next().unwrap());

                body.push(MIRStatement::SetVariable {
                    name: identifier,
                    value,
                });
            }
            _ => unreachable!(),
        }
    }

    body
}

fn parse_function_args(value: Pair<Rule>) -> Vec<MIRVariable> {
    assert_eq!(value.as_rule(), Rule::functionArgs);

    let mut args = vec![];

    for pair in value.into_inner() {
        match pair.as_rule() {
            Rule::functionArgs => {
                let mut data = pair.into_inner();

                let identifier = data.next().unwrap().as_str();
                let ty = parse_type(data.next().unwrap());

                args.push(MIRVariable {
                    name: identifier,
                    ty,
                });
            }
            _ => unreachable!(),
        }
    }

    args
}

fn parse_type(value: Pair<Rule>) -> MIRType {
    assert_eq!(value.as_rule(), Rule::variableType);

    match value.as_str() {
        "u32" => MIRType::U32,
        "()" => MIRType::Unit,
        _ => unreachable!(),
    }
}

fn parse_number(value: Pair<Rule>) -> i64 {
    assert_eq!(value.as_rule(), Rule::number);

    value.as_str().parse::<i64>().unwrap()
}
