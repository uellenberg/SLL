use crate::mir::{
    MIRConstant, MIRExpression, MIRFunction, MIRProgram, MIRStatement, MIRStatic, MIRType,
    MIRVariable,
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

    Ok(output)
}

fn verify_no_duplicates<'a>(program: &MIRProgram<'a>, name: &'a str) {
    if !program.statics.contains_key(name)
        && !program.constants.contains_key(name)
        && !program.functions.contains_key(name)
    {
        return;
    }

    panic!("Duplicate identifier: {}", name);
}

fn parse_static(value: Pair<'_, Rule>) -> MIRStatic<'_> {
    assert_eq!(value.as_rule(), Rule::staticDeclaration);

    let span = value.as_span();
    let mut data = value.into_inner();

    let identifier = data.next().unwrap().as_str();
    let ty = parse_type(data.next().unwrap());
    let expr = parse_expression(data.next().unwrap());

    MIRStatic {
        name: identifier,
        ty,
        value: expr,
        span,
    }
}

fn parse_constant(value: Pair<'_, Rule>) -> MIRConstant<'_> {
    assert_eq!(value.as_rule(), Rule::constDeclaration);

    let span = value.as_span();
    let mut data = value.into_inner();

    let identifier = data.next().unwrap().as_str();
    let ty = parse_type(data.next().unwrap());
    let expr = parse_expression(data.next().unwrap());

    MIRConstant {
        name: identifier,
        ty,
        value: expr,
        span,
    }
}

fn parse_function(value: Pair<'_, Rule>) -> MIRFunction<'_> {
    assert_eq!(value.as_rule(), Rule::functionDeclaration);

    let span = value.as_span();
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
                    span,
                };
            }
            _ => unreachable!(),
        }
    }

    // No function body.
    unreachable!();
}

fn parse_function_body(value: Pair<'_, Rule>) -> Vec<MIRStatement<'_>> {
    assert_eq!(value.as_rule(), Rule::functionBody);

    let mut body = vec![];

    for pair in value.into_inner() {
        let span = pair.as_span();

        match pair.as_rule() {
            Rule::createVariable => {
                let mut data = pair.into_inner();

                let identifier = data.next().unwrap().as_str();
                let ty = parse_type(data.next().unwrap());

                body.push(MIRStatement::CreateVariable(
                    MIRVariable {
                        name: identifier,
                        ty,
                        span,
                    },
                    span,
                ));
            }
            Rule::setVariable => {
                let mut data = pair.into_inner();

                let identifier = data.next().unwrap().as_str();
                let value = parse_expression(data.next().unwrap());

                body.push(MIRStatement::SetVariable {
                    name: identifier,
                    value,
                    span,
                });
            }
            _ => unreachable!(),
        }
    }

    body
}

fn parse_function_args(value: Pair<'_, Rule>) -> Vec<MIRVariable<'_>> {
    assert_eq!(value.as_rule(), Rule::functionArgs);

    let mut args = vec![];

    for pair in value.into_inner() {
        let span = pair.as_span();

        match pair.as_rule() {
            Rule::functionArgs => {
                let mut data = pair.into_inner();

                let identifier = data.next().unwrap().as_str();
                let ty = parse_type(data.next().unwrap());

                args.push(MIRVariable {
                    name: identifier,
                    ty,
                    span,
                });
            }
            _ => unreachable!(),
        }
    }

    args
}

fn parse_expression(value: Pair<'_, Rule>) -> MIRExpression<'_> {
    assert_eq!(value.as_rule(), Rule::expression);

    parse_addition(value.into_inner().next().unwrap())
}

fn parse_addition(value: Pair<'_, Rule>) -> MIRExpression<'_> {
    assert_eq!(value.as_rule(), Rule::addition);

    let span = value.as_span();
    let mut data = value.into_inner();

    let mul = parse_multiplication(data.next().unwrap());

    let Some(op) = data.next() else {
        return mul;
    };

    let add = parse_addition(data.next().unwrap());

    match op.as_str() {
        "+" => MIRExpression::Add(Box::new(mul), Box::new(add), span),
        "-" => MIRExpression::Sub(Box::new(mul), Box::new(add), span),
        _ => unreachable!(),
    }
}

fn parse_multiplication(value: Pair<'_, Rule>) -> MIRExpression<'_> {
    assert_eq!(value.as_rule(), Rule::multiplication);

    let span = value.as_span();
    let mut data = value.into_inner();

    let pri = parse_primary(data.next().unwrap());

    let Some(op) = data.next() else {
        return pri;
    };

    let mul = parse_multiplication(data.next().unwrap());

    match op.as_str() {
        "*" => MIRExpression::Mul(Box::new(pri), Box::new(mul), span),
        "/" => MIRExpression::Div(Box::new(pri), Box::new(mul), span),
        _ => unreachable!(),
    }
}

fn parse_primary(value: Pair<'_, Rule>) -> MIRExpression<'_> {
    assert_eq!(value.as_rule(), Rule::primary);

    let span = value.as_span();
    let data = value.into_inner().next().unwrap();

    match data.as_rule() {
        Rule::number => MIRExpression::Number(parse_number(data), span),
        Rule::identifier => MIRExpression::Variable(data.as_str(), span),
        Rule::expression => parse_expression(data),
        _ => unreachable!(),
    }
}

fn parse_type(value: Pair<'_, Rule>) -> MIRType {
    assert_eq!(value.as_rule(), Rule::variableType);

    match value.as_str() {
        "u32" => MIRType::U32,
        "()" => MIRType::Unit,
        _ => unreachable!(),
    }
}

fn parse_number(value: Pair<'_, Rule>) -> i64 {
    assert_eq!(value.as_rule(), Rule::number);

    value.as_str().parse::<i64>().unwrap()
}
