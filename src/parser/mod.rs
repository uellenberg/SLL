pub mod file_cache;

use crate::mir::{
    MIRConstant, MIRExpression, MIRFunction, MIRProgram, MIRStatement, MIRStatic, MIRType,
    MIRVariable, Span, to_span,
};
use crate::parser::file_cache::FileCache;
use ariadne::Cache;
use pest::Parser;
use pest::error::Error;
use pest::iterators::Pair;
use pest_derive::Parser;
use std::path::Path;

#[derive(Parser)]
#[grammar = "parser/program.pest"]
struct SLLParser;

/// Parses a file into MIR.
pub fn parse_file<'a>(
    location: &'a Path,
    program: &mut MIRProgram<'a>,
    cache: &FileCache,
) -> Result<(), Error<Rule>> {
    let data = cache.get(location).unwrap();

    parse_data(location, data, program, cache)?;

    Ok(())
}

/// Parses some data file into MIR.
fn parse_data<'a>(
    location: &'a Path,
    data: &'a str,
    program: &mut MIRProgram<'a>,
    cache: &FileCache,
) -> Result<(), Error<Rule>> {
    let ast = SLLParser::parse(Rule::program, data)?;

    for pair in ast {
        match pair.as_rule() {
            Rule::constDeclaration => {
                let constant = parse_constant(location, pair);
                verify_no_duplicates(&program, &cache, constant.name, &constant.span);

                program.constants.insert(constant.name, constant);
            }
            Rule::staticDeclaration => {
                let static_data = parse_static(location, pair);
                verify_no_duplicates(&program, &cache, static_data.name, &static_data.span);

                program.statics.insert(static_data.name, static_data);
            }
            Rule::functionDeclaration => {
                let function_data = parse_function(location, pair);
                verify_no_duplicates(&program, &cache, function_data.name, &function_data.span);

                program.functions.insert(function_data.name, function_data);
            }
            Rule::EOI => {}
            _ => unreachable!(),
        }
    }

    Ok(())
}

fn verify_no_duplicates<'a>(
    program: &MIRProgram<'a>,
    cache: &FileCache,
    name: &'a str,
    span: &Span<'a>,
) {
    let defined_span;
    if let Some(var) = program.statics.get(name) {
        defined_span = var.span.clone();
    } else if let Some(var) = program.constants.get(name) {
        defined_span = var.span.clone();
    } else if let Some(var) = program.functions.get(name) {
        defined_span = var.span.clone();
    } else {
        // No duplicates.
        return;
    }

    use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};

    let mut colors = ColorGenerator::new();

    let prev = colors.next();
    let cur = colors.next();

    Report::build(ReportKind::Error, span.clone())
        .with_message("Duplicate identifier".to_string())
        .with_label(
            Label::new(defined_span)
                .with_message(format!("Item with name {name} previously defined here"))
                .with_color(prev),
        )
        .with_label(
            Label::new(span.clone())
                .with_message("Redeclaration here".to_string())
                .with_color(cur),
        )
        .finish()
        // TODO: How to avoid this clone?
        .eprint(cache.clone())
        .unwrap();

    // TODO: Remove this somehow.
    panic!();
}

fn parse_static<'a>(location: &'a Path, value: Pair<'a, Rule>) -> MIRStatic<'a> {
    assert_eq!(value.as_rule(), Rule::staticDeclaration);

    let span = to_span(location, value.as_span());
    let mut data = value.into_inner();

    let identifier = data.next().unwrap().as_str();
    let ty = parse_type(data.next().unwrap());
    let expr = parse_expression(location, data.next().unwrap());

    MIRStatic {
        name: identifier,
        ty,
        value: expr,
        span,
    }
}

fn parse_constant<'a>(location: &'a Path, value: Pair<'a, Rule>) -> MIRConstant<'a> {
    assert_eq!(value.as_rule(), Rule::constDeclaration);

    let span = to_span(location, value.as_span());
    let mut data = value.into_inner();

    let identifier = data.next().unwrap().as_str();
    let ty = parse_type(data.next().unwrap());
    let expr = parse_expression(location, data.next().unwrap());

    MIRConstant {
        name: identifier,
        ty,
        value: expr,
        span,
    }
}

fn parse_function<'a>(location: &'a Path, value: Pair<'a, Rule>) -> MIRFunction<'a> {
    assert_eq!(value.as_rule(), Rule::functionDeclaration);

    let span = to_span(location, value.as_span());
    let mut data = value.into_inner();

    let identifier = data.next().unwrap().as_str();
    let mut args = vec![];
    let mut ret = MIRType::Unit;

    for pair in data {
        match pair.as_rule() {
            Rule::functionArgs => {
                args = parse_function_args(location, pair);
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
                    body: parse_function_body(location, pair),
                    span,
                };
            }
            _ => unreachable!(),
        }
    }

    // No function body.
    unreachable!();
}

fn parse_function_body<'a>(location: &'a Path, value: Pair<'a, Rule>) -> Vec<MIRStatement<'a>> {
    assert_eq!(value.as_rule(), Rule::functionBody);

    let mut body = vec![];

    for pair in value.into_inner() {
        let span = to_span(location, pair.as_span());

        match pair.as_rule() {
            Rule::createVariable => {
                let mut data = pair.into_inner();

                let identifier = data.next().unwrap().as_str();
                let ty = parse_type(data.next().unwrap());

                body.push(MIRStatement::CreateVariable(
                    MIRVariable {
                        name: identifier,
                        ty,
                        span: span.clone(),
                    },
                    span,
                ));
            }
            Rule::setVariable => {
                let mut data = pair.into_inner();

                let identifier = data.next().unwrap().as_str();
                let value = parse_expression(location, data.next().unwrap());

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

fn parse_function_args<'a>(location: &'a Path, value: Pair<'a, Rule>) -> Vec<MIRVariable<'a>> {
    assert_eq!(value.as_rule(), Rule::functionArgs);

    let mut args = vec![];

    for pair in value.into_inner() {
        let span = to_span(location, pair.as_span());

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

fn parse_expression<'a>(location: &'a Path, value: Pair<'a, Rule>) -> MIRExpression<'a> {
    assert_eq!(value.as_rule(), Rule::expression);

    parse_addition(location, value.into_inner().next().unwrap())
}

fn parse_addition<'a>(location: &'a Path, value: Pair<'a, Rule>) -> MIRExpression<'a> {
    assert_eq!(value.as_rule(), Rule::addition);

    let span = to_span(location, value.as_span());
    let mut data = value.into_inner();

    let mul = parse_multiplication(location, data.next().unwrap());

    let Some(op) = data.next() else {
        return mul;
    };

    let add = parse_addition(location, data.next().unwrap());

    match op.as_str() {
        "+" => MIRExpression::Add(Box::new(mul), Box::new(add), span),
        "-" => MIRExpression::Sub(Box::new(mul), Box::new(add), span),
        _ => unreachable!(),
    }
}

fn parse_multiplication<'a>(location: &'a Path, value: Pair<'a, Rule>) -> MIRExpression<'a> {
    assert_eq!(value.as_rule(), Rule::multiplication);

    let span = to_span(location, value.as_span());
    let mut data = value.into_inner();

    let pri = parse_primary(location, data.next().unwrap());

    let Some(op) = data.next() else {
        return pri;
    };

    let mul = parse_multiplication(location, data.next().unwrap());

    match op.as_str() {
        "*" => MIRExpression::Mul(Box::new(pri), Box::new(mul), span),
        "/" => MIRExpression::Div(Box::new(pri), Box::new(mul), span),
        _ => unreachable!(),
    }
}

fn parse_primary<'a>(location: &'a Path, value: Pair<'a, Rule>) -> MIRExpression<'a> {
    assert_eq!(value.as_rule(), Rule::primary);

    let span = to_span(location, value.as_span());
    let data = value.into_inner().next().unwrap();

    match data.as_rule() {
        Rule::number => MIRExpression::Number(parse_number(data), span),
        Rule::identifier => MIRExpression::Variable(data.as_str(), span),
        Rule::expression => parse_expression(location, data),
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
