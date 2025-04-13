use crate::mir::scope::rewrite_block;
use crate::mir::{MIRContext, MIRStatement};
use std::borrow::Cow;
use std::sync::atomic::{AtomicU32, Ordering};

/// Converts every if statement into
/// labels and gotos.
pub fn flatten_ifs(ctx: &mut MIRContext) {
    for function in ctx.program.functions.values_mut() {
        let label_idx = AtomicU32::new(0);

        if !rewrite_block(
            &mut function.body,
            &mut |statement, scope, block| {
                match statement {
                    MIRStatement::IfStatement {
                        condition,
                        mut on_true,
                        mut on_false,
                        span,
                    } => {
                        let label_if_end =
                            format!("$if_{}", { label_idx.fetch_add(1, Ordering::Relaxed) });

                        // Optimized output for no else branch.
                        if on_false.len() == 0 {
                            block.push(MIRStatement::GotoNotEqual {
                                name: Cow::Owned(label_if_end.clone()),
                                condition,
                                span: span.clone(),
                            });

                            block.append(&mut on_true);

                            block.push(MIRStatement::Label {
                                name: Cow::Owned(label_if_end.clone()),
                                span: span.clone(),
                            });
                        } else {
                            let label_else = format!("{}_else", &label_if_end);

                            block.push(MIRStatement::GotoNotEqual {
                                name: Cow::Owned(label_else.clone()),
                                condition,
                                span: span.clone(),
                            });

                            block.append(&mut on_true);

                            block.push(MIRStatement::Goto {
                                name: Cow::Owned(label_if_end.clone()),
                                span: span.clone(),
                            });

                            block.push(MIRStatement::Label {
                                name: Cow::Owned(label_else.clone()),
                                span: span.clone(),
                            });

                            block.append(&mut on_false);

                            block.push(MIRStatement::Label {
                                name: Cow::Owned(label_if_end.clone()),
                                span: span.clone(),
                            });
                        }

                        true
                    }
                    // We're only dealing with if statements.
                    _ => {
                        block.push(statement);
                        true
                    }
                }
            },
            &mut |_, _| true,
        ) {
            panic!("split_exprs_to_locals returned false!");
        }
    }
}
