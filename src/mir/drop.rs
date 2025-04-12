use crate::mir::scope::rewrite_block;
use crate::mir::{MIRContext, MIRStatement};

/// Adds variable drops at the end of
/// every scope in every function.
pub fn drop_at_scope_end(ctx: &mut MIRContext<'_>) {
    for function in ctx.program.functions.values_mut() {
        if !rewrite_block(
            &mut function.body,
            &mut |statement, _scope, block| {
                block.push(statement);
                true
            },
            &mut |scope, block| {
                // Drops occur in reverse order
                // from how they're recorded.
                for var in scope.to_drop.iter().rev() {
                    block.push(MIRStatement::DropVariable(
                        var.name.clone(),
                        var.span.clone(),
                    ));
                }

                true
            },
        ) {
            panic!("drop_at_scope_end returned false!");
        }
    }
}
