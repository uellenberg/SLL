use crate::mir::{MIRStatement, MIRVariable};
use std::collections::HashMap;

/// A scope containing currently
/// available variables.
#[derive(Debug, Default, Clone)]
pub struct Scope<'a> {
    /// The variables currently in scope.
    /// If shadowing occurs, a variable will
    /// be overridden.
    pub variables: HashMap<&'a str, MIRVariable<'a>>,

    /// The variables that will be dropped at the end
    /// of this scope.
    /// This is in reverse drop order (i.e., the first
    /// item should be dropped last).
    /// If shadowing occurs, a variable will
    /// appear twice.
    /// This means deduplication MUST happen
    /// before drop analysis.
    pub to_drop: Vec<MIRVariable<'a>>,
}

/// Explores every statement in a block,
/// running the specified function alongside
/// a scope representing which variables
/// are available.
/// This also runs on_scope_drop for every variable
/// that is AUTOMATICALLY DROPPED.
/// Note that this does not include manual
/// drops.
/// However, manual drops are taken into consideration
/// in the scope.
///
/// The scope is updated AFTER the for_each, not before it.
///
/// Returns whether exploration was successful.
/// Both functions return whether they were successful,
/// and will halt exploration if either returns false.
pub fn explore_block<'a>(
    block: &[MIRStatement<'a>],
    for_each: &impl Fn(&MIRStatement<'a>, &Scope<'a>) -> bool,
    on_scope_drop: &impl Fn(&MIRVariable<'a>, &Scope) -> bool,
) -> bool {
    explore_block_internal(block, for_each, on_scope_drop, &Scope::default())
}

fn explore_block_internal<'a>(
    block: &[MIRStatement<'a>],
    for_each: &impl Fn(&MIRStatement<'a>, &Scope<'a>) -> bool,
    on_scope_drop: &impl Fn(&MIRVariable<'a>, &Scope) -> bool,
    parent_scope: &Scope<'a>,
) -> bool {
    let mut scope = parent_scope.clone();

    for statement in block {
        if !for_each(statement, &scope) {
            return false;
        }

        match statement {
            // Doesn't create / drop variables.
            MIRStatement::SetVariable { .. } => {}
            MIRStatement::CreateVariable(var, ..) => {
                scope.variables.insert(var.name, var.clone());
                scope.to_drop.push(var.clone());
            }
            MIRStatement::DropVariable(name, ..) => {
                // Manual drops don't invoke on_scope_drop.

                if scope.variables.remove(name).is_none() {
                    eprintln!("Failed to drop {name}: variable does not exist!");
                    return false;
                }
            }
        }
    }

    // Drop at the end of scope.
    // This needs to be in reverse
    // order because the to_drop list
    // is constructed in reverse drop
    // order.
    for var in scope.to_drop.iter().rev() {
        on_scope_drop(var, &scope);
    }

    true
}
