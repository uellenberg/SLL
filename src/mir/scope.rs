use crate::mir::{MIRStatement, MIRVariable};
use std::borrow::Cow;
use std::collections::HashMap;
use std::mem::swap;

/// A scope containing currently
/// available variables.
#[derive(Debug, Default, Clone)]
pub struct Scope<'a> {
    /// The variables currently in scope.
    /// If shadowing occurs, a variable will
    /// be overridden.
    pub variables: HashMap<Cow<'a, str>, MIRVariable<'a>>,

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

macro_rules! explore_recurse {
    ($statement:expr, ($list:ident) => $recurse:block) => {
        match $statement {
            // Doesn't include sub blocks.
            MIRStatement::SetVariable { .. } => {}
            MIRStatement::Goto { .. } => {}
            MIRStatement::Label { .. } => {}
            MIRStatement::CreateVariable(..) => {}
            MIRStatement::DropVariable(..) => {}

            MIRStatement::IfStatement {
                on_true, on_false, ..
            } => {
                let $list = on_true;
                {
                    $recurse
                }

                let $list = on_false;
                { $recurse }
            }
        }
    };
}

fn explore_block_internal<'a>(
    block: &[MIRStatement<'a>],
    for_each: &impl Fn(&MIRStatement<'a>, &Scope<'a>) -> bool,
    on_scope_drop: &impl Fn(&MIRVariable<'a>, &Scope) -> bool,
    parent_scope: &Scope<'a>,
) -> bool {
    let mut scope = parent_scope.clone();

    for statement in block {
        explore_recurse!(statement, (block) => {
            explore_block_internal(block, for_each, on_scope_drop, &scope);
        });

        if !for_each(statement, &scope) {
            return false;
        }

        if !explore_block_handle_scope(statement, &mut scope) {
            return false;
        }
    }

    explore_block_handle_drop(&scope, on_scope_drop);

    true
}

fn explore_block_handle_scope<'a>(statement: &MIRStatement<'a>, scope: &mut Scope<'a>) -> bool {
    match statement {
        // Doesn't create / drop variables.
        MIRStatement::SetVariable { .. } => {}
        MIRStatement::IfStatement { .. } => {}
        MIRStatement::Goto { .. } => {}
        MIRStatement::Label { .. } => {}

        MIRStatement::CreateVariable(var, ..) => {
            scope.variables.insert(var.name.clone(), var.clone());
            scope.to_drop.push(var.clone());
        }
        MIRStatement::DropVariable(name, ..) => {
            // Manual drops don't invoke on_scope_drop.

            if scope.variables.remove(name).is_none() {
                eprintln!("Failed to drop {name}: variable does not exist!");
                return false;
            }

            // We already dropped it, so no
            // need to do it automatically.
            scope.to_drop.retain(|var| &var.name != name);
        }
    }

    true
}

fn explore_block_handle_drop<'a>(
    scope: &Scope<'a>,
    on_scope_drop: &impl Fn(&MIRVariable<'a>, &Scope) -> bool,
) {
    // Drop at the end of scope.
    // This needs to be in reverse
    // order because the to_drop list
    // is constructed in reverse drop
    // order.
    for var in scope.to_drop.iter().rev() {
        on_scope_drop(var, &scope);
    }
}

/// This is a version of explore_block
/// that allows modifying each statement.
pub fn explore_block_mut<'a>(
    block: &mut [MIRStatement<'a>],
    for_each: &impl Fn(&mut MIRStatement<'a>, &Scope<'a>) -> bool,
    on_scope_drop: &impl Fn(&MIRVariable<'a>, &Scope) -> bool,
) -> bool {
    explore_block_mut_internal(block, for_each, on_scope_drop, &Scope::default())
}

fn explore_block_mut_internal<'a>(
    block: &mut [MIRStatement<'a>],
    for_each: &impl Fn(&mut MIRStatement<'a>, &Scope<'a>) -> bool,
    on_scope_drop: &impl Fn(&MIRVariable<'a>, &Scope) -> bool,
    parent_scope: &Scope<'a>,
) -> bool {
    let mut scope = parent_scope.clone();

    for statement in block {
        explore_recurse!(statement, (block) => {
            explore_block_mut_internal(block, for_each, on_scope_drop, &scope);
        });

        if !for_each(statement, &scope) {
            return false;
        }

        if !explore_block_handle_scope(statement, &mut scope) {
            return false;
        }
    }

    explore_block_handle_drop(&scope, on_scope_drop);

    true
}

/// Rewrites a block, allowing for arbitrary
/// insertion before and after each statement.
/// The push function must be used inside for_each,
/// or else all statements will be deleted.
///
/// This includes a scope representing which variables
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
/// Returns whether rewriting was successful.
/// Both functions return whether they were successful,
/// and will halt exploration if either returns false.
pub fn rewrite_block<'a>(
    block: &mut Vec<MIRStatement<'a>>,
    for_each: &mut impl FnMut(MIRStatement<'a>, &Scope<'a>, &mut Vec<MIRStatement<'a>>) -> bool,
    on_scope_end: &mut impl FnMut(&Scope<'a>, &mut Vec<MIRStatement<'a>>) -> bool,
) -> bool {
    rewrite_block_internal(block, for_each, on_scope_end, &Scope::default())
}

fn rewrite_block_internal<'a>(
    block: &mut Vec<MIRStatement<'a>>,
    for_each: &mut impl FnMut(MIRStatement<'a>, &Scope<'a>, &mut Vec<MIRStatement<'a>>) -> bool,
    on_scope_end: &mut impl FnMut(&Scope<'a>, &mut Vec<MIRStatement<'a>>) -> bool,
    parent_scope: &Scope<'a>,
) -> bool {
    let mut scope = parent_scope.clone();

    let mut old_block = vec![];
    swap(block, &mut old_block);

    for mut statement in old_block {
        explore_recurse!(&mut statement, (block) => {
            rewrite_block_internal(block, for_each, on_scope_end, &scope);
        });

        if !for_each(statement.clone(), &scope, block) {
            return false;
        }

        if !explore_block_handle_scope(&statement, &mut scope) {
            return false;
        }
    }

    if !on_scope_end(&scope, block) {
        return false;
    }

    true
}
