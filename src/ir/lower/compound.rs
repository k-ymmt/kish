//! Compound-command lowering from parser AST to HIR.

use crate::ir::error::IrError;
use crate::ir::hir::{HirCompoundCommand, HirCompoundCommandNode};
use crate::ir::hir_builder::HirBuilder;
use crate::parser::ast::{CompoundCommandAst, CompoundCommandNodeAst};

use super::control_flow::{
    lower_case_clause, lower_for_clause, lower_if_clause, lower_until_clause, lower_while_clause,
};
use super::from_parser::lower_list;
use super::simple_command::lower_redirects;

/// Lowers a compound command AST variant to HIR.
pub(crate) fn lower_compound_command(
    builder: &HirBuilder,
    command: &CompoundCommandAst,
) -> Result<HirCompoundCommand, IrError> {
    match command {
        CompoundCommandAst::Subshell(list) => {
            let hir_list = lower_list(builder, list)?;
            Ok(builder.compound_subshell(hir_list))
        }
        CompoundCommandAst::BraceGroup(list) => {
            let hir_list = lower_list(builder, list)?;
            Ok(builder.compound_brace_group(hir_list))
        }
        CompoundCommandAst::If(clause) => {
            let hir_clause = lower_if_clause(builder, clause)?;
            Ok(builder.compound_if(hir_clause))
        }
        CompoundCommandAst::For(clause) => {
            let hir_clause = lower_for_clause(builder, clause)?;
            Ok(builder.compound_for(hir_clause))
        }
        CompoundCommandAst::While(clause) => {
            let hir_clause = lower_while_clause(builder, clause)?;
            Ok(builder.compound_while(hir_clause))
        }
        CompoundCommandAst::Until(clause) => {
            let hir_clause = lower_until_clause(builder, clause)?;
            Ok(builder.compound_until(hir_clause))
        }
        CompoundCommandAst::Case(clause) => {
            let hir_clause = lower_case_clause(builder, clause)?;
            Ok(builder.compound_case(hir_clause))
        }
    }
}

/// Lowers a compound command node AST (command + redirects) to HIR.
pub(crate) fn lower_compound_command_node(
    builder: &HirBuilder,
    node: &CompoundCommandNodeAst,
) -> Result<HirCompoundCommandNode, IrError> {
    let kind = lower_compound_command(builder, &node.kind)?;
    let redirects = lower_redirects(builder, &node.redirects)?;
    builder.compound_command_node(kind, redirects, node.span)
}
