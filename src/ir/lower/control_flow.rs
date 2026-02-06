//! Control-flow lowering from parser AST to HIR.

use crate::ir::error::IrError;
use crate::ir::hir::{
    HirCaseClause, HirCaseItem, HirForClause, HirIfClause, HirUntilClause, HirWhileClause,
};
use crate::ir::hir_builder::HirBuilder;
use crate::parser::ast::{
    CaseClauseAst, CaseItemAst, ForClauseAst, IfClauseAst, UntilClauseAst, WhileClauseAst,
};

use super::from_parser::lower_list;
use super::simple_command::{lower_word, lower_words};

/// Lowers an if-clause AST node to HIR.
pub(crate) fn lower_if_clause(
    builder: &HirBuilder,
    clause: &IfClauseAst,
) -> Result<HirIfClause, IrError> {
    let condition = lower_list(builder, &clause.condition)?;
    let then_body = lower_list(builder, &clause.then_body)?;

    let mut elif_arms = Vec::with_capacity(clause.elif_arms.len());
    for (elif_cond, elif_body) in &clause.elif_arms {
        let cond = lower_list(builder, elif_cond)?;
        let body = lower_list(builder, elif_body)?;
        elif_arms.push((cond, body));
    }

    let else_body = clause
        .else_body
        .as_ref()
        .map(|body| lower_list(builder, body))
        .transpose()?;

    builder.if_clause(condition, then_body, elif_arms, else_body, clause.span)
}

/// Lowers a for-clause AST node to HIR.
pub(crate) fn lower_for_clause(
    builder: &HirBuilder,
    clause: &ForClauseAst,
) -> Result<HirForClause, IrError> {
    let name = lower_word(builder, &clause.name)?;
    let words = lower_words(builder, &clause.words)?;
    let body = lower_list(builder, &clause.body)?;
    builder.for_clause(name, words, body, clause.span)
}

/// Lowers a while-clause AST node to HIR.
pub(crate) fn lower_while_clause(
    builder: &HirBuilder,
    clause: &WhileClauseAst,
) -> Result<HirWhileClause, IrError> {
    let condition = lower_list(builder, &clause.condition)?;
    let body = lower_list(builder, &clause.body)?;
    builder.while_clause(condition, body, clause.span)
}

/// Lowers an until-clause AST node to HIR.
pub(crate) fn lower_until_clause(
    builder: &HirBuilder,
    clause: &UntilClauseAst,
) -> Result<HirUntilClause, IrError> {
    let condition = lower_list(builder, &clause.condition)?;
    let body = lower_list(builder, &clause.body)?;
    builder.until_clause(condition, body, clause.span)
}

/// Lowers a case-item AST node to HIR.
pub(crate) fn lower_case_item(
    builder: &HirBuilder,
    item: &CaseItemAst,
) -> Result<HirCaseItem, IrError> {
    let patterns = lower_words(builder, &item.patterns)?;
    let body = item
        .body
        .as_ref()
        .map(|list| lower_list(builder, list))
        .transpose()?;
    builder.case_item(patterns, body, item.terminator, item.span)
}

/// Lowers a case-clause AST node to HIR.
pub(crate) fn lower_case_clause(
    builder: &HirBuilder,
    clause: &CaseClauseAst,
) -> Result<HirCaseClause, IrError> {
    let word = lower_word(builder, &clause.word)?;
    let items = clause
        .items
        .iter()
        .map(|item| lower_case_item(builder, item))
        .collect::<Result<Vec<_>, _>>()?;
    builder.case_clause(word, items, clause.span)
}
