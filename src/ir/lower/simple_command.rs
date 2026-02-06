//! Simple-command lowering from parser AST to HIR.

use crate::ir::error::IrError;
use crate::ir::hir::{HirAssignment, HirRedirect, HirSimpleCommand, HirWord};
use crate::ir::hir_builder::HirBuilder;
use crate::parser::ast::{AssignmentWordAst, RedirectAst, SimpleCommandAst, WordAst};

/// Lowers a single word AST node to HIR.
pub(crate) fn lower_word(builder: &HirBuilder, word: &WordAst) -> Result<HirWord, IrError> {
    builder.word(word.token.clone(), word.span)
}

/// Lowers a single assignment AST node to HIR.
pub(crate) fn lower_assignment(
    builder: &HirBuilder,
    assignment: &AssignmentWordAst,
) -> Result<HirAssignment, IrError> {
    builder.assignment(
        assignment.token.clone(),
        &assignment.name,
        &assignment.value,
        assignment.span,
    )
}

/// Lowers a single redirect AST node to HIR.
pub(crate) fn lower_redirect(
    builder: &HirBuilder,
    redirect: &RedirectAst,
) -> Result<HirRedirect, IrError> {
    let target = lower_word(builder, &redirect.target)?;
    builder.redirect(
        redirect.fd_or_location.clone(),
        redirect.operator,
        target,
        redirect.span,
    )
}

/// Lowers a slice of word AST nodes to HIR.
pub(crate) fn lower_words(
    builder: &HirBuilder,
    words: &[WordAst],
) -> Result<Vec<HirWord>, IrError> {
    words.iter().map(|w| lower_word(builder, w)).collect()
}

/// Lowers a slice of redirect AST nodes to HIR.
pub(crate) fn lower_redirects(
    builder: &HirBuilder,
    redirects: &[RedirectAst],
) -> Result<Vec<HirRedirect>, IrError> {
    redirects.iter().map(|r| lower_redirect(builder, r)).collect()
}

/// Lowers a simple command AST node to HIR.
pub(crate) fn lower_simple_command(
    builder: &HirBuilder,
    command: &SimpleCommandAst,
) -> Result<HirSimpleCommand, IrError> {
    let assignments = command
        .assignments
        .iter()
        .map(|a| lower_assignment(builder, a))
        .collect::<Result<Vec<_>, _>>()?;
    let words = lower_words(builder, &command.words)?;
    let redirects = lower_redirects(builder, &command.redirects)?;
    builder.simple_command(assignments, words, redirects, command.span)
}
