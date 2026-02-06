//! Redirect-program lowering: translates HirRedirect into RedirectProgramOp sequences.
//!
//! Phase 8 replaces the stub redirect programs from Phase 6 with real
//! lowering that generates correct `RedirectProgramOp` sequences for each
//! `HirRedirect`.

use crate::ir::bytecode::{OpenMode, RedirectProgramOp};
use crate::ir::error::IrError;
use crate::ir::hir::{HirRedirect, HirRedirectOperator};
use crate::ir::ids::RedirectProgramId;
use crate::ir::lower::emit::EmitContext;
use crate::ir::lower::word_program::{lower_word_to_program, WordContext};
use crate::ir::program::RedirectProgram;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Lowers one `HirRedirect` into a `RedirectProgram` and registers it.
pub(crate) fn lower_redirect_to_program(
    ctx: &mut EmitContext<'_>,
    redir: &HirRedirect,
) -> Result<RedirectProgramId, IrError> {
    let op = lower_redirect_op(ctx, redir)?;
    let program = RedirectProgram {
        id: RedirectProgramId::default(),
        ops: vec![op],
    };
    ctx.module().add_redirect_program(program)
}

// ---------------------------------------------------------------------------
// Operator dispatch
// ---------------------------------------------------------------------------

/// Lowers one redirect into its corresponding `RedirectProgramOp`.
fn lower_redirect_op(
    ctx: &mut EmitContext<'_>,
    redir: &HirRedirect,
) -> Result<RedirectProgramOp, IrError> {
    match redir.operator {
        HirRedirectOperator::Input => {
            let fd = resolve_default_fd(redir, 0);
            lower_file_redirect(ctx, fd, redir, OpenMode::ReadOnly)
        }
        HirRedirectOperator::Output => {
            let fd = resolve_default_fd(redir, 1);
            lower_file_redirect(ctx, fd, redir, OpenMode::WriteCreate)
        }
        HirRedirectOperator::AppendOutput => {
            let fd = resolve_default_fd(redir, 1);
            lower_file_redirect(ctx, fd, redir, OpenMode::Append)
        }
        HirRedirectOperator::ReadWrite => {
            let fd = resolve_default_fd(redir, 0);
            lower_file_redirect(ctx, fd, redir, OpenMode::ReadWrite)
        }
        HirRedirectOperator::Clobber => {
            let fd = resolve_default_fd(redir, 1);
            lower_file_redirect(ctx, fd, redir, OpenMode::Clobber)
        }
        HirRedirectOperator::DupInput => {
            let fd = resolve_default_fd(redir, 0);
            lower_dup_redirect(redir, fd)
        }
        HirRedirectOperator::DupOutput => {
            let fd = resolve_default_fd(redir, 1);
            lower_dup_redirect(redir, fd)
        }
        HirRedirectOperator::HereDoc | HirRedirectOperator::HereDocStripTabs => {
            let fd = resolve_default_fd(redir, 0);
            lower_heredoc_redirect(ctx, redir, fd)
        }
    }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Returns the explicit fd if present, otherwise the POSIX default.
fn resolve_default_fd(redir: &HirRedirect, default: u16) -> u16 {
    redir.fd.unwrap_or(default)
}

/// Lowers a file-target redirect (Open).
fn lower_file_redirect(
    ctx: &mut EmitContext<'_>,
    fd: u16,
    redir: &HirRedirect,
    mode: OpenMode,
) -> Result<RedirectProgramOp, IrError> {
    let wp_id = lower_word_to_program(ctx, &redir.target, WordContext::CommandArgument)?;
    Ok(RedirectProgramOp::Open {
        fd,
        target: wp_id,
        mode,
    })
}

/// Lowers a dup/close redirect (`<&`, `>&`).
fn lower_dup_redirect(
    redir: &HirRedirect,
    fd: u16,
) -> Result<RedirectProgramOp, IrError> {
    let lexeme = &redir.target.token.lexeme;

    if lexeme == "-" {
        return Ok(RedirectProgramOp::Close { fd });
    }

    match lexeme.parse::<u16>() {
        Ok(source_fd) => Ok(RedirectProgramOp::Dup {
            from: source_fd,
            to: fd,
        }),
        Err(_) => Err(IrError::invalid_redirect_shape(
            Some(redir.span),
            "invalid dup target",
            format!(
                "expected a file descriptor number or `-`, got `{}`",
                lexeme
            ),
        )),
    }
}

/// Lowers a here-document redirect.
fn lower_heredoc_redirect(
    ctx: &mut EmitContext<'_>,
    redir: &HirRedirect,
    fd: u16,
) -> Result<RedirectProgramOp, IrError> {
    let expand = !redir.target.token.has_quote_markers();
    let body = ctx.module().intern_string(&redir.target.token.lexeme)?;
    Ok(RedirectProgramOp::HereDoc { fd, body, expand })
}
