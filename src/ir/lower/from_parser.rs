//! Parser AST lowering stubs.

use crate::ir::error::IrError;
use crate::ir::lower::LoweringContext;
use crate::ir::program::IrModule;
use crate::parser::ast::{CompleteCommandAst, ProgramAst};

pub(crate) fn lower_complete_command(
    _context: &mut LoweringContext,
    command: &CompleteCommandAst,
) -> Result<IrModule, IrError> {
    Err(IrError::unsupported_form(
        Some(command.span),
        "lower_complete_command is not implemented in Phase 0",
    ))
}

pub(crate) fn lower_program(
    _context: &mut LoweringContext,
    program: &ProgramAst,
) -> Result<IrModule, IrError> {
    Err(IrError::unsupported_form(
        program.span,
        "lower_program is not implemented in Phase 0",
    ))
}
