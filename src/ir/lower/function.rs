//! Function-definition lowering from parser AST to HIR.

use crate::ir::error::IrError;
use crate::ir::hir::HirFunctionDefinition;
use crate::ir::hir_builder::HirBuilder;
use crate::parser::ast::FunctionDefinitionAst;

use super::from_parser::lower_command;
use super::simple_command::{lower_redirects, lower_word};

/// Lowers a function definition AST node to HIR.
pub(crate) fn lower_function_definition(
    builder: &HirBuilder,
    function: &FunctionDefinitionAst,
) -> Result<HirFunctionDefinition, IrError> {
    let name = lower_word(builder, &function.name)?;
    let body = lower_command(builder, &function.body)?;
    let redirects = lower_redirects(builder, &function.redirects)?;
    builder.function_definition(name, body, redirects, function.span)
}
