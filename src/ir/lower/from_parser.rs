//! Parser AST to HIR lowering coordination.

use crate::ir::error::IrError;
use crate::ir::hir::{
    HirAndOr, HirCommand, HirCompleteCommand, HirList, HirPipeline, HirProgram,
};
use crate::ir::hir_builder::HirBuilder;
use crate::ir::lower::LoweringContext;
use crate::ir::program::IrModule;
use crate::parser::ast::{
    AndOrAst, CommandAst, CompleteCommandAst, ListAst, PipelineAst, ProgramAst,
};

use super::compound::lower_compound_command_node;
use super::emit;
use super::function::lower_function_definition;
use super::simple_command::lower_simple_command;

// ---------------------------------------------------------------------------
// Entry points called from LoweringContext (mod.rs)
// ---------------------------------------------------------------------------

/// Lowers one complete command to an IrModule (existing public API).
pub(crate) fn lower_complete_command_entry(
    context: &mut LoweringContext,
    command: &CompleteCommandAst,
) -> Result<IrModule, IrError> {
    let hir = lower_complete_command_to_hir(context, command)?;
    let mut module_builder = context.module_builder();
    let mut emit_ctx = emit::EmitContext::new(&mut module_builder);
    emit::emit_complete_command(&mut emit_ctx, &hir)?;
    Ok(module_builder.finish())
}

/// Lowers a whole program to an IrModule (existing public API).
pub(crate) fn lower_program_entry(
    context: &mut LoweringContext,
    program: &ProgramAst,
) -> Result<IrModule, IrError> {
    let hir = lower_program_to_hir(context, program)?;
    let mut module_builder = context.module_builder();
    let mut emit_ctx = emit::EmitContext::new(&mut module_builder);
    emit::emit_program(&mut emit_ctx, &hir)?;
    Ok(module_builder.finish())
}

/// Lowers a single complete command AST to HIR.
pub(crate) fn lower_complete_command_to_hir(
    _context: &mut LoweringContext,
    command: &CompleteCommandAst,
) -> Result<HirCompleteCommand, IrError> {
    let builder = HirBuilder::new();
    lower_complete_command(&builder, command)
}

/// Lowers a full program AST to HIR.
pub(crate) fn lower_program_to_hir(
    _context: &mut LoweringContext,
    program: &ProgramAst,
) -> Result<HirProgram, IrError> {
    let builder = HirBuilder::new();

    let complete_commands = program
        .complete_commands
        .iter()
        .map(|cmd| lower_complete_command(&builder, cmd))
        .collect::<Result<Vec<_>, _>>()?;

    builder.program(complete_commands, program.span)
}

// ---------------------------------------------------------------------------
// Recursive lowering functions (used by sibling modules)
// ---------------------------------------------------------------------------

/// Lowers a complete command AST to HIR.
pub(crate) fn lower_complete_command(
    builder: &HirBuilder,
    command: &CompleteCommandAst,
) -> Result<HirCompleteCommand, IrError> {
    let list = lower_list(builder, &command.list)?;
    builder.complete_command(list, command.separator_op, command.span)
}

/// Lowers a list AST to HIR.
pub(crate) fn lower_list(builder: &HirBuilder, list: &ListAst) -> Result<HirList, IrError> {
    let and_ors = list
        .and_ors
        .iter()
        .map(|ao| lower_and_or(builder, ao))
        .collect::<Result<Vec<_>, _>>()?;
    builder.list(and_ors, list.span)
}

/// Lowers an and-or chain AST to HIR.
fn lower_and_or(builder: &HirBuilder, and_or: &AndOrAst) -> Result<HirAndOr, IrError> {
    let head = lower_pipeline(builder, &and_or.head)?;
    let tail = and_or
        .tail
        .iter()
        .map(|(op, pipeline)| Ok((*op, lower_pipeline(builder, pipeline)?)))
        .collect::<Result<Vec<_>, IrError>>()?;
    builder.and_or(head, tail, and_or.span)
}

/// Lowers a pipeline AST to HIR.
fn lower_pipeline(builder: &HirBuilder, pipeline: &PipelineAst) -> Result<HirPipeline, IrError> {
    let commands = pipeline
        .commands
        .iter()
        .map(|cmd| lower_command(builder, cmd))
        .collect::<Result<Vec<_>, _>>()?;
    builder.pipeline(pipeline.negated, commands, pipeline.span)
}

/// Lowers a command AST to HIR.
pub(crate) fn lower_command(
    builder: &HirBuilder,
    command: &CommandAst,
) -> Result<HirCommand, IrError> {
    match command {
        CommandAst::Simple(simple) => {
            let hir = lower_simple_command(builder, simple)?;
            Ok(builder.command_simple(hir))
        }
        CommandAst::Compound(compound) => {
            let hir = lower_compound_command_node(builder, compound)?;
            Ok(builder.command_compound(hir))
        }
        CommandAst::FunctionDefinition(func) => {
            let hir = lower_function_definition(builder, func)?;
            Ok(builder.command_function_definition(hir))
        }
    }
}
