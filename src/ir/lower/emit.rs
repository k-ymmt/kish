//! HIR to VM IR emission (Phase 5: control flow lowering).
//!
//! Translates structured HIR control-flow into flat `Instruction` streams
//! using `CodeObjectBuilder` and `IrModuleBuilder`.

use crate::ir::bytecode::Instruction;
use crate::ir::error::IrError;
use crate::ir::hir::{
    HirAndOr, HirAndOrConnector, HirCaseClause, HirCaseTerminator, HirCommand,
    HirCompleteCommand, HirCompoundCommand, HirCompoundCommandNode, HirForClause,
    HirFunctionDefinition, HirIfClause, HirList, HirListTerminator, HirPipeline, HirProgram,
    HirUntilClause, HirWhileClause,
};
use crate::ir::ids::{CodeObjectId, WordProgramId};
use crate::ir::program::{CodeObjectBuilder, IrModuleBuilder, WordProgram};

// ---------------------------------------------------------------------------
// EmitContext
// ---------------------------------------------------------------------------

/// Stateful context for HIR -> VM IR emission.
pub(crate) struct EmitContext<'a> {
    module: &'a mut IrModuleBuilder,
}

impl<'a> EmitContext<'a> {
    /// Creates a new emission context.
    pub(crate) fn new(module: &'a mut IrModuleBuilder) -> Self {
        Self { module }
    }

    /// Returns a mutable reference to the module builder.
    fn module(&mut self) -> &mut IrModuleBuilder {
        self.module
    }
}

// ---------------------------------------------------------------------------
// Public entry points
// ---------------------------------------------------------------------------

/// Emits a whole HIR program into a top-level code object.
pub(crate) fn emit_program(
    ctx: &mut EmitContext<'_>,
    program: &HirProgram,
) -> Result<CodeObjectId, IrError> {
    let co_id = ctx.module().add_code_object(
        CodeObjectBuilder::new(CodeObjectId::default()).finalize()?,
    )?;
    let mut builder = CodeObjectBuilder::new(co_id);

    if program.complete_commands.is_empty() {
        builder.emit(Instruction::PushInt(0))?;
    } else {
        for (i, cc) in program.complete_commands.iter().enumerate() {
            emit_complete_command_inline(&mut builder, ctx, cc)?;
            if i < program.complete_commands.len() - 1 {
                builder.emit(Instruction::Drop)?;
            }
        }
    }

    builder.emit(Instruction::Ret)?;
    let co = builder.finalize()?;

    // Replace the placeholder code object with actual one.
    ctx.module().replace_code_object(co_id, co)?;
    Ok(co_id)
}

/// Emits a single HIR complete command into its own code object.
pub(crate) fn emit_complete_command(
    ctx: &mut EmitContext<'_>,
    cc: &HirCompleteCommand,
) -> Result<CodeObjectId, IrError> {
    let co_id = ctx.module().add_code_object(
        CodeObjectBuilder::new(CodeObjectId::default()).finalize()?,
    )?;
    let mut builder = CodeObjectBuilder::new(co_id);

    emit_complete_command_inline(&mut builder, ctx, cc)?;
    builder.emit(Instruction::Ret)?;

    let co = builder.finalize()?;
    ctx.module().replace_code_object(co_id, co)?;
    Ok(co_id)
}

// ---------------------------------------------------------------------------
// Complete command
// ---------------------------------------------------------------------------

/// Emits a complete command inline into the given builder.
fn emit_complete_command_inline(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    cc: &HirCompleteCommand,
) -> Result<(), IrError> {
    match cc.separator_op {
        Some(HirListTerminator::Ampersand) => {
            let co_id = compile_list_as_code_object(ctx, &cc.list)?;
            builder.emit(Instruction::ExecBackground(co_id))?;
        }
        _ => {
            emit_list(builder, ctx, &cc.list)?;
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// List (sequence of and_or groups)
// ---------------------------------------------------------------------------

/// Emits a list: each and_or's status is dropped except the last.
fn emit_list(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    list: &HirList,
) -> Result<(), IrError> {
    for (i, ao) in list.and_ors.iter().enumerate() {
        emit_and_or(builder, ctx, ao)?;
        if i < list.and_ors.len() - 1 {
            builder.emit(Instruction::Drop)?;
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// AND-OR chain
// ---------------------------------------------------------------------------

/// Emits an and-or chain with short-circuit semantics.
fn emit_and_or(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    and_or: &HirAndOr,
) -> Result<(), IrError> {
    emit_pipeline(builder, ctx, &and_or.head)?;

    for (connector, pipeline) in &and_or.tail {
        let skip = builder.new_label();
        builder.emit(Instruction::Dup)?;

        match connector {
            HirAndOrConnector::AndIf => {
                // If nonzero (failed), skip the next pipeline.
                builder.emit_jmp_if_non_zero(skip)?;
            }
            HirAndOrConnector::OrIf => {
                // If zero (succeeded), skip the next pipeline.
                builder.emit_jmp_if_zero(skip)?;
            }
        }

        builder.emit(Instruction::Drop)?;
        emit_pipeline(builder, ctx, pipeline)?;

        builder.bind_label(skip)?;
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Pipeline
// ---------------------------------------------------------------------------

/// Emits a pipeline (single command inline, multi-command via sub code objects).
fn emit_pipeline(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    pipeline: &HirPipeline,
) -> Result<(), IrError> {
    if pipeline.commands.len() == 1 {
        // Single command: emit inline.
        emit_command(builder, ctx, &pipeline.commands[0])?;
    } else {
        // Multi-command pipeline: compile each as separate code object.
        let stage_count = pipeline.commands.len() as u32;
        builder.emit(Instruction::BeginPipeline(stage_count))?;

        for cmd in &pipeline.commands {
            let co_id = compile_command_as_code_object(ctx, cmd)?;
            builder.emit(Instruction::AddPipelineStage(co_id))?;
        }

        builder.emit(Instruction::ExecPipeline)?;
    }

    if pipeline.negated {
        builder.emit(Instruction::NegateStatus)?;
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Command dispatch
// ---------------------------------------------------------------------------

/// Emits a single command (simple, compound, or function definition).
fn emit_command(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    command: &HirCommand,
) -> Result<(), IrError> {
    match command {
        HirCommand::Simple(_simple) => {
            // Phase 6 stub: push 0 as exit status.
            builder.emit(Instruction::PushInt(0))?;
        }
        HirCommand::Compound(node) => {
            emit_compound_command_node(builder, ctx, node)?;
        }
        HirCommand::FunctionDefinition(func) => {
            emit_function_definition(builder, ctx, func)?;
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Compound command node (with redirects)
// ---------------------------------------------------------------------------

/// Emits a compound command node. Redirects are ignored (Phase 8).
fn emit_compound_command_node(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    node: &HirCompoundCommandNode,
) -> Result<(), IrError> {
    emit_compound_command(builder, ctx, &node.kind)
}

/// Emits a compound command body.
fn emit_compound_command(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    compound: &HirCompoundCommand,
) -> Result<(), IrError> {
    match compound {
        HirCompoundCommand::BraceGroup(list) => {
            emit_list(builder, ctx, list)?;
        }
        HirCompoundCommand::Subshell(list) => {
            let co_id = compile_list_as_code_object(ctx, list)?;
            builder.emit(Instruction::ExecSubshell(co_id))?;
        }
        HirCompoundCommand::If(clause) => {
            emit_if(builder, ctx, clause)?;
        }
        HirCompoundCommand::While(clause) => {
            emit_while(builder, ctx, clause)?;
        }
        HirCompoundCommand::Until(clause) => {
            emit_until(builder, ctx, clause)?;
        }
        HirCompoundCommand::For(clause) => {
            emit_for(builder, ctx, clause)?;
        }
        HirCompoundCommand::Case(clause) => {
            emit_case(builder, ctx, clause)?;
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// If / elif / else
// ---------------------------------------------------------------------------

/// Emits an if/elif/else chain.
fn emit_if(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    clause: &HirIfClause,
) -> Result<(), IrError> {
    let end_label = builder.new_label();

    // Primary condition.
    emit_list(builder, ctx, &clause.condition)?;
    let else_or_elif = builder.new_label();
    builder.emit_jmp_if_non_zero(else_or_elif)?;

    // Then body.
    emit_list(builder, ctx, &clause.then_body)?;
    builder.emit_jmp(end_label)?;

    builder.bind_label(else_or_elif)?;

    // Elif arms.
    for (elif_cond, elif_body) in &clause.elif_arms {
        emit_list(builder, ctx, elif_cond)?;
        let next_elif_or_else = builder.new_label();
        builder.emit_jmp_if_non_zero(next_elif_or_else)?;
        emit_list(builder, ctx, elif_body)?;
        builder.emit_jmp(end_label)?;
        builder.bind_label(next_elif_or_else)?;
    }

    // Else body or default.
    match &clause.else_body {
        Some(body) => {
            emit_list(builder, ctx, body)?;
        }
        None => {
            // POSIX: exit 0 if no branch taken.
            builder.emit(Instruction::PushInt(0))?;
        }
    }

    builder.bind_label(end_label)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// While loop
// ---------------------------------------------------------------------------

/// Emits a while loop.
fn emit_while(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    clause: &HirWhileClause,
) -> Result<(), IrError> {
    let loop_top = builder.new_label();
    let loop_end = builder.new_label();

    // Default exit status.
    builder.emit(Instruction::PushInt(0))?;

    builder.bind_label(loop_top)?;

    // Condition.
    emit_list(builder, ctx, &clause.condition)?;
    builder.emit_jmp_if_non_zero(loop_end)?;

    // Drop previous body/default status.
    builder.emit(Instruction::Drop)?;

    // Body.
    emit_list(builder, ctx, &clause.body)?;
    builder.emit_jmp(loop_top)?;

    builder.bind_label(loop_end)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Until loop
// ---------------------------------------------------------------------------

/// Emits an until loop (same as while but with inverted condition).
fn emit_until(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    clause: &HirUntilClause,
) -> Result<(), IrError> {
    let loop_top = builder.new_label();
    let loop_end = builder.new_label();

    // Default exit status.
    builder.emit(Instruction::PushInt(0))?;

    builder.bind_label(loop_top)?;

    // Condition.
    emit_list(builder, ctx, &clause.condition)?;
    builder.emit_jmp_if_zero(loop_end)?;

    // Drop previous body/default status.
    builder.emit(Instruction::Drop)?;

    // Body.
    emit_list(builder, ctx, &clause.body)?;
    builder.emit_jmp(loop_top)?;

    builder.bind_label(loop_end)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// For loop
// ---------------------------------------------------------------------------

/// Emits a for loop.
fn emit_for(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    clause: &HirForClause,
) -> Result<(), IrError> {
    let loop_top = builder.new_label();
    let loop_end = builder.new_label();

    let var_sym = ctx.module().intern_symbol(&clause.name.token.lexeme)?;
    let word_count = clause.words.len() as u32;

    // Default exit status.
    builder.emit(Instruction::PushInt(0))?;

    builder.emit(Instruction::ForSetup {
        var: var_sym,
        word_count,
    })?;

    // Add word programs for each iteration word.
    for _word in &clause.words {
        let wp_id = stub_word_program(ctx)?;
        builder.emit(Instruction::ForAddWord(wp_id))?;
    }

    builder.bind_label(loop_top)?;

    // Advance iterator.
    builder.emit(Instruction::ForNext)?;
    builder.emit_jmp_if_zero(loop_end)?;

    // Drop previous status.
    builder.emit(Instruction::Drop)?;

    // Body.
    emit_list(builder, ctx, &clause.body)?;
    builder.emit_jmp(loop_top)?;

    builder.bind_label(loop_end)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Case statement
// ---------------------------------------------------------------------------

/// Emits a case statement with `;;` (Break) and `;&` (Fallthrough) support.
fn emit_case(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    clause: &HirCaseClause,
) -> Result<(), IrError> {
    let case_end = builder.new_label();

    // Stub case subject (Phase 7 will expand the word).
    builder.emit(Instruction::PushInt(0))?;
    builder.emit(Instruction::CaseSetSubject)?;

    // Default exit status.
    builder.emit(Instruction::PushInt(0))?;

    let item_count = clause.items.len();

    // Pre-allocate body labels for fallthrough support.
    let body_labels: Vec<_> = (0..item_count).map(|_| builder.new_label()).collect();

    for (i, item) in clause.items.iter().enumerate() {
        // Pattern matching: test each pattern.
        for _pattern in &item.patterns {
            let wp_id = stub_word_program(ctx)?;
            builder.emit(Instruction::CaseTestPattern(wp_id))?;
            builder.emit_jmp_if_non_zero(body_labels[i])?;
        }

        // No match: jump to next item's patterns.
        let next_item = builder.new_label();
        builder.emit_jmp(next_item)?;

        // Body label.
        builder.bind_label(body_labels[i])?;

        // Drop previous exit status.
        builder.emit(Instruction::Drop)?;

        // Emit body or push 0 for empty body.
        if let Some(body) = &item.body {
            emit_list(builder, ctx, body)?;
        } else {
            builder.emit(Instruction::PushInt(0))?;
        }

        // Terminator handling.
        match item.terminator {
            Some(HirCaseTerminator::Fallthrough) => {
                // Fall through to next item's body (skip its patterns).
                if i + 1 < item_count {
                    builder.emit_jmp(body_labels[i + 1])?;
                } else {
                    builder.emit_jmp(case_end)?;
                }
            }
            _ => {
                // Break (;;) or no terminator: jump to case end.
                builder.emit_jmp(case_end)?;
            }
        }

        builder.bind_label(next_item)?;
    }

    builder.emit(Instruction::CaseClear)?;
    builder.bind_label(case_end)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Function definition
// ---------------------------------------------------------------------------

/// Emits a function definition.
fn emit_function_definition(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    func: &HirFunctionDefinition,
) -> Result<(), IrError> {
    let body_co_id = compile_compound_as_code_object(ctx, &func.body)?;
    let name_sym = ctx.module().intern_symbol(&func.name.token.lexeme)?;
    builder.emit(Instruction::DefineFunction {
        name: name_sym,
        body: body_co_id,
    })?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Helper: compile into separate code objects
// ---------------------------------------------------------------------------

/// Compiles a `HirList` into a separate code object.
fn compile_list_as_code_object(
    ctx: &mut EmitContext<'_>,
    list: &HirList,
) -> Result<CodeObjectId, IrError> {
    let co_id = ctx.module().add_code_object(
        CodeObjectBuilder::new(CodeObjectId::default()).finalize()?,
    )?;
    let mut builder = CodeObjectBuilder::new(co_id);

    emit_list(&mut builder, ctx, list)?;
    builder.emit(Instruction::Ret)?;

    let co = builder.finalize()?;
    ctx.module().replace_code_object(co_id, co)?;
    Ok(co_id)
}

/// Compiles a `HirCommand` into a separate code object.
fn compile_command_as_code_object(
    ctx: &mut EmitContext<'_>,
    cmd: &HirCommand,
) -> Result<CodeObjectId, IrError> {
    let co_id = ctx.module().add_code_object(
        CodeObjectBuilder::new(CodeObjectId::default()).finalize()?,
    )?;
    let mut builder = CodeObjectBuilder::new(co_id);

    emit_command(&mut builder, ctx, cmd)?;
    builder.emit(Instruction::Ret)?;

    let co = builder.finalize()?;
    ctx.module().replace_code_object(co_id, co)?;
    Ok(co_id)
}

/// Compiles a `HirCompoundCommandNode` into a separate code object.
fn compile_compound_as_code_object(
    ctx: &mut EmitContext<'_>,
    node: &HirCompoundCommandNode,
) -> Result<CodeObjectId, IrError> {
    let co_id = ctx.module().add_code_object(
        CodeObjectBuilder::new(CodeObjectId::default()).finalize()?,
    )?;
    let mut builder = CodeObjectBuilder::new(co_id);

    emit_compound_command_node(&mut builder, ctx, node)?;
    builder.emit(Instruction::Ret)?;

    let co = builder.finalize()?;
    ctx.module().replace_code_object(co_id, co)?;
    Ok(co_id)
}

// ---------------------------------------------------------------------------
// Helper: stub word program
// ---------------------------------------------------------------------------

/// Creates a stub (empty) word program as a Phase 7 placeholder.
fn stub_word_program(ctx: &mut EmitContext<'_>) -> Result<WordProgramId, IrError> {
    ctx.module().add_word_program(WordProgram::default())
}
