//! HIR to VM IR emission (Phase 5: control flow lowering).
//!
//! Translates structured HIR control-flow into flat `Instruction` streams
//! using `CodeObjectBuilder` and `IrModuleBuilder`.

use crate::ir::bytecode::{CommandDispatchHint, Instruction};
use crate::ir::error::IrError;
use crate::ir::hir::{
    HirAndOr, HirAndOrConnector, HirCaseClause, HirCaseTerminator, HirCommand,
    HirCompleteCommand, HirCompoundCommand, HirCompoundCommandNode, HirForClause,
    HirFunctionDefinition, HirIfClause, HirList, HirListTerminator, HirPipeline, HirProgram,
    HirSimpleCommand, HirUntilClause, HirWhileClause, HirWord,
};
use crate::ir::ids::CodeObjectId;
use crate::ir::lower::redirect_program::lower_redirect_to_program;
use crate::ir::lower::word_program::{
    lower_assignment_value_to_program, lower_word_to_program, WordContext,
};
use crate::ir::program::{CodeObjectBuilder, IrModuleBuilder};

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
    pub(crate) fn module(&mut self) -> &mut IrModuleBuilder {
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
        HirCommand::Simple(simple) => {
            emit_simple_command(builder, ctx, simple)?;
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

/// Emits a compound command node with optional redirect scope.
fn emit_compound_command_node(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    node: &HirCompoundCommandNode,
) -> Result<(), IrError> {
    if node.redirects.is_empty() {
        emit_compound_command(builder, ctx, &node.kind)
    } else {
        builder.emit(Instruction::PushRedirectScope)?;
        for redir in &node.redirects {
            let rp = lower_redirect_to_program(ctx, redir)?;
            builder.emit(Instruction::AddRedir(rp))?;
        }
        emit_compound_command(builder, ctx, &node.kind)?;
        builder.emit(Instruction::PopRedirectScope)?;
        Ok(())
    }
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
    for word in &clause.words {
        let wp_id = lower_word_to_program(ctx, word, WordContext::ForWord)?;
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

    // Expand case subject word.
    let subject_wp = lower_word_to_program(ctx, &clause.word, WordContext::CaseSubject)?;
    builder.emit(Instruction::CaseSetSubject(subject_wp))?;

    // Default exit status.
    builder.emit(Instruction::PushInt(0))?;

    let item_count = clause.items.len();

    // Pre-allocate body labels for fallthrough support.
    let body_labels: Vec<_> = (0..item_count).map(|_| builder.new_label()).collect();

    for (i, item) in clause.items.iter().enumerate() {
        // Pattern matching: test each pattern.
        for pattern in &item.patterns {
            let wp_id = lower_word_to_program(ctx, pattern, WordContext::CasePattern)?;
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
///
/// POSIX: `fname() compound_command redirect_list` — the redirect_list applies
/// each time the function is called, so redirects are embedded in the function
/// body's code object.
fn emit_function_definition(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    func: &HirFunctionDefinition,
) -> Result<(), IrError> {
    let body_co_id = if func.redirects.is_empty() {
        compile_compound_as_code_object(ctx, &func.body)?
    } else {
        compile_function_body_with_redirects(ctx, func)?
    };
    let name_sym = ctx.module().intern_symbol(&func.name.token.lexeme)?;
    builder.emit(Instruction::DefineFunction {
        name: name_sym,
        body: body_co_id,
    })?;
    Ok(())
}

/// Compiles a function body wrapped with redirect scope for function-level redirects.
fn compile_function_body_with_redirects(
    ctx: &mut EmitContext<'_>,
    func: &HirFunctionDefinition,
) -> Result<CodeObjectId, IrError> {
    let co_id = ctx.module().add_code_object(
        CodeObjectBuilder::new(CodeObjectId::default()).finalize()?,
    )?;
    let mut body_builder = CodeObjectBuilder::new(co_id);

    body_builder.emit(Instruction::PushRedirectScope)?;
    for redir in &func.redirects {
        let rp = lower_redirect_to_program(ctx, redir)?;
        body_builder.emit(Instruction::AddRedir(rp))?;
    }
    emit_compound_command_node(&mut body_builder, ctx, &func.body)?;
    body_builder.emit(Instruction::PopRedirectScope)?;
    body_builder.emit(Instruction::Ret)?;

    let co = body_builder.finalize()?;
    ctx.module().replace_code_object(co_id, co)?;
    Ok(co_id)
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
// Simple command emission
// ---------------------------------------------------------------------------

/// Emits a simple command: assignments, arguments, redirects, then exec.
fn emit_simple_command(
    builder: &mut CodeObjectBuilder,
    ctx: &mut EmitContext<'_>,
    simple: &HirSimpleCommand,
) -> Result<(), IrError> {
    builder.emit(Instruction::BeginSimple)?;

    // Emit assignments.
    for assign in &simple.assignments {
        let sym = ctx.module().intern_symbol(&assign.name)?;
        let wp = lower_assignment_value_to_program(ctx, assign)?;
        builder.emit(Instruction::AddAssign(sym, wp))?;
    }

    // Emit word arguments.
    for word in &simple.words {
        let wp = lower_word_to_program(ctx, word, WordContext::CommandArgument)?;
        builder.emit(Instruction::AddArg(wp))?;
    }

    // Emit redirects.
    for redir in &simple.redirects {
        let rp = lower_redirect_to_program(ctx, redir)?;
        builder.emit(Instruction::AddRedir(rp))?;
    }

    builder.emit(Instruction::EndSimple)?;

    let hint = classify_dispatch_hint(simple);
    builder.emit(Instruction::ExecSimple(hint))?;

    Ok(())
}

/// Determines the dispatch hint for a simple command.
fn classify_dispatch_hint(simple: &HirSimpleCommand) -> CommandDispatchHint {
    if simple.words.is_empty() {
        return CommandDispatchHint::NoCommand;
    }

    let first_word = &simple.words[0];
    if is_plain_literal_word(first_word) && is_posix_special_builtin(&first_word.token.lexeme) {
        return CommandDispatchHint::SpecialBuiltin;
    }

    CommandDispatchHint::Standard
}

/// Returns `true` if the word has no quote markers and no substitution markers.
fn is_plain_literal_word(word: &HirWord) -> bool {
    word.token.quote_markers.is_empty() && word.token.substitution_markers.is_empty()
}

/// Returns `true` if the name is a POSIX special builtin.
fn is_posix_special_builtin(name: &str) -> bool {
    matches!(
        name,
        "break"
            | ":"
            | "continue"
            | "."
            | "eval"
            | "exec"
            | "exit"
            | "export"
            | "readonly"
            | "return"
            | "set"
            | "shift"
            | "times"
            | "trap"
            | "unset"
    )
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::program::IrOptions;
    use crate::lexer::{Lexer, LexerMode, SourceId};
    use crate::parser::{ParseOptions, ParseStep, Parser, TokenStream};

    fn lower_command(input: &str) -> crate::ir::program::IrModule {
        let lexer = Lexer::new(input, LexerMode::Normal);
        let stream = TokenStream::new(lexer);
        let mut parser = Parser::new(SourceId::new(0), ParseOptions::default(), stream);
        let step = parser
            .parse_complete_command()
            .expect("parse should succeed");
        let ParseStep::Complete(command) = step else {
            panic!("expected complete command, got {step:?}");
        };
        let mut lctx = crate::ir::lower::LoweringContext::new(IrOptions::default());
        lctx.lower_complete_command(&command)
            .expect("lowering should succeed")
    }

    fn top_instructions(module: &crate::ir::program::IrModule) -> &[Instruction] {
        &module.code_objects[0].instructions
    }

    // -----------------------------------------------------------------------
    // Assignment-only
    // -----------------------------------------------------------------------

    #[test]
    fn assignment_only_emits_no_command_hint() {
        let module = lower_command("A=1\n");
        let instrs = top_instructions(&module);
        assert_eq!(instrs[0], Instruction::BeginSimple);
        assert!(matches!(instrs[1], Instruction::AddAssign(_, _)));
        assert_eq!(instrs[2], Instruction::EndSimple);
        assert_eq!(
            instrs[3],
            Instruction::ExecSimple(CommandDispatchHint::NoCommand)
        );
        assert_eq!(instrs[4], Instruction::Ret);
    }

    #[test]
    fn multiple_assignments_only() {
        let module = lower_command("A=1 B=2\n");
        let instrs = top_instructions(&module);
        let assign_count = instrs
            .iter()
            .filter(|i| matches!(i, Instruction::AddAssign(_, _)))
            .count();
        assert_eq!(assign_count, 2);
        assert!(instrs.contains(&Instruction::ExecSimple(CommandDispatchHint::NoCommand)));
    }

    // -----------------------------------------------------------------------
    // Arg-only
    // -----------------------------------------------------------------------

    #[test]
    fn arg_only_emits_standard_hint() {
        let module = lower_command("ls -la\n");
        let instrs = top_instructions(&module);
        assert_eq!(instrs[0], Instruction::BeginSimple);
        assert!(matches!(instrs[1], Instruction::AddArg(_)));
        assert!(matches!(instrs[2], Instruction::AddArg(_)));
        assert_eq!(instrs[3], Instruction::EndSimple);
        assert_eq!(
            instrs[4],
            Instruction::ExecSimple(CommandDispatchHint::Standard)
        );
        assert_eq!(instrs[5], Instruction::Ret);
    }

    // -----------------------------------------------------------------------
    // Redirect-only
    // -----------------------------------------------------------------------

    #[test]
    fn redirect_only_emits_no_command_hint() {
        let module = lower_command(">out.txt\n");
        let instrs = top_instructions(&module);
        assert_eq!(instrs[0], Instruction::BeginSimple);
        assert!(matches!(instrs[1], Instruction::AddRedir(_)));
        assert_eq!(instrs[2], Instruction::EndSimple);
        assert_eq!(
            instrs[3],
            Instruction::ExecSimple(CommandDispatchHint::NoCommand)
        );
    }

    // -----------------------------------------------------------------------
    // Mixed: assignment + args + redirect
    // -----------------------------------------------------------------------

    #[test]
    fn mixed_command_emits_standard_hint() {
        let module = lower_command("A=1 cmd arg >out\n");
        let instrs = top_instructions(&module);
        assert_eq!(instrs[0], Instruction::BeginSimple);
        // Assignment first
        assert!(matches!(instrs[1], Instruction::AddAssign(_, _)));
        // Then args
        assert!(matches!(instrs[2], Instruction::AddArg(_)));
        assert!(matches!(instrs[3], Instruction::AddArg(_)));
        // Then redirect
        assert!(matches!(instrs[4], Instruction::AddRedir(_)));
        assert_eq!(instrs[5], Instruction::EndSimple);
        assert_eq!(
            instrs[6],
            Instruction::ExecSimple(CommandDispatchHint::Standard)
        );
    }

    // -----------------------------------------------------------------------
    // Assignment + redirect (no command name)
    // -----------------------------------------------------------------------

    #[test]
    fn assignment_plus_redirect_emits_no_command_hint() {
        let module = lower_command("A=1 >file\n");
        let instrs = top_instructions(&module);
        assert!(matches!(instrs[1], Instruction::AddAssign(_, _)));
        assert!(matches!(instrs[2], Instruction::AddRedir(_)));
        assert!(instrs.contains(&Instruction::ExecSimple(CommandDispatchHint::NoCommand)));
    }

    // -----------------------------------------------------------------------
    // Special builtin detection
    // -----------------------------------------------------------------------

    #[test]
    fn export_emits_special_builtin_hint() {
        let module = lower_command("export FOO=bar\n");
        let instrs = top_instructions(&module);
        assert!(instrs.contains(&Instruction::ExecSimple(
            CommandDispatchHint::SpecialBuiltin
        )));
    }

    #[test]
    fn all_posix_special_builtins_detected() {
        let builtins = [
            "break", ":", "continue", ".", "eval", "exec", "exit", "export", "readonly",
            "return", "set", "shift", "times", "trap", "unset",
        ];
        for name in builtins {
            let input = format!("{name} arg\n");
            let module = lower_command(&input);
            let instrs = top_instructions(&module);
            assert!(
                instrs.contains(&Instruction::ExecSimple(
                    CommandDispatchHint::SpecialBuiltin
                )),
                "expected SpecialBuiltin for `{name}`"
            );
        }
    }

    #[test]
    fn non_special_builtin_emits_standard_hint() {
        let module = lower_command("echo hello\n");
        let instrs = top_instructions(&module);
        assert!(instrs.contains(&Instruction::ExecSimple(CommandDispatchHint::Standard)));
    }

    // -----------------------------------------------------------------------
    // Assignment name interning
    // -----------------------------------------------------------------------

    #[test]
    fn assignment_name_is_interned_as_symbol() {
        let module = lower_command("MYVAR=hello\n");
        assert!(module.symbol_pool.iter().any(|s| s == "MYVAR"));
    }

    // -----------------------------------------------------------------------
    // Word programs and redirect programs are created
    // -----------------------------------------------------------------------

    #[test]
    fn word_programs_created_for_args() {
        let module = lower_command("ls -la\n");
        assert_eq!(module.word_programs.len(), 2);
    }

    #[test]
    fn redirect_programs_created_for_redirects() {
        let module = lower_command("cmd >out.txt\n");
        assert_eq!(module.redirect_programs.len(), 1);
    }

    #[test]
    fn assignment_value_creates_word_program() {
        let module = lower_command("A=1 B=2\n");
        // Two assignments -> two stub word programs for values.
        assert_eq!(module.word_programs.len(), 2);
    }
}
