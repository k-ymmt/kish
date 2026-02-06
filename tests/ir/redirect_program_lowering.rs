//! Phase 8 tests: RedirectProgram lowering.

use kish::ir::{
    Instruction, IrModule, IrOptions, LoweringContext, OpenMode, RedirectProgramOp,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

fn parser_for(input: &str) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), ParseOptions::default(), stream)
}

fn lower_command(input: &str) -> IrModule {
    let mut parser = parser_for(input);
    let step = parser
        .parse_complete_command()
        .expect("parse should succeed");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command, got {step:?}");
    };
    let mut context = LoweringContext::new(IrOptions::default());
    context
        .lower_complete_command(&command)
        .expect("lowering should succeed")
}

fn top_instructions(module: &IrModule) -> &[Instruction] {
    &module.code_objects[0].instructions
}

/// Returns the first redirect program ops in the module.
fn first_redirect_ops(module: &IrModule) -> &[RedirectProgramOp] {
    &module.redirect_programs[0].ops
}

// ---------------------------------------------------------------------------
// Output redirect
// ---------------------------------------------------------------------------

#[test]
fn output_redirect() {
    let module = lower_command("cmd >out.txt\n");
    assert_eq!(module.redirect_programs.len(), 1);
    let ops = first_redirect_ops(&module);
    assert_eq!(ops.len(), 1);
    match &ops[0] {
        RedirectProgramOp::Open { fd, mode, .. } => {
            assert_eq!(*fd, 1);
            assert_eq!(*mode, OpenMode::WriteCreate);
        }
        other => panic!("expected Open, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Input redirect
// ---------------------------------------------------------------------------

#[test]
fn input_redirect() {
    let module = lower_command("cmd <in.txt\n");
    assert_eq!(module.redirect_programs.len(), 1);
    let ops = first_redirect_ops(&module);
    match &ops[0] {
        RedirectProgramOp::Open { fd, mode, .. } => {
            assert_eq!(*fd, 0);
            assert_eq!(*mode, OpenMode::ReadOnly);
        }
        other => panic!("expected Open, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Append redirect
// ---------------------------------------------------------------------------

#[test]
fn append_redirect() {
    let module = lower_command("cmd >>log.txt\n");
    let ops = first_redirect_ops(&module);
    match &ops[0] {
        RedirectProgramOp::Open { fd, mode, .. } => {
            assert_eq!(*fd, 1);
            assert_eq!(*mode, OpenMode::Append);
        }
        other => panic!("expected Open, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Read-write redirect
// ---------------------------------------------------------------------------

#[test]
fn readwrite_redirect() {
    let module = lower_command("cmd <>file\n");
    let ops = first_redirect_ops(&module);
    match &ops[0] {
        RedirectProgramOp::Open { fd, mode, .. } => {
            assert_eq!(*fd, 0);
            assert_eq!(*mode, OpenMode::ReadWrite);
        }
        other => panic!("expected Open, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Clobber redirect
// ---------------------------------------------------------------------------

#[test]
fn clobber_redirect() {
    let module = lower_command("cmd >|file\n");
    let ops = first_redirect_ops(&module);
    match &ops[0] {
        RedirectProgramOp::Open { fd, mode, .. } => {
            assert_eq!(*fd, 1);
            assert_eq!(*mode, OpenMode::Clobber);
        }
        other => panic!("expected Open, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Explicit fd
// ---------------------------------------------------------------------------

#[test]
fn explicit_fd() {
    let module = lower_command("cmd 2>err.txt\n");
    let ops = first_redirect_ops(&module);
    match &ops[0] {
        RedirectProgramOp::Open { fd, mode, .. } => {
            assert_eq!(*fd, 2);
            assert_eq!(*mode, OpenMode::WriteCreate);
        }
        other => panic!("expected Open, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Dup output
// ---------------------------------------------------------------------------

#[test]
fn dup_output() {
    let module = lower_command("cmd 2>&1\n");
    let ops = first_redirect_ops(&module);
    assert_eq!(
        ops[0],
        RedirectProgramOp::Dup { from: 1, to: 2 }
    );
}

// ---------------------------------------------------------------------------
// Dup input
// ---------------------------------------------------------------------------

#[test]
fn dup_input() {
    let module = lower_command("cmd 0<&3\n");
    let ops = first_redirect_ops(&module);
    assert_eq!(
        ops[0],
        RedirectProgramOp::Dup { from: 3, to: 0 }
    );
}

// ---------------------------------------------------------------------------
// Close fd
// ---------------------------------------------------------------------------

#[test]
fn close_fd() {
    let module = lower_command("cmd 2>&-\n");
    let ops = first_redirect_ops(&module);
    assert_eq!(ops[0], RedirectProgramOp::Close { fd: 2 });
}

// ---------------------------------------------------------------------------
// Multiple redirects order
// ---------------------------------------------------------------------------

#[test]
fn multiple_redirects_order() {
    let module = lower_command("cmd >out 2>&1\n");
    assert_eq!(module.redirect_programs.len(), 2);

    // First: >out (fd 1, WriteCreate)
    match &module.redirect_programs[0].ops[0] {
        RedirectProgramOp::Open { fd, mode, .. } => {
            assert_eq!(*fd, 1);
            assert_eq!(*mode, OpenMode::WriteCreate);
        }
        other => panic!("expected Open for first redirect, got {other:?}"),
    }

    // Second: 2>&1 (Dup from 1 to 2)
    assert_eq!(
        module.redirect_programs[1].ops[0],
        RedirectProgramOp::Dup { from: 1, to: 2 }
    );
}

// ---------------------------------------------------------------------------
// Redirect-only command has non-empty program
// ---------------------------------------------------------------------------

#[test]
fn redirect_only_has_ops() {
    let module = lower_command(">out.txt\n");
    assert!(!module.redirect_programs.is_empty());
    assert!(!module.redirect_programs[0].ops.is_empty());
}

// ---------------------------------------------------------------------------
// Here-document (unquoted delimiter -> expand: true)
// ---------------------------------------------------------------------------

#[test]
fn heredoc_unquoted() {
    let module = lower_command("cmd <<EOF\nbody\nEOF\n");
    let ops = first_redirect_ops(&module);
    match &ops[0] {
        RedirectProgramOp::HereDoc { fd, expand, .. } => {
            assert_eq!(*fd, 0);
            assert!(*expand, "unquoted heredoc should expand");
        }
        other => panic!("expected HereDoc, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Here-document (quoted delimiter -> expand: false)
// ---------------------------------------------------------------------------

#[test]
fn heredoc_quoted() {
    let module = lower_command("cmd <<'EOF'\nbody\nEOF\n");
    let ops = first_redirect_ops(&module);
    match &ops[0] {
        RedirectProgramOp::HereDoc { fd, expand, .. } => {
            assert_eq!(*fd, 0);
            assert!(!*expand, "quoted heredoc should not expand");
        }
        other => panic!("expected HereDoc, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Word program created for file redirect target
// ---------------------------------------------------------------------------

#[test]
fn word_program_for_target() {
    let module = lower_command("cmd >out.txt\n");
    // "cmd" and "out.txt" each get a word program.
    assert!(module.word_programs.len() >= 2);
}

// ---------------------------------------------------------------------------
// Compound command with redirect
// ---------------------------------------------------------------------------

#[test]
fn compound_with_redirect() {
    let module = lower_command("{ cmd; } >out\n");
    let instrs = top_instructions(&module);

    assert!(
        instrs.contains(&Instruction::PushRedirectScope),
        "expected PushRedirectScope in instructions: {instrs:?}"
    );
    assert!(
        instrs.contains(&Instruction::PopRedirectScope),
        "expected PopRedirectScope in instructions: {instrs:?}"
    );

    // There should be an AddRedir instruction between Push and Pop scope.
    let push_pos = instrs
        .iter()
        .position(|i| *i == Instruction::PushRedirectScope)
        .unwrap();
    let pop_pos = instrs
        .iter()
        .position(|i| *i == Instruction::PopRedirectScope)
        .unwrap();
    let has_add_redir = instrs[push_pos..pop_pos]
        .iter()
        .any(|i| matches!(i, Instruction::AddRedir(_)));
    assert!(has_add_redir, "expected AddRedir between scope instructions");
}

// ---------------------------------------------------------------------------
// Compound command without redirect has no scope instructions
// ---------------------------------------------------------------------------

#[test]
fn compound_without_redirect_has_no_scope() {
    let module = lower_command("{ cmd; }\n");
    let instrs = top_instructions(&module);
    assert!(
        !instrs.contains(&Instruction::PushRedirectScope),
        "expected no PushRedirectScope: {instrs:?}"
    );
    assert!(
        !instrs.contains(&Instruction::PopRedirectScope),
        "expected no PopRedirectScope: {instrs:?}"
    );
}

// ---------------------------------------------------------------------------
// Default fd for dup without explicit fd
// ---------------------------------------------------------------------------

#[test]
fn dup_output_default_fd() {
    let module = lower_command("cmd >&2\n");
    let ops = first_redirect_ops(&module);
    assert_eq!(
        ops[0],
        RedirectProgramOp::Dup { from: 2, to: 1 }
    );
}

#[test]
fn dup_input_default_fd() {
    let module = lower_command("cmd <&3\n");
    let ops = first_redirect_ops(&module);
    assert_eq!(
        ops[0],
        RedirectProgramOp::Dup { from: 3, to: 0 }
    );
}
