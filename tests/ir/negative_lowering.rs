//! Negative lowering tests: each parser AST family has at least one test that
//! triggers a `LimitExceeded` error via restrictive `IrOptions`.
//!
//! The lowering pipeline uses placeholder code objects (0 instructions) that are
//! later replaced, so `max_instructions` is not enforced during lowering.
//! Instead, we use `max_code_objects`, `max_word_program_ops`, `max_redirect_ops`,
//! and `max_arith_program_ops` which are all checked strictly on allocation.

use kish::ir::{IrErrorKind, IrModule, IrOptions, LoweringContext};
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

fn try_lower_program_with(
    input: &str,
    options: IrOptions,
) -> Result<IrModule, kish::ir::IrError> {
    let mut parser = parser_for(input);
    let program = parser.parse_program().expect("parse should succeed");
    let mut context = LoweringContext::new(options);
    context.lower_program(&program)
}

fn try_lower_command_with(
    input: &str,
    options: IrOptions,
) -> Result<IrModule, kish::ir::IrError> {
    let mut parser = parser_for(input);
    let step = parser
        .parse_complete_command()
        .expect("parse should succeed");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command, got {step:?}");
    };
    let mut context = LoweringContext::new(options);
    context.lower_complete_command(&command)
}

fn assert_limit_exceeded(result: Result<IrModule, kish::ir::IrError>) {
    let err = result.expect_err("lowering should fail with LimitExceeded");
    assert_eq!(
        err.kind,
        IrErrorKind::LimitExceeded,
        "expected LimitExceeded, got {err:?}"
    );
}

// ---------------------------------------------------------------------------
// ProgramAst — max_code_objects=0 prevents even the first allocation
// ---------------------------------------------------------------------------

#[test]
fn program_exceeds_code_object_limit() {
    let opts = IrOptions {
        max_code_objects: 0,
        ..IrOptions::default()
    };
    let result = try_lower_program_with("echo a\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// CompleteCommandAst — word program op limit blocks word lowering
// ---------------------------------------------------------------------------

#[test]
fn complete_command_exceeds_word_program_ops() {
    let opts = IrOptions {
        max_word_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("echo hello\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// ListAst — word program op limit fires during multi-command list
// ---------------------------------------------------------------------------

#[test]
fn list_exceeds_word_program_ops() {
    let opts = IrOptions {
        max_word_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("a; b; c; d\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// PipelineAst — exceeds code object limit
// ---------------------------------------------------------------------------

#[test]
fn pipeline_exceeds_code_object_limit() {
    // `a | b | c` creates 1 top-level + 3 child code objects = 4 total.
    // max_code_objects=2 triggers the limit on the third allocation.
    let opts = IrOptions {
        max_code_objects: 2,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("a | b | c\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// AndOrAst — word program op limit fires during chain lowering
// ---------------------------------------------------------------------------

#[test]
fn and_or_exceeds_word_program_ops() {
    let opts = IrOptions {
        max_word_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("a && b && c\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// SimpleCommandAst — exceeds word program ops limit
// ---------------------------------------------------------------------------

#[test]
fn simple_command_exceeds_word_program_ops() {
    // Each word creates a word program with ~4 ops. max_word_program_ops=1
    // triggers the limit on the first word program (which has 4 ops > 1).
    let opts = IrOptions {
        max_word_program_ops: 1,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("echo hello\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// AssignmentWordAst — exceeds word program ops limit
// ---------------------------------------------------------------------------

#[test]
fn assignment_exceeds_word_program_limit() {
    let opts = IrOptions {
        max_word_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("FOO=bar\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// RedirectAst — exceeds redirect op limit
// ---------------------------------------------------------------------------

#[test]
fn redirect_exceeds_op_limit() {
    let opts = IrOptions {
        max_redirect_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("cmd >out\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// IfClauseAst — word program op limit fires during condition lowering
// ---------------------------------------------------------------------------

#[test]
fn if_exceeds_word_program_ops() {
    let opts = IrOptions {
        max_word_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("if true; then echo y; fi\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// ForClauseAst — word program op limit fires during word list lowering
// ---------------------------------------------------------------------------

#[test]
fn for_exceeds_word_program_ops() {
    let opts = IrOptions {
        max_word_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("for i in a b c; do echo $i; done\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// WhileClauseAst — word program op limit fires during condition lowering
// ---------------------------------------------------------------------------

#[test]
fn while_exceeds_word_program_ops() {
    let opts = IrOptions {
        max_word_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("while true; do echo loop; done\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// UntilClauseAst — word program op limit fires during condition lowering
// ---------------------------------------------------------------------------

#[test]
fn until_exceeds_word_program_ops() {
    let opts = IrOptions {
        max_word_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("until false; do echo loop; done\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// CaseClauseAst — word program op limit fires during subject/pattern lowering
// ---------------------------------------------------------------------------

#[test]
fn case_exceeds_word_program_ops() {
    let opts = IrOptions {
        max_word_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("case x in a) echo a;; esac\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// FunctionDefinitionAst — exceeds code object limit
// ---------------------------------------------------------------------------

#[test]
fn function_exceeds_code_object_limit() {
    // Function definition creates a body code object in addition to the top-level.
    // max_code_objects=1 allows the top-level but not the function body.
    let opts = IrOptions {
        max_code_objects: 1,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("f() { echo hello; }\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// CompoundCommandAst (Subshell) — exceeds code object limit
// ---------------------------------------------------------------------------

#[test]
fn subshell_exceeds_code_object_limit() {
    let opts = IrOptions {
        max_code_objects: 1,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("(echo sub)\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// CompoundCommandAst (BraceGroup) — word program op limit
// ---------------------------------------------------------------------------

#[test]
fn brace_group_exceeds_word_program_ops() {
    let opts = IrOptions {
        max_word_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("{ echo hello; }\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// CompleteCommandAst (background) — exceeds code object limit
// ---------------------------------------------------------------------------

#[test]
fn background_exceeds_code_object_limit() {
    // Background compiles the list into a separate code object.
    let opts = IrOptions {
        max_code_objects: 1,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("echo bg &\n", opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// ArithProgramOp — exceeds arith program op limit
// ---------------------------------------------------------------------------

#[test]
fn arith_exceeds_op_limit() {
    let opts = IrOptions {
        max_arith_program_ops: 0,
        ..IrOptions::default()
    };
    let result = try_lower_command_with("echo $((1+2*3))\n", opts);
    assert_limit_exceeded(result);
}
