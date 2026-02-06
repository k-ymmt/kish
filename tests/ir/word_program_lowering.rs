//! Phase 7 tests: WordProgram lowering integration tests.

use kish::ir::{
    Instruction, IrModule, IrOptions, LoweringContext, WordProgramOp,
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

/// Returns the ops for a word program by index.
fn word_ops(module: &IrModule, index: usize) -> &[WordProgramOp] {
    &module.word_programs[index].ops
}

/// Checks that a word program contains an ExpandParameter with the given symbol name.
fn has_expand_parameter(module: &IrModule, wp_index: usize, param_name: &str) -> bool {
    word_ops(module, wp_index).iter().any(|op| match op {
        WordProgramOp::ExpandParameter(sym_id) => {
            module.symbol_at(*sym_id) == Some(param_name)
        }
        _ => false,
    })
}

/// Checks that a word program contains a PushLiteral with the given string.
fn has_push_literal(module: &IrModule, wp_index: usize, literal: &str) -> bool {
    word_ops(module, wp_index).iter().any(|op| match op {
        WordProgramOp::PushLiteral(str_id) => {
            module.string_at(*str_id) == Some(literal)
        }
        _ => false,
    })
}

// ---------------------------------------------------------------------------
// Plain literals
// ---------------------------------------------------------------------------

#[test]
fn plain_literal_word_contains_push_literal() {
    let module = lower_command("echo hello\n");
    // "echo" is word program 0, "hello" is word program 1.
    assert_eq!(module.word_programs.len(), 2);
    assert!(has_push_literal(&module, 0, "echo"));
    assert!(has_push_literal(&module, 1, "hello"));
}

#[test]
fn plain_literal_has_trailing_field_split_glob_quote_removal() {
    let module = lower_command("echo hello\n");
    let ops = word_ops(&module, 0);
    let len = ops.len();
    assert!(len >= 4);
    assert_eq!(ops[len - 3], WordProgramOp::FieldSplit);
    assert_eq!(ops[len - 2], WordProgramOp::Glob);
    assert_eq!(ops[len - 1], WordProgramOp::QuoteRemoval);
}

// ---------------------------------------------------------------------------
// Bare parameter expansion
// ---------------------------------------------------------------------------

#[test]
fn bare_parameter_expansion() {
    let module = lower_command("echo $HOME\n");
    // word program 0: "echo", word program 1: "$HOME"
    assert_eq!(module.word_programs.len(), 2);
    assert!(has_expand_parameter(&module, 1, "HOME"));
}

// ---------------------------------------------------------------------------
// Braced parameter expansion
// ---------------------------------------------------------------------------

#[test]
fn braced_parameter_expansion() {
    let module = lower_command("echo ${HOME}\n");
    assert_eq!(module.word_programs.len(), 2);
    assert!(has_expand_parameter(&module, 1, "HOME"));
}

// ---------------------------------------------------------------------------
// Mixed literal + expansion
// ---------------------------------------------------------------------------

#[test]
fn mixed_literal_and_expansion() {
    let module = lower_command("echo hello$W\n");
    assert_eq!(module.word_programs.len(), 2);
    // Word program 1 should have both PushLiteral("hello") and ExpandParameter("W").
    assert!(has_push_literal(&module, 1, "hello"));
    assert!(has_expand_parameter(&module, 1, "W"));
}

// ---------------------------------------------------------------------------
// Single-quoted (no expansion)
// ---------------------------------------------------------------------------

#[test]
fn single_quoted_no_expansion() {
    let module = lower_command("echo '$HOME'\n");
    assert_eq!(module.word_programs.len(), 2);
    // Word program 1 should contain PushLiteral("$HOME"), NOT ExpandParameter.
    assert!(has_push_literal(&module, 1, "$HOME"));
    assert!(!word_ops(&module, 1)
        .iter()
        .any(|op| matches!(op, WordProgramOp::ExpandParameter(_))));
}

// ---------------------------------------------------------------------------
// Assignment value
// ---------------------------------------------------------------------------

#[test]
fn assignment_value_plain() {
    let module = lower_command("FOO=bar\n");
    // One word program for assignment value.
    assert_eq!(module.word_programs.len(), 1);
    assert!(has_push_literal(&module, 0, "bar"));
    // Assignment value should have QuoteRemoval but NOT FieldSplit or Glob.
    let ops = word_ops(&module, 0);
    assert!(ops.contains(&WordProgramOp::QuoteRemoval));
    assert!(!ops.contains(&WordProgramOp::FieldSplit));
    assert!(!ops.contains(&WordProgramOp::Glob));
}

#[test]
fn assignment_with_expansion() {
    let module = lower_command("FOO=$BAR\n");
    assert_eq!(module.word_programs.len(), 1);
    assert!(has_expand_parameter(&module, 0, "BAR"));
    let ops = word_ops(&module, 0);
    assert!(ops.contains(&WordProgramOp::QuoteRemoval));
    assert!(!ops.contains(&WordProgramOp::FieldSplit));
}

// ---------------------------------------------------------------------------
// Tilde expansion
// ---------------------------------------------------------------------------

#[test]
fn tilde_expansion() {
    let module = lower_command("echo ~\n");
    assert_eq!(module.word_programs.len(), 2);
    // Word program 1 should have ExpandTilde.
    assert!(word_ops(&module, 1)
        .iter()
        .any(|op| matches!(op, WordProgramOp::ExpandTilde(_))));
}

// ---------------------------------------------------------------------------
// For-loop words
// ---------------------------------------------------------------------------

#[test]
fn for_loop_word_expansion() {
    let module = lower_command("for i in a $B; do echo x; done\n");
    // Word programs: "a" (for word), "$B" (for word), "echo" (arg), "x" (arg)
    // Find the word program that has ExpandParameter("B").
    let has_b = module.word_programs.iter().any(|wp| {
        wp.ops.iter().any(|op| match op {
            WordProgramOp::ExpandParameter(sym_id) => {
                module.symbol_at(*sym_id) == Some("B")
            }
            _ => false,
        })
    });
    assert!(has_b, "should have a word program with ExpandParameter(B)");
}

// ---------------------------------------------------------------------------
// Case subject
// ---------------------------------------------------------------------------

#[test]
fn case_subject_has_word_program() {
    let module = lower_command("case $x in a) echo y;; esac\n");
    let instrs = &module.code_objects[0].instructions;
    // CaseSetSubject should carry a WordProgramId.
    assert!(instrs
        .iter()
        .any(|i| matches!(i, Instruction::CaseSetSubject(_))));
    // The word program for the subject should have ExpandParameter("x").
    let has_x = module.word_programs.iter().any(|wp| {
        wp.ops.iter().any(|op| match op {
            WordProgramOp::ExpandParameter(sym_id) => {
                module.symbol_at(*sym_id) == Some("x")
            }
            _ => false,
        })
    });
    assert!(has_x, "case subject word program should expand $x");
}

// ---------------------------------------------------------------------------
// Context-specific trailing ops
// ---------------------------------------------------------------------------

#[test]
fn command_argument_has_field_split_and_glob() {
    let module = lower_command("echo hello\n");
    let ops = word_ops(&module, 0);
    assert!(ops.contains(&WordProgramOp::FieldSplit));
    assert!(ops.contains(&WordProgramOp::Glob));
    assert!(ops.contains(&WordProgramOp::QuoteRemoval));
}

#[test]
fn assignment_value_has_no_field_split_or_glob() {
    let module = lower_command("A=val\n");
    let ops = word_ops(&module, 0);
    assert!(!ops.contains(&WordProgramOp::FieldSplit));
    assert!(!ops.contains(&WordProgramOp::Glob));
    assert!(ops.contains(&WordProgramOp::QuoteRemoval));
}

// ---------------------------------------------------------------------------
// Word programs are no longer empty
// ---------------------------------------------------------------------------

#[test]
fn word_programs_not_empty_for_simple_command() {
    let module = lower_command("echo hello\n");
    for wp in &module.word_programs {
        assert!(!wp.ops.is_empty(), "word program should not be empty");
    }
}

#[test]
fn word_program_counts_preserved() {
    // Same counts as before: 2 args = 2 word programs.
    let module = lower_command("ls -la\n");
    assert_eq!(module.word_programs.len(), 2);
}

#[test]
fn assignment_value_word_program_count() {
    let module = lower_command("A=1 B=2\n");
    assert_eq!(module.word_programs.len(), 2);
}
