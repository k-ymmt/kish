//! Tests for parser AST to HIR lowering (Phase 4).

use kish::ir::{
    HirAndOrConnector, HirCommand, HirCompoundCommand, HirListTerminator, HirOriginKind,
    HirRedirectOperator, IrOptions, LoweringContext,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};

fn parser_for(input: &str) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), ParseOptions::default(), stream)
}

fn parse_complete_command(input: &str) -> kish::parser::CompleteCommandAst {
    let mut parser = parser_for(input);
    let step = parser
        .parse_complete_command()
        .expect("parser fixture should parse");

    let ParseStep::Complete(command) = step else {
        panic!("expected one complete command, got {step:?}");
    };

    command
}

fn lower_input(input: &str) -> kish::ir::HirProgram {
    let mut parser = parser_for(input);
    let program = parser.parse_program().expect("program fixture should parse");
    let mut context = LoweringContext::new(IrOptions::default());
    context
        .lower_program_to_hir(&program)
        .expect("lowering should succeed")
}

fn lower_complete(input: &str) -> kish::ir::HirCompleteCommand {
    let command = parse_complete_command(input);
    let mut context = LoweringContext::new(IrOptions::default());
    context
        .lower_complete_command_to_hir(&command)
        .expect("lowering should succeed")
}

// ---------------------------------------------------------------------------
// Simple commands
// ---------------------------------------------------------------------------

#[test]
fn simple_command_echo_hello() {
    let hir = lower_complete("echo hello\n");
    let list = &hir.list;
    assert_eq!(list.and_ors.len(), 1);

    let pipeline = &list.and_ors[0].head;
    assert!(!pipeline.negated);
    assert_eq!(pipeline.commands.len(), 1);

    let HirCommand::Simple(ref cmd) = pipeline.commands[0] else {
        panic!("expected Simple command");
    };
    assert_eq!(cmd.words.len(), 2);
    assert_eq!(cmd.words[0].token.lexeme, "echo");
    assert_eq!(cmd.words[1].token.lexeme, "hello");
    assert!(cmd.assignments.is_empty());
    assert!(cmd.redirects.is_empty());
}

#[test]
fn simple_command_with_assignment() {
    let hir = lower_complete("x=1 cmd\n");
    let HirCommand::Simple(ref cmd) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Simple command");
    };
    assert_eq!(cmd.assignments.len(), 1);
    assert_eq!(cmd.assignments[0].name, "x");
    assert_eq!(cmd.assignments[0].value, "1");
    assert_eq!(cmd.words.len(), 1);
    assert_eq!(cmd.words[0].token.lexeme, "cmd");
}

#[test]
fn simple_command_with_output_redirect() {
    let hir = lower_complete("cmd >out\n");
    let HirCommand::Simple(ref cmd) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Simple command");
    };
    assert_eq!(cmd.words.len(), 1);
    assert_eq!(cmd.redirects.len(), 1);
    assert_eq!(cmd.redirects[0].operator, HirRedirectOperator::Output);
    assert_eq!(cmd.redirects[0].target.token.lexeme, "out");
    assert!(cmd.redirects[0].fd.is_none());
}

#[test]
fn simple_command_with_fd_redirect() {
    let hir = lower_complete("cmd 2>/dev/null\n");
    let HirCommand::Simple(ref cmd) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Simple command");
    };
    assert_eq!(cmd.redirects.len(), 1);
    assert_eq!(cmd.redirects[0].fd, Some(2));
    assert_eq!(cmd.redirects[0].operator, HirRedirectOperator::Output);
    assert_eq!(cmd.redirects[0].target.token.lexeme, "/dev/null");
}

// ---------------------------------------------------------------------------
// Pipelines
// ---------------------------------------------------------------------------

#[test]
fn pipeline_two_commands() {
    let hir = lower_complete("echo a | cat\n");
    let pipeline = &hir.list.and_ors[0].head;
    assert!(!pipeline.negated);
    assert_eq!(pipeline.commands.len(), 2);
}

#[test]
fn pipeline_negated() {
    let hir = lower_complete("! echo a\n");
    let pipeline = &hir.list.and_ors[0].head;
    assert!(pipeline.negated);
    assert_eq!(pipeline.commands.len(), 1);
}

// ---------------------------------------------------------------------------
// AND-OR chains
// ---------------------------------------------------------------------------

#[test]
fn and_or_chain() {
    let hir = lower_complete("true && echo yes || echo no\n");
    let and_or = &hir.list.and_ors[0];
    assert_eq!(and_or.tail.len(), 2);
    assert_eq!(and_or.tail[0].0, HirAndOrConnector::AndIf);
    assert_eq!(and_or.tail[1].0, HirAndOrConnector::OrIf);
}

// ---------------------------------------------------------------------------
// Lists and complete commands
// ---------------------------------------------------------------------------

#[test]
fn list_with_semicolons() {
    let hir = lower_complete("echo a; echo b\n");
    assert_eq!(hir.list.and_ors.len(), 2);
}

#[test]
fn complete_command_with_background() {
    let hir = lower_complete("echo hello &\n");
    assert_eq!(hir.separator_op, Some(HirListTerminator::Ampersand));
}

// ---------------------------------------------------------------------------
// Compound commands
// ---------------------------------------------------------------------------

#[test]
fn compound_subshell() {
    let hir = lower_complete("(echo a)\n");
    let HirCommand::Compound(ref node) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Compound command");
    };
    assert!(matches!(node.kind, HirCompoundCommand::Subshell(_)));
}

#[test]
fn compound_brace_group() {
    let hir = lower_complete("{ echo a; }\n");
    let HirCommand::Compound(ref node) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Compound command");
    };
    assert!(matches!(node.kind, HirCompoundCommand::BraceGroup(_)));
}

#[test]
fn compound_if_simple() {
    let hir = lower_complete("if true; then echo y; fi\n");
    let HirCommand::Compound(ref node) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Compound command");
    };
    let HirCompoundCommand::If(ref clause) = node.kind else {
        panic!("expected If compound");
    };
    assert!(clause.elif_arms.is_empty());
    assert!(clause.else_body.is_none());
}

#[test]
fn compound_if_full() {
    let hir =
        lower_complete("if true; then echo y; elif false; then echo n; else echo x; fi\n");
    let HirCommand::Compound(ref node) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Compound command");
    };
    let HirCompoundCommand::If(ref clause) = node.kind else {
        panic!("expected If compound");
    };
    assert_eq!(clause.elif_arms.len(), 1);
    assert!(clause.else_body.is_some());
}

#[test]
fn compound_for() {
    let hir = lower_complete("for i in a b; do echo $i; done\n");
    let HirCommand::Compound(ref node) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Compound command");
    };
    let HirCompoundCommand::For(ref clause) = node.kind else {
        panic!("expected For compound");
    };
    assert_eq!(clause.name.token.lexeme, "i");
    assert_eq!(clause.words.len(), 2);
}

#[test]
fn compound_while() {
    let hir = lower_complete("while true; do echo loop; done\n");
    let HirCommand::Compound(ref node) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Compound command");
    };
    assert!(matches!(node.kind, HirCompoundCommand::While(_)));
}

#[test]
fn compound_until() {
    let hir = lower_complete("until false; do echo loop; done\n");
    let HirCommand::Compound(ref node) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Compound command");
    };
    assert!(matches!(node.kind, HirCompoundCommand::Until(_)));
}

#[test]
fn compound_case() {
    let hir = lower_complete("case x in a) echo a;; esac\n");
    let HirCommand::Compound(ref node) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Compound command");
    };
    let HirCompoundCommand::Case(ref clause) = node.kind else {
        panic!("expected Case compound");
    };
    assert_eq!(clause.word.token.lexeme, "x");
    assert_eq!(clause.items.len(), 1);
    assert_eq!(clause.items[0].patterns.len(), 1);
    assert_eq!(clause.items[0].patterns[0].token.lexeme, "a");
}

// ---------------------------------------------------------------------------
// Function definition
// ---------------------------------------------------------------------------

#[test]
fn function_definition() {
    let hir = lower_complete("f() { echo hello; }\n");
    let HirCommand::FunctionDefinition(ref func) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected FunctionDefinition command");
    };
    assert_eq!(func.name.token.lexeme, "f");
    assert!(matches!(func.body.kind, HirCompoundCommand::BraceGroup(_)));
}

// ---------------------------------------------------------------------------
// Origin tracking
// ---------------------------------------------------------------------------

#[test]
fn origin_tracking_simple_command() {
    let hir = lower_complete("echo hello\n");
    assert_eq!(hir.origin.kind, HirOriginKind::CompleteCommandAst);
    assert_eq!(hir.list.origin.kind, HirOriginKind::ListAst);
    assert_eq!(hir.list.and_ors[0].origin.kind, HirOriginKind::AndOrAst);
    assert_eq!(
        hir.list.and_ors[0].head.origin.kind,
        HirOriginKind::PipelineAst
    );

    let HirCommand::Simple(ref cmd) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Simple command");
    };
    assert_eq!(cmd.origin.kind, HirOriginKind::SimpleCommandAst);
    assert_eq!(cmd.words[0].origin.kind, HirOriginKind::WordAst);
}

#[test]
fn origin_tracking_compound_if() {
    let hir = lower_complete("if true; then echo y; fi\n");
    let HirCommand::Compound(ref node) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Compound command");
    };
    assert_eq!(
        node.origin.kind,
        HirOriginKind::CompoundCommandNodeAst
    );
    let HirCompoundCommand::If(ref clause) = node.kind else {
        panic!("expected If");
    };
    assert_eq!(clause.origin.kind, HirOriginKind::IfClauseAst);
}

#[test]
fn origin_tracking_function_definition() {
    let hir = lower_complete("f() { echo hello; }\n");
    let HirCommand::FunctionDefinition(ref func) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected FunctionDefinition");
    };
    assert_eq!(func.origin.kind, HirOriginKind::FunctionDefinitionAst);
}

#[test]
fn origin_tracking_assignment_and_redirect() {
    let hir = lower_complete("x=1 cmd >out\n");
    let HirCommand::Simple(ref cmd) = hir.list.and_ors[0].head.commands[0] else {
        panic!("expected Simple command");
    };
    assert_eq!(
        cmd.assignments[0].origin.kind,
        HirOriginKind::AssignmentWordAst
    );
    assert_eq!(cmd.redirects[0].origin.kind, HirOriginKind::RedirectAst);
}

// ---------------------------------------------------------------------------
// Empty and multi-command programs
// ---------------------------------------------------------------------------

#[test]
fn empty_program() {
    let hir = lower_input("");
    assert!(hir.complete_commands.is_empty());
    assert_eq!(hir.origin.kind, HirOriginKind::ProgramAst);
}

#[test]
fn multiple_complete_commands() {
    let hir = lower_input("echo a\necho b\n");
    assert_eq!(hir.complete_commands.len(), 2);
    assert_eq!(hir.origin.kind, HirOriginKind::ProgramAst);
}
