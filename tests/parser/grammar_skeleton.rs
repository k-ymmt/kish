use kish::lexer::{Lexer, LexerMode, OperatorKind, SourceId};
use kish::parser::{CommandAst, ParseErrorKind, ParseOptions, ParseStep, Parser, TokenStream};

fn parser_for(input: &str, options: ParseOptions) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), options, token_stream)
}

fn parse_one(input: &str) -> ParseStep {
    let mut parser = parser_for(input, ParseOptions::default());
    parser
        .parse_complete_command()
        .expect("complete command should parse")
}

fn first_simple(step: &ParseStep) -> &kish::parser::SimpleCommandAst {
    let ParseStep::Complete(command) = step else {
        panic!("expected ParseStep::Complete");
    };

    let and_or = command.list.and_ors.first().expect("and_or exists");
    let pipeline = &and_or.head;
    match pipeline.commands.first().expect("command exists") {
        CommandAst::Simple(simple) => simple,
        other => panic!("expected simple command, got {other:?}"),
    }
}

#[test]
fn parse_program_handles_leading_and_trailing_blank_lines() {
    let mut parser = parser_for("\n\n echo hi\n\n", ParseOptions::default());
    let program = parser.parse_program().expect("program should parse");
    assert_eq!(program.complete_commands.len(), 1);
}

#[test]
fn one_word_simple_command_parses() {
    let step = parse_one("echo\n");
    let simple = first_simple(&step);
    assert_eq!(simple.words.len(), 1);
    assert_eq!(simple.words[0].token.lexeme, "echo");
}

#[test]
fn word_sequence_simple_command_parses() {
    let step = parse_one("echo hi there\n");
    let simple = first_simple(&step);
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo", "hi", "there"]);
}

#[test]
fn pipeline_parses_two_commands() {
    let step = parse_one("a|b\n");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command");
    };

    let pipeline = &command.list.and_ors[0].head;
    assert_eq!(pipeline.commands.len(), 2);
}

#[test]
fn and_or_chain_parses() {
    let step = parse_one("a && b || c\n");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command");
    };

    let and_or = &command.list.and_ors[0];
    assert_eq!(and_or.tail.len(), 2);
    assert_eq!(and_or.tail[0].0, OperatorKind::AndIf);
    assert_eq!(and_or.tail[1].0, OperatorKind::OrIf);
}

#[test]
fn list_with_separators_parses_multiple_and_ors() {
    let step = parse_one("a;b&c\n");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command");
    };

    assert_eq!(command.list.and_ors.len(), 3);
}

#[test]
fn parse_complete_command_is_boundary_safe_across_multiple_commands() {
    let mut parser = parser_for("a\nb\n", ParseOptions::default());

    let first = parser
        .parse_complete_command()
        .expect("first command should parse");
    let second = parser
        .parse_complete_command()
        .expect("second command should parse");
    let third = parser
        .parse_complete_command()
        .expect("third call should return end");

    assert!(matches!(first, ParseStep::Complete(_)));
    assert!(matches!(second, ParseStep::Complete(_)));
    assert_eq!(third, ParseStep::EndOfInput);
}

#[test]
fn unimplemented_reserved_word_fails_fast() {
    let mut parser = parser_for("if true\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("if clause should be deferred");

    assert_eq!(error.kind, ParseErrorKind::GrammarNotImplemented);
    assert_eq!(error.found.as_deref(), Some("if"));
}

#[test]
fn redirection_in_command_fails_fast() {
    let mut parser = parser_for("echo > out\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("redirects are deferred to phase 5");

    assert_eq!(error.kind, ParseErrorKind::GrammarNotImplemented);
    assert_eq!(error.found.as_deref(), Some(">"));
}

#[test]
fn pipeline_negation_sets_flag() {
    let step = parse_one("! echo\n");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command");
    };

    assert!(command.list.and_ors[0].head.negated);
}

#[test]
fn node_limit_failure_stays_deterministic_on_valid_parse_path() {
    let mut parser = parser_for(
        "echo hi there\n",
        ParseOptions {
            max_ast_nodes: 2,
            ..Default::default()
        },
    );

    let error = parser
        .parse_complete_command()
        .expect_err("low node limit should fail");
    assert_eq!(error.kind, ParseErrorKind::AstNodeLimitExceeded);
}
