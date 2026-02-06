use kish::lexer::{Lexer, LexerMode, OperatorKind, SourceId};
use kish::parser::{CommandAst, ParseErrorKind, ParseOptions, ParseStep, Parser, TokenStream};

fn parser_for(input: &str, options: ParseOptions) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), options, token_stream)
}

fn parse_one(input: &str) -> ParseStep {
    parse_one_with_options(input, ParseOptions::default())
}

fn parse_one_with_options(input: &str, options: ParseOptions) -> ParseStep {
    let mut parser = parser_for(input, options);
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
fn redirection_in_command_parses() {
    let step = parse_one("echo > out\n");
    let simple = first_simple(&step);

    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo"]);
    assert_eq!(simple.redirects.len(), 1);
    assert_eq!(simple.redirects[0].operator, OperatorKind::Greater);
    assert_eq!(simple.redirects[0].target.token.lexeme, "out");
    assert!(simple.redirects[0].fd_or_location.is_none());
}

#[test]
fn redirect_prefix_form_parses() {
    let step = parse_one(">out echo\n");
    let simple = first_simple(&step);

    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo"]);
    assert_eq!(simple.redirects.len(), 1);
    assert_eq!(simple.redirects[0].operator, OperatorKind::Greater);
    assert_eq!(simple.redirects[0].target.token.lexeme, "out");
}

#[test]
fn assignment_only_simple_command_parses() {
    let step = parse_one("VAR=1\n");
    let simple = first_simple(&step);

    assert_eq!(simple.assignments.len(), 1);
    assert_eq!(simple.assignments[0].name, "VAR");
    assert_eq!(simple.assignments[0].value, "1");
    assert!(simple.words.is_empty());
    assert!(simple.redirects.is_empty());
}

#[test]
fn assignment_prefix_with_command_words_parses() {
    let step = parse_one("VAR=1 echo a b\n");
    let simple = first_simple(&step);

    assert_eq!(simple.assignments.len(), 1);
    assert_eq!(simple.assignments[0].name, "VAR");
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo", "a", "b"]);
}

#[test]
fn assignment_like_word_in_suffix_stays_word() {
    let step = parse_one("echo a=1\n");
    let simple = first_simple(&step);

    assert!(simple.assignments.is_empty());
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo", "a=1"]);
}

#[test]
fn io_number_redirect_parses() {
    let step = parse_one("2>out echo\n");
    let simple = first_simple(&step);

    assert_eq!(simple.redirects.len(), 1);
    assert_eq!(simple.redirects[0].operator, OperatorKind::Greater);
    assert_eq!(simple.redirects[0].target.token.lexeme, "out");
    assert_eq!(
        simple.redirects[0]
            .fd_or_location
            .as_ref()
            .map(|token| token.lexeme.as_str()),
        Some("2")
    );
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo"]);
}

#[test]
fn io_location_redirect_is_gated_by_option() {
    let disabled = parse_one("{fd}>out echo\n");
    let disabled_simple = first_simple(&disabled);
    assert_eq!(disabled_simple.redirects.len(), 1);
    assert!(disabled_simple.redirects[0].fd_or_location.is_none());
    let disabled_words: Vec<&str> = disabled_simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(disabled_words, vec!["{fd}", "echo"]);

    let enabled = parse_one_with_options(
        "{fd}>out echo\n",
        ParseOptions {
            allow_io_location: true,
            ..Default::default()
        },
    );
    let enabled_simple = first_simple(&enabled);
    assert_eq!(enabled_simple.redirects.len(), 1);
    assert_eq!(
        enabled_simple.redirects[0]
            .fd_or_location
            .as_ref()
            .map(|token| token.lexeme.as_str()),
        Some("{fd}")
    );
    let enabled_words: Vec<&str> = enabled_simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(enabled_words, vec!["echo"]);
}

#[test]
fn redirect_target_word_can_be_numeric() {
    let step = parse_one("echo > 2 > out\n");
    let simple = first_simple(&step);

    assert_eq!(simple.redirects.len(), 2);
    assert_eq!(simple.redirects[0].target.token.lexeme, "2");
    assert_eq!(simple.redirects[1].target.token.lexeme, "out");
}

#[test]
fn redirect_order_is_preserved_across_prefix_and_suffix() {
    let step = parse_one("a=1 >x cmd y <z\n");
    let simple = first_simple(&step);

    assert_eq!(simple.assignments.len(), 1);
    assert_eq!(simple.assignments[0].name, "a");
    assert_eq!(simple.redirects.len(), 2);
    assert_eq!(simple.redirects[0].operator, OperatorKind::Greater);
    assert_eq!(simple.redirects[0].target.token.lexeme, "x");
    assert_eq!(simple.redirects[1].operator, OperatorKind::Less);
    assert_eq!(simple.redirects[1].target.token.lexeme, "z");
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["cmd", "y"]);
}

#[test]
fn redirect_missing_target_reports_deterministic_error() {
    let mut parser = parser_for("echo >\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("redirect target is required");
    assert!(matches!(
        error.kind,
        ParseErrorKind::UnexpectedEndOfInput | ParseErrorKind::UnexpectedToken
    ));
}

#[test]
fn function_definition_head_is_detected_and_deferred() {
    let mut parser = parser_for("foo()\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("function definitions are deferred to phase 7");

    assert_eq!(error.kind, ParseErrorKind::GrammarNotImplemented);
    assert_eq!(error.found.as_deref(), Some("foo"));
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
