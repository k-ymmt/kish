use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{
    CommandAst, CompleteCommandAst, NeedMoreInputReason, ParseErrorKind, ParseOptions, ParseStep,
    Parser, TokenStream,
};

fn parser_for(input: &str, options: ParseOptions) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), options, token_stream)
}

fn parse_need_more_reason(input: &str) -> NeedMoreInputReason {
    let mut parser = parser_for(
        input,
        ParseOptions {
            interactive: true,
            ..Default::default()
        },
    );
    let step = parser
        .parse_complete_command()
        .expect("interactive parse should not hard-fail");
    let ParseStep::NeedMoreInput(reason) = step else {
        panic!("expected ParseStep::NeedMoreInput, got {step:?}");
    };
    reason
}

fn first_command(step: &ParseStep) -> &CommandAst {
    let ParseStep::Complete(complete) = step else {
        panic!("expected ParseStep::Complete, got {step:?}");
    };
    let and_or = complete.list.and_ors.first().expect("and_or should exist");
    and_or.head.commands.first().expect("command should exist")
}

fn first_simple_word_lexeme(complete: &CompleteCommandAst) -> &str {
    let and_or = complete.list.and_ors.first().expect("and_or should exist");
    let command = and_or.head.commands.first().expect("command should exist");
    match command {
        CommandAst::Simple(simple) => simple.words[0].token.lexeme.as_str(),
        other => panic!("expected simple command, got {other:?}"),
    }
}

#[test]
fn parse_complete_command_returns_end_of_input_on_empty_source() {
    let mut parser = parser_for("", ParseOptions::default());
    let step = parser
        .parse_complete_command()
        .expect("empty source should not error");
    assert_eq!(step, ParseStep::EndOfInput);
}

#[test]
fn parse_complete_command_returns_need_more_input_in_interactive_mode() {
    let mut parser = parser_for(
        "\"unterminated",
        ParseOptions {
            interactive: true,
            ..Default::default()
        },
    );

    let step = parser
        .parse_complete_command()
        .expect("interactive mode should request continuation");
    assert_eq!(
        step,
        ParseStep::NeedMoreInput(NeedMoreInputReason::UnterminatedDoubleQuote)
    );
}

#[test]
fn parse_complete_command_returns_error_in_non_interactive_mode() {
    let mut parser = parser_for(
        "\"unterminated",
        ParseOptions {
            interactive: false,
            ..Default::default()
        },
    );

    let error = parser
        .parse_complete_command()
        .expect_err("non-interactive mode should fail");
    assert_eq!(error.kind, ParseErrorKind::UnexpectedEndOfInput);
}

#[test]
fn parse_complete_command_returns_complete_for_simple_command() {
    let mut parser = parser_for("echo hi\n", ParseOptions::default());
    let step = parser
        .parse_complete_command()
        .expect("simple command should parse");
    assert!(matches!(step, ParseStep::Complete(_)));
}

#[test]
fn interactive_incomplete_constructs_return_specific_need_more_reason() {
    let cases = [
        ("(echo\n", NeedMoreInputReason::UnclosedSubshell),
        ("{ echo\n", NeedMoreInputReason::UnclosedBraceGroup),
        (
            "if echo a; then echo b;\n",
            NeedMoreInputReason::UnclosedIfClause,
        ),
        (
            "case x in a) echo a ;;\n",
            NeedMoreInputReason::UnclosedCaseClause,
        ),
        (
            "while echo a; do echo b;\n",
            NeedMoreInputReason::UnclosedDoGroup,
        ),
    ];

    for (input, expected) in cases {
        let reason = parse_need_more_reason(input);
        assert_eq!(reason, expected, "input: {input}");
    }
}

#[test]
fn interactive_trailing_operators_return_specific_need_more_reason() {
    let cases = [
        ("echo a |\n", NeedMoreInputReason::TrailingPipeOperator),
        ("echo a &&\n", NeedMoreInputReason::TrailingAndIfOperator),
        ("echo a ||\n", NeedMoreInputReason::TrailingOrIfOperator),
    ];

    for (input, expected) in cases {
        let reason = parse_need_more_reason(input);
        assert_eq!(reason, expected, "input: {input}");
    }
}

#[test]
fn non_interactive_incomplete_constructs_still_return_unexpected_end_of_input() {
    let cases = ["(echo\n", "echo a |\n", "while echo a; do echo b;\n"];

    for input in cases {
        let mut parser = parser_for(
            input,
            ParseOptions {
                interactive: false,
                ..Default::default()
            },
        );
        let error = parser
            .parse_complete_command()
            .expect_err("non-interactive mode should fail on incomplete input");
        assert_eq!(
            error.kind,
            ParseErrorKind::UnexpectedEndOfInput,
            "input: {input}"
        );
    }
}

#[test]
fn boundary_preserves_following_command_after_trailing_semicolon_newline() {
    let mut parser = parser_for("a;\nb\n", ParseOptions::default());

    let first = parser
        .parse_complete_command()
        .expect("first command should parse");
    let second = parser
        .parse_complete_command()
        .expect("second command should parse");
    let third = parser
        .parse_complete_command()
        .expect("third call should return end of input");

    let ParseStep::Complete(first_complete) = first else {
        panic!("expected first step to be complete");
    };
    assert_eq!(
        first_complete.separator_op,
        Some(kish::lexer::OperatorKind::Semicolon)
    );
    let ParseStep::Complete(second_complete) = second else {
        panic!("expected second step to be complete");
    };
    assert_eq!(first_simple_word_lexeme(&second_complete), "b");
    assert_eq!(third, ParseStep::EndOfInput);
}

#[test]
fn boundary_preserves_following_command_after_trailing_ampersand_newline() {
    let mut parser = parser_for("a&\nb\n", ParseOptions::default());

    let first = parser
        .parse_complete_command()
        .expect("first command should parse");
    let second = parser
        .parse_complete_command()
        .expect("second command should parse");

    let ParseStep::Complete(first_complete) = first else {
        panic!("expected first step to be complete");
    };
    assert_eq!(
        first_complete.separator_op,
        Some(kish::lexer::OperatorKind::Ampersand)
    );
    let ParseStep::Complete(second_complete) = second else {
        panic!("expected second step to be complete");
    };
    assert_eq!(first_simple_word_lexeme(&second_complete), "b");
}

#[test]
fn boundary_keeps_second_command_after_function_definition() {
    let mut parser = parser_for("foo() { echo x; }\necho y\n", ParseOptions::default());

    let first = parser
        .parse_complete_command()
        .expect("function definition should parse");
    let second = parser
        .parse_complete_command()
        .expect("following command should parse");

    assert!(matches!(
        first_command(&first),
        CommandAst::FunctionDefinition(_)
    ));
    let ParseStep::Complete(second_complete) = second else {
        panic!("expected second step to be complete");
    };
    assert_eq!(first_simple_word_lexeme(&second_complete), "echo");
}

#[test]
fn boundary_stepping_is_stable_with_leading_and_trailing_blank_lines() {
    let mut parser = parser_for("\n\nalpha\nbeta\n\n", ParseOptions::default());

    let first = parser
        .parse_complete_command()
        .expect("first command should parse");
    let second = parser
        .parse_complete_command()
        .expect("second command should parse");
    let third = parser
        .parse_complete_command()
        .expect("third call should return end");

    let ParseStep::Complete(first_complete) = first else {
        panic!("expected first step to be complete");
    };
    assert_eq!(first_simple_word_lexeme(&first_complete), "alpha");
    let ParseStep::Complete(second_complete) = second else {
        panic!("expected second step to be complete");
    };
    assert_eq!(first_simple_word_lexeme(&second_complete), "beta");
    assert_eq!(third, ParseStep::EndOfInput);
}

#[test]
fn mixed_validity_interactive_returns_complete_then_need_more() {
    let mut parser = parser_for(
        "echo ok\n\"unterminated",
        ParseOptions {
            interactive: true,
            ..Default::default()
        },
    );

    let first = parser
        .parse_complete_command()
        .expect("first command should parse");
    let second = parser
        .parse_complete_command()
        .expect("interactive mode should request continuation");

    assert!(matches!(first, ParseStep::Complete(_)));
    assert_eq!(
        second,
        ParseStep::NeedMoreInput(NeedMoreInputReason::UnterminatedDoubleQuote)
    );
}

#[test]
fn mixed_validity_non_interactive_returns_complete_then_error() {
    let mut parser = parser_for("echo ok\n\"unterminated", ParseOptions::default());

    let first = parser
        .parse_complete_command()
        .expect("first command should parse");
    let second = parser
        .parse_complete_command()
        .expect_err("second command should fail");

    assert!(matches!(first, ParseStep::Complete(_)));
    assert_eq!(second.kind, ParseErrorKind::UnexpectedEndOfInput);
}

#[test]
fn parse_complete_command_sequence_matches_parse_program_order() {
    let input = "alpha\nbeta\ngamma\n";

    let mut incremental = parser_for(input, ParseOptions::default());
    let mut incremental_words = Vec::new();
    loop {
        let step = incremental
            .parse_complete_command()
            .expect("incremental parsing should succeed");
        match step {
            ParseStep::Complete(complete) => {
                incremental_words.push(first_simple_word_lexeme(&complete).to_string());
            }
            ParseStep::EndOfInput => break,
            ParseStep::NeedMoreInput(reason) => {
                panic!("did not expect need-more-input for complete script: {reason:?}")
            }
        }
    }

    let mut whole = parser_for(input, ParseOptions::default());
    let program = whole
        .parse_program()
        .expect("program parsing should succeed");
    let program_words: Vec<String> = program
        .complete_commands
        .iter()
        .map(|complete| first_simple_word_lexeme(complete).to_string())
        .collect();

    assert_eq!(incremental_words, program_words);
    assert_eq!(incremental_words, vec!["alpha", "beta", "gamma"]);
}

fn nested_subshell_input(depth: usize) -> String {
    let mut input = String::new();
    for _ in 0..depth {
        input.push('(');
    }
    input.push_str("echo");
    for _ in 0..depth {
        input.push(')');
    }
    input.push('\n');
    input
}

#[test]
fn max_nesting_guard_rejects_pathological_depth() {
    let input = nested_subshell_input(6);
    let mut parser = parser_for(
        &input,
        ParseOptions {
            max_nesting: 3,
            ..Default::default()
        },
    );

    let error = parser
        .parse_complete_command()
        .expect_err("depth beyond max_nesting should fail fast");
    assert_eq!(error.kind, ParseErrorKind::MaxNestingExceeded);
    assert_eq!(error.expected, vec!["max_nesting <= 3".to_string()]);
    assert_eq!(error.found, Some("nesting depth 4".to_string()));
}

#[test]
fn max_nesting_guard_allows_depth_within_limit() {
    let input = nested_subshell_input(4);
    let mut parser = parser_for(
        &input,
        ParseOptions {
            max_nesting: 4,
            ..Default::default()
        },
    );

    let step = parser
        .parse_complete_command()
        .expect("depth at max_nesting should parse");
    assert!(matches!(step, ParseStep::Complete(_)));
}

#[test]
fn stress_long_simple_command_stream_is_stable() {
    let command_count = 1500usize;
    let mut input = String::new();
    for i in 0..command_count {
        input.push_str("echo ");
        input.push_str(&i.to_string());
        input.push('\n');
    }

    let mut parser = parser_for(&input, ParseOptions::default());
    let mut complete_count = 0usize;
    loop {
        match parser
            .parse_complete_command()
            .expect("long simple-command stream should parse")
        {
            ParseStep::Complete(_) => complete_count += 1,
            ParseStep::EndOfInput => break,
            ParseStep::NeedMoreInput(reason) => {
                panic!("did not expect need-more-input for closed stream: {reason:?}")
            }
        }
    }

    assert_eq!(complete_count, command_count);
}

#[test]
fn stress_deep_nesting_with_default_limit_is_stable() {
    let input = nested_subshell_input(64);
    let mut parser = parser_for(&input, ParseOptions::default());
    let step = parser
        .parse_complete_command()
        .expect("deep nesting under default max_nesting should parse");
    assert!(matches!(step, ParseStep::Complete(_)));
}

#[test]
fn stress_mixed_pipeline_and_list_sequences_is_stable() {
    let command_count = 400usize;
    let mut input = String::new();
    for i in 0..command_count {
        input.push_str(&format!("a{i}|b{i} && c{i} || d{i}; e{i} &\n"));
    }

    let mut parser = parser_for(&input, ParseOptions::default());
    let mut complete_count = 0usize;
    loop {
        match parser
            .parse_complete_command()
            .expect("mixed pipeline/list stream should parse")
        {
            ParseStep::Complete(_) => complete_count += 1,
            ParseStep::EndOfInput => break,
            ParseStep::NeedMoreInput(reason) => {
                panic!("did not expect need-more-input for closed stream: {reason:?}")
            }
        }
    }

    assert_eq!(complete_count, command_count);
}
