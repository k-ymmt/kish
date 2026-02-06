use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{
    NeedMoreInputReason, ParseErrorKind, ParseOptions, ParseStep, Parser, TokenStream,
};

fn parser_for(input: &str, options: ParseOptions) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), options, token_stream)
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
