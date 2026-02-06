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
fn grammar_stub_returns_not_implemented_when_tokens_exist() {
    let mut parser = parser_for("echo hi\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("grammar phase is intentionally stubbed");

    assert_eq!(error.kind, ParseErrorKind::GrammarNotImplemented);
    assert_eq!(error.found.as_deref(), Some("echo"));
}
