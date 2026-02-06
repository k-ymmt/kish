use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseErrorKind, ParseOptions, ParseStep, Parser, TokenStream};

#[test]
fn parse_options_default_matches_phase_contract() {
    let options = ParseOptions::default();
    assert!(!options.interactive);
    assert_eq!(options.max_nesting, 256);
    assert_eq!(options.max_ast_nodes, 100_000);
    assert!(!options.allow_io_location);
}

#[test]
fn parser_constructor_and_source_id_are_available() {
    let lexer = Lexer::new("", LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    let parser = Parser::new(SourceId::new(42), ParseOptions::default(), token_stream);

    assert_eq!(parser.source_id(), SourceId::new(42));
    assert!(parser.diagnostics().is_empty());
}

#[test]
fn parse_program_returns_empty_program_on_empty_input() {
    let lexer = Lexer::new("", LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    let mut parser = Parser::new(SourceId::new(1), ParseOptions::default(), token_stream);

    let program = parser
        .parse_program()
        .expect("empty input should parse as empty program");
    assert!(program.complete_commands.is_empty());
    assert_eq!(program.span, None);
}

#[test]
fn parse_step_and_error_kinds_are_public_contracts() {
    let step = ParseStep::EndOfInput;
    assert_eq!(step, ParseStep::EndOfInput);

    let kind = ParseErrorKind::GrammarNotImplemented;
    assert_eq!(kind, ParseErrorKind::GrammarNotImplemented);

    let nesting_kind = ParseErrorKind::MaxNestingExceeded;
    assert_eq!(nesting_kind, ParseErrorKind::MaxNestingExceeded);
}
