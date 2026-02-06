use kish::lexer::{
    DiagnosticCode, FatalLexError, HereDocEofPolicy, LexStep, Lexer, LexerMode, LexerOptions,
};

#[test]
fn unterminated_single_quote_diagnostic_fields_are_stable() {
    let mut lexer = Lexer::new("'abc", LexerMode::Normal);

    match lexer.next_token() {
        Err(FatalLexError::UnterminatedSingleQuote(diagnostic)) => {
            assert_eq!(diagnostic.code, DiagnosticCode::UnterminatedSingleQuote);
            assert_eq!(diagnostic.message, "unterminated single quote");
            assert_eq!(diagnostic.near_text.as_deref(), Some("'abc"));
            assert_eq!(
                diagnostic.suggestion.as_deref(),
                Some("close the single quote with `'`.")
            );
        }
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn malformed_arithmetic_diagnostic_fields_are_stable() {
    let mut lexer = Lexer::new("$(( ))", LexerMode::Normal);

    match lexer.next_token() {
        Err(FatalLexError::MalformedArithmeticExpansion(diagnostic)) => {
            assert_eq!(
                diagnostic.code,
                DiagnosticCode::MalformedArithmeticExpansion
            );
            assert_eq!(diagnostic.message, "malformed arithmetic expansion");
            assert_eq!(diagnostic.near_text.as_deref(), Some("$(( ))"));
            assert_eq!(
                diagnostic.suggestion.as_deref(),
                Some("use a non-empty arithmetic expression and close it with `))`.")
            );
        }
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn heredoc_warning_diagnostic_fields_are_stable() {
    let mut lexer = Lexer::with_options(
        "cat <<EOF\nbody\n",
        LexerMode::Normal,
        LexerOptions {
            here_doc_eof_policy: HereDocEofPolicy::Warning,
            ..Default::default()
        },
    );

    loop {
        match lexer
            .next_token()
            .expect("scan should succeed in warning mode")
        {
            LexStep::Token(_) => {}
            LexStep::Recoverable(_) => panic!("unexpected recoverable state"),
            LexStep::EndOfInput => break,
        }
    }

    let warnings = lexer.warnings();
    assert_eq!(warnings.len(), 1);
    assert_eq!(warnings[0].code, DiagnosticCode::HereDocDelimiterNotFound);
    assert_eq!(
        warnings[0].message,
        "here-document delimiter `EOF` was not found before end of input"
    );
    assert_eq!(warnings[0].near_text.as_deref(), Some("EOF\\nbody\\n"));
    assert_eq!(
        warnings[0].suggestion.as_deref(),
        Some("add a line containing delimiter `EOF` exactly.")
    );
}
