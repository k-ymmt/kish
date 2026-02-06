use kish::lexer::{
    BoundaryResult, ByteOffset, DiagnosticCode, FatalLexError, LexStep, Lexer, LexerMode,
    NeedMoreReason, QuoteMarker, QuoteProvenance, SourceId, Span, TokenKind,
};

#[test]
fn quote_markers_preserve_ranges_and_lexeme() {
    let mut lexer = Lexer::new("'a'\"b\"$'c'\\ d\n", LexerMode::Normal);

    let first = lexer.next_token().expect("scan should succeed");
    let second = lexer.next_token().expect("scan should succeed");

    match first {
        LexStep::Token(token) => {
            assert_eq!(token.kind, TokenKind::Token);
            assert_eq!(token.lexeme, "'a'\"b\"$'c'\\ d");
            assert_eq!(
                token.quote_markers,
                vec![
                    QuoteMarker::new(
                        QuoteProvenance::SingleQuoted,
                        kish::lexer::TokenRange::new(
                            kish::lexer::TokenOffset::new(0),
                            kish::lexer::TokenOffset::new(3),
                        ),
                    ),
                    QuoteMarker::new(
                        QuoteProvenance::DoubleQuoted,
                        kish::lexer::TokenRange::new(
                            kish::lexer::TokenOffset::new(3),
                            kish::lexer::TokenOffset::new(6),
                        ),
                    ),
                    QuoteMarker::new(
                        QuoteProvenance::DollarSingleQuoted,
                        kish::lexer::TokenRange::new(
                            kish::lexer::TokenOffset::new(6),
                            kish::lexer::TokenOffset::new(10),
                        ),
                    ),
                    QuoteMarker::new(
                        QuoteProvenance::BackslashEscaped,
                        kish::lexer::TokenRange::new(
                            kish::lexer::TokenOffset::new(10),
                            kish::lexer::TokenOffset::new(12),
                        ),
                    ),
                ],
            );
        }
        _ => panic!("expected token"),
    }

    match second {
        LexStep::Token(token) => assert_eq!(token.kind, TokenKind::Newline),
        _ => panic!("expected newline token"),
    }
}

#[test]
fn unterminated_single_quote_returns_fatal_error() {
    let mut lexer = Lexer::new("'abc", LexerMode::Normal);

    match lexer.next_token() {
        Err(FatalLexError::UnterminatedSingleQuote(diagnostic)) => {
            assert_eq!(diagnostic.code, DiagnosticCode::UnterminatedSingleQuote);
            assert_eq!(
                diagnostic.span,
                Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(4))
            );
        }
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn unterminated_dollar_single_quote_returns_fatal_error() {
    let mut lexer = Lexer::new("$'abc", LexerMode::Normal);

    match lexer.next_token() {
        Err(FatalLexError::UnterminatedDollarSingleQuote(diagnostic)) => {
            assert_eq!(
                diagnostic.code,
                DiagnosticCode::UnterminatedDollarSingleQuote
            );
            assert_eq!(
                diagnostic.span,
                Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(5))
            );
        }
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn boundary_reports_unterminated_double_quote() {
    let mut lexer = Lexer::new("echo \"abc", LexerMode::Normal);

    match lexer
        .tokenize_complete_command_boundary()
        .expect("scan should succeed")
    {
        BoundaryResult::NeedMoreInput(incomplete) => {
            assert_eq!(incomplete.reason, NeedMoreReason::UnterminatedDoubleQuote);
        }
        BoundaryResult::Complete(_) => panic!("expected incomplete input"),
    }
}

#[test]
fn boundary_reports_unterminated_dollar_single_quote() {
    let mut lexer = Lexer::new("echo $'abc", LexerMode::Normal);

    match lexer
        .tokenize_complete_command_boundary()
        .expect("scan should succeed")
    {
        BoundaryResult::NeedMoreInput(incomplete) => {
            assert_eq!(
                incomplete.reason,
                NeedMoreReason::UnterminatedDollarSingleQuote
            );
        }
        BoundaryResult::Complete(_) => panic!("expected incomplete input"),
    }
}
