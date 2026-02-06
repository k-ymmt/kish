use kish::lexer::{
    ByteOffset, DiagnosticCode, FatalLexError, LexDiagnostic, RecoverableLexError, SourceId, Span,
};

#[test]
fn recoverable_and_fatal_errors_are_distinct_types() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(0));
    let diagnostic = LexDiagnostic::new(DiagnosticCode::IncompleteInput, "need more input", span);
    assert_eq!(diagnostic.near_text, None);
    assert_eq!(diagnostic.suggestion, None);

    let recoverable = RecoverableLexError::IncompleteInput(diagnostic.clone());
    let fatal = FatalLexError::InternalInvariant(LexDiagnostic::new(
        DiagnosticCode::InternalInvariant,
        "fatal",
        span,
    ));

    match recoverable {
        RecoverableLexError::IncompleteInput(inner) => {
            assert_eq!(inner.code, DiagnosticCode::IncompleteInput)
        }
    }
    match fatal {
        FatalLexError::InternalInvariant(inner) => {
            assert_eq!(inner.code, DiagnosticCode::InternalInvariant)
        }
        FatalLexError::UnterminatedSingleQuote(_)
        | FatalLexError::UnterminatedDoubleQuote(_)
        | FatalLexError::UnterminatedDollarSingleQuote(_)
        | FatalLexError::UnterminatedParameterExpansion(_)
        | FatalLexError::UnterminatedCommandSubstitution(_)
        | FatalLexError::UnterminatedBackquotedCommandSubstitution(_)
        | FatalLexError::UnterminatedArithmeticExpansion(_)
        | FatalLexError::MalformedArithmeticExpansion(_)
        | FatalLexError::SubstitutionRecursionDepthExceeded(_)
        | FatalLexError::HereDocDelimiterNotFound(_)
        | FatalLexError::TokenSizeLimitExceeded(_)
        | FatalLexError::HereDocBodySizeLimitExceeded(_)
        | FatalLexError::BoundaryTokenLimitExceeded(_)
        | FatalLexError::IncompleteInput(_) => {
            panic!("unexpected fatal variant")
        }
    }
}

#[test]
fn diagnostic_with_context_populates_optional_fields() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(1), ByteOffset::new(4));
    let diagnostic = LexDiagnostic::with_context(
        DiagnosticCode::MalformedArithmeticExpansion,
        "malformed arithmetic expansion",
        span,
        Some("$(( ))".to_string()),
        Some("use a non-empty arithmetic expression and close it with `))`.".to_string()),
    );

    assert_eq!(
        diagnostic.code,
        DiagnosticCode::MalformedArithmeticExpansion
    );
    assert_eq!(diagnostic.near_text.as_deref(), Some("$(( ))"));
    assert_eq!(
        diagnostic.suggestion.as_deref(),
        Some("use a non-empty arithmetic expression and close it with `))`.")
    );
}
