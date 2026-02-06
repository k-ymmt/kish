use kish::lexer::{
    ByteOffset, DiagnosticCode, FatalLexError, LexDiagnostic, RecoverableLexError, SourceId, Span,
};

#[test]
fn recoverable_and_fatal_errors_are_distinct_types() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(0));
    let diagnostic = LexDiagnostic::new(DiagnosticCode::IncompleteInput, "need more input", span);

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
    }
}
