use kish::lexer::{
    BoundaryResult, ByteOffset, DiagnosticCode, FatalLexError, LexDiagnostic, LexStep, Lexer,
    LexerMode, NeedMoreReason, RecoverableLexError, SourceId, Span,
};

#[test]
fn lexer_new_sets_mode() {
    let lexer = Lexer::new("echo hi", LexerMode::HereDocPending);
    assert_eq!(lexer.mode(), LexerMode::HereDocPending);
}

#[test]
fn source_and_span_contracts_hold_ordering() {
    let source = SourceId::new(7);
    assert_eq!(source.value(), 7);

    let high = ByteOffset::new(10);
    let low = ByteOffset::new(2);
    let span = Span::new(source, high, low);

    assert_eq!(span.start, low);
    assert_eq!(span.end, high);
    assert_eq!(span.len(), 8);
    assert!(!span.is_empty());
}

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

#[test]
fn boundary_api_returns_complete_on_basic_input() {
    let mut lexer = Lexer::new("echo hi\n", LexerMode::Normal);
    let boundary = lexer
        .tokenize_complete_command_boundary()
        .expect("boundary tokenization should succeed");

    match boundary {
        BoundaryResult::Complete(tokens) => assert!(!tokens.tokens.is_empty()),
        BoundaryResult::NeedMoreInput(_) => panic!("expected complete boundary"),
    }
}

#[test]
fn boundary_api_can_report_need_more_input() {
    let mut lexer = Lexer::new("echo \\", LexerMode::Normal);
    let boundary = lexer
        .tokenize_complete_command_boundary()
        .expect("boundary tokenization should succeed");

    match boundary {
        BoundaryResult::NeedMoreInput(incomplete) => {
            assert_eq!(incomplete.reason, NeedMoreReason::TrailingBackslash)
        }
        BoundaryResult::Complete(_) => panic!("expected need more input"),
    }
}

#[test]
fn next_token_smoke_does_not_panic() {
    let mut lexer = Lexer::new("echo hi\n", LexerMode::Normal);

    loop {
        let step = lexer.next_token().expect("Phase 0 scan should not fail");
        match step {
            LexStep::Token(_) => {}
            LexStep::Recoverable(_) => {}
            LexStep::EndOfInput => break,
        }
    }
}
