use kish::lexer::{
    BoundaryResult, FatalLexError, Lexer, LexerMode, LexerOptions, NeedMoreReason, RecoveryPolicy,
};

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
fn interactive_mode_keeps_unterminated_quote_as_need_more_input() {
    let mut lexer = Lexer::new("echo \"abc", LexerMode::Normal);
    let boundary = lexer
        .tokenize_complete_command_boundary()
        .expect("boundary tokenization should succeed");

    match boundary {
        BoundaryResult::NeedMoreInput(incomplete) => {
            assert_eq!(incomplete.reason, NeedMoreReason::UnterminatedDoubleQuote)
        }
        BoundaryResult::Complete(_) => panic!("expected need more input"),
    }
}

#[test]
fn non_interactive_mode_returns_fatal_for_unterminated_quote() {
    let mut lexer = Lexer::with_options(
        "echo \"abc",
        LexerMode::Normal,
        LexerOptions {
            recovery_policy: RecoveryPolicy::NonInteractive,
            ..Default::default()
        },
    );

    match lexer.tokenize_complete_command_boundary() {
        Err(FatalLexError::UnterminatedDoubleQuote(_)) => {}
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn non_interactive_mode_returns_fatal_incomplete_for_trailing_backslash() {
    let mut lexer = Lexer::with_options(
        "echo \\",
        LexerMode::Normal,
        LexerOptions {
            recovery_policy: RecoveryPolicy::NonInteractive,
            ..Default::default()
        },
    );

    match lexer.tokenize_complete_command_boundary() {
        Err(FatalLexError::IncompleteInput(diagnostic)) => {
            assert_eq!(diagnostic.near_text.as_deref(), Some("\\"));
        }
        other => panic!("unexpected result: {other:?}"),
    }
}
