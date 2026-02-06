use kish::lexer::{BoundaryResult, Lexer, LexerMode, NeedMoreReason};

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
