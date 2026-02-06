use kish::lexer::{BoundaryResult, LexStep, Lexer, LexerMode, NeedMoreReason, TokenKind};

#[test]
fn unquoted_line_continuation_joins_token_without_separator() {
    let mut lexer = Lexer::new("echo foo\\\nbar\n", LexerMode::Normal);

    let first = lexer.next_token().expect("scan should succeed");
    let second = lexer.next_token().expect("scan should succeed");

    match first {
        LexStep::Token(token) => {
            assert_eq!(token.kind, TokenKind::Token);
            assert_eq!(token.lexeme, "echo");
        }
        _ => panic!("expected token"),
    }

    match second {
        LexStep::Token(token) => {
            assert_eq!(token.kind, TokenKind::Token);
            assert_eq!(token.lexeme, "foobar");
        }
        _ => panic!("expected token"),
    }
}

#[test]
fn line_continuation_between_words_does_not_emit_newline_token() {
    let mut lexer = Lexer::new("echo \\\nfoo\n", LexerMode::Normal);

    let mut tokens = Vec::new();
    loop {
        match lexer.next_token().expect("scan should succeed") {
            LexStep::Token(token) => {
                tokens.push(token);
                if tokens.last().is_some_and(|t| t.kind == TokenKind::Newline) {
                    break;
                }
            }
            LexStep::EndOfInput => break,
            LexStep::Recoverable(_) => panic!("unexpected recoverable state"),
        }
    }

    let lexemes: Vec<String> = tokens
        .iter()
        .filter(|token| token.kind == TokenKind::Token)
        .map(|token| token.lexeme.clone())
        .collect();
    assert_eq!(lexemes, vec!["echo".to_string(), "foo".to_string()]);
}

#[test]
fn trailing_unquoted_backslash_requests_more_input() {
    let mut lexer = Lexer::new("echo \\", LexerMode::Normal);

    match lexer
        .tokenize_complete_command_boundary()
        .expect("scan should succeed")
    {
        BoundaryResult::NeedMoreInput(incomplete) => {
            assert_eq!(incomplete.reason, NeedMoreReason::TrailingBackslash);
        }
        BoundaryResult::Complete(_) => panic!("expected incomplete input"),
    }
}

#[test]
fn repeated_next_token_after_line_join_does_not_panic() {
    let mut lexer = Lexer::new("a\\\nb\\\nc\n", LexerMode::Normal);

    loop {
        match lexer.next_token().expect("scan should not fail") {
            LexStep::Token(_) => {}
            LexStep::Recoverable(_) => {}
            LexStep::EndOfInput => break,
        }
    }
}
