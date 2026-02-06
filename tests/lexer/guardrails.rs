use kish::lexer::{DiagnosticCode, FatalLexError, Lexer, LexerLimits, LexerMode, LexerOptions};

#[test]
fn token_exceed_limit_returns_token_size_error() {
    let mut lexer = Lexer::with_options(
        "abcdef\n",
        LexerMode::Normal,
        LexerOptions {
            limits: LexerLimits {
                max_token_bytes: 4,
                ..Default::default()
            },
            ..Default::default()
        },
    );

    match lexer.next_token() {
        Err(FatalLexError::TokenSizeLimitExceeded(diagnostic)) => {
            assert_eq!(diagnostic.code, DiagnosticCode::TokenSizeLimitExceeded);
            assert_eq!(
                diagnostic.message,
                "token size exceeded configured limit (4 bytes)"
            );
            assert_eq!(diagnostic.near_text.as_deref(), Some("abcde"));
            assert_eq!(
                diagnostic.suggestion.as_deref(),
                Some("reduce token size or raise lexer token-size limit.")
            );
        }
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn heredoc_body_exceed_limit_returns_heredoc_size_error() {
    let mut lexer = Lexer::with_options(
        "cat <<EOF\n12345\nEOF\n",
        LexerMode::Normal,
        LexerOptions {
            limits: LexerLimits {
                max_here_doc_body_bytes: 4,
                ..Default::default()
            },
            ..Default::default()
        },
    );

    match lexer.next_token().expect("first token should be available") {
        kish::lexer::LexStep::Token(token) => assert_eq!(token.lexeme, "cat"),
        _ => panic!("expected token"),
    }

    match lexer.next_token() {
        Err(FatalLexError::HereDocBodySizeLimitExceeded(diagnostic)) => {
            assert_eq!(
                diagnostic.code,
                DiagnosticCode::HereDocBodySizeLimitExceeded
            );
            assert_eq!(
                diagnostic.message,
                "here-document body exceeded configured limit (4 bytes)"
            );
            assert_eq!(diagnostic.near_text.as_deref(), Some("12345\\n"));
            assert_eq!(
                diagnostic.suggestion.as_deref(),
                Some("reduce here-document body size or raise lexer here-doc body limit.")
            );
        }
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn boundary_token_count_exceed_limit_returns_boundary_limit_error() {
    let mut lexer = Lexer::with_options(
        "a b c d\n",
        LexerMode::Normal,
        LexerOptions {
            limits: LexerLimits {
                max_boundary_tokens: 3,
                ..Default::default()
            },
            ..Default::default()
        },
    );

    match lexer.tokenize_complete_command_boundary() {
        Err(FatalLexError::BoundaryTokenLimitExceeded(diagnostic)) => {
            assert_eq!(diagnostic.code, DiagnosticCode::BoundaryTokenLimitExceeded);
            assert_eq!(
                diagnostic.message,
                "boundary token count exceeded configured limit (3)"
            );
            assert_eq!(diagnostic.near_text.as_deref(), Some("d"));
            assert_eq!(
                diagnostic.suggestion.as_deref(),
                Some("split the command across boundaries or raise boundary token limit.")
            );
        }
        other => panic!("unexpected result: {other:?}"),
    }
}
