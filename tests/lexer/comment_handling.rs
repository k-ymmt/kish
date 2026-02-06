use kish::lexer::{LexStep, Lexer, LexerMode, TokenKind};

fn collect_tokens(input: &str) -> Vec<(TokenKind, String)> {
    let mut lexer = Lexer::new(input, LexerMode::Normal);
    let mut tokens = Vec::new();
    loop {
        match lexer.next_token().expect("scan should succeed") {
            LexStep::Token(token) => tokens.push((token.kind, token.lexeme)),
            LexStep::Recoverable(_) => panic!("unexpected recoverable result"),
            LexStep::EndOfInput => break,
        }
    }
    tokens
}

#[test]
fn leading_comment_line_preserves_newline_token() {
    let tokens = collect_tokens("# comment\necho hi\n");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Newline, "\n".to_string()),
            (TokenKind::Token, "echo".to_string()),
            (TokenKind::Token, "hi".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn comment_after_word_discards_comment_text_and_keeps_newline() {
    let tokens = collect_tokens("echo #x\n");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "echo".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn quoted_hash_does_not_start_comment() {
    let tokens = collect_tokens("echo '#x'\n");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "echo".to_string()),
            (TokenKind::Token, "'#x'".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn hash_inside_word_is_not_comment_start() {
    let tokens = collect_tokens("a#b\n");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "a#b".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}
