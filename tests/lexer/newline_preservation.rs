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
fn emits_newline_tokens_for_regular_line_ends() {
    let tokens = collect_tokens("a\nb\n");
    let newline_count = tokens
        .iter()
        .filter(|(kind, _)| *kind == TokenKind::Newline)
        .count();
    assert_eq!(newline_count, 2);
}

#[test]
fn removed_line_continuation_does_not_emit_intermediate_newline() {
    let tokens = collect_tokens("a\\\nb\n");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "ab".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}
