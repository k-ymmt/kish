use kish::lexer::{LexStep, Lexer, LexerMode, OperatorKind, TokenKind};

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
fn unquoted_blanks_delimit_words() {
    let tokens = collect_tokens("echo   hi\tthere\n");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "echo".to_string()),
            (TokenKind::Token, "hi".to_string()),
            (TokenKind::Token, "there".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn operators_delimit_words_without_merging() {
    let tokens = collect_tokens("echo|cat&&true\n");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "echo".to_string()),
            (TokenKind::Operator(OperatorKind::Pipe), "|".to_string()),
            (TokenKind::Token, "cat".to_string()),
            (TokenKind::Operator(OperatorKind::AndIf), "&&".to_string()),
            (TokenKind::Token, "true".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn quoted_operator_and_blank_stay_in_single_token() {
    let tokens = collect_tokens("'a b|c'\" d;e \"\n");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "'a b|c'\" d;e \"".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn mixed_stream_order_is_preserved() {
    let tokens = collect_tokens("x|y;z\n");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "x".to_string()),
            (TokenKind::Operator(OperatorKind::Pipe), "|".to_string()),
            (TokenKind::Token, "y".to_string()),
            (
                TokenKind::Operator(OperatorKind::Semicolon),
                ";".to_string()
            ),
            (TokenKind::Token, "z".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn io_number_like_text_stays_raw_token_in_lexer_stream() {
    let tokens = collect_tokens("2>file\n");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "2".to_string()),
            (TokenKind::Operator(OperatorKind::Greater), ">".to_string()),
            (TokenKind::Token, "file".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}
