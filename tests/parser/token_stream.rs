use kish::lexer::{Lexer, LexerMode, OperatorKind, TokenKind};
use kish::parser::{ParseErrorKind, TokenStream, TokenStreamError};

fn make_stream(input: &str) -> TokenStream<'_> {
    TokenStream::new(Lexer::new(input, LexerMode::Normal))
}

#[test]
fn peek_supports_bounded_lookahead() {
    let mut stream = make_stream("echo a b c\n");

    assert_eq!(stream.peek(0).expect("peek 0").unwrap().lexeme, "echo");
    assert_eq!(stream.peek(1).expect("peek 1").unwrap().lexeme, "a");
    assert_eq!(stream.peek(2).expect("peek 2").unwrap().lexeme, "b");
    assert_eq!(stream.peek(3).expect("peek 3").unwrap().lexeme, "c");
}

#[test]
fn peek_rejects_lookahead_over_limit() {
    let mut stream = make_stream("echo\n");
    match stream.peek(4) {
        Err(TokenStreamError::Parse(error)) => {
            assert_eq!(error.kind, ParseErrorKind::LookaheadExceeded);
            assert_eq!(error.found.as_deref(), Some("lookahead 4"));
        }
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn peek_is_non_consuming_and_next_consumes() {
    let mut stream = make_stream("echo hi\n");

    let first = stream.peek(0).expect("peek").unwrap().lexeme.clone();
    assert_eq!(first, "echo");
    let consumed = stream.next().expect("next").unwrap();
    assert_eq!(consumed.lexeme, "echo");
    assert_eq!(stream.peek(0).expect("peek").unwrap().lexeme, "hi");
}

#[test]
fn expect_success_and_failure_are_deterministic() {
    let mut ok = make_stream("echo\n");
    let token = ok
        .expect(|token| token.kind == TokenKind::Token, "word")
        .expect("expect should accept TOKEN");
    assert_eq!(token.lexeme, "echo");

    let mut fail = make_stream("|\n");
    match fail.expect(|token| token.kind == TokenKind::Token, "word") {
        Err(TokenStreamError::Parse(error)) => {
            assert_eq!(error.kind, ParseErrorKind::UnexpectedToken);
            assert_eq!(error.expected, vec!["word".to_string()]);
            assert_eq!(error.found.as_deref(), Some("|"));
        }
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn newline_and_operator_tokens_are_preserved() {
    let mut stream = make_stream("echo|cat\n");

    let word = stream.next().expect("next").unwrap();
    assert_eq!(word.kind, TokenKind::Token);
    assert_eq!(word.lexeme, "echo");

    let pipe = stream.next().expect("next").unwrap();
    assert_eq!(pipe.kind, TokenKind::Operator(OperatorKind::Pipe));
    assert_eq!(pipe.lexeme, "|");

    let cat = stream.next().expect("next").unwrap();
    assert_eq!(cat.kind, TokenKind::Token);
    assert_eq!(cat.lexeme, "cat");

    let newline = stream.next().expect("next").unwrap();
    assert_eq!(newline.kind, TokenKind::Newline);
    assert_eq!(newline.lexeme, "\n");
}

#[test]
fn eof_is_idempotent_for_peek_and_next() {
    let mut stream = make_stream("echo\n");
    while stream.next().expect("drain").is_some() {}

    assert!(stream.peek(0).expect("peek eof 1").is_none());
    assert!(stream.peek(0).expect("peek eof 2").is_none());
    assert!(stream.next().expect("next eof 1").is_none());
    assert!(stream.next().expect("next eof 2").is_none());
}
