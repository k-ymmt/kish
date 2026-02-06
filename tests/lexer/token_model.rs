use kish::lexer::{
    ByteOffset, OperatorKind, QuoteMarker, QuoteProvenance, SourceId, Span, SubstitutionKind,
    SubstitutionMarker, Token, TokenKind, TokenOffset, TokenRange,
};

#[test]
fn operator_kind_covers_phase1_operator_set() {
    let lexemes: Vec<&str> = OperatorKind::ALL.iter().map(|op| op.as_str()).collect();
    assert_eq!(
        lexemes,
        vec![
            "&&", "||", ";;", ";&", "<<", "<<-", ">>", "<&", ">&", "<>", ">|", "|", ";", "&", "(",
            ")", "<", ">"
        ]
    );
}

#[test]
fn token_preserves_raw_text_span_and_markers() {
    let span = Span::new(SourceId::new(9), ByteOffset::new(12), ByteOffset::new(20));
    let quote = QuoteMarker::new(
        QuoteProvenance::DoubleQuoted,
        TokenRange::new(TokenOffset::new(0), TokenOffset::new(4)),
    );
    let substitution = SubstitutionMarker::new(
        SubstitutionKind::CommandSubstitution,
        TokenRange::new(TokenOffset::new(4), TokenOffset::new(8)),
    );

    let token = Token::with_metadata(
        TokenKind::Token,
        "\"$(x)\"".to_string(),
        span,
        vec![quote.clone()],
        vec![substitution.clone()],
    );

    assert_eq!(token.kind, TokenKind::Token);
    assert_eq!(token.lexeme, "\"$(x)\"");
    assert_eq!(token.span, span);
    assert_eq!(token.quote_markers, vec![quote]);
    assert_eq!(token.substitution_markers, vec![substitution]);
}

#[test]
fn io_number_helper_accepts_plain_digits_token_only() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(1));
    let token = Token::new(TokenKind::Token, "2".to_string(), span);
    assert_eq!(token.io_number_candidate(), Some(2));

    let not_digits = Token::new(TokenKind::Token, "2x".to_string(), span);
    assert_eq!(not_digits.io_number_candidate(), None);

    let not_token = Token::new(
        TokenKind::Operator(OperatorKind::Less),
        "<".to_string(),
        span,
    );
    assert_eq!(not_token.io_number_candidate(), None);
}

#[test]
fn io_location_helper_uses_braced_identifier_shape() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(6));

    let ok = Token::new(TokenKind::Token, "{fd0}".to_string(), span);
    assert!(ok.is_io_location_candidate());

    let bad_start = Token::new(TokenKind::Token, "{0fd}".to_string(), span);
    assert!(!bad_start.is_io_location_candidate());

    let bad_shape = Token::new(TokenKind::Token, "fd0".to_string(), span);
    assert!(!bad_shape.is_io_location_candidate());
}
