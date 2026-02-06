use kish::lexer::{
    ByteOffset, DelimiterContext, OperatorKind, ParserClassificationOptions, ParserTokenClass,
    QuoteMarker, QuoteProvenance, SourceId, Span, SubstitutionKind, SubstitutionMarker, Token,
    TokenKind, TokenOffset, TokenRange,
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

#[test]
fn operator_kind_delimiter_context_maps_redirection_operators() {
    assert_eq!(
        OperatorKind::Less.delimiter_context(),
        Some(DelimiterContext::Less)
    );
    assert_eq!(
        OperatorKind::HereDoc.delimiter_context(),
        Some(DelimiterContext::Less)
    );
    assert_eq!(
        OperatorKind::HereDocStripTabs.delimiter_context(),
        Some(DelimiterContext::Less)
    );
    assert_eq!(
        OperatorKind::DupInput.delimiter_context(),
        Some(DelimiterContext::Less)
    );
    assert_eq!(
        OperatorKind::ReadWrite.delimiter_context(),
        Some(DelimiterContext::Less)
    );

    assert_eq!(
        OperatorKind::Greater.delimiter_context(),
        Some(DelimiterContext::Greater)
    );
    assert_eq!(
        OperatorKind::AppendOutput.delimiter_context(),
        Some(DelimiterContext::Greater)
    );
    assert_eq!(
        OperatorKind::DupOutput.delimiter_context(),
        Some(DelimiterContext::Greater)
    );
    assert_eq!(
        OperatorKind::Clobber.delimiter_context(),
        Some(DelimiterContext::Greater)
    );

    assert_eq!(OperatorKind::Pipe.delimiter_context(), None);
    assert_eq!(OperatorKind::AndIf.delimiter_context(), None);
}

#[test]
fn classify_for_parser_recognizes_io_number_only_with_delimiter_context() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(1));
    let token = Token::new(TokenKind::Token, "2".to_string(), span);

    assert_eq!(
        token.classify_for_parser(Some(DelimiterContext::Less)),
        ParserTokenClass::IoNumber
    );
    assert_eq!(
        token.classify_for_parser(Some(DelimiterContext::Greater)),
        ParserTokenClass::IoNumber
    );
    assert_eq!(token.classify_for_parser(None), ParserTokenClass::Token);
}

#[test]
fn classify_for_parser_keeps_io_location_disabled_by_default() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(4));
    let token = Token::new(TokenKind::Token, "{fd}".to_string(), span);

    assert_eq!(
        token.classify_for_parser(Some(DelimiterContext::Less)),
        ParserTokenClass::Token
    );
    assert_eq!(
        token.classify_for_parser(Some(DelimiterContext::Greater)),
        ParserTokenClass::Token
    );
}

#[test]
fn classify_for_parser_can_enable_io_location_gate() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(5));
    let token = Token::new(TokenKind::Token, "{fd0}".to_string(), span);
    let options = ParserClassificationOptions {
        allow_io_location: true,
    };

    assert_eq!(
        token.classify_for_parser_with_options(Some(DelimiterContext::Less), options),
        ParserTokenClass::IoLocation
    );
    assert_eq!(
        token.classify_for_parser_with_options(Some(DelimiterContext::Greater), options),
        ParserTokenClass::IoLocation
    );
}

#[test]
fn classify_for_parser_rejects_invalid_io_location_shape_even_when_enabled() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(4));
    let token = Token::new(TokenKind::Token, "{0f}".to_string(), span);
    let options = ParserClassificationOptions {
        allow_io_location: true,
    };

    assert_eq!(
        token.classify_for_parser_with_options(Some(DelimiterContext::Less), options),
        ParserTokenClass::Token
    );
}

#[test]
fn classify_for_parser_never_converts_non_plain_or_non_token_values() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(3));

    let quoted = Token::with_metadata(
        TokenKind::Token,
        "123".to_string(),
        span,
        vec![QuoteMarker::new(
            QuoteProvenance::DoubleQuoted,
            TokenRange::new(TokenOffset::new(0), TokenOffset::new(3)),
        )],
        vec![],
    );
    assert_eq!(
        quoted.classify_for_parser(Some(DelimiterContext::Less)),
        ParserTokenClass::Token
    );

    let substituted = Token::with_metadata(
        TokenKind::Token,
        "123".to_string(),
        span,
        vec![],
        vec![SubstitutionMarker::new(
            SubstitutionKind::ArithmeticExpansion,
            TokenRange::new(TokenOffset::new(0), TokenOffset::new(3)),
        )],
    );
    assert_eq!(
        substituted.classify_for_parser(Some(DelimiterContext::Greater)),
        ParserTokenClass::Token
    );

    let operator = Token::new(
        TokenKind::Operator(OperatorKind::Greater),
        ">".to_string(),
        span,
    );
    assert_eq!(
        operator.classify_for_parser(Some(DelimiterContext::Greater)),
        ParserTokenClass::Token
    );
}

#[test]
fn classify_for_parser_gives_io_number_precedence() {
    let span = Span::new(SourceId::new(0), ByteOffset::new(0), ByteOffset::new(2));
    let token = Token::new(TokenKind::Token, "10".to_string(), span);
    let options = ParserClassificationOptions {
        allow_io_location: true,
    };

    assert_eq!(
        token.classify_for_parser_with_options(Some(DelimiterContext::Less), options),
        ParserTokenClass::IoNumber
    );
}
