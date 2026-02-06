use kish::lexer::{
    BoundaryResult, DelimiterContext, Lexer, LexerMode, ParserClassificationOptions,
    ParserTokenClass, Token, TokenKind,
};

fn tokenize_boundary(input: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(input, LexerMode::Normal);
    match lexer
        .tokenize_complete_command_boundary()
        .expect("boundary tokenization should succeed")
    {
        BoundaryResult::Complete(tokens) => tokens.tokens,
        BoundaryResult::NeedMoreInput(reason) => {
            panic!("expected complete boundary, got {reason:?}")
        }
    }
}

fn delimiter_from_next_operator(tokens: &[Token], index: usize) -> Option<DelimiterContext> {
    match tokens.get(index.saturating_add(1)).map(|token| &token.kind) {
        Some(TokenKind::Operator(operator)) => operator.delimiter_context(),
        _ => None,
    }
}

#[test]
fn lexer_stream_remains_raw_token_operator_newline_only() {
    let tokens = tokenize_boundary("2>out echo|cat && true\n");

    for token in &tokens {
        match token.kind {
            TokenKind::Token | TokenKind::Operator(_) | TokenKind::Newline => {}
        }
    }

    assert_eq!(tokens[0].kind, TokenKind::Token);
    assert_eq!(
        tokens[1].kind,
        TokenKind::Operator(kish::lexer::OperatorKind::Greater)
    );
}

#[test]
fn parser_side_lookahead_can_classify_io_number_without_mutating_stream_kind() {
    let tokens = tokenize_boundary("2>out\n");
    assert_eq!(tokens.len(), 4);

    let first_context = delimiter_from_next_operator(&tokens, 0);
    assert_eq!(first_context, Some(DelimiterContext::Greater));
    assert_eq!(tokens[0].kind, TokenKind::Token);
    assert_eq!(
        tokens[0].classify_for_parser(first_context),
        ParserTokenClass::IoNumber
    );

    let out_context = delimiter_from_next_operator(&tokens, 2);
    assert_eq!(out_context, None);
    assert_eq!(
        tokens[2].classify_for_parser(out_context),
        ParserTokenClass::Token
    );
}

#[test]
fn io_location_classification_is_gated_by_explicit_parser_option() {
    let tokens = tokenize_boundary("{fd}>out\n");
    let context = delimiter_from_next_operator(&tokens, 0);
    assert_eq!(context, Some(DelimiterContext::Greater));

    assert_eq!(
        tokens[0].classify_for_parser(context),
        ParserTokenClass::Token
    );

    let enabled = ParserClassificationOptions {
        allow_io_location: true,
    };
    assert_eq!(
        tokens[0].classify_for_parser_with_options(context, enabled),
        ParserTokenClass::IoLocation
    );
}

#[test]
fn quoted_or_substituted_tokens_remain_token_class_under_delimiter_context() {
    let quoted = tokenize_boundary("'2'>out\n");
    let quoted_context = delimiter_from_next_operator(&quoted, 0);
    assert_eq!(quoted_context, Some(DelimiterContext::Greater));
    assert!(quoted[0].has_quote_markers());
    assert_eq!(
        quoted[0].classify_for_parser(quoted_context),
        ParserTokenClass::Token
    );

    let substituted = tokenize_boundary("$((1+1))>out\n");
    let substituted_context = delimiter_from_next_operator(&substituted, 0);
    assert_eq!(substituted_context, Some(DelimiterContext::Greater));
    assert!(substituted[0].has_substitution_markers());
    assert_eq!(
        substituted[0].classify_for_parser(substituted_context),
        ParserTokenClass::Token
    );
}
