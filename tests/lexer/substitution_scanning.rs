use kish::lexer::{
    BoundaryResult, FatalLexError, LexStep, Lexer, LexerMode, NeedMoreReason, SubstitutionKind,
    SubstitutionMarker, Token, TokenKind, TokenOffset, TokenRange,
};

fn next_token(input: &str) -> Token {
    let mut lexer = Lexer::new(input, LexerMode::Normal);
    match lexer.next_token().expect("scan should succeed") {
        LexStep::Token(token) => token,
        LexStep::Recoverable(_) => panic!("unexpected recoverable state"),
        LexStep::EndOfInput => panic!("expected token"),
    }
}

fn token_range(start: usize, end: usize) -> TokenRange {
    TokenRange::new(TokenOffset::from_usize(start), TokenOffset::from_usize(end))
}

#[test]
fn marker_creation_for_recursive_substitution_forms() {
    let parameter = next_token("${x}\n");
    assert_eq!(
        parameter.substitution_markers,
        vec![SubstitutionMarker::new(
            SubstitutionKind::ParameterExpansion,
            token_range(0, 4)
        )]
    );

    let command = next_token("$(echo hi)\n");
    assert_eq!(
        command.substitution_markers,
        vec![SubstitutionMarker::new(
            SubstitutionKind::CommandSubstitution,
            token_range(0, 10)
        )]
    );

    let backquote = next_token("`echo hi`\n");
    assert_eq!(
        backquote.substitution_markers,
        vec![SubstitutionMarker::new(
            SubstitutionKind::BackquotedCommandSubstitution,
            token_range(0, 9)
        )]
    );

    let arithmetic = next_token("$((1+2))\n");
    assert_eq!(
        arithmetic.substitution_markers,
        vec![SubstitutionMarker::new(
            SubstitutionKind::ArithmeticExpansion,
            token_range(0, 8)
        )]
    );
}

#[test]
fn nesting_keeps_single_token_and_collects_overlapping_markers() {
    let token = next_token("x$(echo ${a} $((1+2)))y\n");
    assert_eq!(token.kind, TokenKind::Token);
    assert_eq!(token.lexeme, "x$(echo ${a} $((1+2)))y");

    assert!(
        token
            .substitution_markers
            .contains(&SubstitutionMarker::new(
                SubstitutionKind::CommandSubstitution,
                token_range(1, 22)
            ))
    );
    assert!(
        token
            .substitution_markers
            .contains(&SubstitutionMarker::new(
                SubstitutionKind::ParameterExpansion,
                token_range(8, 12)
            ))
    );
    assert!(
        token
            .substitution_markers
            .contains(&SubstitutionMarker::new(
                SubstitutionKind::ArithmeticExpansion,
                token_range(13, 21)
            ))
    );
}

#[test]
fn substitution_recognized_in_double_quotes_but_not_single_quotes() {
    let double_quoted = next_token("\"$(echo hi)\"\n");
    assert!(double_quoted.has_substitution_markers());
    assert_eq!(
        double_quoted.substitution_markers[0],
        SubstitutionMarker::new(SubstitutionKind::CommandSubstitution, token_range(1, 11))
    );

    let single_quoted = next_token("'$(echo hi)'\n");
    assert!(!single_quoted.has_substitution_markers());
}

#[test]
fn delimiters_inside_substitution_do_not_split_token() {
    let mut lexer = Lexer::new("$(echo a|b c)|cat\n", LexerMode::Normal);
    let mut got = Vec::new();

    loop {
        match lexer.next_token().expect("scan should succeed") {
            LexStep::Token(token) => {
                let kind = token.kind.clone();
                got.push((token.kind, token.lexeme));
                if kind == TokenKind::Newline {
                    break;
                }
            }
            LexStep::Recoverable(_) => panic!("unexpected recoverable state"),
            LexStep::EndOfInput => break,
        }
    }

    assert_eq!(
        got,
        vec![
            (TokenKind::Token, "$(echo a|b c)".to_string()),
            (
                TokenKind::Operator(kish::lexer::OperatorKind::Pipe),
                "|".to_string()
            ),
            (TokenKind::Token, "cat".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn unterminated_substitutions_return_fatal_error_from_next_token() {
    match Lexer::new("${x", LexerMode::Normal).next_token() {
        Err(FatalLexError::UnterminatedParameterExpansion(_)) => {}
        other => panic!("unexpected result: {other:?}"),
    }

    match Lexer::new("$(echo", LexerMode::Normal).next_token() {
        Err(FatalLexError::UnterminatedCommandSubstitution(_)) => {}
        other => panic!("unexpected result: {other:?}"),
    }

    match Lexer::new("`echo", LexerMode::Normal).next_token() {
        Err(FatalLexError::UnterminatedBackquotedCommandSubstitution(_)) => {}
        other => panic!("unexpected result: {other:?}"),
    }

    match Lexer::new("$((1+2)", LexerMode::Normal).next_token() {
        Err(FatalLexError::UnterminatedArithmeticExpansion(_)) => {}
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn boundary_reports_unterminated_substitution_need_more_reason() {
    match Lexer::new("${x", LexerMode::Normal)
        .tokenize_complete_command_boundary()
        .expect("scan should succeed")
    {
        BoundaryResult::NeedMoreInput(input) => {
            assert_eq!(input.reason, NeedMoreReason::UnterminatedParameterExpansion);
        }
        BoundaryResult::Complete(_) => panic!("expected incomplete input"),
    }

    match Lexer::new("$(echo", LexerMode::Normal)
        .tokenize_complete_command_boundary()
        .expect("scan should succeed")
    {
        BoundaryResult::NeedMoreInput(input) => {
            assert_eq!(
                input.reason,
                NeedMoreReason::UnterminatedCommandSubstitution
            );
        }
        BoundaryResult::Complete(_) => panic!("expected incomplete input"),
    }

    match Lexer::new("`echo", LexerMode::Normal)
        .tokenize_complete_command_boundary()
        .expect("scan should succeed")
    {
        BoundaryResult::NeedMoreInput(input) => {
            assert_eq!(
                input.reason,
                NeedMoreReason::UnterminatedBackquotedCommandSubstitution
            );
        }
        BoundaryResult::Complete(_) => panic!("expected incomplete input"),
    }

    match Lexer::new("$((1+2)", LexerMode::Normal)
        .tokenize_complete_command_boundary()
        .expect("scan should succeed")
    {
        BoundaryResult::NeedMoreInput(input) => {
            assert_eq!(
                input.reason,
                NeedMoreReason::UnterminatedArithmeticExpansion
            );
        }
        BoundaryResult::Complete(_) => panic!("expected incomplete input"),
    }
}

#[test]
fn depth_overflow_returns_fatal_error() {
    let mut nested = "x".to_string();
    for _ in 0..65 {
        nested = format!("$({nested})");
    }

    match Lexer::new(&nested, LexerMode::Normal).next_token() {
        Err(FatalLexError::SubstitutionRecursionDepthExceeded(_)) => {}
        other => panic!("unexpected result: {other:?}"),
    }
}
