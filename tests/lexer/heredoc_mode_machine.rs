use kish::lexer::{
    BoundaryResult, FatalLexError, LexStep, Lexer, LexerMode, NeedMoreReason, OperatorKind,
    TokenKind,
};

fn collect_tokens(input: &str) -> Result<Vec<(TokenKind, String)>, FatalLexError> {
    let mut lexer = Lexer::new(input, LexerMode::Normal);
    let mut out = Vec::new();

    loop {
        match lexer.next_token()? {
            LexStep::Token(token) => out.push((token.kind, token.lexeme)),
            LexStep::Recoverable(_) => panic!("unexpected recoverable state"),
            LexStep::EndOfInput => break,
        }
    }

    Ok(out)
}

#[test]
fn here_doc_line_tokens_are_replayed_and_body_is_consumed() {
    let tokens = collect_tokens("cat <<EOF arg\nbody\nEOF\nnext\n").expect("scan should succeed");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "cat".to_string()),
            (TokenKind::Operator(OperatorKind::HereDoc), "<<".to_string()),
            (TokenKind::Token, "EOF".to_string()),
            (TokenKind::Token, "arg".to_string()),
            (TokenKind::Newline, "\n".to_string()),
            (TokenKind::Token, "next".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn multiple_here_docs_are_consumed_in_encounter_order() {
    let tokens =
        collect_tokens("cat <<A x <<B y\none\nA\ntwo\nB\nz\n").expect("scan should succeed");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "cat".to_string()),
            (TokenKind::Operator(OperatorKind::HereDoc), "<<".to_string()),
            (TokenKind::Token, "A".to_string()),
            (TokenKind::Token, "x".to_string()),
            (TokenKind::Operator(OperatorKind::HereDoc), "<<".to_string()),
            (TokenKind::Token, "B".to_string()),
            (TokenKind::Token, "y".to_string()),
            (TokenKind::Newline, "\n".to_string()),
            (TokenKind::Token, "z".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn quoted_delimiter_uses_quote_removed_key() {
    let tokens = collect_tokens("cat <<'EOF'\n$HOME\nEOF\nok\n").expect("scan should succeed");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "cat".to_string()),
            (TokenKind::Operator(OperatorKind::HereDoc), "<<".to_string()),
            (TokenKind::Token, "'EOF'".to_string()),
            (TokenKind::Newline, "\n".to_string()),
            (TokenKind::Token, "ok".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn strip_tabs_mode_matches_delimiter_with_leading_tabs() {
    let tokens = collect_tokens("cat <<-EOF\n\tline\n\tEOF\nok\n").expect("scan should succeed");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "cat".to_string()),
            (
                TokenKind::Operator(OperatorKind::HereDocStripTabs),
                "<<-".to_string()
            ),
            (TokenKind::Token, "EOF".to_string()),
            (TokenKind::Newline, "\n".to_string()),
            (TokenKind::Token, "ok".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn next_token_reports_missing_here_doc_delimiter_as_fatal() {
    let mut lexer = Lexer::new("cat <<EOF\nbody\n", LexerMode::Normal);
    match lexer.next_token().expect("first token should succeed") {
        LexStep::Token(token) => assert_eq!(token.lexeme, "cat"),
        _ => panic!("expected token"),
    }

    match lexer.next_token() {
        Err(FatalLexError::HereDocDelimiterNotFound(_)) => {}
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn boundary_api_reports_missing_here_doc_delimiter_as_need_more() {
    let mut lexer = Lexer::new("cat <<EOF\nbody\n", LexerMode::Normal);
    match lexer
        .tokenize_complete_command_boundary()
        .expect("boundary tokenization should not fatal")
    {
        BoundaryResult::NeedMoreInput(input) => {
            assert_eq!(input.reason, NeedMoreReason::HereDocDelimiterNotFound);
        }
        BoundaryResult::Complete(_) => panic!("expected incomplete boundary"),
    }
}
