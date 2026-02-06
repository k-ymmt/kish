use kish::lexer::{
    BoundaryResult, DiagnosticCode, FatalLexError, HereDocEofPolicy, LexStep, Lexer, LexerMode,
    LexerOptions, NeedMoreReason, OperatorKind, Span, TokenKind,
};

fn tokenize_all(lexer: &mut Lexer<'_>) -> Result<Vec<(TokenKind, String)>, FatalLexError> {
    let mut out = Vec::new();

    loop {
        match lexer.next_token() {
            Ok(LexStep::Token(token)) => out.push((token.kind, token.lexeme)),
            Ok(LexStep::Recoverable(_)) => panic!("unexpected recoverable state"),
            Ok(LexStep::EndOfInput) => break,
            Err(error) => return Err(error),
        }
    }

    Ok(out)
}

fn collect_tokens(input: &str) -> Result<Vec<(TokenKind, String)>, FatalLexError> {
    let mut lexer = Lexer::new(input, LexerMode::Normal);
    tokenize_all(&mut lexer)
}

fn span_text<'a>(input: &'a str, span: Span) -> &'a str {
    &input[span.start.as_usize()..span.end.as_usize()]
}

#[test]
fn captures_here_doc_body_payload_and_metadata() {
    let input = "cat <<EOF arg\nbody\nEOF\nnext\n";
    let mut lexer = Lexer::new(input, LexerMode::Normal);
    let tokens = tokenize_all(&mut lexer).expect("scan should succeed");

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

    let captures = lexer.here_doc_bodies();
    assert_eq!(captures.len(), 1);
    let capture = &captures[0];
    assert_eq!(capture.raw_body, "body\n");
    assert_eq!(capture.raw_delimiter, "EOF");
    assert_eq!(capture.delimiter_key, "EOF");
    assert!(!capture.strip_tabs);
    assert!(!capture.quoted);
    assert_eq!(span_text(input, capture.origin_operator_span), "<<");
    assert_eq!(span_text(input, capture.delimiter_span), "EOF");
    assert_eq!(span_text(input, capture.body_span), "body\n");
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
fn strip_tabs_mode_strips_tabs_from_stored_body_lines() {
    let input = "cat <<A <<-B\none\nA\n\ttwo\n\tB\nok\n";
    let mut lexer = Lexer::new(input, LexerMode::Normal);
    let tokens = tokenize_all(&mut lexer).expect("scan should succeed");

    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "cat".to_string()),
            (TokenKind::Operator(OperatorKind::HereDoc), "<<".to_string()),
            (TokenKind::Token, "A".to_string()),
            (
                TokenKind::Operator(OperatorKind::HereDocStripTabs),
                "<<-".to_string()
            ),
            (TokenKind::Token, "B".to_string()),
            (TokenKind::Newline, "\n".to_string()),
            (TokenKind::Token, "ok".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );

    let captures = lexer.here_doc_bodies();
    assert_eq!(captures.len(), 2);
    assert_eq!(captures[0].raw_delimiter, "A");
    assert_eq!(captures[0].raw_body, "one\n");
    assert!(!captures[0].strip_tabs);

    assert_eq!(captures[1].raw_delimiter, "B");
    assert_eq!(captures[1].raw_body, "two\n");
    assert!(captures[1].strip_tabs);
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
        Err(FatalLexError::HereDocDelimiterNotFound(diagnostic)) => {
            assert_eq!(diagnostic.code, DiagnosticCode::HereDocDelimiterNotFound);
            assert_eq!(diagnostic.near_text.as_deref(), Some("EOF\\nbody\\n"));
            assert_eq!(
                diagnostic.suggestion.as_deref(),
                Some("add a line containing delimiter `EOF` exactly.")
            );
        }
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

#[test]
fn warning_policy_keeps_partial_body_and_records_warning() {
    let input = "cat <<EOF\nbody\n";
    let mut lexer = Lexer::with_options(
        input,
        LexerMode::Normal,
        LexerOptions {
            here_doc_eof_policy: HereDocEofPolicy::Warning,
            ..Default::default()
        },
    );

    let tokens = tokenize_all(&mut lexer).expect("scan should succeed in warning mode");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Token, "cat".to_string()),
            (TokenKind::Operator(OperatorKind::HereDoc), "<<".to_string()),
            (TokenKind::Token, "EOF".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );

    let warnings = lexer.warnings();
    assert_eq!(warnings.len(), 1);
    assert_eq!(warnings[0].code, DiagnosticCode::HereDocDelimiterNotFound);
    assert_eq!(warnings[0].near_text.as_deref(), Some("EOF\\nbody\\n"));
    assert_eq!(
        warnings[0].suggestion.as_deref(),
        Some("add a line containing delimiter `EOF` exactly.")
    );

    let captures = lexer.here_doc_bodies();
    assert_eq!(captures.len(), 1);
    assert_eq!(captures[0].raw_body, "body\n");
    assert_eq!(span_text(input, captures[0].body_span), "body\n");
}

#[test]
fn warning_policy_captures_remaining_pending_heredocs_as_empty() {
    let input = "cat <<A <<B\nfoo\n";
    let mut lexer = Lexer::with_options(
        input,
        LexerMode::Normal,
        LexerOptions {
            here_doc_eof_policy: HereDocEofPolicy::Warning,
            ..Default::default()
        },
    );

    let _ = tokenize_all(&mut lexer).expect("scan should succeed in warning mode");

    let warnings = lexer.warnings();
    assert_eq!(warnings.len(), 2);
    assert!(
        warnings
            .iter()
            .all(|warning| warning.code == DiagnosticCode::HereDocDelimiterNotFound)
    );

    let captures = lexer.here_doc_bodies();
    assert_eq!(captures.len(), 2);
    assert_eq!(captures[0].raw_delimiter, "A");
    assert_eq!(captures[0].raw_body, "foo\n");
    assert_eq!(captures[1].raw_delimiter, "B");
    assert_eq!(captures[1].raw_body, "");
    assert_eq!(captures[1].body_span.start, captures[1].body_span.end);
}

#[test]
fn boundary_api_is_complete_in_warning_mode() {
    let mut lexer = Lexer::with_options(
        "cat <<EOF\nbody\n",
        LexerMode::Normal,
        LexerOptions {
            here_doc_eof_policy: HereDocEofPolicy::Warning,
            ..Default::default()
        },
    );

    match lexer
        .tokenize_complete_command_boundary()
        .expect("boundary tokenization should not fatal")
    {
        BoundaryResult::Complete(tokens) => {
            let rendered: Vec<(TokenKind, String)> = tokens
                .tokens
                .into_iter()
                .map(|token| (token.kind, token.lexeme))
                .collect();
            assert_eq!(
                rendered,
                vec![
                    (TokenKind::Token, "cat".to_string()),
                    (TokenKind::Operator(OperatorKind::HereDoc), "<<".to_string()),
                    (TokenKind::Token, "EOF".to_string()),
                    (TokenKind::Newline, "\n".to_string()),
                ]
            );
            assert_eq!(lexer.warnings().len(), 1);
        }
        BoundaryResult::NeedMoreInput(_) => panic!("expected complete boundary"),
    }
}

#[test]
fn drain_accessors_clear_collected_values() {
    let mut lexer = Lexer::with_options(
        "cat <<EOF\nbody\n",
        LexerMode::Normal,
        LexerOptions {
            here_doc_eof_policy: HereDocEofPolicy::Warning,
            ..Default::default()
        },
    );

    let _ = tokenize_all(&mut lexer).expect("scan should succeed");

    let drained_bodies = lexer.drain_here_doc_bodies();
    let drained_warnings = lexer.drain_warnings();
    assert_eq!(drained_bodies.len(), 1);
    assert_eq!(drained_warnings.len(), 1);
    assert!(lexer.here_doc_bodies().is_empty());
    assert!(lexer.warnings().is_empty());
}
