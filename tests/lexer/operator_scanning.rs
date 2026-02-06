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
fn scans_all_operators_with_exact_lexemes() {
    let tokens = collect_tokens("&& || ;; ;& << <<- >> <& >& <> >| | ; & ( ) < >\n");
    let expected = vec![
        (TokenKind::Operator(OperatorKind::AndIf), "&&"),
        (TokenKind::Operator(OperatorKind::OrIf), "||"),
        (TokenKind::Operator(OperatorKind::DoubleSemicolon), ";;"),
        (TokenKind::Operator(OperatorKind::SemicolonAmpersand), ";&"),
        (TokenKind::Operator(OperatorKind::HereDoc), "<<"),
        (TokenKind::Operator(OperatorKind::HereDocStripTabs), "<<-"),
        (TokenKind::Operator(OperatorKind::AppendOutput), ">>"),
        (TokenKind::Operator(OperatorKind::DupInput), "<&"),
        (TokenKind::Operator(OperatorKind::DupOutput), ">&"),
        (TokenKind::Operator(OperatorKind::ReadWrite), "<>"),
        (TokenKind::Operator(OperatorKind::Clobber), ">|"),
        (TokenKind::Operator(OperatorKind::Pipe), "|"),
        (TokenKind::Operator(OperatorKind::Semicolon), ";"),
        (TokenKind::Operator(OperatorKind::Ampersand), "&"),
        (TokenKind::Operator(OperatorKind::LeftParen), "("),
        (TokenKind::Operator(OperatorKind::RightParen), ")"),
        (TokenKind::Operator(OperatorKind::Less), "<"),
        (TokenKind::Operator(OperatorKind::Greater), ">"),
        (TokenKind::Newline, "\n"),
    ];

    let expected: Vec<(TokenKind, String)> = expected
        .into_iter()
        .map(|(kind, lexeme)| (kind, lexeme.to_string()))
        .collect();
    assert_eq!(tokens, expected);
}

#[test]
fn honors_longest_match_precedence() {
    let tokens = collect_tokens("<<- << >| >>\n");
    let operator_kinds: Vec<TokenKind> = tokens.into_iter().map(|(kind, _)| kind).collect();
    assert_eq!(
        operator_kinds,
        vec![
            TokenKind::Operator(OperatorKind::HereDocStripTabs),
            TokenKind::Operator(OperatorKind::HereDoc),
            TokenKind::Operator(OperatorKind::Clobber),
            TokenKind::Operator(OperatorKind::AppendOutput),
            TokenKind::Newline,
        ]
    );
}
