use kish::lexer::{
    AliasExpander, AliasExpansionOptions, AliasTokenOrigin, BoundaryResult, CompleteCommandTokens,
    Lexer, LexerMode, OperatorKind, Token, TokenKind,
};
use std::collections::HashMap;

fn tokenize_boundary(input: &str) -> CompleteCommandTokens {
    let mut lexer = Lexer::new(input, LexerMode::Normal);
    match lexer
        .tokenize_complete_command_boundary()
        .expect("boundary tokenization should succeed")
    {
        BoundaryResult::Complete(tokens) => tokens,
        BoundaryResult::NeedMoreInput(reason) => {
            panic!("expected complete boundary, got {reason:?}")
        }
    }
}

fn render(tokens: &[Token]) -> Vec<(TokenKind, String)> {
    tokens
        .iter()
        .map(|token| (token.kind.clone(), token.lexeme.clone()))
        .collect()
}

fn alias_map(entries: &[(&str, &str)]) -> HashMap<String, String> {
    entries
        .iter()
        .map(|(name, value)| (name.to_string(), value.to_string()))
        .collect()
}

#[test]
fn expands_alias_in_command_position() {
    let tokens = tokenize_boundary("ll -a\n");
    let expander = AliasExpander::new(alias_map(&[("ll", "ls")]));
    let expanded = expander
        .expand_complete_command_tokens(&tokens)
        .expect("expansion should succeed");

    let rendered: Vec<(TokenKind, String)> = expanded
        .tokens
        .iter()
        .map(|item| (item.token.kind.clone(), item.token.lexeme.clone()))
        .collect();
    assert_eq!(
        rendered,
        vec![
            (TokenKind::Token, "ls".to_string()),
            (TokenKind::Token, "-a".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn does_not_expand_alias_outside_command_position() {
    let tokens = tokenize_boundary("echo ll\n");
    let expander = AliasExpander::new(alias_map(&[("ll", "ls")]));
    let expanded = expander
        .expand_complete_command_tokens(&tokens)
        .expect("expansion should succeed");

    let rendered: Vec<(TokenKind, String)> = expanded
        .tokens
        .iter()
        .map(|item| (item.token.kind.clone(), item.token.lexeme.clone()))
        .collect();
    assert_eq!(
        rendered,
        vec![
            (TokenKind::Token, "echo".to_string()),
            (TokenKind::Token, "ll".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn expands_after_command_separator() {
    let tokens = tokenize_boundary("echo x;ll\n");
    let expander = AliasExpander::new(alias_map(&[("ll", "ls")]));
    let expanded = expander
        .expand_complete_command_tokens(&tokens)
        .expect("expansion should succeed");

    let rendered: Vec<(TokenKind, String)> = expanded
        .tokens
        .iter()
        .map(|item| (item.token.kind.clone(), item.token.lexeme.clone()))
        .collect();
    assert_eq!(
        rendered,
        vec![
            (TokenKind::Token, "echo".to_string()),
            (TokenKind::Token, "x".to_string()),
            (TokenKind::Operator(OperatorKind::Semicolon), ";".to_string()),
            (TokenKind::Token, "ls".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn recursion_guard_prevents_infinite_self_alias_expansion() {
    let tokens = tokenize_boundary("a\n");
    let expander = AliasExpander::new(alias_map(&[("a", "b"), ("b", "a")]));
    let expanded = expander
        .expand_complete_command_tokens(&tokens)
        .expect("expansion should succeed");
    let rendered: Vec<(TokenKind, String)> = expanded
        .tokens
        .iter()
        .map(|item| (item.token.kind.clone(), item.token.lexeme.clone()))
        .collect();
    assert_eq!(
        rendered,
        vec![
            (TokenKind::Token, "a".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );
}

#[test]
fn trailing_blank_continuation_can_be_enabled_or_disabled() {
    let tokens = tokenize_boundary("foo bar\n");
    let aliases = alias_map(&[("foo", "echo "), ("bar", "world")]);

    let default_expander = AliasExpander::new(aliases.clone());
    let default_expanded = default_expander
        .expand_complete_command_tokens(&tokens)
        .expect("expansion should succeed");
    assert_eq!(
        default_expanded
            .tokens
            .iter()
            .map(|item| item.token.lexeme.clone())
            .collect::<Vec<_>>(),
        vec!["echo".to_string(), "world".to_string(), "\n".to_string()]
    );

    let no_blank_expander = AliasExpander::with_options(
        aliases,
        AliasExpansionOptions {
            allow_trailing_blank: false,
            ..AliasExpansionOptions::default()
        },
    );
    let no_blank_expanded = no_blank_expander
        .expand_complete_command_tokens(&tokens)
        .expect("expansion should succeed");
    assert_eq!(
        no_blank_expanded
            .tokens
            .iter()
            .map(|item| item.token.lexeme.clone())
            .collect::<Vec<_>>(),
        vec!["echo".to_string(), "bar".to_string(), "\n".to_string()]
    );
}

#[test]
fn quoted_or_invalid_alias_names_are_not_expanded() {
    let quoted_tokens = tokenize_boundary("'foo'\n");
    let expander = AliasExpander::new(alias_map(&[("foo", "echo")]));
    let quoted = expander
        .expand_complete_command_tokens(&quoted_tokens)
        .expect("expansion should succeed");
    assert_eq!(
        quoted
            .tokens
            .iter()
            .map(|item| item.token.lexeme.clone())
            .collect::<Vec<_>>(),
        vec!["'foo'".to_string(), "\n".to_string()]
    );

    let invalid_name_tokens = tokenize_boundary("l-l\n");
    let invalid_name_expander = AliasExpander::new(alias_map(&[("l-l", "echo")]));
    let invalid = invalid_name_expander
        .expand_complete_command_tokens(&invalid_name_tokens)
        .expect("expansion should succeed");
    assert_eq!(
        invalid
            .tokens
            .iter()
            .map(|item| item.token.lexeme.clone())
            .collect::<Vec<_>>(),
        vec!["l-l".to_string(), "\n".to_string()]
    );
}

#[test]
fn retokenizes_alias_value_and_preserves_source_mapping() {
    let input = "ll\n";
    let tokens = tokenize_boundary(input);
    let original_alias_span = tokens.tokens[0].span;
    let expander = AliasExpander::new(alias_map(&[("ll", "ls|cat")]));
    let expanded = expander
        .expand_complete_command_tokens(&tokens)
        .expect("expansion should succeed");

    let rendered = render(
        &expanded
            .tokens
            .iter()
            .map(|item| item.token.clone())
            .collect::<Vec<_>>(),
    );
    assert_eq!(
        rendered,
        vec![
            (TokenKind::Token, "ls".to_string()),
            (TokenKind::Operator(OperatorKind::Pipe), "|".to_string()),
            (TokenKind::Token, "cat".to_string()),
            (TokenKind::Newline, "\n".to_string()),
        ]
    );

    let first = &expanded.tokens[0];
    match &first.origin {
        AliasTokenOrigin::AliasExpansion {
            alias_name,
            expansion_site,
            synthetic_source_id,
            synthetic_span,
        } => {
            assert_eq!(alias_name, "ll");
            assert_eq!(expansion_site, &original_alias_span);
            assert_eq!(synthetic_span, &first.token.span);
            assert_eq!(*synthetic_source_id, first.token.span.source_id);
            assert_ne!(first.token.span.source_id, original_alias_span.source_id);
        }
        AliasTokenOrigin::Original { .. } => panic!("expected alias origin"),
    }

    let last = expanded.tokens.last().expect("newline token should exist");
    match last.origin {
        AliasTokenOrigin::Original { span } => {
            assert_eq!(span, tokens.tokens.last().expect("original newline").span)
        }
        AliasTokenOrigin::AliasExpansion { .. } => panic!("expected original origin"),
    }
}
