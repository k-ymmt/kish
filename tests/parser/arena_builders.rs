use kish::lexer::{ByteOffset, Lexer, LexerMode, SourceId, Span, Token, TokenKind};
use kish::parser::{AstArena, AstBuilder, ParseErrorKind, ParseOptions, Parser, TokenStream};

fn token(lexeme: &str) -> Token {
    Token::new(
        TokenKind::Token,
        lexeme.to_string(),
        Span::new(
            SourceId::new(0),
            ByteOffset::new(0),
            ByteOffset::from_usize(lexeme.len()),
        ),
    )
}

#[test]
fn arena_allocation_limit_failure_is_deterministic() {
    let mut arena = AstArena::new(2);
    assert!(arena.allocate().is_ok());
    assert!(arena.allocate().is_ok());

    let error = arena
        .allocate()
        .expect_err("third allocation must exceed limit");
    assert_eq!(
        error,
        kish::parser::ArenaError::NodeLimitExceeded {
            limit: 2,
            attempted: 3
        }
    );
}

#[test]
fn ast_builder_creates_nodes_and_tracks_allocations() {
    let mut arena = AstArena::new(8);
    let mut builder = AstBuilder::new(&mut arena);

    let first = builder
        .word_from_token(token("echo"))
        .expect("first node allocates");
    let second = builder
        .word_from_token(token("hello"))
        .expect("second node allocates");

    let simple = builder
        .simple_command(vec![], vec![first, second], vec![], None)
        .expect("simple command allocates");
    let _command = builder
        .command_simple(simple)
        .expect("command wrapper allocates");

    assert_eq!(builder.allocated_nodes(), 4);
}

#[test]
fn ast_builder_limit_overflow_maps_to_parse_error() {
    let mut arena = AstArena::new(1);
    let mut builder = AstBuilder::new(&mut arena);

    builder
        .word_from_token(token("one"))
        .expect("first allocation should pass");
    let error = builder
        .word_from_token(token("two"))
        .expect_err("second allocation should fail");

    assert_eq!(error.kind, ParseErrorKind::AstNodeLimitExceeded);
    assert_eq!(error.expected, vec!["max_ast_nodes <= 1".to_string()]);
    assert_eq!(error.found.as_deref(), Some("node allocation attempt 2"));
}

#[test]
fn parser_non_empty_input_respects_max_ast_nodes_guard() {
    let lexer = Lexer::new("echo hi\n", LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    let mut parser = Parser::new(
        SourceId::new(0),
        ParseOptions {
            max_ast_nodes: 0,
            ..Default::default()
        },
        token_stream,
    );

    let error = parser
        .parse_complete_command()
        .expect_err("node limit should fail before grammar placeholder");
    assert_eq!(error.kind, ParseErrorKind::AstNodeLimitExceeded);
    assert_eq!(error.found.as_deref(), Some("node allocation attempt 1"));
}

#[test]
fn parse_program_empty_input_uses_program_node_allocation() {
    let lexer = Lexer::new("", LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    let mut parser = Parser::new(
        SourceId::new(0),
        ParseOptions {
            max_ast_nodes: 0,
            ..Default::default()
        },
        token_stream,
    );

    let error = parser
        .parse_program()
        .expect_err("empty program still requires a root node");
    assert_eq!(error.kind, ParseErrorKind::AstNodeLimitExceeded);
    assert_eq!(error.found.as_deref(), Some("node allocation attempt 1"));
}
