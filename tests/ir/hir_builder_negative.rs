use kish::ir::{HirBuilder, IrErrorKind};
use kish::lexer::{ByteOffset, OperatorKind, SourceId, Span, Token, TokenKind};

fn span(start: u32, end: u32) -> Span {
    Span::new(
        SourceId::new(0),
        ByteOffset::new(start),
        ByteOffset::new(end),
    )
}

fn token(lexeme: &str, start: u32, end: u32) -> Token {
    Token::new(TokenKind::Token, lexeme.to_string(), span(start, end))
}

fn assert_kind(kind: IrErrorKind, expected: IrErrorKind) {
    assert_eq!(kind, expected);
}

#[test]
fn pipeline_requires_at_least_one_command() {
    let builder = HirBuilder::new();
    let error = builder
        .pipeline(false, Vec::new(), span(0, 0))
        .expect_err("empty pipeline must fail");
    assert_kind(error.kind, IrErrorKind::InvariantViolation);
}

#[test]
fn list_requires_at_least_one_and_or() {
    let builder = HirBuilder::new();
    let error = builder
        .list(Vec::new(), span(0, 0))
        .expect_err("empty list must fail");
    assert_kind(error.kind, IrErrorKind::InvariantViolation);
}

#[test]
fn simple_command_requires_any_content() {
    let builder = HirBuilder::new();
    let error = builder
        .simple_command(Vec::new(), Vec::new(), Vec::new(), None)
        .expect_err("empty simple command must fail");
    assert_kind(error.kind, IrErrorKind::InvariantViolation);
}

#[test]
fn assignment_name_validation_rejects_invalid_shapes() {
    let builder = HirBuilder::new();

    let empty = builder
        .assignment(token("=x", 0, 2), "", "x", span(0, 2))
        .expect_err("empty name must fail");
    assert_kind(empty.kind, IrErrorKind::InvalidAssignmentShape);

    let has_equal = builder
        .assignment(token("x=y", 0, 3), "x=y", "1", span(0, 3))
        .expect_err("name containing '=' must fail");
    assert_kind(has_equal.kind, IrErrorKind::InvalidAssignmentShape);

    let has_nul = builder
        .assignment(token("x", 0, 1), "x\0y", "1", span(0, 1))
        .expect_err("name containing NUL must fail");
    assert_kind(has_nul.kind, IrErrorKind::InvalidAssignmentShape);
}

#[test]
fn case_item_requires_patterns() {
    let builder = HirBuilder::new();
    let error = builder
        .case_item(Vec::new(), None, None, span(0, 0))
        .expect_err("case item without patterns must fail");
    assert_kind(error.kind, IrErrorKind::InvariantViolation);
}

#[test]
fn function_definition_requires_compound_body() {
    let builder = HirBuilder::new();

    let name = builder
        .word(token("f", 0, 1), span(0, 1))
        .expect("name word should be valid");
    let body_word = builder
        .word(token("echo", 2, 6), span(2, 6))
        .expect("body word should be valid");
    let simple = builder
        .simple_command(Vec::new(), vec![body_word], Vec::new(), Some(span(2, 6)))
        .expect("simple command should be valid");

    let error = builder
        .function_definition(name, builder.command_simple(simple), Vec::new(), span(0, 6))
        .expect_err("function body must be compound");
    assert_kind(error.kind, IrErrorKind::InvariantViolation);
}

#[test]
fn redirect_requires_numeric_fd_token() {
    let builder = HirBuilder::new();

    let target = builder
        .word(token("out", 2, 5), span(2, 5))
        .expect("target word should be valid");

    let error = builder
        .redirect(
            Some(token("abc", 0, 3)),
            OperatorKind::Greater,
            target,
            span(0, 5),
        )
        .expect_err("non-numeric fd must fail");

    assert_kind(error.kind, IrErrorKind::InvalidRedirectShape);
}

#[test]
fn redirect_rejects_unsupported_operator() {
    let builder = HirBuilder::new();

    let target = builder
        .word(token("out", 2, 5), span(2, 5))
        .expect("target word should be valid");

    let error = builder
        .redirect(None, OperatorKind::Pipe, target, span(0, 5))
        .expect_err("unsupported redirect operator must fail");

    assert_kind(error.kind, IrErrorKind::InvalidRedirectShape);
}

#[test]
fn structural_inconsistency_returns_invariant_violation() {
    let builder = HirBuilder::new();

    let separator_error = builder
        .complete_command(
            builder
                .list(
                    vec![
                        builder
                            .and_or(
                                builder
                                    .pipeline(
                                        false,
                                        vec![
                                            builder.command_simple(
                                                builder
                                                    .simple_command(
                                                        Vec::new(),
                                                        vec![
                                                            builder
                                                                .word(
                                                                    token("echo", 0, 4),
                                                                    span(0, 4),
                                                                )
                                                                .expect("word should be valid"),
                                                        ],
                                                        Vec::new(),
                                                        Some(span(0, 4)),
                                                    )
                                                    .expect("simple command should be valid"),
                                            ),
                                        ],
                                        span(0, 4),
                                    )
                                    .expect("pipeline should be valid"),
                                Vec::new(),
                                span(0, 4),
                            )
                            .expect("and_or should be valid"),
                    ],
                    span(0, 4),
                )
                .expect("list should be valid"),
            Some(OperatorKind::Pipe),
            span(0, 4),
        )
        .expect_err("pipe cannot be a complete command separator");

    assert_kind(separator_error.kind, IrErrorKind::InvariantViolation);

    let connector_error = HirBuilder::and_or_connector_from_operator(OperatorKind::Semicolon)
        .expect_err("semicolon cannot be an and_or connector");
    assert_kind(connector_error.kind, IrErrorKind::InvariantViolation);
}
