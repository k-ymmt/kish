use kish::ir::{
    HirBuilder, HirCaseTerminator, HirCommand, HirCompoundCommand, HirListTerminator,
    HirRedirectOperator,
};
use kish::lexer::{ByteOffset, OperatorKind, SourceId, Span, Token, TokenKind};
use kish::parser::CaseTerminatorAst;

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

fn build_simple_command(builder: &HirBuilder, name: &str) -> HirCommand {
    let word = builder
        .word(
            token(name, 0, name.len() as u32),
            span(0, name.len() as u32),
        )
        .expect("word should be valid");
    let simple = builder
        .simple_command(
            Vec::new(),
            vec![word],
            Vec::new(),
            Some(span(0, name.len() as u32)),
        )
        .expect("simple command should be valid");
    builder.command_simple(simple)
}

fn build_min_list(builder: &HirBuilder) -> kish::ir::HirList {
    let command = build_simple_command(builder, "echo");
    let pipeline = builder
        .pipeline(false, vec![command], span(0, 4))
        .expect("pipeline should be valid");
    let and_or = builder
        .and_or(pipeline, Vec::new(), span(0, 4))
        .expect("and_or should be valid");
    builder
        .list(vec![and_or], span(0, 4))
        .expect("list should be valid")
}

#[test]
fn builder_constructs_simple_command_with_assignment_word_and_redirect() {
    let builder = HirBuilder::new();

    let assignment = builder
        .assignment(token("x=1", 0, 3), "x", "1", span(0, 3))
        .expect("assignment should be valid");
    let word = builder
        .word(token("echo", 4, 8), span(4, 8))
        .expect("word should be valid");
    let target = builder
        .word(token("out", 9, 12), span(9, 12))
        .expect("target word should be valid");
    let redirect = builder
        .redirect(
            Some(token("2", 8, 9)),
            OperatorKind::Greater,
            target,
            span(8, 12),
        )
        .expect("redirect should be valid");

    let simple = builder
        .simple_command(
            vec![assignment],
            vec![word],
            vec![redirect],
            Some(span(0, 12)),
        )
        .expect("simple command should be valid");

    assert_eq!(simple.assignments.len(), 1);
    assert_eq!(simple.words.len(), 1);
    assert_eq!(simple.redirects.len(), 1);
}

#[test]
fn builder_constructs_all_compound_variants_and_function_definition() {
    let builder = HirBuilder::new();
    let list = build_min_list(&builder);

    let if_clause = builder
        .if_clause(
            list.clone(),
            list.clone(),
            vec![(list.clone(), list.clone())],
            Some(list.clone()),
            span(0, 20),
        )
        .expect("if clause should be valid");

    let name_word = builder
        .word(token("i", 0, 1), span(0, 1))
        .expect("name word should be valid");

    let for_clause = builder
        .for_clause(
            name_word.clone(),
            vec![name_word.clone()],
            list.clone(),
            span(0, 20),
        )
        .expect("for clause should be valid");

    let while_clause = builder
        .while_clause(list.clone(), list.clone(), span(0, 20))
        .expect("while clause should be valid");

    let until_clause = builder
        .until_clause(list.clone(), list.clone(), span(0, 20))
        .expect("until clause should be valid");

    let case_item = builder
        .case_item(
            vec![name_word.clone()],
            Some(list.clone()),
            Some(CaseTerminatorAst::DoubleSemicolon),
            span(0, 20),
        )
        .expect("case item should be valid");
    let case_clause = builder
        .case_clause(name_word.clone(), vec![case_item], span(0, 20))
        .expect("case clause should be valid");

    let _compound_variants = vec![
        builder.compound_subshell(list.clone()),
        builder.compound_brace_group(list.clone()),
        builder.compound_if(if_clause),
        builder.compound_for(for_clause),
        builder.compound_while(while_clause),
        builder.compound_until(until_clause),
        builder.compound_case(case_clause),
    ];

    let body_node = builder
        .compound_command_node(
            HirCompoundCommand::Subshell(list.clone()),
            Vec::new(),
            span(0, 20),
        )
        .expect("compound node should be valid");

    let function = builder
        .function_definition(
            name_word,
            builder.command_compound(body_node),
            Vec::new(),
            span(0, 20),
        )
        .expect("function definition should be valid");

    assert_eq!(function.redirects.len(), 0);
}

#[test]
fn builder_constructs_complete_command_and_program() {
    let builder = HirBuilder::new();
    let list = build_min_list(&builder);

    let complete = builder
        .complete_command(list, Some(OperatorKind::Semicolon), span(0, 8))
        .expect("complete command should be valid");
    let program = builder
        .program(vec![complete], Some(span(0, 8)))
        .expect("program should be valid");

    assert_eq!(program.complete_commands.len(), 1);
}

#[test]
fn conversion_helpers_map_semantics() {
    assert_eq!(
        HirBuilder::list_terminator_from_operator(OperatorKind::Semicolon)
            .expect("semicolon should map"),
        HirListTerminator::Semicolon
    );
    assert_eq!(
        HirBuilder::and_or_connector_from_operator(OperatorKind::AndIf).expect("and_if should map"),
        kish::ir::HirAndOrConnector::AndIf
    );
    assert_eq!(
        HirBuilder::redirect_operator_from_operator(OperatorKind::HereDoc)
            .expect("heredoc should map"),
        HirRedirectOperator::HereDoc
    );
    assert_eq!(
        HirBuilder::case_terminator_from_ast(CaseTerminatorAst::SemicolonAmpersand),
        HirCaseTerminator::Fallthrough
    );
}
