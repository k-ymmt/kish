use kish::lexer::{Lexer, LexerMode, OperatorKind, SourceId};
use kish::parser::{
    CaseTerminatorAst, CommandAst, CompoundCommandAst, CompoundCommandNodeAst,
    FunctionDefinitionAst, ListAst, ParseErrorKind, ParseOptions, ParseStep, Parser, TokenStream,
};

fn parser_for(input: &str, options: ParseOptions) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), options, token_stream)
}

fn parse_one(input: &str) -> ParseStep {
    parse_one_with_options(input, ParseOptions::default())
}

fn parse_one_with_options(input: &str, options: ParseOptions) -> ParseStep {
    let mut parser = parser_for(input, options);
    parser
        .parse_complete_command()
        .expect("complete command should parse")
}

fn first_simple(step: &ParseStep) -> &kish::parser::SimpleCommandAst {
    let ParseStep::Complete(command) = step else {
        panic!("expected ParseStep::Complete");
    };

    let and_or = command.list.and_ors.first().expect("and_or exists");
    let pipeline = &and_or.head;
    match pipeline.commands.first().expect("command exists") {
        CommandAst::Simple(simple) => simple,
        other => panic!("expected simple command, got {other:?}"),
    }
}

fn first_compound(step: &ParseStep) -> &CompoundCommandNodeAst {
    let ParseStep::Complete(command) = step else {
        panic!("expected ParseStep::Complete");
    };

    let and_or = command.list.and_ors.first().expect("and_or exists");
    let pipeline = &and_or.head;
    match pipeline.commands.first().expect("command exists") {
        CommandAst::Compound(compound) => compound,
        other => panic!("expected compound command, got {other:?}"),
    }
}

fn first_function_definition(step: &ParseStep) -> &FunctionDefinitionAst {
    let ParseStep::Complete(command) = step else {
        panic!("expected ParseStep::Complete");
    };

    let and_or = command.list.and_ors.first().expect("and_or exists");
    let pipeline = &and_or.head;
    match pipeline.commands.first().expect("command exists") {
        CommandAst::FunctionDefinition(function) => function,
        other => panic!("expected function definition, got {other:?}"),
    }
}

fn first_simple_in_list(list: &ListAst) -> &kish::parser::SimpleCommandAst {
    let and_or = list.and_ors.first().expect("and_or exists");
    let pipeline = &and_or.head;
    match pipeline.commands.first().expect("command exists") {
        CommandAst::Simple(simple) => simple,
        other => panic!("expected simple command, got {other:?}"),
    }
}

#[test]
fn parse_program_handles_leading_and_trailing_blank_lines() {
    let mut parser = parser_for("\n\n echo hi\n\n", ParseOptions::default());
    let program = parser.parse_program().expect("program should parse");
    assert_eq!(program.complete_commands.len(), 1);
}

#[test]
fn one_word_simple_command_parses() {
    let step = parse_one("echo\n");
    let simple = first_simple(&step);
    assert_eq!(simple.words.len(), 1);
    assert_eq!(simple.words[0].token.lexeme, "echo");
}

#[test]
fn word_sequence_simple_command_parses() {
    let step = parse_one("echo hi there\n");
    let simple = first_simple(&step);
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo", "hi", "there"]);
}

#[test]
fn pipeline_parses_two_commands() {
    let step = parse_one("a|b\n");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command");
    };

    let pipeline = &command.list.and_ors[0].head;
    assert_eq!(pipeline.commands.len(), 2);
}

#[test]
fn and_or_chain_parses() {
    let step = parse_one("a && b || c\n");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command");
    };

    let and_or = &command.list.and_ors[0];
    assert_eq!(and_or.tail.len(), 2);
    assert_eq!(and_or.tail[0].0, OperatorKind::AndIf);
    assert_eq!(and_or.tail[1].0, OperatorKind::OrIf);
}

#[test]
fn list_with_separators_parses_multiple_and_ors() {
    let step = parse_one("a;b&c\n");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command");
    };

    assert_eq!(command.list.and_ors.len(), 3);
}

#[test]
fn parse_complete_command_is_boundary_safe_across_multiple_commands() {
    let mut parser = parser_for("a\nb\n", ParseOptions::default());

    let first = parser
        .parse_complete_command()
        .expect("first command should parse");
    let second = parser
        .parse_complete_command()
        .expect("second command should parse");
    let third = parser
        .parse_complete_command()
        .expect("third call should return end");

    assert!(matches!(first, ParseStep::Complete(_)));
    assert!(matches!(second, ParseStep::Complete(_)));
    assert_eq!(third, ParseStep::EndOfInput);
}

#[test]
fn missing_then_in_if_fails() {
    let mut parser = parser_for("if echo a; fi\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("if clause must require then");

    assert!(matches!(
        error.kind,
        ParseErrorKind::UnexpectedEndOfInput | ParseErrorKind::UnexpectedToken
    ));
}

#[test]
fn redirection_in_command_parses() {
    let step = parse_one("echo > out\n");
    let simple = first_simple(&step);

    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo"]);
    assert_eq!(simple.redirects.len(), 1);
    assert_eq!(simple.redirects[0].operator, OperatorKind::Greater);
    assert_eq!(simple.redirects[0].target.token.lexeme, "out");
    assert!(simple.redirects[0].fd_or_location.is_none());
}

#[test]
fn redirect_prefix_form_parses() {
    let step = parse_one(">out echo\n");
    let simple = first_simple(&step);

    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo"]);
    assert_eq!(simple.redirects.len(), 1);
    assert_eq!(simple.redirects[0].operator, OperatorKind::Greater);
    assert_eq!(simple.redirects[0].target.token.lexeme, "out");
}

#[test]
fn assignment_only_simple_command_parses() {
    let step = parse_one("VAR=1\n");
    let simple = first_simple(&step);

    assert_eq!(simple.assignments.len(), 1);
    assert_eq!(simple.assignments[0].name, "VAR");
    assert_eq!(simple.assignments[0].value, "1");
    assert!(simple.words.is_empty());
    assert!(simple.redirects.is_empty());
}

#[test]
fn assignment_prefix_with_command_words_parses() {
    let step = parse_one("VAR=1 echo a b\n");
    let simple = first_simple(&step);

    assert_eq!(simple.assignments.len(), 1);
    assert_eq!(simple.assignments[0].name, "VAR");
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo", "a", "b"]);
}

#[test]
fn assignment_like_word_in_suffix_stays_word() {
    let step = parse_one("echo a=1\n");
    let simple = first_simple(&step);

    assert!(simple.assignments.is_empty());
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo", "a=1"]);
}

#[test]
fn io_number_redirect_parses() {
    let step = parse_one("2>out echo\n");
    let simple = first_simple(&step);

    assert_eq!(simple.redirects.len(), 1);
    assert_eq!(simple.redirects[0].operator, OperatorKind::Greater);
    assert_eq!(simple.redirects[0].target.token.lexeme, "out");
    assert_eq!(
        simple.redirects[0]
            .fd_or_location
            .as_ref()
            .map(|token| token.lexeme.as_str()),
        Some("2")
    );
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo"]);
}

#[test]
fn io_location_redirect_is_gated_by_option() {
    let disabled = parse_one("{fd}>out echo\n");
    let disabled_simple = first_simple(&disabled);
    assert_eq!(disabled_simple.redirects.len(), 1);
    assert!(disabled_simple.redirects[0].fd_or_location.is_none());
    let disabled_words: Vec<&str> = disabled_simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(disabled_words, vec!["{fd}", "echo"]);

    let enabled = parse_one_with_options(
        "{fd}>out echo\n",
        ParseOptions {
            allow_io_location: true,
            ..Default::default()
        },
    );
    let enabled_simple = first_simple(&enabled);
    assert_eq!(enabled_simple.redirects.len(), 1);
    assert_eq!(
        enabled_simple.redirects[0]
            .fd_or_location
            .as_ref()
            .map(|token| token.lexeme.as_str()),
        Some("{fd}")
    );
    let enabled_words: Vec<&str> = enabled_simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(enabled_words, vec!["echo"]);
}

#[test]
fn redirect_target_word_can_be_numeric() {
    let step = parse_one("echo > 2 > out\n");
    let simple = first_simple(&step);

    assert_eq!(simple.redirects.len(), 2);
    assert_eq!(simple.redirects[0].target.token.lexeme, "2");
    assert_eq!(simple.redirects[1].target.token.lexeme, "out");
}

#[test]
fn redirect_order_is_preserved_across_prefix_and_suffix() {
    let step = parse_one("a=1 >x cmd y <z\n");
    let simple = first_simple(&step);

    assert_eq!(simple.assignments.len(), 1);
    assert_eq!(simple.assignments[0].name, "a");
    assert_eq!(simple.redirects.len(), 2);
    assert_eq!(simple.redirects[0].operator, OperatorKind::Greater);
    assert_eq!(simple.redirects[0].target.token.lexeme, "x");
    assert_eq!(simple.redirects[1].operator, OperatorKind::Less);
    assert_eq!(simple.redirects[1].target.token.lexeme, "z");
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["cmd", "y"]);
}

#[test]
fn redirect_missing_target_reports_deterministic_error() {
    let mut parser = parser_for("echo >\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("redirect target is required");
    assert!(matches!(
        error.kind,
        ParseErrorKind::UnexpectedEndOfInput | ParseErrorKind::UnexpectedToken
    ));
}

#[test]
fn function_definition_parses_brace_group_body() {
    let step = parse_one("foo() { echo hi; }\n");
    let function = first_function_definition(&step);

    assert_eq!(function.name.token.lexeme, "foo");
    assert!(function.redirects.is_empty());
    assert_eq!(function.span.start, function.name.span.start);

    let CommandAst::Compound(compound_body) = function.body.as_ref() else {
        panic!("expected compound body");
    };
    assert!(matches!(
        compound_body.kind,
        CompoundCommandAst::BraceGroup(_)
    ));
    assert!(compound_body.redirects.is_empty());
    assert_eq!(function.span.end, compound_body.span.end);
}

#[test]
fn function_definition_parses_subshell_body_after_linebreak() {
    let step = parse_one("foo()\n(\n echo hi\n)\n");
    let function = first_function_definition(&step);

    let CommandAst::Compound(compound_body) = function.body.as_ref() else {
        panic!("expected compound body");
    };
    assert!(matches!(
        compound_body.kind,
        CompoundCommandAst::Subshell(_)
    ));
    assert!(function.redirects.is_empty());
}

#[test]
fn function_definition_redirects_attach_to_function_body_wrapper() {
    let step = parse_one("foo() { echo; } >out 2>&1\n");
    let function = first_function_definition(&step);

    assert_eq!(function.redirects.len(), 2);
    assert_eq!(function.redirects[0].operator, OperatorKind::Greater);
    assert_eq!(function.redirects[0].target.token.lexeme, "out");
    assert_eq!(function.redirects[1].operator, OperatorKind::DupOutput);
    assert_eq!(function.redirects[1].target.token.lexeme, "1");
    assert_eq!(
        function.redirects[1]
            .fd_or_location
            .as_ref()
            .map(|token| token.lexeme.as_str()),
        Some("2")
    );

    let CommandAst::Compound(compound_body) = function.body.as_ref() else {
        panic!("expected compound body");
    };
    assert!(compound_body.redirects.is_empty());
    assert_eq!(function.span.end, function.redirects[1].span.end);
}

#[test]
fn function_body_rule9_keeps_assignment_like_tokens_as_words() {
    let step = parse_one("foo() { VAR=1; }\n");
    let function = first_function_definition(&step);

    let CommandAst::Compound(compound_body) = function.body.as_ref() else {
        panic!("expected compound body");
    };
    let CompoundCommandAst::BraceGroup(list) = &compound_body.kind else {
        panic!("expected brace-group body");
    };

    let simple = first_simple_in_list(list);
    assert!(simple.assignments.is_empty());
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["VAR=1"]);
}

#[test]
fn function_definition_missing_body_fails() {
    let mut parser = parser_for("foo()\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("function body is required");

    assert!(matches!(
        error.kind,
        ParseErrorKind::UnexpectedEndOfInput | ParseErrorKind::UnexpectedToken
    ));
}

#[test]
fn function_definition_requires_compound_body() {
    let mut parser = parser_for("foo() echo\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("function body must be compound command");

    assert!(matches!(
        error.kind,
        ParseErrorKind::UnexpectedEndOfInput | ParseErrorKind::UnexpectedToken
    ));
}

#[test]
fn non_name_function_head_is_not_accepted() {
    let mut parser = parser_for("foo-bar() { echo; }\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("invalid function names should not parse");

    assert!(matches!(
        error.kind,
        ParseErrorKind::UnexpectedEndOfInput | ParseErrorKind::UnexpectedToken
    ));
}

#[test]
fn subshell_compound_parses() {
    let step = parse_one("(echo hi)\n");
    let compound = first_compound(&step);
    assert!(compound.redirects.is_empty());

    let CompoundCommandAst::Subshell(list) = &compound.kind else {
        panic!("expected subshell");
    };
    let simple = first_simple_in_list(list);
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo", "hi"]);
}

#[test]
fn brace_group_compound_parses() {
    let step = parse_one("{ echo hi; }\n");
    let compound = first_compound(&step);
    assert!(compound.redirects.is_empty());

    let CompoundCommandAst::BraceGroup(list) = &compound.kind else {
        panic!("expected brace group");
    };
    let simple = first_simple_in_list(list);
    let lexemes: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(lexemes, vec!["echo", "hi"]);
}

#[test]
fn compound_redirects_attach_and_preserve_order() {
    let step = parse_one("{ echo; } >out 2>&1\n");
    let compound = first_compound(&step);

    assert_eq!(compound.redirects.len(), 2);
    assert_eq!(compound.redirects[0].operator, OperatorKind::Greater);
    assert_eq!(compound.redirects[0].target.token.lexeme, "out");
    assert_eq!(compound.redirects[1].operator, OperatorKind::DupOutput);
    assert_eq!(compound.redirects[1].target.token.lexeme, "1");
    assert_eq!(
        compound.redirects[1]
            .fd_or_location
            .as_ref()
            .map(|token| token.lexeme.as_str()),
        Some("2")
    );
}

#[test]
fn if_elif_else_compound_parses() {
    let step = parse_one("if echo a; then echo b; elif echo c; then echo d; else echo e; fi\n");
    let compound = first_compound(&step);

    let CompoundCommandAst::If(if_clause) = &compound.kind else {
        panic!("expected if clause");
    };
    assert_eq!(if_clause.elif_arms.len(), 1);
    assert!(if_clause.else_body.is_some());
}

#[test]
fn while_compound_parses() {
    let step = parse_one("while echo a; do echo b; done\n");
    let compound = first_compound(&step);
    assert!(matches!(compound.kind, CompoundCommandAst::While(_)));
}

#[test]
fn until_compound_parses() {
    let step = parse_one("until echo a; do echo b; done\n");
    let compound = first_compound(&step);
    assert!(matches!(compound.kind, CompoundCommandAst::Until(_)));
}

#[test]
fn for_forms_parse() {
    let a = parse_one("for i do echo x; done\n");
    let b = parse_one("for i; do echo x; done\n");
    let c = parse_one("for i in a b; do echo x; done\n");

    let CompoundCommandAst::For(for_a) = &first_compound(&a).kind else {
        panic!("expected for clause");
    };
    assert!(for_a.words.is_empty());

    let CompoundCommandAst::For(for_b) = &first_compound(&b).kind else {
        panic!("expected for clause");
    };
    assert!(for_b.words.is_empty());

    let CompoundCommandAst::For(for_c) = &first_compound(&c).kind else {
        panic!("expected for clause");
    };
    let words: Vec<&str> = for_c
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(words, vec!["a", "b"]);
}

#[test]
fn case_with_terminators_parses() {
    let step = parse_one("case x in a) echo a ;; b) echo b ;& esac\n");
    let compound = first_compound(&step);

    let CompoundCommandAst::Case(case_clause) = &compound.kind else {
        panic!("expected case clause");
    };
    assert_eq!(case_clause.items.len(), 2);
    assert_eq!(
        case_clause.items[0].terminator,
        Some(CaseTerminatorAst::DoubleSemicolon)
    );
    assert_eq!(
        case_clause.items[1].terminator,
        Some(CaseTerminatorAst::SemicolonAmpersand)
    );
}

#[test]
fn case_non_terminated_item_parses() {
    let step = parse_one("case x in a) echo a\nesac\n");
    let compound = first_compound(&step);

    let CompoundCommandAst::Case(case_clause) = &compound.kind else {
        panic!("expected case clause");
    };
    assert_eq!(case_clause.items.len(), 1);
    assert_eq!(case_clause.items[0].terminator, None);
}

#[test]
fn missing_do_after_while_fails() {
    let mut parser = parser_for("while echo a; done\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("while clause must require do");
    assert!(matches!(
        error.kind,
        ParseErrorKind::UnexpectedEndOfInput | ParseErrorKind::UnexpectedToken
    ));
}

#[test]
fn missing_esac_fails() {
    let mut parser = parser_for("case x in a) echo a ;;\n", ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("case clause must require esac");
    assert!(matches!(
        error.kind,
        ParseErrorKind::UnexpectedEndOfInput | ParseErrorKind::UnexpectedToken
    ));
}

#[test]
fn unterminated_subshell_and_brace_group_fail() {
    let mut subshell = parser_for("(echo\n", ParseOptions::default());
    let subshell_error = subshell
        .parse_complete_command()
        .expect_err("unterminated subshell must fail");
    assert!(matches!(
        subshell_error.kind,
        ParseErrorKind::UnexpectedEndOfInput | ParseErrorKind::UnexpectedToken
    ));

    let mut brace = parser_for("{ echo\n", ParseOptions::default());
    let brace_error = brace
        .parse_complete_command()
        .expect_err("unterminated brace group must fail");
    assert!(matches!(
        brace_error.kind,
        ParseErrorKind::UnexpectedEndOfInput | ParseErrorKind::UnexpectedToken
    ));
}

#[test]
fn pipeline_negation_sets_flag() {
    let step = parse_one("! echo\n");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command");
    };

    assert!(command.list.and_ors[0].head.negated);
}

#[test]
fn node_limit_failure_stays_deterministic_on_valid_parse_path() {
    let mut parser = parser_for(
        "echo hi there\n",
        ParseOptions {
            max_ast_nodes: 2,
            ..Default::default()
        },
    );

    let error = parser
        .parse_complete_command()
        .expect_err("low node limit should fail");
    assert_eq!(error.kind, ParseErrorKind::AstNodeLimitExceeded);
}
