use kish::ir::{HirBuilder, HirCaseTerminator, HirOriginKind, HirRedirectOperator};
use kish::lexer::{ByteOffset, OperatorKind, SourceId, Span, Token, TokenKind};
use kish::parser::{
    AndOrAst, CaseClauseAst, CaseItemAst, CaseTerminatorAst, CommandAst, CompleteCommandAst,
    CompoundCommandAst, CompoundCommandNodeAst, ForClauseAst, FunctionDefinitionAst, IfClauseAst,
    ListAst, PipelineAst, ProgramAst, SimpleCommandAst, UntilClauseAst, WhileClauseAst, WordAst,
};

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

fn word_ast(lexeme: &str, start: u32, end: u32) -> WordAst {
    WordAst {
        token: token(lexeme, start, end),
        span: span(start, end),
    }
}

fn dummy_list() -> ListAst {
    ListAst {
        and_ors: vec![AndOrAst {
            head: PipelineAst {
                negated: false,
                commands: vec![CommandAst::Simple(SimpleCommandAst::default())],
                span: span(0, 1),
            },
            tail: Vec::new(),
            span: span(0, 1),
        }],
        span: span(0, 1),
    }
}

fn map_command_to_origin_kind(command: &CommandAst) -> HirOriginKind {
    match command {
        CommandAst::Simple(_) => HirOriginKind::SimpleCommandAst,
        CommandAst::Compound(_) => HirOriginKind::CompoundCommandNodeAst,
        CommandAst::FunctionDefinition(_) => HirOriginKind::FunctionDefinitionAst,
    }
}

fn map_compound_variant(compound: &CompoundCommandAst) -> &'static str {
    match compound {
        CompoundCommandAst::Subshell(_) => "subshell",
        CompoundCommandAst::BraceGroup(_) => "brace_group",
        CompoundCommandAst::If(_) => "if",
        CompoundCommandAst::For(_) => "for",
        CompoundCommandAst::While(_) => "while",
        CompoundCommandAst::Until(_) => "until",
        CompoundCommandAst::Case(_) => "case",
    }
}

fn classify_operator_for_hir(operator: OperatorKind) -> &'static str {
    match operator {
        OperatorKind::AndIf => "and_or",
        OperatorKind::OrIf => "and_or",
        OperatorKind::DoubleSemicolon => "none",
        OperatorKind::SemicolonAmpersand => "none",
        OperatorKind::HereDoc => "redirect",
        OperatorKind::HereDocStripTabs => "redirect",
        OperatorKind::AppendOutput => "redirect",
        OperatorKind::DupInput => "redirect",
        OperatorKind::DupOutput => "redirect",
        OperatorKind::ReadWrite => "redirect",
        OperatorKind::Clobber => "redirect",
        OperatorKind::Pipe => "none",
        OperatorKind::Semicolon => "list_terminator",
        OperatorKind::Ampersand => "list_terminator",
        OperatorKind::LeftParen => "none",
        OperatorKind::RightParen => "none",
        OperatorKind::Less => "redirect",
        OperatorKind::Greater => "redirect",
    }
}

#[test]
fn parser_command_variants_map_to_hir_targets_exhaustively() {
    let list = dummy_list();
    let command_variants = vec![
        CommandAst::Simple(SimpleCommandAst::default()),
        CommandAst::Compound(CompoundCommandNodeAst {
            kind: CompoundCommandAst::Subshell(list.clone()),
            redirects: Vec::new(),
            span: span(0, 1),
        }),
        CommandAst::FunctionDefinition(FunctionDefinitionAst {
            name: word_ast("f", 0, 1),
            body: Box::new(CommandAst::Compound(CompoundCommandNodeAst {
                kind: CompoundCommandAst::BraceGroup(list.clone()),
                redirects: Vec::new(),
                span: span(0, 1),
            })),
            redirects: Vec::new(),
            span: span(0, 1),
        }),
    ];

    let mapped: Vec<HirOriginKind> = command_variants
        .iter()
        .map(map_command_to_origin_kind)
        .collect();

    assert_eq!(
        mapped,
        vec![
            HirOriginKind::SimpleCommandAst,
            HirOriginKind::CompoundCommandNodeAst,
            HirOriginKind::FunctionDefinitionAst,
        ]
    );
}

#[test]
fn parser_compound_variants_map_to_hir_targets_exhaustively() {
    let list = dummy_list();

    let variants = vec![
        CompoundCommandAst::Subshell(list.clone()),
        CompoundCommandAst::BraceGroup(list.clone()),
        CompoundCommandAst::If(IfClauseAst {
            condition: list.clone(),
            then_body: list.clone(),
            elif_arms: vec![(list.clone(), list.clone())],
            else_body: Some(list.clone()),
            span: span(0, 1),
        }),
        CompoundCommandAst::For(ForClauseAst {
            name: word_ast("i", 0, 1),
            words: vec![word_ast("a", 2, 3)],
            body: list.clone(),
            span: span(0, 3),
        }),
        CompoundCommandAst::While(WhileClauseAst {
            condition: list.clone(),
            body: list.clone(),
            span: span(0, 1),
        }),
        CompoundCommandAst::Until(UntilClauseAst {
            condition: list.clone(),
            body: list.clone(),
            span: span(0, 1),
        }),
        CompoundCommandAst::Case(CaseClauseAst {
            word: word_ast("x", 0, 1),
            items: vec![CaseItemAst {
                patterns: vec![word_ast("a", 2, 3)],
                body: Some(list.clone()),
                terminator: Some(CaseTerminatorAst::DoubleSemicolon),
                span: span(2, 3),
            }],
            span: span(0, 3),
        }),
    ];

    let labels: Vec<&'static str> = variants.iter().map(map_compound_variant).collect();

    assert_eq!(
        labels,
        vec![
            "subshell",
            "brace_group",
            "if",
            "for",
            "while",
            "until",
            "case",
        ]
    );
}

#[test]
fn parser_case_terminators_map_to_hir_case_terminators() {
    assert_eq!(
        HirBuilder::case_terminator_from_ast(CaseTerminatorAst::DoubleSemicolon),
        HirCaseTerminator::Break
    );
    assert_eq!(
        HirBuilder::case_terminator_from_ast(CaseTerminatorAst::SemicolonAmpersand),
        HirCaseTerminator::Fallthrough
    );
}

#[test]
fn operator_mappings_are_exhaustive_for_hir_semantics() {
    for operator in OperatorKind::ALL {
        match classify_operator_for_hir(operator) {
            "and_or" => {
                assert!(HirBuilder::and_or_connector_from_operator(operator).is_ok());
                assert!(HirBuilder::list_terminator_from_operator(operator).is_err());
            }
            "list_terminator" => {
                assert!(HirBuilder::list_terminator_from_operator(operator).is_ok());
                assert!(HirBuilder::and_or_connector_from_operator(operator).is_err());
            }
            "redirect" => {
                let redirect = HirBuilder::redirect_operator_from_operator(operator)
                    .expect("redirect operator should map");
                match redirect {
                    HirRedirectOperator::Input
                    | HirRedirectOperator::Output
                    | HirRedirectOperator::AppendOutput
                    | HirRedirectOperator::DupInput
                    | HirRedirectOperator::DupOutput
                    | HirRedirectOperator::ReadWrite
                    | HirRedirectOperator::Clobber
                    | HirRedirectOperator::HereDoc
                    | HirRedirectOperator::HereDocStripTabs => {}
                }
            }
            "none" => {
                assert!(HirBuilder::redirect_operator_from_operator(operator).is_err());
            }
            other => panic!("unexpected classification label: {other}"),
        }
    }
}

#[test]
fn parser_program_contract_still_exists_for_phase4_lowering() {
    let program = ProgramAst {
        complete_commands: vec![CompleteCommandAst {
            list: dummy_list(),
            separator_op: None,
            span: span(0, 1),
        }],
        span: Some(span(0, 1)),
    };

    assert_eq!(program.complete_commands.len(), 1);
}
