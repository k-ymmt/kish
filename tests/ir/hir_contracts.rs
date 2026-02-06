use kish::ir::{
    HirAndOr, HirAndOrConnector, HirAssignment, HirCaseClause, HirCaseItem, HirCaseTerminator,
    HirCommand, HirCompleteCommand, HirCompoundCommand, HirCompoundCommandNode, HirForClause,
    HirFunctionDefinition, HirIfClause, HirList, HirListTerminator, HirModule, HirOrigin,
    HirOriginKind, HirPipeline, HirProgram, HirRedirect, HirRedirectOperator, HirSimpleCommand,
    HirUntilClause, HirWhileClause, HirWord,
};
use kish::lexer::{ByteOffset, SourceId, Span, Token, TokenKind};

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

#[test]
fn hir_contract_types_are_public_and_constructible() {
    let _origin_kinds = [
        HirOriginKind::ProgramAst,
        HirOriginKind::CompleteCommandAst,
        HirOriginKind::ListAst,
        HirOriginKind::AndOrAst,
        HirOriginKind::PipelineAst,
        HirOriginKind::CommandAst,
        HirOriginKind::SimpleCommandAst,
        HirOriginKind::CompoundCommandNodeAst,
        HirOriginKind::CompoundCommandAst,
        HirOriginKind::FunctionDefinitionAst,
        HirOriginKind::RedirectAst,
        HirOriginKind::WordAst,
        HirOriginKind::AssignmentWordAst,
        HirOriginKind::IfClauseAst,
        HirOriginKind::ForClauseAst,
        HirOriginKind::WhileClauseAst,
        HirOriginKind::UntilClauseAst,
        HirOriginKind::CaseClauseAst,
        HirOriginKind::CaseItemAst,
        HirOriginKind::CaseTerminatorAst,
    ];

    let _redirect_ops = [
        HirRedirectOperator::Input,
        HirRedirectOperator::Output,
        HirRedirectOperator::AppendOutput,
        HirRedirectOperator::DupInput,
        HirRedirectOperator::DupOutput,
        HirRedirectOperator::ReadWrite,
        HirRedirectOperator::Clobber,
        HirRedirectOperator::HereDoc,
        HirRedirectOperator::HereDocStripTabs,
    ];

    let base_origin = HirOrigin::new(HirOriginKind::WordAst);
    let word = HirWord {
        token: token("echo", 0, 4),
        span: span(0, 4),
        origin: base_origin,
    };

    let assignment = HirAssignment {
        token: token("x=1", 5, 8),
        name: "x".to_string(),
        value: "1".to_string(),
        span: span(5, 8),
        origin: HirOrigin::new(HirOriginKind::AssignmentWordAst),
    };

    let redirect = HirRedirect {
        fd_or_location: Some(token("2", 9, 10)),
        fd: Some(2),
        operator: HirRedirectOperator::Output,
        target: HirWord {
            token: token("out", 11, 14),
            span: span(11, 14),
            origin: HirOrigin::new(HirOriginKind::WordAst),
        },
        span: span(9, 14),
        origin: HirOrigin::new(HirOriginKind::RedirectAst),
    };

    let simple = HirSimpleCommand {
        assignments: vec![assignment],
        words: vec![word.clone()],
        redirects: vec![redirect],
        span: Some(span(0, 14)),
        origin: HirOrigin::new(HirOriginKind::SimpleCommandAst),
    };

    let simple_command = HirCommand::Simple(simple.clone());
    let pipeline = HirPipeline {
        negated: false,
        commands: vec![simple_command.clone()],
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::PipelineAst),
    };

    let and_or = HirAndOr {
        head: pipeline.clone(),
        tail: vec![(HirAndOrConnector::AndIf, pipeline.clone())],
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::AndOrAst),
    };

    let list = HirList {
        and_ors: vec![and_or.clone()],
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::ListAst),
    };

    let if_clause = HirIfClause {
        condition: list.clone(),
        then_body: list.clone(),
        elif_arms: vec![(list.clone(), list.clone())],
        else_body: Some(list.clone()),
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::IfClauseAst),
    };

    let for_clause = HirForClause {
        name: word.clone(),
        words: vec![word.clone()],
        body: list.clone(),
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::ForClauseAst),
    };

    let while_clause = HirWhileClause {
        condition: list.clone(),
        body: list.clone(),
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::WhileClauseAst),
    };

    let until_clause = HirUntilClause {
        condition: list.clone(),
        body: list.clone(),
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::UntilClauseAst),
    };

    let case_item = HirCaseItem {
        patterns: vec![word.clone()],
        body: Some(list.clone()),
        terminator: Some(HirCaseTerminator::Break),
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::CaseItemAst),
    };

    let case_clause = HirCaseClause {
        word: word.clone(),
        items: vec![case_item],
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::CaseClauseAst),
    };

    let subshell_node = HirCompoundCommandNode {
        kind: HirCompoundCommand::Subshell(list.clone()),
        redirects: Vec::new(),
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::CompoundCommandNodeAst),
    };

    let _compound_variants = vec![
        HirCompoundCommand::Subshell(list.clone()),
        HirCompoundCommand::BraceGroup(list.clone()),
        HirCompoundCommand::If(if_clause),
        HirCompoundCommand::For(for_clause),
        HirCompoundCommand::While(while_clause),
        HirCompoundCommand::Until(until_clause),
        HirCompoundCommand::Case(case_clause),
    ];

    let function = HirFunctionDefinition {
        name: word,
        body: Box::new(subshell_node.clone()),
        redirects: Vec::new(),
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::FunctionDefinitionAst),
    };

    let _command_variants = vec![
        simple_command,
        HirCommand::Compound(subshell_node),
        HirCommand::FunctionDefinition(function),
    ];

    let complete = HirCompleteCommand {
        list,
        separator_op: Some(HirListTerminator::Semicolon),
        span: span(0, 14),
        origin: HirOrigin::new(HirOriginKind::CompleteCommandAst),
    };

    let program = HirProgram {
        complete_commands: vec![complete],
        span: Some(span(0, 14)),
        origin: HirOrigin::new(HirOriginKind::ProgramAst),
    };

    let module_alias: HirModule = program.clone();
    assert_eq!(module_alias.complete_commands.len(), 1);
}
