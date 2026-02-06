//! HIR builder utilities with shape validation.

use crate::ir::error::IrError;
use crate::ir::hir::{
    HirAndOr, HirAndOrConnector, HirAssignment, HirCaseClause, HirCaseItem, HirCaseTerminator,
    HirCommand, HirCompleteCommand, HirCompoundCommand, HirCompoundCommandNode, HirForClause,
    HirFunctionDefinition, HirIfClause, HirList, HirListTerminator, HirOrigin, HirOriginKind,
    HirPipeline, HirProgram, HirRedirect, HirRedirectOperator, HirSimpleCommand, HirUntilClause,
    HirWhileClause, HirWord,
};
use crate::lexer::{OperatorKind, Span, Token};
use crate::parser::CaseTerminatorAst;

/// Builder for constructing validated HIR nodes.
#[derive(Debug, Default, Clone, Copy)]
pub struct HirBuilder;

impl HirBuilder {
    /// Creates an empty HIR builder.
    pub fn new() -> Self {
        Self
    }

    /// Creates one origin marker.
    pub const fn origin(kind: HirOriginKind) -> HirOrigin {
        HirOrigin::new(kind)
    }

    /// Builds a word node from a lexer token.
    pub fn word(&self, token: Token, span: Span) -> Result<HirWord, IrError> {
        if token.span != span {
            return Err(IrError::invariant_violation(
                Some(span),
                "word span does not match token span",
                format!(
                    "word span [{}, {}) does not equal token span [{}, {})",
                    span.start.value(),
                    span.end.value(),
                    token.span.start.value(),
                    token.span.end.value()
                ),
            ));
        }

        Ok(HirWord {
            token,
            span,
            origin: Self::origin(HirOriginKind::WordAst),
        })
    }

    /// Builds an assignment node and validates assignment shape.
    pub fn assignment(
        &self,
        token: Token,
        name: impl Into<String>,
        value: impl Into<String>,
        span: Span,
    ) -> Result<HirAssignment, IrError> {
        if token.span != span {
            return Err(IrError::invariant_violation(
                Some(span),
                "assignment span does not match token span",
                format!(
                    "assignment span [{}, {}) does not equal token span [{}, {})",
                    span.start.value(),
                    span.end.value(),
                    token.span.start.value(),
                    token.span.end.value()
                ),
            ));
        }

        let name = name.into();
        let value = value.into();

        if name.is_empty() {
            return Err(IrError::invalid_assignment_shape(
                Some(span),
                "assignment name must not be empty",
                "received empty assignment name",
            ));
        }

        if name.contains('=') {
            return Err(IrError::invalid_assignment_shape(
                Some(span),
                "assignment name must not contain '='",
                format!("received assignment name: {name}"),
            ));
        }

        if name.contains('\0') {
            return Err(IrError::invalid_assignment_shape(
                Some(span),
                "assignment name must not contain NUL",
                format!("received assignment name bytes: {:?}", name.as_bytes()),
            ));
        }

        Ok(HirAssignment {
            token,
            name,
            value,
            span,
            origin: Self::origin(HirOriginKind::AssignmentWordAst),
        })
    }

    /// Builds a redirect node and validates redirect shape.
    pub fn redirect(
        &self,
        fd_or_location: Option<Token>,
        operator: OperatorKind,
        target: HirWord,
        span: Span,
    ) -> Result<HirRedirect, IrError> {
        let fd = fd_or_location
            .as_ref()
            .map(|token| Self::parse_fd_token(token, span))
            .transpose()?;

        let operator = Self::redirect_operator_from_operator_with_span(operator, Some(span))?;

        Ok(HirRedirect {
            fd_or_location,
            fd,
            operator,
            target,
            span,
            origin: Self::origin(HirOriginKind::RedirectAst),
        })
    }

    /// Builds a simple-command node.
    pub fn simple_command(
        &self,
        assignments: Vec<HirAssignment>,
        words: Vec<HirWord>,
        redirects: Vec<HirRedirect>,
        span: Option<Span>,
    ) -> Result<HirSimpleCommand, IrError> {
        if assignments.is_empty() && words.is_empty() && redirects.is_empty() {
            return Err(IrError::invariant_violation(
                span,
                "simple command must not be empty",
                "expected at least one assignment, word, or redirect",
            ));
        }

        Ok(HirSimpleCommand {
            assignments,
            words,
            redirects,
            span,
            origin: Self::origin(HirOriginKind::SimpleCommandAst),
        })
    }

    /// Wraps a simple command into a command node.
    pub fn command_simple(&self, command: HirSimpleCommand) -> HirCommand {
        HirCommand::Simple(command)
    }

    /// Wraps a compound command into a command node.
    pub fn command_compound(&self, command: HirCompoundCommandNode) -> HirCommand {
        HirCommand::Compound(command)
    }

    /// Wraps a function definition into a command node.
    pub fn command_function_definition(&self, function: HirFunctionDefinition) -> HirCommand {
        HirCommand::FunctionDefinition(function)
    }

    /// Builds a pipeline node.
    pub fn pipeline(
        &self,
        negated: bool,
        commands: Vec<HirCommand>,
        span: Span,
    ) -> Result<HirPipeline, IrError> {
        if commands.is_empty() {
            return Err(IrError::invariant_violation(
                Some(span),
                "pipeline must include at least one command",
                "received an empty pipeline command list",
            ));
        }

        Ok(HirPipeline {
            negated,
            commands,
            span,
            origin: Self::origin(HirOriginKind::PipelineAst),
        })
    }

    /// Builds an and-or node.
    pub fn and_or(
        &self,
        head: HirPipeline,
        tail: Vec<(OperatorKind, HirPipeline)>,
        span: Span,
    ) -> Result<HirAndOr, IrError> {
        let mut normalized_tail = Vec::with_capacity(tail.len());
        for (operator, pipeline) in tail {
            let connector = Self::and_or_connector_from_operator_with_span(operator, Some(span))?;
            normalized_tail.push((connector, pipeline));
        }

        Ok(HirAndOr {
            head,
            tail: normalized_tail,
            span,
            origin: Self::origin(HirOriginKind::AndOrAst),
        })
    }

    /// Builds a list node.
    pub fn list(&self, and_ors: Vec<HirAndOr>, span: Span) -> Result<HirList, IrError> {
        if and_ors.is_empty() {
            return Err(IrError::invariant_violation(
                Some(span),
                "list must include at least one and_or",
                "received an empty list",
            ));
        }

        Ok(HirList {
            and_ors,
            span,
            origin: Self::origin(HirOriginKind::ListAst),
        })
    }

    /// Builds a complete-command node.
    pub fn complete_command(
        &self,
        list: HirList,
        separator_op: Option<OperatorKind>,
        span: Span,
    ) -> Result<HirCompleteCommand, IrError> {
        let separator_op = separator_op
            .map(|operator| Self::list_terminator_from_operator_with_span(operator, Some(span)))
            .transpose()?;

        Ok(HirCompleteCommand {
            list,
            separator_op,
            span,
            origin: Self::origin(HirOriginKind::CompleteCommandAst),
        })
    }

    /// Builds a program node.
    pub fn program(
        &self,
        complete_commands: Vec<HirCompleteCommand>,
        span: Option<Span>,
    ) -> Result<HirProgram, IrError> {
        Ok(HirProgram {
            complete_commands,
            span,
            origin: Self::origin(HirOriginKind::ProgramAst),
        })
    }

    /// Builds a compound-command wrapper.
    pub fn compound_command_node(
        &self,
        kind: HirCompoundCommand,
        redirects: Vec<HirRedirect>,
        span: Span,
    ) -> Result<HirCompoundCommandNode, IrError> {
        Ok(HirCompoundCommandNode {
            kind,
            redirects,
            span,
            origin: Self::origin(HirOriginKind::CompoundCommandNodeAst),
        })
    }

    /// Builds a subshell compound payload.
    pub fn compound_subshell(&self, list: HirList) -> HirCompoundCommand {
        HirCompoundCommand::Subshell(list)
    }

    /// Builds a brace-group compound payload.
    pub fn compound_brace_group(&self, list: HirList) -> HirCompoundCommand {
        HirCompoundCommand::BraceGroup(list)
    }

    /// Builds an if-clause payload.
    pub fn compound_if(&self, clause: HirIfClause) -> HirCompoundCommand {
        HirCompoundCommand::If(clause)
    }

    /// Builds a for-clause payload.
    pub fn compound_for(&self, clause: HirForClause) -> HirCompoundCommand {
        HirCompoundCommand::For(clause)
    }

    /// Builds a while-clause payload.
    pub fn compound_while(&self, clause: HirWhileClause) -> HirCompoundCommand {
        HirCompoundCommand::While(clause)
    }

    /// Builds an until-clause payload.
    pub fn compound_until(&self, clause: HirUntilClause) -> HirCompoundCommand {
        HirCompoundCommand::Until(clause)
    }

    /// Builds a case-clause payload.
    pub fn compound_case(&self, clause: HirCaseClause) -> HirCompoundCommand {
        HirCompoundCommand::Case(clause)
    }

    /// Builds a function-definition node.
    pub fn function_definition(
        &self,
        name: HirWord,
        body: HirCommand,
        redirects: Vec<HirRedirect>,
        span: Span,
    ) -> Result<HirFunctionDefinition, IrError> {
        let HirCommand::Compound(body) = body else {
            return Err(IrError::invariant_violation(
                Some(span),
                "function definition body must be compound",
                "received non-compound command body",
            ));
        };

        Ok(HirFunctionDefinition {
            name,
            body: Box::new(body),
            redirects,
            span,
            origin: Self::origin(HirOriginKind::FunctionDefinitionAst),
        })
    }

    /// Builds an if clause node.
    pub fn if_clause(
        &self,
        condition: HirList,
        then_body: HirList,
        elif_arms: Vec<(HirList, HirList)>,
        else_body: Option<HirList>,
        span: Span,
    ) -> Result<HirIfClause, IrError> {
        Ok(HirIfClause {
            condition,
            then_body,
            elif_arms,
            else_body,
            span,
            origin: Self::origin(HirOriginKind::IfClauseAst),
        })
    }

    /// Builds a for clause node.
    pub fn for_clause(
        &self,
        name: HirWord,
        words: Vec<HirWord>,
        body: HirList,
        span: Span,
    ) -> Result<HirForClause, IrError> {
        Ok(HirForClause {
            name,
            words,
            body,
            span,
            origin: Self::origin(HirOriginKind::ForClauseAst),
        })
    }

    /// Builds a while clause node.
    pub fn while_clause(
        &self,
        condition: HirList,
        body: HirList,
        span: Span,
    ) -> Result<HirWhileClause, IrError> {
        Ok(HirWhileClause {
            condition,
            body,
            span,
            origin: Self::origin(HirOriginKind::WhileClauseAst),
        })
    }

    /// Builds an until clause node.
    pub fn until_clause(
        &self,
        condition: HirList,
        body: HirList,
        span: Span,
    ) -> Result<HirUntilClause, IrError> {
        Ok(HirUntilClause {
            condition,
            body,
            span,
            origin: Self::origin(HirOriginKind::UntilClauseAst),
        })
    }

    /// Builds a case item node.
    pub fn case_item(
        &self,
        patterns: Vec<HirWord>,
        body: Option<HirList>,
        terminator: Option<CaseTerminatorAst>,
        span: Span,
    ) -> Result<HirCaseItem, IrError> {
        if patterns.is_empty() {
            return Err(IrError::invariant_violation(
                Some(span),
                "case item must include at least one pattern",
                "received an empty case pattern list",
            ));
        }

        Ok(HirCaseItem {
            patterns,
            body,
            terminator: terminator.map(Self::case_terminator_from_ast),
            span,
            origin: Self::origin(HirOriginKind::CaseItemAst),
        })
    }

    /// Builds a case clause node.
    pub fn case_clause(
        &self,
        word: HirWord,
        items: Vec<HirCaseItem>,
        span: Span,
    ) -> Result<HirCaseClause, IrError> {
        Ok(HirCaseClause {
            word,
            items,
            span,
            origin: Self::origin(HirOriginKind::CaseClauseAst),
        })
    }

    /// Converts parser `CaseTerminatorAst` to HIR semantics.
    pub fn case_terminator_from_ast(terminator: CaseTerminatorAst) -> HirCaseTerminator {
        match terminator {
            CaseTerminatorAst::DoubleSemicolon => HirCaseTerminator::Break,
            CaseTerminatorAst::SemicolonAmpersand => HirCaseTerminator::Fallthrough,
        }
    }

    /// Converts parser operator to HIR list terminator semantics.
    pub fn list_terminator_from_operator(
        operator: OperatorKind,
    ) -> Result<HirListTerminator, IrError> {
        Self::list_terminator_from_operator_with_span(operator, None)
    }

    /// Converts parser operator to HIR connector semantics.
    pub fn and_or_connector_from_operator(
        operator: OperatorKind,
    ) -> Result<HirAndOrConnector, IrError> {
        Self::and_or_connector_from_operator_with_span(operator, None)
    }

    /// Converts parser operator to HIR redirect semantics.
    pub fn redirect_operator_from_operator(
        operator: OperatorKind,
    ) -> Result<HirRedirectOperator, IrError> {
        Self::redirect_operator_from_operator_with_span(operator, None)
    }

    fn list_terminator_from_operator_with_span(
        operator: OperatorKind,
        span: Option<Span>,
    ) -> Result<HirListTerminator, IrError> {
        match operator {
            OperatorKind::Semicolon => Ok(HirListTerminator::Semicolon),
            OperatorKind::Ampersand => Ok(HirListTerminator::Ampersand),
            _ => Err(IrError::invariant_violation(
                span,
                "unsupported list terminator operator",
                format!("operator {:?} cannot terminate complete_command", operator),
            )),
        }
    }

    fn and_or_connector_from_operator_with_span(
        operator: OperatorKind,
        span: Option<Span>,
    ) -> Result<HirAndOrConnector, IrError> {
        match operator {
            OperatorKind::AndIf => Ok(HirAndOrConnector::AndIf),
            OperatorKind::OrIf => Ok(HirAndOrConnector::OrIf),
            _ => Err(IrError::invariant_violation(
                span,
                "unsupported and_or connector operator",
                format!("operator {:?} cannot connect and_or tail", operator),
            )),
        }
    }

    fn redirect_operator_from_operator_with_span(
        operator: OperatorKind,
        span: Option<Span>,
    ) -> Result<HirRedirectOperator, IrError> {
        match operator {
            OperatorKind::Less => Ok(HirRedirectOperator::Input),
            OperatorKind::Greater => Ok(HirRedirectOperator::Output),
            OperatorKind::AppendOutput => Ok(HirRedirectOperator::AppendOutput),
            OperatorKind::DupInput => Ok(HirRedirectOperator::DupInput),
            OperatorKind::DupOutput => Ok(HirRedirectOperator::DupOutput),
            OperatorKind::ReadWrite => Ok(HirRedirectOperator::ReadWrite),
            OperatorKind::Clobber => Ok(HirRedirectOperator::Clobber),
            OperatorKind::HereDoc => Ok(HirRedirectOperator::HereDoc),
            OperatorKind::HereDocStripTabs => Ok(HirRedirectOperator::HereDocStripTabs),
            _ => Err(IrError::invalid_redirect_shape(
                span,
                "unsupported redirect operator",
                format!("operator {:?} is not a redirect operator", operator),
            )),
        }
    }

    fn parse_fd_token(token: &Token, span: Span) -> Result<u16, IrError> {
        if token.lexeme.is_empty() || !token.lexeme.bytes().all(|byte| byte.is_ascii_digit()) {
            return Err(IrError::invalid_redirect_shape(
                Some(span),
                "redirect file descriptor must be numeric",
                format!("received fd_or_location token: {}", token.lexeme),
            ));
        }

        token.lexeme.parse::<u16>().map_err(|error| {
            IrError::invalid_redirect_shape(
                Some(span),
                "redirect file descriptor is out of range",
                format!("fd token '{}' parse error: {error}", token.lexeme),
            )
        })
    }
}
