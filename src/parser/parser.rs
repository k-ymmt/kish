//! Parser entrypoints and phase contracts.

use crate::lexer::{DelimiterContext, OperatorKind, SourceId, Span, Token, TokenKind};
use crate::parser::arena::AstArena;
use crate::parser::ast::{
    AndOrAst, AssignmentWordAst, AstBuilder, CaseClauseAst, CaseItemAst, CaseTerminatorAst,
    CommandAst, CompleteCommandAst, CompoundCommandAst, CompoundCommandNodeAst, ForClauseAst,
    FunctionDefinitionAst, IfClauseAst, ListAst, PipelineAst, ProgramAst, RedirectAst,
    SimpleCommandAst, UntilClauseAst, WhileClauseAst, WordAst,
};
use crate::parser::classifier::{
    ClassificationContext, ClassificationOptions, ClassifiedTokenKind, Classifier, NameContext,
    ReservedWord, ReservedWordPolicy, split_assignment_word,
};
use crate::parser::error::{ParseError, ParseErrorKind};
use crate::parser::recovery::{NeedMoreInputReason, ParseDiagnostic};
use crate::parser::token_stream::{TokenStream, TokenStreamError};

/// Parser behavior options.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ParseOptions {
    /// Interactive-mode parsing behavior.
    pub interactive: bool,
    /// Maximum syntactic nesting depth.
    pub max_nesting: usize,
    /// Maximum AST nodes allowed during parsing.
    pub max_ast_nodes: usize,
    /// Enables optional `IO_LOCATION` classification.
    pub allow_io_location: bool,
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            interactive: false,
            max_nesting: 256,
            max_ast_nodes: 100_000,
            allow_io_location: false,
        }
    }
}

/// One complete-command parse step result.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseStep {
    /// One complete command was parsed.
    Complete(CompleteCommandAst),
    /// More input is needed to continue safely.
    NeedMoreInput(NeedMoreInputReason),
    /// No additional commands are available.
    EndOfInput,
}

/// POSIX-first parser scaffold.
pub struct Parser<'a> {
    source_id: SourceId,
    options: ParseOptions,
    token_stream: TokenStream<'a>,
    classifier: Classifier,
    diagnostics: Vec<ParseDiagnostic>,
    arena: AstArena,
    function_body_rule9_depth: usize,
    nesting_depth: usize,
}

impl<'a> Parser<'a> {
    /// Creates a parser with explicit source id, options, and token stream.
    pub fn new(source_id: SourceId, options: ParseOptions, token_stream: TokenStream<'a>) -> Self {
        Self {
            source_id,
            options,
            token_stream,
            classifier: Classifier::new(),
            diagnostics: Vec::new(),
            arena: AstArena::new(options.max_ast_nodes),
            function_body_rule9_depth: 0,
            nesting_depth: 0,
        }
    }

    /// Parses one complete command boundary.
    pub fn parse_complete_command(&mut self) -> Result<ParseStep, ParseError> {
        match self.parse_complete_command_public() {
            Ok(step) => Ok(step),
            Err(ParseFailure::NeedMoreInput(reason)) => {
                if self.options.interactive {
                    Ok(ParseStep::NeedMoreInput(reason))
                } else {
                    Err(ParseError::from_need_more_reason(reason))
                }
            }
            Err(ParseFailure::Error(error)) => Err(error),
        }
    }

    /// Parses a whole program.
    pub fn parse_program(&mut self) -> Result<ProgramAst, ParseError> {
        match self.parse_program_nonterminal() {
            Ok(program) => Ok(program),
            Err(ParseFailure::NeedMoreInput(reason)) => {
                Err(ParseError::from_need_more_reason(reason))
            }
            Err(ParseFailure::Error(error)) => Err(error),
        }
    }

    /// Returns parser diagnostics collected so far.
    pub fn diagnostics(&self) -> &[ParseDiagnostic] {
        &self.diagnostics
    }

    /// Returns parser source id.
    pub fn source_id(&self) -> SourceId {
        self.source_id
    }

    fn parse_complete_command_public(&mut self) -> ParseResult<ParseStep> {
        self.parse_linebreak()?;

        let Some(_) = self.peek_token_ref(0)? else {
            return Ok(ParseStep::EndOfInput);
        };

        let command = self.parse_complete_command_nonterminal()?;
        Ok(ParseStep::Complete(command))
    }

    fn parse_program_nonterminal(&mut self) -> ParseResult<ProgramAst> {
        self.parse_linebreak()?;

        let mut complete_commands = Vec::new();
        if self.can_start_complete_command()? {
            complete_commands = self.parse_complete_commands_nonterminal()?;
        }

        self.parse_linebreak()?;

        if let Some(token) = self.peek_token_ref(0)? {
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                token,
                ["EOF"],
            )));
        }

        let span = span_of_complete_commands(&complete_commands);
        self.build_program(complete_commands, span)
    }

    fn parse_complete_commands_nonterminal(&mut self) -> ParseResult<Vec<CompleteCommandAst>> {
        let mut commands = vec![self.parse_complete_command_nonterminal()?];

        loop {
            if self.parse_newline_list()? == 0 {
                break;
            }

            if !self.can_start_complete_command()? {
                break;
            }

            commands.push(self.parse_complete_command_nonterminal()?);
        }

        Ok(commands)
    }

    fn parse_complete_command_nonterminal(&mut self) -> ParseResult<CompleteCommandAst> {
        let list = self.parse_list_nonterminal()?;
        let separator = self.parse_separator_op_token()?;
        let separator_kind = separator
            .as_ref()
            .map(|token| expect_operator_kind(token, "separator operator"))
            .transpose()?;

        let span = match separator {
            Some(token) => merge_spans(list.span, token.span),
            None => list.span,
        };

        self.build_complete_command(list, separator_kind, span)
    }

    fn parse_list_nonterminal(&mut self) -> ParseResult<ListAst> {
        let mut and_ors = vec![self.parse_and_or_nonterminal()?];

        loop {
            if !self.peek_is_separator_op()? {
                break;
            }

            if !self.can_start_pipeline_at(1)? {
                break;
            }

            let _separator = self.next_token()?.ok_or_else(|| {
                ParseFailure::Error(ParseError::unexpected_end_of_input(["and_or"]))
            })?;
            and_ors.push(self.parse_and_or_nonterminal()?);
        }

        let span = span_of_and_ors(&and_ors)
            .ok_or_else(|| ParseFailure::Error(ParseError::unexpected_end_of_input(["and_or"])))?;

        self.build_list(and_ors, span)
    }

    fn parse_and_or_nonterminal(&mut self) -> ParseResult<AndOrAst> {
        let head = self.parse_pipeline_nonterminal()?;
        let mut tail = Vec::new();

        loop {
            let operator_kind = match self.peek_operator_kind(0)? {
                Some(OperatorKind::AndIf) => OperatorKind::AndIf,
                Some(OperatorKind::OrIf) => OperatorKind::OrIf,
                _ => break,
            };

            let _operator_token = self.consume_operator_kind(operator_kind)?;
            self.parse_linebreak()?;
            let pipeline = match operator_kind {
                OperatorKind::AndIf => Self::map_unexpected_eof_to_need_more(
                    self.parse_pipeline_nonterminal(),
                    NeedMoreInputReason::TrailingAndIfOperator,
                )?,
                OperatorKind::OrIf => Self::map_unexpected_eof_to_need_more(
                    self.parse_pipeline_nonterminal(),
                    NeedMoreInputReason::TrailingOrIfOperator,
                )?,
                _ => unreachable!("only && and || are accepted here"),
            };
            tail.push((operator_kind, pipeline));
        }

        let span = span_of_and_or(&head, &tail);
        self.build_and_or(head, tail, span)
    }

    fn parse_pipeline_nonterminal(&mut self) -> ParseResult<PipelineAst> {
        let mut negated = false;
        let mut start_span = None;

        if let Some(token) = self.peek_token(0)?
            && is_bang_token(&token)
        {
            let consumed = self.next_token()?.ok_or_else(|| {
                ParseFailure::Error(ParseError::unexpected_end_of_input(["pipeline command"]))
            })?;
            negated = true;
            start_span = Some(consumed.span);
        }

        let mut commands = vec![self.parse_command_nonterminal()?];

        while self.peek_operator_kind(0)? == Some(OperatorKind::Pipe) {
            let _pipe = self.consume_operator_kind(OperatorKind::Pipe)?;
            self.parse_linebreak()?;
            let command = Self::map_unexpected_eof_to_need_more(
                self.parse_command_nonterminal(),
                NeedMoreInputReason::TrailingPipeOperator,
            )?;
            commands.push(command);
        }

        let first_span = span_of_command(&commands[0]);
        let last_span = span_of_command(commands.last().expect("non-empty command list"));
        let span = if let Some(negate_span) = start_span {
            merge_spans(negate_span, last_span)
        } else {
            merge_spans(first_span, last_span)
        };

        self.build_pipeline(negated, commands, span)
    }

    fn parse_command_nonterminal(&mut self) -> ParseResult<CommandAst> {
        let head = self
            .peek_token(0)?
            .ok_or_else(|| ParseFailure::Error(ParseError::unexpected_end_of_input(["command"])))?;

        match head.kind {
            TokenKind::Newline => {
                return Err(ParseFailure::Error(ParseError::unexpected_token(
                    &head,
                    ["command"],
                )));
            }
            TokenKind::Operator(kind) => {
                if kind == OperatorKind::LeftParen {
                    let compound = self.parse_compound_command_nonterminal()?;
                    return self.build_command_compound(compound);
                }
                if is_redirect_operator(kind) {
                    let simple = self.parse_simple_command_nonterminal()?;
                    return self.build_command_simple(simple);
                }
                return Err(ParseFailure::Error(ParseError::unexpected_token(
                    &head,
                    ["command"],
                )));
            }
            TokenKind::Token => {}
        }

        if self.is_function_definition_head_at_command_position()? {
            return self.parse_function_definition_nonterminal();
        }
        if self.can_start_compound_command()? {
            let compound = self.parse_compound_command_nonterminal()?;
            return self.build_command_compound(compound);
        }

        let head_kind = self.classify_token_at(
            0,
            ClassificationContext {
                reserved_word_policy: ReservedWordPolicy::Any,
                ..Default::default()
            },
        )?;
        match head_kind {
            ClassifiedTokenKind::ReservedWord(word) if is_unimplemented_reserved(word) => {
                return Err(ParseFailure::Error(ParseError::unexpected_token(
                    &head,
                    ["WORD"],
                )));
            }
            ClassifiedTokenKind::Word
            | ClassifiedTokenKind::Name
            | ClassifiedTokenKind::AssignmentWord
            | ClassifiedTokenKind::IoNumber(_)
            | ClassifiedTokenKind::IoLocation => {}
            _ => {
                return Err(ParseFailure::Error(ParseError::unexpected_token(
                    &head,
                    ["WORD"],
                )));
            }
        }

        let simple = self.parse_simple_command_nonterminal()?;
        self.build_command_simple(simple)
    }

    fn can_start_compound_command(&mut self) -> ParseResult<bool> {
        let Some(head) = self.peek_token(0)? else {
            return Ok(false);
        };

        match head.kind {
            TokenKind::Operator(OperatorKind::LeftParen) => Ok(true),
            TokenKind::Operator(_) | TokenKind::Newline => Ok(false),
            TokenKind::Token => {
                if is_plain_brace_open_token(&head) {
                    return Ok(true);
                }

                Ok(matches!(
                    self.peek_reserved_word(ReservedWordPolicy::Any)?,
                    Some(
                        ReservedWord::If
                            | ReservedWord::For
                            | ReservedWord::While
                            | ReservedWord::Until
                            | ReservedWord::Case
                    )
                ))
            }
        }
    }

    fn parse_compound_command_nonterminal(&mut self) -> ParseResult<CompoundCommandNodeAst> {
        let (kind, payload_span) = self.parse_compound_payload_nonterminal()?;
        let redirects = self.parse_redirect_list_nonterminal()?;
        let span = redirects
            .last()
            .map(|redirect| merge_spans(payload_span, redirect.span))
            .unwrap_or(payload_span);
        self.build_compound_command_node(kind, redirects, span)
    }

    fn parse_compound_payload_nonterminal(&mut self) -> ParseResult<(CompoundCommandAst, Span)> {
        self.with_nesting_guard(|parser| {
            let head = parser.peek_token(0)?.ok_or_else(|| {
                ParseFailure::Error(ParseError::unexpected_end_of_input(["compound_command"]))
            })?;

            match head.kind {
                TokenKind::Operator(OperatorKind::LeftParen) => parser.parse_subshell_nonterminal(),
                TokenKind::Operator(_) | TokenKind::Newline => Err(ParseFailure::Error(
                    ParseError::unexpected_token(&head, ["compound_command"]),
                )),
                TokenKind::Token if is_plain_brace_open_token(&head) => {
                    parser.parse_brace_group_nonterminal()
                }
                TokenKind::Token => match parser.peek_reserved_word(ReservedWordPolicy::Any)? {
                    Some(ReservedWord::If) => parser.parse_if_clause_nonterminal(),
                    Some(ReservedWord::For) => parser.parse_for_clause_nonterminal(),
                    Some(ReservedWord::While) => parser.parse_while_clause_nonterminal(),
                    Some(ReservedWord::Until) => parser.parse_until_clause_nonterminal(),
                    Some(ReservedWord::Case) => parser.parse_case_clause_nonterminal(),
                    _ => Err(ParseFailure::Error(ParseError::unexpected_token(
                        &head,
                        ["compound_command"],
                    ))),
                },
            }
        })
    }

    fn parse_subshell_nonterminal(&mut self) -> ParseResult<(CompoundCommandAst, Span)> {
        let start = self.consume_operator_kind(OperatorKind::LeftParen)?;
        let body = Self::map_unexpected_eof_to_need_more(
            self.parse_compound_list_nonterminal(StopSet::right_paren()),
            NeedMoreInputReason::UnclosedSubshell,
        )?;
        let end = Self::map_unexpected_eof_to_need_more(
            self.consume_operator_kind(OperatorKind::RightParen),
            NeedMoreInputReason::UnclosedSubshell,
        )?;
        let span = merge_spans(start.span, end.span);
        let kind = self.build_compound_subshell(body)?;
        Ok((kind, span))
    }

    fn parse_brace_group_nonterminal(&mut self) -> ParseResult<(CompoundCommandAst, Span)> {
        let start = self.consume_plain_token("{")?;
        let body = Self::map_unexpected_eof_to_need_more(
            self.parse_compound_list_nonterminal(StopSet::right_brace()),
            NeedMoreInputReason::UnclosedBraceGroup,
        )?;
        let end = Self::map_unexpected_eof_to_need_more(
            self.consume_plain_token("}"),
            NeedMoreInputReason::UnclosedBraceGroup,
        )?;
        let span = merge_spans(start.span, end.span);
        let kind = self.build_compound_brace_group(body)?;
        Ok((kind, span))
    }

    fn parse_if_clause_nonterminal(&mut self) -> ParseResult<(CompoundCommandAst, Span)> {
        let start = self.consume_reserved_word(ReservedWord::If, ReservedWordPolicy::Any)?;
        let condition = Self::map_unexpected_eof_to_need_more(
            self.parse_compound_list_nonterminal(StopSet::then_only()),
            NeedMoreInputReason::UnclosedIfClause,
        )?;
        let _then = Self::map_unexpected_eof_to_need_more(
            self.consume_reserved_word(ReservedWord::Then, ReservedWordPolicy::Any),
            NeedMoreInputReason::UnclosedIfClause,
        )?;
        let then_body = Self::map_unexpected_eof_to_need_more(
            self.parse_compound_list_nonterminal(StopSet::else_elif_fi()),
            NeedMoreInputReason::UnclosedIfClause,
        )?;

        let mut elif_arms = Vec::new();
        while self.peek_reserved_word(ReservedWordPolicy::Any)? == Some(ReservedWord::Elif) {
            let _elif = Self::map_unexpected_eof_to_need_more(
                self.consume_reserved_word(ReservedWord::Elif, ReservedWordPolicy::Any),
                NeedMoreInputReason::UnclosedIfClause,
            )?;
            let elif_condition = Self::map_unexpected_eof_to_need_more(
                self.parse_compound_list_nonterminal(StopSet::then_only()),
                NeedMoreInputReason::UnclosedIfClause,
            )?;
            let _then = Self::map_unexpected_eof_to_need_more(
                self.consume_reserved_word(ReservedWord::Then, ReservedWordPolicy::Any),
                NeedMoreInputReason::UnclosedIfClause,
            )?;
            let elif_body = Self::map_unexpected_eof_to_need_more(
                self.parse_compound_list_nonterminal(StopSet::else_elif_fi()),
                NeedMoreInputReason::UnclosedIfClause,
            )?;
            elif_arms.push((elif_condition, elif_body));
        }

        let else_body =
            if self.peek_reserved_word(ReservedWordPolicy::Any)? == Some(ReservedWord::Else) {
                let _else = Self::map_unexpected_eof_to_need_more(
                    self.consume_reserved_word(ReservedWord::Else, ReservedWordPolicy::Any),
                    NeedMoreInputReason::UnclosedIfClause,
                )?;
                Some(Self::map_unexpected_eof_to_need_more(
                    self.parse_compound_list_nonterminal(StopSet::fi_only()),
                    NeedMoreInputReason::UnclosedIfClause,
                )?)
            } else {
                None
            };

        let end = Self::map_unexpected_eof_to_need_more(
            self.consume_reserved_word(ReservedWord::Fi, ReservedWordPolicy::Any),
            NeedMoreInputReason::UnclosedIfClause,
        )?;
        let span = merge_spans(start.span, end.span);
        let clause = self.build_if_clause(condition, then_body, elif_arms, else_body, span)?;
        let kind = self.build_compound_if(clause)?;
        Ok((kind, span))
    }

    fn parse_for_clause_nonterminal(&mut self) -> ParseResult<(CompoundCommandAst, Span)> {
        let start = self.consume_reserved_word(ReservedWord::For, ReservedWordPolicy::Any)?;
        let name = self.parse_name_word_nonterminal()?;

        self.parse_linebreak()?;
        let mut words = Vec::new();
        if self.peek_reserved_word(ReservedWordPolicy::InOnly)? == Some(ReservedWord::In) {
            let _in = self.consume_reserved_word(ReservedWord::In, ReservedWordPolicy::InOnly)?;
            if !self.parse_sequential_sep()? {
                words = self.parse_wordlist_nonterminal()?;
                if !self.parse_sequential_sep()? {
                    let found = self.peek_token(0)?.ok_or_else(|| {
                        ParseFailure::Error(ParseError::unexpected_end_of_input([
                            "for clause separator",
                        ]))
                    })?;
                    return Err(ParseFailure::Error(ParseError::unexpected_token(
                        &found,
                        ["for clause separator"],
                    )));
                }
            }
        } else {
            let _ = self.parse_sequential_sep()?;
        }

        let (body, do_group_span) = self.parse_do_group_nonterminal()?;
        let span = merge_spans(start.span, do_group_span);
        let clause = self.build_for_clause(name, words, body, span)?;
        let kind = self.build_compound_for(clause)?;
        Ok((kind, span))
    }

    fn parse_while_clause_nonterminal(&mut self) -> ParseResult<(CompoundCommandAst, Span)> {
        let start = self.consume_reserved_word(ReservedWord::While, ReservedWordPolicy::Any)?;
        let condition = self.parse_compound_list_nonterminal(StopSet::do_only())?;
        let (body, do_group_span) = self.parse_do_group_nonterminal()?;
        let span = merge_spans(start.span, do_group_span);
        let clause = self.build_while_clause(condition, body, span)?;
        let kind = self.build_compound_while(clause)?;
        Ok((kind, span))
    }

    fn parse_until_clause_nonterminal(&mut self) -> ParseResult<(CompoundCommandAst, Span)> {
        let start = self.consume_reserved_word(ReservedWord::Until, ReservedWordPolicy::Any)?;
        let condition = self.parse_compound_list_nonterminal(StopSet::do_only())?;
        let (body, do_group_span) = self.parse_do_group_nonterminal()?;
        let span = merge_spans(start.span, do_group_span);
        let clause = self.build_until_clause(condition, body, span)?;
        let kind = self.build_compound_until(clause)?;
        Ok((kind, span))
    }

    fn parse_case_clause_nonterminal(&mut self) -> ParseResult<(CompoundCommandAst, Span)> {
        let start = self.consume_reserved_word(ReservedWord::Case, ReservedWordPolicy::Any)?;
        let word = self.parse_plain_word_nonterminal()?;
        self.parse_linebreak()?;
        let _in = Self::map_unexpected_eof_to_need_more(
            self.consume_reserved_word(ReservedWord::In, ReservedWordPolicy::InOnly),
            NeedMoreInputReason::UnclosedCaseClause,
        )?;
        self.parse_linebreak()?;

        let mut items = Vec::new();
        while self.peek_reserved_word(ReservedWordPolicy::EsacOnly)? != Some(ReservedWord::Esac) {
            let item = Self::map_unexpected_eof_to_need_more(
                self.parse_case_item_nonterminal(),
                NeedMoreInputReason::UnclosedCaseClause,
            )?;
            let has_terminator = item.terminator.is_some();
            items.push(item);

            if !has_terminator {
                if self.peek_reserved_word(ReservedWordPolicy::EsacOnly)?
                    != Some(ReservedWord::Esac)
                {
                    let found = self.peek_token(0)?.ok_or_else(|| {
                        ParseFailure::Error(ParseError::unexpected_end_of_input(["esac"]))
                    })?;
                    return Err(ParseFailure::Error(ParseError::unexpected_token(
                        &found,
                        ["esac"],
                    )));
                }
                break;
            }
        }

        let end = Self::map_unexpected_eof_to_need_more(
            self.consume_reserved_word(ReservedWord::Esac, ReservedWordPolicy::EsacOnly),
            NeedMoreInputReason::UnclosedCaseClause,
        )?;
        let span = merge_spans(start.span, end.span);
        let clause = self.build_case_clause(word, items, span)?;
        let kind = self.build_compound_case(clause)?;
        Ok((kind, span))
    }

    fn parse_case_item_nonterminal(&mut self) -> ParseResult<CaseItemAst> {
        let (patterns, start_span) = self.parse_case_pattern_list_nonterminal()?;
        let close = self.consume_operator_kind(OperatorKind::RightParen)?;
        self.parse_linebreak()?;

        let mut body = None;
        let mut terminator = None;
        let mut end_span = close.span;

        if let Some((term, token)) = self.consume_case_item_terminator()? {
            terminator = Some(term);
            end_span = token.span;
            self.parse_linebreak()?;
        } else if self.peek_reserved_word(ReservedWordPolicy::EsacOnly)? == Some(ReservedWord::Esac)
        {
            // no-op: empty non-terminated case item
        } else {
            let parsed_body = self.parse_compound_list_nonterminal(StopSet::case_item_body())?;
            end_span = parsed_body.span;
            body = Some(parsed_body);

            if let Some((term, token)) = self.consume_case_item_terminator()? {
                terminator = Some(term);
                end_span = token.span;
                self.parse_linebreak()?;
            }
        }

        let span = merge_spans(start_span, end_span);
        self.build_case_item(patterns, body, terminator, span)
    }

    fn parse_case_pattern_list_nonterminal(&mut self) -> ParseResult<(Vec<WordAst>, Span)> {
        let mut start_span = None;
        if self.peek_operator_kind(0)? == Some(OperatorKind::LeftParen) {
            let token = self.consume_operator_kind(OperatorKind::LeftParen)?;
            start_span = Some(token.span);
        }

        let first = self.parse_plain_word_nonterminal()?;
        let mut patterns = vec![first];
        while self.peek_operator_kind(0)? == Some(OperatorKind::Pipe) {
            let _pipe = self.consume_operator_kind(OperatorKind::Pipe)?;
            patterns.push(self.parse_plain_word_nonterminal()?);
        }

        let span = start_span.unwrap_or(patterns[0].span);
        Ok((patterns, span))
    }

    fn consume_case_item_terminator(&mut self) -> ParseResult<Option<(CaseTerminatorAst, Token)>> {
        match self.peek_operator_kind(0)? {
            Some(OperatorKind::DoubleSemicolon) => {
                let token = self.consume_operator_kind(OperatorKind::DoubleSemicolon)?;
                Ok(Some((CaseTerminatorAst::DoubleSemicolon, token)))
            }
            Some(OperatorKind::SemicolonAmpersand) => {
                let token = self.consume_operator_kind(OperatorKind::SemicolonAmpersand)?;
                Ok(Some((CaseTerminatorAst::SemicolonAmpersand, token)))
            }
            _ => Ok(None),
        }
    }

    fn parse_do_group_nonterminal(&mut self) -> ParseResult<(ListAst, Span)> {
        let start = Self::map_unexpected_eof_to_need_more(
            self.consume_reserved_word(ReservedWord::Do, ReservedWordPolicy::InOrDo),
            NeedMoreInputReason::UnclosedDoGroup,
        )?;
        let body = Self::map_unexpected_eof_to_need_more(
            self.parse_compound_list_nonterminal(StopSet::done_only()),
            NeedMoreInputReason::UnclosedDoGroup,
        )?;
        let end = Self::map_unexpected_eof_to_need_more(
            self.consume_reserved_word(ReservedWord::Done, ReservedWordPolicy::Any),
            NeedMoreInputReason::UnclosedDoGroup,
        )?;
        Ok((body, merge_spans(start.span, end.span)))
    }

    fn parse_compound_list_nonterminal(&mut self, stop: StopSet) -> ParseResult<ListAst> {
        self.parse_linebreak()?;
        self.parse_term_nonterminal(stop)
    }

    fn parse_term_nonterminal(&mut self, stop: StopSet) -> ParseResult<ListAst> {
        if self.at_compound_stop(stop)? {
            let found = self.peek_token(0)?.ok_or_else(|| {
                ParseFailure::Error(ParseError::unexpected_end_of_input(["and_or"]))
            })?;
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                &found,
                ["and_or"],
            )));
        }

        let mut and_ors = vec![self.parse_and_or_nonterminal()?];
        loop {
            if !self.parse_separator_nonterminal(stop)? {
                break;
            }

            if self.at_compound_stop(stop)? {
                break;
            }

            and_ors.push(self.parse_and_or_nonterminal()?);
        }

        let span = span_of_and_ors(&and_ors)
            .ok_or_else(|| ParseFailure::Error(ParseError::unexpected_end_of_input(["and_or"])))?;
        self.build_list(and_ors, span)
    }

    fn parse_separator_nonterminal(&mut self, _stop: StopSet) -> ParseResult<bool> {
        if self.peek_is_separator_op()? {
            let _separator = self.parse_separator_op_token()?.ok_or_else(|| {
                ParseFailure::Error(ParseError::unexpected_end_of_input(["separator"]))
            })?;
            self.parse_linebreak()?;
            return Ok(true);
        }

        Ok(self.parse_newline_list()? > 0)
    }

    fn parse_redirect_list_nonterminal(&mut self) -> ParseResult<Vec<RedirectAst>> {
        let mut redirects = Vec::new();
        while self.can_start_io_redirect_at(0)? {
            redirects.push(self.parse_io_redirect_nonterminal()?);
        }
        Ok(redirects)
    }

    fn parse_name_word_nonterminal(&mut self) -> ParseResult<WordAst> {
        let token = self
            .peek_token(0)?
            .ok_or_else(|| ParseFailure::Error(ParseError::unexpected_end_of_input(["NAME"])))?;
        if token.kind != TokenKind::Token {
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                &token,
                ["NAME"],
            )));
        }

        let classified = self.classify_token_at(
            0,
            ClassificationContext {
                reserved_word_policy: ReservedWordPolicy::None,
                name_context: NameContext::ForName,
                ..Default::default()
            },
        )?;
        if classified != ClassifiedTokenKind::Name {
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                &token,
                ["NAME"],
            )));
        }

        let token = self
            .next_token()?
            .ok_or_else(|| ParseFailure::Error(ParseError::unexpected_end_of_input(["NAME"])))?;
        self.build_word(token)
    }

    fn parse_plain_word_nonterminal(&mut self) -> ParseResult<WordAst> {
        let token = self
            .next_token()?
            .ok_or_else(|| ParseFailure::Error(ParseError::unexpected_end_of_input(["WORD"])))?;
        if token.kind != TokenKind::Token {
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                &token,
                ["WORD"],
            )));
        }
        self.build_word(token)
    }

    fn parse_wordlist_nonterminal(&mut self) -> ParseResult<Vec<WordAst>> {
        let mut words = Vec::new();
        loop {
            let Some(token) = self.peek_token(0)? else {
                break;
            };

            if token.kind != TokenKind::Token {
                break;
            }
            if self.peek_reserved_word(ReservedWordPolicy::InOrDo)? == Some(ReservedWord::Do) {
                break;
            }
            words.push(self.parse_plain_word_nonterminal()?);
        }

        if words.is_empty() {
            let found = self.peek_token(0)?.ok_or_else(|| {
                ParseFailure::Error(ParseError::unexpected_end_of_input(["WORD"]))
            })?;
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                &found,
                ["WORD"],
            )));
        }

        Ok(words)
    }

    fn consume_plain_token(&mut self, lexeme: &'static str) -> ParseResult<Token> {
        let token = self
            .next_token()?
            .ok_or_else(|| ParseFailure::Error(ParseError::unexpected_end_of_input([lexeme])))?;
        if token.kind == TokenKind::Token && token.is_plain() && token.lexeme == lexeme {
            Ok(token)
        } else {
            Err(ParseFailure::Error(ParseError::unexpected_token(
                &token,
                [lexeme],
            )))
        }
    }

    fn peek_reserved_word(
        &mut self,
        policy: ReservedWordPolicy,
    ) -> ParseResult<Option<ReservedWord>> {
        let Some(token) = self.peek_token(0)? else {
            return Ok(None);
        };
        if token.kind != TokenKind::Token {
            return Ok(None);
        }

        let classified = self.classify_token_at(
            0,
            ClassificationContext {
                reserved_word_policy: policy,
                ..Default::default()
            },
        )?;
        match classified {
            ClassifiedTokenKind::ReservedWord(word) => Ok(Some(word)),
            _ => Ok(None),
        }
    }

    fn consume_reserved_word(
        &mut self,
        expected: ReservedWord,
        policy: ReservedWordPolicy,
    ) -> ParseResult<Token> {
        if self.peek_reserved_word(policy)? != Some(expected) {
            let Some(found) = self.peek_token(0)? else {
                return Err(ParseFailure::Error(ParseError::unexpected_end_of_input([
                    reserved_word_label(expected),
                ])));
            };
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                &found,
                [reserved_word_label(expected)],
            )));
        }

        self.next_token()?.ok_or_else(|| {
            ParseFailure::Error(ParseError::unexpected_end_of_input([reserved_word_label(
                expected,
            )]))
        })
    }

    fn at_compound_stop(&mut self, stop: StopSet) -> ParseResult<bool> {
        let Some(token) = self.peek_token(0)? else {
            return Ok(false);
        };

        match token.kind {
            TokenKind::Operator(kind) => Ok((stop.right_paren && kind == OperatorKind::RightParen)
                || (stop.case_terminators
                    && matches!(
                        kind,
                        OperatorKind::DoubleSemicolon | OperatorKind::SemicolonAmpersand
                    ))),
            TokenKind::Newline => Ok(false),
            TokenKind::Token => {
                if stop.right_brace && is_plain_brace_close_token(&token) {
                    return Ok(true);
                }

                Ok(self
                    .peek_reserved_word(ReservedWordPolicy::Any)?
                    .is_some_and(|word| stop.matches_reserved(word)))
            }
        }
    }

    fn parse_simple_command_nonterminal(&mut self) -> ParseResult<SimpleCommandAst> {
        let mut assignments = Vec::new();
        let mut words = Vec::new();
        let mut redirects = Vec::new();
        let mut saw_command_word = false;
        let mut span_start = None;
        let mut span_end = None;

        loop {
            let Some(next) = self.peek_token(0)? else {
                break;
            };

            match next.kind {
                TokenKind::Newline => break,
                TokenKind::Operator(kind) => {
                    if is_command_delimiter_operator(kind) {
                        break;
                    }

                    if is_redirect_operator(kind) {
                        let redirect = self.parse_io_redirect_nonterminal()?;
                        extend_span_bounds(redirect.span, &mut span_start, &mut span_end);
                        redirects.push(redirect);
                        continue;
                    }

                    return Err(ParseFailure::Error(ParseError::unexpected_token(
                        &next,
                        ["WORD"],
                    )));
                }
                TokenKind::Token => {
                    if self.can_start_io_redirect_at(0)? {
                        let redirect = self.parse_io_redirect_nonterminal()?;
                        extend_span_bounds(redirect.span, &mut span_start, &mut span_end);
                        redirects.push(redirect);
                        continue;
                    }

                    let classified = self.classify_token_at(
                        0,
                        ClassificationContext {
                            reserved_word_policy: ReservedWordPolicy::None,
                            allow_assignment_word: !saw_command_word,
                            function_body_rule9: self.in_function_body_mode(),
                            ..Default::default()
                        },
                    )?;

                    match classified {
                        ClassifiedTokenKind::AssignmentWord if !saw_command_word => {
                            let token = self.next_token()?.ok_or_else(|| {
                                ParseFailure::Error(ParseError::unexpected_end_of_input([
                                    "ASSIGNMENT_WORD",
                                ]))
                            })?;
                            let assignment = self.build_assignment_word(token)?;
                            extend_span_bounds(assignment.span, &mut span_start, &mut span_end);
                            assignments.push(assignment);
                        }
                        ClassifiedTokenKind::Word | ClassifiedTokenKind::Name => {
                            let token = self.next_token()?.ok_or_else(|| {
                                ParseFailure::Error(ParseError::unexpected_end_of_input(["WORD"]))
                            })?;
                            let word = self.build_word(token)?;
                            extend_span_bounds(word.span, &mut span_start, &mut span_end);
                            words.push(word);
                            saw_command_word = true;
                        }
                        _ => {
                            return Err(ParseFailure::Error(ParseError::unexpected_token(
                                &next,
                                ["WORD"],
                            )));
                        }
                    }
                }
            }
        }

        if assignments.is_empty() && words.is_empty() && redirects.is_empty() {
            return Err(ParseFailure::Error(ParseError::unexpected_end_of_input([
                "simple command",
            ])));
        }

        let span = match (span_start, span_end) {
            (Some(start), Some(end)) => Some(merge_spans(start, end)),
            _ => None,
        };

        self.build_simple_command(assignments, words, redirects, span)
    }

    fn parse_io_redirect_nonterminal(&mut self) -> ParseResult<RedirectAst> {
        let mut fd_or_location = None;
        let mut start_span = None;

        if let Some(token) = self.peek_token(0)?
            && token.kind == TokenKind::Token
        {
            let classified = self.classify_token_at(
                0,
                ClassificationContext {
                    reserved_word_policy: ReservedWordPolicy::None,
                    ..Default::default()
                },
            )?;
            if matches!(
                classified,
                ClassifiedTokenKind::IoNumber(_) | ClassifiedTokenKind::IoLocation
            ) {
                let prefix = self.next_token()?.ok_or_else(|| {
                    ParseFailure::Error(ParseError::unexpected_end_of_input(["io_redirect"]))
                })?;
                start_span = Some(prefix.span);
                fd_or_location = Some(prefix);
            }
        }

        let operator_token = self.next_token()?.ok_or_else(|| {
            ParseFailure::Error(ParseError::unexpected_end_of_input(["redirect operator"]))
        })?;
        let operator = match operator_token.kind {
            TokenKind::Operator(kind) if is_redirect_operator(kind) => kind,
            _ => {
                return Err(ParseFailure::Error(ParseError::unexpected_token(
                    &operator_token,
                    ["redirect operator"],
                )));
            }
        };

        let target = self.parse_redirect_target_word()?;
        let span = merge_spans(start_span.unwrap_or(operator_token.span), target.span);
        self.build_redirect(fd_or_location, operator, target, span)
    }

    fn parse_redirect_target_word(&mut self) -> ParseResult<WordAst> {
        let token = self
            .next_token()?
            .ok_or_else(|| ParseFailure::Error(ParseError::unexpected_end_of_input(["WORD"])))?;
        if token.kind != TokenKind::Token {
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                &token,
                ["WORD"],
            )));
        }
        self.build_word(token)
    }

    fn is_function_definition_head_at_command_position(&mut self) -> ParseResult<bool> {
        let Some(name_token) = self.peek_token(0)? else {
            return Ok(false);
        };
        if name_token.kind != TokenKind::Token {
            return Ok(false);
        }

        let classified = self.classify_token_at(
            0,
            ClassificationContext {
                reserved_word_policy: ReservedWordPolicy::Any,
                name_context: NameContext::FunctionName,
                ..Default::default()
            },
        )?;
        if classified != ClassifiedTokenKind::Name {
            return Ok(false);
        }
        if self.peek_operator_kind(1)? != Some(OperatorKind::LeftParen)
            || self.peek_operator_kind(2)? != Some(OperatorKind::RightParen)
        {
            return Ok(false);
        }

        Ok(true)
    }

    fn parse_function_definition_nonterminal(&mut self) -> ParseResult<CommandAst> {
        let name = self.parse_function_name_nonterminal()?;
        let _left_paren = self.consume_operator_kind(OperatorKind::LeftParen)?;
        let _right_paren = self.consume_operator_kind(OperatorKind::RightParen)?;
        self.parse_linebreak()?;

        let (body, redirects, body_span) = self.parse_function_body_nonterminal()?;
        let span = redirects
            .last()
            .map(|redirect| merge_spans(name.span, redirect.span))
            .unwrap_or_else(|| merge_spans(name.span, body_span));

        let function = self.build_function_definition(name, body, redirects, span)?;
        self.build_command_function_definition(function)
    }

    fn parse_function_body_nonterminal(
        &mut self,
    ) -> ParseResult<(CommandAst, Vec<RedirectAst>, Span)> {
        self.with_function_body_rule9(|parser| {
            let (kind, payload_span) = parser.parse_compound_payload_nonterminal()?;
            let compound = parser.build_compound_command_node(kind, Vec::new(), payload_span)?;
            let body = parser.build_command_compound(compound)?;
            let redirects = parser.parse_redirect_list_nonterminal()?;
            let span = redirects
                .last()
                .map(|redirect| merge_spans(payload_span, redirect.span))
                .unwrap_or(payload_span);
            Ok((body, redirects, span))
        })
    }

    fn parse_function_name_nonterminal(&mut self) -> ParseResult<WordAst> {
        let token = self
            .peek_token(0)?
            .ok_or_else(|| ParseFailure::Error(ParseError::unexpected_end_of_input(["NAME"])))?;
        if token.kind != TokenKind::Token {
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                &token,
                ["NAME"],
            )));
        }

        let classified = self.classify_token_at(
            0,
            ClassificationContext {
                reserved_word_policy: ReservedWordPolicy::Any,
                name_context: NameContext::FunctionName,
                ..Default::default()
            },
        )?;
        if classified != ClassifiedTokenKind::Name {
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                &token,
                ["NAME"],
            )));
        }

        let token = self
            .next_token()?
            .ok_or_else(|| ParseFailure::Error(ParseError::unexpected_end_of_input(["NAME"])))?;
        self.build_word(token)
    }

    fn parse_linebreak(&mut self) -> ParseResult<()> {
        while self.peek_is_newline()? {
            let _newline = self.next_token()?;
        }
        Ok(())
    }

    fn parse_newline_list(&mut self) -> ParseResult<usize> {
        let mut count = 0;

        while self.peek_is_newline()? {
            let _newline = self.next_token()?;
            count += 1;
        }

        Ok(count)
    }

    fn parse_separator_op(&mut self) -> ParseResult<Option<OperatorKind>> {
        self
            .parse_separator_op_token()?
            .map(|token| expect_operator_kind(&token, "separator operator"))
            .transpose()
    }

    #[allow(dead_code)]
    fn parse_separator(&mut self) -> ParseResult<bool> {
        if self.peek_is_separator_op()? {
            let _ = self.parse_separator_op()?;
            self.parse_linebreak()?;
            return Ok(true);
        }

        if self.parse_newline_list()? > 0 {
            return Ok(true);
        }

        Ok(false)
    }

    #[allow(dead_code)]
    fn parse_sequential_sep(&mut self) -> ParseResult<bool> {
        if self.peek_operator_kind(0)? == Some(OperatorKind::Semicolon) {
            let _ = self.consume_operator_kind(OperatorKind::Semicolon)?;
            self.parse_linebreak()?;
            return Ok(true);
        }

        if self.parse_newline_list()? > 0 {
            return Ok(true);
        }

        Ok(false)
    }

    fn parse_separator_op_token(&mut self) -> ParseResult<Option<Token>> {
        let Some(kind) = self.peek_operator_kind(0)? else {
            return Ok(None);
        };
        if !is_list_separator_operator(kind) {
            return Ok(None);
        }

        self.next_token()
    }

    fn can_start_complete_command(&mut self) -> ParseResult<bool> {
        match self.peek_token_ref(0)? {
            Some(token) => Ok(token_can_start_pipeline(token)),
            None => Ok(false),
        }
    }

    fn can_start_pipeline_at(&mut self, offset: usize) -> ParseResult<bool> {
        match self.peek_token_ref(offset)? {
            Some(token) => Ok(token_can_start_pipeline(token)),
            None => Ok(false),
        }
    }

    fn classify_token_at(
        &mut self,
        offset: usize,
        mut context: ClassificationContext,
    ) -> ParseResult<ClassifiedTokenKind> {
        let classifier = self.classifier;
        let allow_io_location = self.options.allow_io_location;
        if context.delimiter_context.is_none() {
            context.delimiter_context = self.peek_delimiter_context(offset.saturating_add(1))?;
        }
        let token = self.peek_token_ref(offset)?.ok_or_else(|| {
            ParseFailure::Error(ParseError::unexpected_end_of_input(["command token"]))
        })?;
        let classified = classifier.classify_kind_only(
            token,
            context,
            ClassificationOptions { allow_io_location },
        );

        Ok(classified)
    }

    fn can_start_io_redirect_at(&mut self, offset: usize) -> ParseResult<bool> {
        let Some(token) = self.peek_token(offset)? else {
            return Ok(false);
        };

        match token.kind {
            TokenKind::Operator(kind) => Ok(is_redirect_operator(kind)),
            TokenKind::Newline => Ok(false),
            TokenKind::Token => {
                let classified = self.classify_token_at(
                    offset,
                    ClassificationContext {
                        reserved_word_policy: ReservedWordPolicy::None,
                        ..Default::default()
                    },
                )?;
                if !matches!(
                    classified,
                    ClassifiedTokenKind::IoNumber(_) | ClassifiedTokenKind::IoLocation
                ) {
                    return Ok(false);
                }

                let next_kind = self.peek_operator_kind(offset.saturating_add(1))?;
                Ok(next_kind.is_some_and(is_redirect_operator))
            }
        }
    }

    fn peek_delimiter_context(&mut self, offset: usize) -> ParseResult<Option<DelimiterContext>> {
        let next = self.peek_token_ref(offset)?;
        Ok(next.and_then(delimiter_context_from_token))
    }

    fn peek_token_ref(&mut self, n: usize) -> ParseResult<Option<&Token>> {
        let interactive = self.options.interactive;
        match self.token_stream.peek(n) {
            Ok(token) => Ok(token),
            Err(error) => Err(Self::stream_error_to_failure_with_interactive(
                interactive,
                error,
            )),
        }
    }

    fn peek_token(&mut self, n: usize) -> ParseResult<Option<Token>> {
        let interactive = self.options.interactive;
        match self.token_stream.peek(n) {
            Ok(token) => Ok(token.cloned()),
            Err(error) => Err(Self::stream_error_to_failure_with_interactive(
                interactive,
                error,
            )),
        }
    }

    fn next_token(&mut self) -> ParseResult<Option<Token>> {
        let interactive = self.options.interactive;
        match self.token_stream.next() {
            Ok(token) => Ok(token),
            Err(error) => Err(Self::stream_error_to_failure_with_interactive(
                interactive,
                error,
            )),
        }
    }

    fn peek_operator_kind(&mut self, offset: usize) -> ParseResult<Option<OperatorKind>> {
        match self.peek_token_ref(offset)? {
            Some(token) => match token.kind {
                TokenKind::Operator(kind) => Ok(Some(kind)),
                _ => Ok(None),
            },
            None => Ok(None),
        }
    }

    fn peek_is_newline(&mut self) -> ParseResult<bool> {
        Ok(self
            .peek_token_ref(0)?
            .is_some_and(|token| token.kind == TokenKind::Newline))
    }

    fn consume_operator_kind(&mut self, kind: OperatorKind) -> ParseResult<Token> {
        let token = self.next_token()?.ok_or_else(|| {
            ParseFailure::Error(ParseError::unexpected_end_of_input([kind.as_str()]))
        })?;

        match token.kind {
            TokenKind::Operator(found) if found == kind => Ok(token),
            _ => Err(ParseFailure::Error(ParseError::unexpected_token(
                &token,
                [kind.as_str()],
            ))),
        }
    }

    fn peek_is_separator_op(&mut self) -> ParseResult<bool> {
        Ok(matches!(
            self.peek_operator_kind(0)?,
            Some(OperatorKind::Semicolon | OperatorKind::Ampersand)
        ))
    }

    fn map_unexpected_eof_to_need_more<T>(
        result: ParseResult<T>,
        reason: NeedMoreInputReason,
    ) -> ParseResult<T> {
        match result {
            Err(ParseFailure::Error(error))
                if error.kind == ParseErrorKind::UnexpectedEndOfInput =>
            {
                Err(ParseFailure::NeedMoreInput(reason))
            }
            other => other,
        }
    }

    fn with_function_body_rule9<T>(
        &mut self,
        parse: impl FnOnce(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<T> {
        self.function_body_rule9_depth += 1;
        let result = parse(self);
        self.function_body_rule9_depth -= 1;
        result
    }

    fn with_nesting_guard<T>(
        &mut self,
        parse: impl FnOnce(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<T> {
        let attempted = self.nesting_depth.saturating_add(1);
        if attempted > self.options.max_nesting {
            return Err(ParseFailure::Error(ParseError::max_nesting_exceeded(
                self.options.max_nesting,
                attempted,
            )));
        }

        self.nesting_depth = attempted;
        let result = parse(self);
        self.nesting_depth -= 1;
        result
    }

    fn in_function_body_mode(&self) -> bool {
        self.function_body_rule9_depth > 0
    }

    fn build_word(&mut self, token: Token) -> ParseResult<WordAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder.word_from_token(token).map_err(ParseFailure::Error)
    }

    fn build_assignment_word(&mut self, token: Token) -> ParseResult<AssignmentWordAst> {
        let (name, value) = split_assignment_word(&token)
            .map(|(name, value)| (name.to_string(), value.to_string()))
            .ok_or_else(|| {
                ParseFailure::Error(ParseError::unexpected_token(&token, ["ASSIGNMENT_WORD"]))
            })?;
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .assignment_word_from_parts(token, name, value)
            .map_err(ParseFailure::Error)
    }

    fn build_redirect(
        &mut self,
        fd_or_location: Option<Token>,
        operator: OperatorKind,
        target: WordAst,
        span: Span,
    ) -> ParseResult<RedirectAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .redirect(fd_or_location, operator, target, span)
            .map_err(ParseFailure::Error)
    }

    fn build_case_item(
        &mut self,
        patterns: Vec<WordAst>,
        body: Option<ListAst>,
        terminator: Option<CaseTerminatorAst>,
        span: Span,
    ) -> ParseResult<CaseItemAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .case_item(patterns, body, terminator, span)
            .map_err(ParseFailure::Error)
    }

    fn build_if_clause(
        &mut self,
        condition: ListAst,
        then_body: ListAst,
        elif_arms: Vec<(ListAst, ListAst)>,
        else_body: Option<ListAst>,
        span: Span,
    ) -> ParseResult<IfClauseAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .if_clause(condition, then_body, elif_arms, else_body, span)
            .map_err(ParseFailure::Error)
    }

    fn build_for_clause(
        &mut self,
        name: WordAst,
        words: Vec<WordAst>,
        body: ListAst,
        span: Span,
    ) -> ParseResult<ForClauseAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .for_clause(name, words, body, span)
            .map_err(ParseFailure::Error)
    }

    fn build_while_clause(
        &mut self,
        condition: ListAst,
        body: ListAst,
        span: Span,
    ) -> ParseResult<WhileClauseAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .while_clause(condition, body, span)
            .map_err(ParseFailure::Error)
    }

    fn build_until_clause(
        &mut self,
        condition: ListAst,
        body: ListAst,
        span: Span,
    ) -> ParseResult<UntilClauseAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .until_clause(condition, body, span)
            .map_err(ParseFailure::Error)
    }

    fn build_case_clause(
        &mut self,
        word: WordAst,
        items: Vec<CaseItemAst>,
        span: Span,
    ) -> ParseResult<CaseClauseAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .case_clause(word, items, span)
            .map_err(ParseFailure::Error)
    }

    fn build_compound_subshell(&mut self, list: ListAst) -> ParseResult<CompoundCommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder.compound_subshell(list).map_err(ParseFailure::Error)
    }

    fn build_compound_brace_group(&mut self, list: ListAst) -> ParseResult<CompoundCommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .compound_brace_group(list)
            .map_err(ParseFailure::Error)
    }

    fn build_compound_if(&mut self, clause: IfClauseAst) -> ParseResult<CompoundCommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder.compound_if(clause).map_err(ParseFailure::Error)
    }

    fn build_compound_for(&mut self, clause: ForClauseAst) -> ParseResult<CompoundCommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder.compound_for(clause).map_err(ParseFailure::Error)
    }

    fn build_compound_while(&mut self, clause: WhileClauseAst) -> ParseResult<CompoundCommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder.compound_while(clause).map_err(ParseFailure::Error)
    }

    fn build_compound_until(&mut self, clause: UntilClauseAst) -> ParseResult<CompoundCommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder.compound_until(clause).map_err(ParseFailure::Error)
    }

    fn build_compound_case(&mut self, clause: CaseClauseAst) -> ParseResult<CompoundCommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder.compound_case(clause).map_err(ParseFailure::Error)
    }

    fn build_compound_command_node(
        &mut self,
        kind: CompoundCommandAst,
        redirects: Vec<RedirectAst>,
        span: Span,
    ) -> ParseResult<CompoundCommandNodeAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .compound_command_node(kind, redirects, span)
            .map_err(ParseFailure::Error)
    }

    fn build_simple_command(
        &mut self,
        assignments: Vec<AssignmentWordAst>,
        words: Vec<WordAst>,
        redirects: Vec<RedirectAst>,
        span: Option<Span>,
    ) -> ParseResult<SimpleCommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .simple_command(assignments, words, redirects, span)
            .map_err(ParseFailure::Error)
    }

    fn build_command_simple(&mut self, command: SimpleCommandAst) -> ParseResult<CommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder.command_simple(command).map_err(ParseFailure::Error)
    }

    fn build_command_compound(
        &mut self,
        command: CompoundCommandNodeAst,
    ) -> ParseResult<CommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .command_compound(command)
            .map_err(ParseFailure::Error)
    }

    fn build_function_definition(
        &mut self,
        name: WordAst,
        body: CommandAst,
        redirects: Vec<RedirectAst>,
        span: Span,
    ) -> ParseResult<FunctionDefinitionAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .function_definition(name, body, redirects, span)
            .map_err(ParseFailure::Error)
    }

    fn build_command_function_definition(
        &mut self,
        function: FunctionDefinitionAst,
    ) -> ParseResult<CommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .command_function_definition(function)
            .map_err(ParseFailure::Error)
    }

    fn build_pipeline(
        &mut self,
        negated: bool,
        commands: Vec<CommandAst>,
        span: Span,
    ) -> ParseResult<PipelineAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .pipeline(negated, commands, span)
            .map_err(ParseFailure::Error)
    }

    fn build_and_or(
        &mut self,
        head: PipelineAst,
        tail: Vec<(OperatorKind, PipelineAst)>,
        span: Span,
    ) -> ParseResult<AndOrAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .and_or(head, tail, span)
            .map_err(ParseFailure::Error)
    }

    fn build_list(&mut self, and_ors: Vec<AndOrAst>, span: Span) -> ParseResult<ListAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder.list(and_ors, span).map_err(ParseFailure::Error)
    }

    fn build_complete_command(
        &mut self,
        list: ListAst,
        separator_op: Option<OperatorKind>,
        span: Span,
    ) -> ParseResult<CompleteCommandAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .complete_command(list, separator_op, span)
            .map_err(ParseFailure::Error)
    }

    fn build_program(
        &mut self,
        complete_commands: Vec<CompleteCommandAst>,
        span: Option<Span>,
    ) -> ParseResult<ProgramAst> {
        let mut builder = AstBuilder::new(&mut self.arena);
        builder
            .program(complete_commands, span)
            .map_err(ParseFailure::Error)
    }

    fn stream_error_to_failure_with_interactive(
        interactive: bool,
        error: TokenStreamError,
    ) -> ParseFailure {
        match error {
            TokenStreamError::NeedMoreInput(reason) => {
                if interactive {
                    ParseFailure::NeedMoreInput(reason)
                } else {
                    ParseFailure::Error(ParseError::from_need_more_reason(reason))
                }
            }
            TokenStreamError::Parse(error) => ParseFailure::Error(error),
        }
    }
}

type ParseResult<T> = Result<T, ParseFailure>;

#[derive(Debug)]
enum ParseFailure {
    NeedMoreInput(NeedMoreInputReason),
    Error(ParseError),
}

#[derive(Debug, Clone, Copy, Default)]
struct StopSet {
    right_paren: bool,
    right_brace: bool,
    then_kw: bool,
    do_kw: bool,
    done_kw: bool,
    fi_kw: bool,
    esac_kw: bool,
    else_kw: bool,
    elif_kw: bool,
    case_terminators: bool,
}

impl StopSet {
    const fn right_paren() -> Self {
        Self {
            right_paren: true,
            ..Self::empty()
        }
    }

    const fn right_brace() -> Self {
        Self {
            right_brace: true,
            ..Self::empty()
        }
    }

    const fn then_only() -> Self {
        Self {
            then_kw: true,
            ..Self::empty()
        }
    }

    const fn do_only() -> Self {
        Self {
            do_kw: true,
            ..Self::empty()
        }
    }

    const fn done_only() -> Self {
        Self {
            done_kw: true,
            ..Self::empty()
        }
    }

    const fn fi_only() -> Self {
        Self {
            fi_kw: true,
            ..Self::empty()
        }
    }

    const fn else_elif_fi() -> Self {
        Self {
            else_kw: true,
            elif_kw: true,
            fi_kw: true,
            ..Self::empty()
        }
    }

    const fn case_item_body() -> Self {
        Self {
            esac_kw: true,
            case_terminators: true,
            ..Self::empty()
        }
    }

    const fn empty() -> Self {
        Self {
            right_paren: false,
            right_brace: false,
            then_kw: false,
            do_kw: false,
            done_kw: false,
            fi_kw: false,
            esac_kw: false,
            else_kw: false,
            elif_kw: false,
            case_terminators: false,
        }
    }

    const fn matches_reserved(self, word: ReservedWord) -> bool {
        match word {
            ReservedWord::Then => self.then_kw,
            ReservedWord::Do => self.do_kw,
            ReservedWord::Done => self.done_kw,
            ReservedWord::Fi => self.fi_kw,
            ReservedWord::Esac => self.esac_kw,
            ReservedWord::Else => self.else_kw,
            ReservedWord::Elif => self.elif_kw,
            ReservedWord::If
            | ReservedWord::Case
            | ReservedWord::While
            | ReservedWord::Until
            | ReservedWord::For
            | ReservedWord::In => false,
        }
    }
}

fn delimiter_context_from_token(token: &Token) -> Option<DelimiterContext> {
    match token.kind {
        TokenKind::Operator(kind) => kind.delimiter_context(),
        _ => None,
    }
}

#[inline]
fn is_list_separator_operator(kind: OperatorKind) -> bool {
    matches!(kind, OperatorKind::Semicolon | OperatorKind::Ampersand)
}

#[inline]
fn is_command_delimiter_operator(kind: OperatorKind) -> bool {
    matches!(
        kind,
        OperatorKind::Pipe
            | OperatorKind::AndIf
            | OperatorKind::OrIf
            | OperatorKind::Semicolon
            | OperatorKind::Ampersand
            | OperatorKind::RightParen
            | OperatorKind::DoubleSemicolon
            | OperatorKind::SemicolonAmpersand
    )
}

#[inline]
fn is_redirect_operator(kind: OperatorKind) -> bool {
    matches!(
        kind,
        OperatorKind::Less
            | OperatorKind::Greater
            | OperatorKind::HereDoc
            | OperatorKind::HereDocStripTabs
            | OperatorKind::AppendOutput
            | OperatorKind::DupInput
            | OperatorKind::DupOutput
            | OperatorKind::ReadWrite
            | OperatorKind::Clobber
    )
}

fn is_unimplemented_command_operator(kind: OperatorKind) -> bool {
    kind == OperatorKind::LeftParen
}

fn is_unimplemented_reserved(word: ReservedWord) -> bool {
    matches!(
        word,
        ReservedWord::Do
            | ReservedWord::Done
            | ReservedWord::Then
            | ReservedWord::Else
            | ReservedWord::Elif
            | ReservedWord::Fi
            | ReservedWord::Esac
            | ReservedWord::In
    )
}

#[inline]
fn is_bang_token(token: &Token) -> bool {
    token.kind == TokenKind::Token && token.is_plain() && token.lexeme == "!"
}

#[inline]
fn token_can_start_pipeline(token: &Token) -> bool {
    if is_bang_token(token) {
        return true;
    }

    match token.kind {
        TokenKind::Token => true,
        TokenKind::Newline => false,
        TokenKind::Operator(kind) => {
            is_redirect_operator(kind) || is_unimplemented_command_operator(kind)
        }
    }
}

fn span_of_command(command: &CommandAst) -> Span {
    match command {
        CommandAst::Simple(simple) => simple
            .span
            .or_else(|| span_of_simple_command(simple))
            .expect("simple command must have a span"),
        CommandAst::Compound(compound) => compound.span,
        CommandAst::FunctionDefinition(function) => function.span,
    }
}

fn span_of_simple_command(command: &SimpleCommandAst) -> Option<Span> {
    let mut start = None;
    let mut end = None;

    for assignment in &command.assignments {
        extend_span_bounds(assignment.span, &mut start, &mut end);
    }
    for word in &command.words {
        extend_span_bounds(word.span, &mut start, &mut end);
    }
    for redirect in &command.redirects {
        extend_span_bounds(redirect.span, &mut start, &mut end);
    }

    match (start, end) {
        (Some(first), Some(last)) => Some(merge_spans(first, last)),
        _ => None,
    }
}

fn span_of_and_or(head: &PipelineAst, tail: &[(OperatorKind, PipelineAst)]) -> Span {
    let end = tail
        .last()
        .map(|(_, pipeline)| pipeline.span)
        .unwrap_or(head.span);
    merge_spans(head.span, end)
}

fn span_of_and_ors(and_ors: &[AndOrAst]) -> Option<Span> {
    let first = and_ors.first()?.span;
    let last = and_ors.last()?.span;
    Some(merge_spans(first, last))
}

fn span_of_complete_commands(commands: &[CompleteCommandAst]) -> Option<Span> {
    let first = commands.first()?.span;
    let last = commands.last()?.span;
    Some(merge_spans(first, last))
}

fn merge_spans(start: Span, end: Span) -> Span {
    Span::new(start.source_id, start.start, end.end)
}

fn expect_operator_kind(
    token: &Token,
    expected: &'static str,
) -> Result<OperatorKind, ParseFailure> {
    match token.kind {
        TokenKind::Operator(kind) => Ok(kind),
        _ => Err(ParseFailure::Error(ParseError::unexpected_token(
            token,
            [expected],
        ))),
    }
}

fn extend_span_bounds(span: Span, start: &mut Option<Span>, end: &mut Option<Span>) {
    match start {
        Some(current) => {
            if span.start < current.start {
                *current = span;
            }
        }
        None => *start = Some(span),
    }

    match end {
        Some(current) => {
            if span.end > current.end {
                *current = span;
            }
        }
        None => *end = Some(span),
    }
}

fn is_plain_brace_open_token(token: &Token) -> bool {
    token.kind == TokenKind::Token && token.is_plain() && token.lexeme == "{"
}

fn is_plain_brace_close_token(token: &Token) -> bool {
    token.kind == TokenKind::Token && token.is_plain() && token.lexeme == "}"
}

fn reserved_word_label(word: ReservedWord) -> &'static str {
    match word {
        ReservedWord::If => "if",
        ReservedWord::Then => "then",
        ReservedWord::Else => "else",
        ReservedWord::Elif => "elif",
        ReservedWord::Fi => "fi",
        ReservedWord::Do => "do",
        ReservedWord::Done => "done",
        ReservedWord::Case => "case",
        ReservedWord::Esac => "esac",
        ReservedWord::While => "while",
        ReservedWord::Until => "until",
        ReservedWord::For => "for",
        ReservedWord::In => "in",
    }
}
