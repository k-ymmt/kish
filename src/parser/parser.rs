//! Parser entrypoints and phase contracts.

use crate::lexer::{DelimiterContext, OperatorKind, SourceId, Span, Token, TokenKind};
use crate::parser::arena::AstArena;
use crate::parser::ast::{
    AndOrAst, AssignmentWordAst, AstBuilder, CommandAst, CompleteCommandAst, ListAst, PipelineAst,
    ProgramAst, RedirectAst, SimpleCommandAst, WordAst,
};
use crate::parser::classifier::{
    ClassificationContext, ClassificationOptions, ClassifiedTokenKind, Classifier, NameContext,
    ReservedWord, ReservedWordPolicy, split_assignment_word,
};
use crate::parser::error::ParseError;
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

        let Some(_) = self.peek_token(0)? else {
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

        if let Some(token) = self.peek_token(0)? {
            return Err(ParseFailure::Error(ParseError::unexpected_token(
                &token,
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
            let pipeline = self.parse_pipeline_nonterminal()?;
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
            commands.push(self.parse_command_nonterminal()?);
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
                    return Err(ParseFailure::Error(ParseError::grammar_not_implemented(
                        Some(head.span),
                        Some(head.lexeme),
                    )));
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

        let head_kind = self.classify_token_at(
            0,
            ClassificationContext {
                reserved_word_policy: ReservedWordPolicy::Any,
                ..Default::default()
            },
        )?;
        match head_kind {
            ClassifiedTokenKind::ReservedWord(word) if is_unimplemented_reserved(word) => {
                return Err(ParseFailure::Error(ParseError::grammar_not_implemented(
                    Some(head.span),
                    Some(head.lexeme),
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

        let _ = self.try_parse_function_definition_head()?;
        let simple = self.parse_simple_command_nonterminal()?;
        self.build_command_simple(simple)
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

                    if kind == OperatorKind::LeftParen {
                        return Err(ParseFailure::Error(ParseError::grammar_not_implemented(
                            Some(next.span),
                            Some(next.lexeme),
                        )));
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

    fn try_parse_function_definition_head(&mut self) -> ParseResult<bool> {
        let Some(name_token) = self.peek_token(0)? else {
            return Ok(false);
        };
        if name_token.kind != TokenKind::Token {
            return Ok(false);
        }

        let classified = self.classify_token_at(
            0,
            ClassificationContext {
                reserved_word_policy: ReservedWordPolicy::None,
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

        Err(ParseFailure::Error(ParseError::grammar_not_implemented(
            Some(name_token.span),
            Some(name_token.lexeme),
        )))
    }

    fn parse_linebreak(&mut self) -> ParseResult<()> {
        while let Some(token) = self.peek_token(0)? {
            if token.kind != TokenKind::Newline {
                break;
            }
            let _newline = self.next_token()?;
        }
        Ok(())
    }

    fn parse_newline_list(&mut self) -> ParseResult<usize> {
        let mut count = 0;

        while let Some(token) = self.peek_token(0)? {
            if token.kind != TokenKind::Newline {
                break;
            }
            let _newline = self.next_token()?;
            count += 1;
        }

        Ok(count)
    }

    fn parse_separator_op(&mut self) -> ParseResult<Option<OperatorKind>> {
        Ok(self
            .parse_separator_op_token()?
            .map(|token| expect_operator_kind(&token, "separator operator"))
            .transpose()?)
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
        let Some(token) = self.peek_token(0)? else {
            return Ok(None);
        };

        let TokenKind::Operator(kind) = token.kind else {
            return Ok(None);
        };

        if !is_list_separator_operator(kind) {
            return Ok(None);
        }

        self.next_token()
    }

    fn can_start_complete_command(&mut self) -> ParseResult<bool> {
        match self.peek_token(0)? {
            Some(token) => Ok(token_can_start_pipeline(&token)),
            None => Ok(false),
        }
    }

    fn can_start_pipeline_at(&mut self, offset: usize) -> ParseResult<bool> {
        match self.peek_token(offset)? {
            Some(token) => Ok(token_can_start_pipeline(&token)),
            None => Ok(false),
        }
    }

    fn classify_token_at(
        &mut self,
        offset: usize,
        mut context: ClassificationContext,
    ) -> ParseResult<ClassifiedTokenKind> {
        let token = self.peek_token(offset)?.ok_or_else(|| {
            ParseFailure::Error(ParseError::unexpected_end_of_input(["command token"]))
        })?;
        if context.delimiter_context.is_none() {
            context.delimiter_context = self.peek_delimiter_context(offset.saturating_add(1))?;
        }
        let classified = self.classifier.classify(
            &token,
            context,
            ClassificationOptions {
                allow_io_location: self.options.allow_io_location,
            },
        );

        Ok(classified.kind)
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
        let next = self.peek_token(offset)?;
        Ok(next.as_ref().and_then(delimiter_context_from_token))
    }

    fn peek_token(&mut self, n: usize) -> ParseResult<Option<Token>> {
        self.token_stream
            .peek(n)
            .map(|token| token.cloned())
            .map_err(|error| self.stream_error_to_failure(error))
    }

    fn next_token(&mut self) -> ParseResult<Option<Token>> {
        self.token_stream
            .next()
            .map_err(|error| self.stream_error_to_failure(error))
    }

    fn peek_operator_kind(&mut self, offset: usize) -> ParseResult<Option<OperatorKind>> {
        match self.peek_token(offset)? {
            Some(token) => match token.kind {
                TokenKind::Operator(kind) => Ok(Some(kind)),
                _ => Ok(None),
            },
            None => Ok(None),
        }
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

    fn stream_error_to_failure(&self, error: TokenStreamError) -> ParseFailure {
        match error {
            TokenStreamError::NeedMoreInput(reason) => {
                if self.options.interactive {
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

fn delimiter_context_from_token(token: &Token) -> Option<DelimiterContext> {
    match token.kind {
        TokenKind::Operator(kind) => kind.delimiter_context(),
        _ => None,
    }
}

fn is_list_separator_operator(kind: OperatorKind) -> bool {
    matches!(kind, OperatorKind::Semicolon | OperatorKind::Ampersand)
}

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
        ReservedWord::If
            | ReservedWord::For
            | ReservedWord::Case
            | ReservedWord::While
            | ReservedWord::Until
            | ReservedWord::Do
            | ReservedWord::Done
            | ReservedWord::Then
            | ReservedWord::Else
            | ReservedWord::Elif
            | ReservedWord::Fi
            | ReservedWord::Esac
            | ReservedWord::In
    )
}

fn is_bang_token(token: &Token) -> bool {
    token.kind == TokenKind::Token && token.is_plain() && token.lexeme == "!"
}

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
        CommandAst::Compound(_) => {
            panic!("compound commands are not produced in phase 4")
        }
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
