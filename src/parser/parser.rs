//! Parser entrypoints and phase contracts.

use crate::lexer::{DelimiterContext, OperatorKind, SourceId, Token, TokenKind};
use crate::parser::arena::AstArena;
use crate::parser::ast::{CompleteCommandAst, ProgramAst};
use crate::parser::classifier::{
    ClassificationContext, ClassificationOptions, Classifier, NameContext, ReservedWordPolicy,
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
    #[allow(dead_code)]
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
    ///
    /// Phase 0-2 behavior:
    /// - returns [`ParseStep::EndOfInput`] on EOF
    /// - returns [`ParseStep::NeedMoreInput`] for interactive incomplete input
    /// - otherwise returns `GrammarNotImplemented`
    pub fn parse_complete_command(&mut self) -> Result<ParseStep, ParseError> {
        let head = match self.peek_owned(0)? {
            PeekOutcome::NeedMore(reason) => return Ok(ParseStep::NeedMoreInput(reason)),
            PeekOutcome::Token(token) => token,
        };

        let Some(head) = head else {
            return Ok(ParseStep::EndOfInput);
        };

        let delimiter_context = match self.peek_owned(1)? {
            PeekOutcome::NeedMore(reason) => return Ok(ParseStep::NeedMoreInput(reason)),
            PeekOutcome::Token(Some(next)) => delimiter_context_from_token(&next),
            PeekOutcome::Token(None) => None,
        };

        // Phase 2 integration: parser always routes lexical tokens through
        // contextual classifier before grammar reduction.
        let _classified = self.classifier.classify(
            &head,
            ClassificationContext {
                delimiter_context,
                reserved_word_policy: ReservedWordPolicy::Any,
                name_context: NameContext::None,
                allow_assignment_word: true,
                function_body_rule9: false,
            },
            ClassificationOptions {
                allow_io_location: self.options.allow_io_location,
            },
        );

        Err(ParseError::grammar_not_implemented(
            Some(head.span),
            Some(head.lexeme),
        ))
    }

    /// Parses a whole program.
    pub fn parse_program(&mut self) -> Result<ProgramAst, ParseError> {
        let mut program = ProgramAst::default();

        loop {
            match self.parse_complete_command()? {
                ParseStep::Complete(command) => {
                    program.complete_commands.push(command);
                }
                ParseStep::NeedMoreInput(reason) => {
                    return Err(ParseError::from_need_more_reason(reason));
                }
                ParseStep::EndOfInput => {
                    if let Some(first) = program.complete_commands.first() {
                        program.span = Some(first.span);
                    }
                    return Ok(program);
                }
            }
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

    fn peek_owned(&mut self, n: usize) -> Result<PeekOutcome, ParseError> {
        match self.token_stream.peek(n) {
            Ok(token) => Ok(PeekOutcome::Token(token.cloned())),
            Err(TokenStreamError::NeedMoreInput(reason)) => {
                if self.options.interactive {
                    Ok(PeekOutcome::NeedMore(reason))
                } else {
                    Err(ParseError::from_need_more_reason(reason))
                }
            }
            Err(TokenStreamError::Parse(error)) => Err(error),
        }
    }
}

fn delimiter_context_from_token(token: &Token) -> Option<DelimiterContext> {
    match token.kind {
        TokenKind::Operator(kind) => kind.delimiter_context(),
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum PeekOutcome {
    Token(Option<Token>),
    NeedMore(NeedMoreInputReason),
}

#[allow(dead_code)]
fn is_list_separator_operator(kind: OperatorKind) -> bool {
    matches!(kind, OperatorKind::Semicolon | OperatorKind::Ampersand)
}
