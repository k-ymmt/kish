//! Parser error contracts.

use crate::lexer::{FatalLexError, Span, Token};

use crate::parser::arena::ArenaError;
use crate::parser::recovery::NeedMoreInputReason;

/// Stable parser error categories.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
    /// A concrete token did not match grammar expectations.
    UnexpectedToken,
    /// Input ended before required grammar elements were found.
    UnexpectedEndOfInput,
    /// Parser requested lookahead beyond configured bound.
    LookaheadExceeded,
    /// Lexer-level fatal error surfaced through parser stream.
    LexerError,
    /// Grammar reduction is intentionally not implemented in this phase.
    GrammarNotImplemented,
    /// AST node allocation exceeded configured parser limit.
    AstNodeLimitExceeded,
}

/// Parser error payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// Error category.
    pub kind: ParseErrorKind,
    /// Optional source span near the failure.
    pub span: Option<Span>,
    /// Expected token labels.
    pub expected: Vec<String>,
    /// Found token/terminal label.
    pub found: Option<String>,
}

impl ParseError {
    /// Creates a parser error.
    pub fn new(
        kind: ParseErrorKind,
        span: Option<Span>,
        expected: Vec<String>,
        found: Option<String>,
    ) -> Self {
        Self {
            kind,
            span,
            expected,
            found,
        }
    }

    /// Creates an `UnexpectedToken` error.
    pub fn unexpected_token(
        token: &Token,
        expected: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        Self::new(
            ParseErrorKind::UnexpectedToken,
            Some(token.span),
            expected.into_iter().map(Into::into).collect(),
            Some(token.lexeme.clone()),
        )
    }

    /// Creates an `UnexpectedEndOfInput` error.
    pub fn unexpected_end_of_input(expected: impl IntoIterator<Item = impl Into<String>>) -> Self {
        Self::new(
            ParseErrorKind::UnexpectedEndOfInput,
            None,
            expected.into_iter().map(Into::into).collect(),
            Some("EOF".to_string()),
        )
    }

    /// Creates a `LookaheadExceeded` error.
    pub fn lookahead_exceeded(requested: usize, max: usize) -> Self {
        Self::new(
            ParseErrorKind::LookaheadExceeded,
            None,
            vec![format!("lookahead <= {max}")],
            Some(format!("lookahead {requested}")),
        )
    }

    /// Creates a lexer-wrapper parse error.
    pub fn lexer_error(error: &FatalLexError) -> Self {
        let (span, found) = match error {
            FatalLexError::InternalInvariant(diagnostic)
            | FatalLexError::UnterminatedSingleQuote(diagnostic)
            | FatalLexError::UnterminatedDoubleQuote(diagnostic)
            | FatalLexError::UnterminatedDollarSingleQuote(diagnostic)
            | FatalLexError::UnterminatedParameterExpansion(diagnostic)
            | FatalLexError::UnterminatedCommandSubstitution(diagnostic)
            | FatalLexError::UnterminatedBackquotedCommandSubstitution(diagnostic)
            | FatalLexError::UnterminatedArithmeticExpansion(diagnostic)
            | FatalLexError::MalformedArithmeticExpansion(diagnostic)
            | FatalLexError::SubstitutionRecursionDepthExceeded(diagnostic)
            | FatalLexError::HereDocDelimiterNotFound(diagnostic)
            | FatalLexError::TokenSizeLimitExceeded(diagnostic)
            | FatalLexError::HereDocBodySizeLimitExceeded(diagnostic)
            | FatalLexError::BoundaryTokenLimitExceeded(diagnostic)
            | FatalLexError::IncompleteInput(diagnostic) => {
                (Some(diagnostic.span), Some(diagnostic.message.clone()))
            }
        };

        Self::new(ParseErrorKind::LexerError, span, Vec::new(), found)
    }

    /// Creates a grammar-phase placeholder error.
    pub fn grammar_not_implemented(span: Option<Span>, found: Option<String>) -> Self {
        Self::new(
            ParseErrorKind::GrammarNotImplemented,
            span,
            vec!["full grammar reductions (Phase 4+)".to_string()],
            found,
        )
    }

    /// Creates an AST node limit exceeded error.
    pub fn ast_node_limit_exceeded(limit: usize, attempted: usize) -> Self {
        Self::new(
            ParseErrorKind::AstNodeLimitExceeded,
            None,
            vec![format!("max_ast_nodes <= {limit}")],
            Some(format!("node allocation attempt {attempted}")),
        )
    }

    /// Converts a continuation reason into a non-interactive parse error.
    pub fn from_need_more_reason(reason: NeedMoreInputReason) -> Self {
        Self::new(
            ParseErrorKind::UnexpectedEndOfInput,
            None,
            vec!["additional input to complete command".to_string()],
            Some(format!("{reason:?}")),
        )
    }
}

impl From<ArenaError> for ParseError {
    fn from(value: ArenaError) -> Self {
        match value {
            ArenaError::NodeLimitExceeded { limit, attempted } => {
                ParseError::ast_node_limit_exceeded(limit, attempted)
            }
        }
    }
}
