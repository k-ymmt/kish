//! Token and boundary result contracts for the lexer.

use crate::lexer::diagnostics::RecoverableLexError;
use crate::lexer::span::{ByteOffset, Span};

/// Core token kinds available in Phase 0.
///
/// Later phases may add operator and richer metadata variants.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    /// Raw POSIX TOKEN placeholder.
    Token,
    /// Newline delimiter token.
    Newline,
}

/// A lexical token with raw text and source span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// Token category.
    pub kind: TokenKind,
    /// Token text preserved as scanned.
    pub lexeme: String,
    /// Byte-oriented source span.
    pub span: Span,
}

impl Token {
    /// Creates a token value.
    pub fn new(kind: TokenKind, lexeme: String, span: Span) -> Self {
        Self { kind, lexeme, span }
    }
}

/// One step produced by [`crate::lexer::Lexer::next_token`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexStep {
    /// A concrete token was produced.
    Token(Token),
    /// End of input was reached.
    EndOfInput,
    /// Recoverable issue requiring more data or context.
    Recoverable(RecoverableLexError),
}

/// Result of tokenization at complete-command boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoundaryResult {
    /// A complete command boundary was reached.
    Complete(CompleteCommandTokens),
    /// More input is required to continue safely.
    NeedMoreInput(NeedMoreInput),
}

/// Tokens gathered up to a command boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompleteCommandTokens {
    /// Token sequence for a single command boundary unit.
    pub tokens: Vec<Token>,
}

impl CompleteCommandTokens {
    /// Creates a boundary token container.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }
}

/// Placeholder reason for incomplete input conditions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NeedMoreReason {
    /// Line continuation marker with no following line.
    TrailingBackslash,
    /// Odd number of `'` in the remaining input (Phase 0 heuristic).
    UnterminatedSingleQuote,
    /// Odd number of `"` in the remaining input (Phase 0 heuristic).
    UnterminatedDoubleQuote,
    /// Incomplete state propagated from recoverable lexer diagnostics.
    Recoverable(RecoverableLexError),
}

/// Metadata for an incomplete command boundary scan.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NeedMoreInput {
    /// Byte position where scanning should resume.
    pub checkpoint: ByteOffset,
    /// The reason more input is required.
    pub reason: NeedMoreReason,
}

impl NeedMoreInput {
    /// Creates an incomplete boundary value.
    pub fn new(checkpoint: ByteOffset, reason: NeedMoreReason) -> Self {
        Self { checkpoint, reason }
    }
}
