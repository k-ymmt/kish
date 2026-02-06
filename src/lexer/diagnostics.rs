//! Diagnostic and error contracts for lexer stages.
//!
//! Phase 2 keeps diagnostics byte-span based. Line/column tracking now lives in
//! the cursor and will be wired into diagnostics in a later phase.

use crate::lexer::span::Span;

/// Stable diagnostic codes defined for lexer phases.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticCode {
    /// Placeholder for incomplete input that may recover with another line.
    IncompleteInput,
    /// Unterminated single quote (`'...'`) sequence.
    UnterminatedSingleQuote,
    /// Unterminated double quote (`"..."`) sequence.
    UnterminatedDoubleQuote,
    /// Unterminated dollar-single quote (`$'...'`) sequence.
    UnterminatedDollarSingleQuote,
    /// Unterminated parameter expansion (`${...}`) sequence.
    UnterminatedParameterExpansion,
    /// Unterminated command substitution (`$(...)`) sequence.
    UnterminatedCommandSubstitution,
    /// Unterminated backquoted command substitution (`` `...` ``) sequence.
    UnterminatedBackquotedCommandSubstitution,
    /// Unterminated arithmetic expansion (`$((...))`) sequence.
    UnterminatedArithmeticExpansion,
    /// Substitution recursion depth exceeded the lexer cap.
    SubstitutionRecursionDepthExceeded,
    /// Here-document delimiter was not found before input end.
    HereDocDelimiterNotFound,
    /// Placeholder for internal invariant violations.
    InternalInvariant,
}

/// User-facing diagnostic payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexDiagnostic {
    /// Machine-readable diagnostic code.
    pub code: DiagnosticCode,
    /// Human-readable message text.
    pub message: String,
    /// Source span associated with this diagnostic.
    pub span: Span,
}

impl LexDiagnostic {
    /// Creates a diagnostic value.
    pub fn new(code: DiagnosticCode, message: impl Into<String>, span: Span) -> Self {
        Self {
            code,
            message: message.into(),
            span,
        }
    }
}

/// Recoverable lexical error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecoverableLexError {
    /// Input is incomplete and may continue with more data.
    IncompleteInput(LexDiagnostic),
}

/// Fatal lexical error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FatalLexError {
    /// An invariant required for scanning was violated.
    InternalInvariant(LexDiagnostic),
    /// Input ended before a single-quoted region closed.
    UnterminatedSingleQuote(LexDiagnostic),
    /// Input ended before a double-quoted region closed.
    UnterminatedDoubleQuote(LexDiagnostic),
    /// Input ended before a dollar-single-quoted region closed.
    UnterminatedDollarSingleQuote(LexDiagnostic),
    /// Input ended before a parameter expansion closed.
    UnterminatedParameterExpansion(LexDiagnostic),
    /// Input ended before a command substitution closed.
    UnterminatedCommandSubstitution(LexDiagnostic),
    /// Input ended before a backquoted command substitution closed.
    UnterminatedBackquotedCommandSubstitution(LexDiagnostic),
    /// Input ended before an arithmetic expansion closed.
    UnterminatedArithmeticExpansion(LexDiagnostic),
    /// Substitution recursion depth exceeded the lexer limit.
    SubstitutionRecursionDepthExceeded(LexDiagnostic),
    /// Here-document delimiter was not found before input end.
    HereDocDelimiterNotFound(LexDiagnostic),
}
