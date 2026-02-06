//! Diagnostic and error contracts for lexer stages.

use crate::lexer::span::Span;

/// Stable diagnostic codes defined for lexer phases.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticCode {
    /// Placeholder for incomplete input that may recover with another line.
    IncompleteInput,
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
}
