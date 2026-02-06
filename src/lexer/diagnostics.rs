//! Diagnostic and error contracts for lexer stages.
//!
//! Phase 2 keeps diagnostics byte-span based. Line/column tracking now lives in
//! the cursor and will be wired into diagnostics in a later phase.

use crate::lexer::span::{ByteOffset, Span};

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
    /// Malformed arithmetic expansion (`$((...))`) sequence.
    MalformedArithmeticExpansion,
    /// Substitution recursion depth exceeded the lexer cap.
    SubstitutionRecursionDepthExceeded,
    /// Here-document delimiter was not found before input end.
    HereDocDelimiterNotFound,
    /// Token bytes exceeded configured lexer token limit.
    TokenSizeLimitExceeded,
    /// Here-document body bytes exceeded configured lexer limit.
    HereDocBodySizeLimitExceeded,
    /// Token count exceeded configured boundary token limit.
    BoundaryTokenLimitExceeded,
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
    /// Optional snippet near the failure point.
    pub near_text: Option<String>,
    /// Optional suggestion for recovering from the failure.
    pub suggestion: Option<String>,
}

impl LexDiagnostic {
    /// Creates a diagnostic value.
    pub fn new(code: DiagnosticCode, message: impl Into<String>, span: Span) -> Self {
        Self::with_context(code, message, span, None, None)
    }

    /// Creates a diagnostic value with contextual details.
    pub fn with_context(
        code: DiagnosticCode,
        message: impl Into<String>,
        span: Span,
        near_text: Option<String>,
        suggestion: Option<String>,
    ) -> Self {
        Self {
            code,
            message: message.into(),
            span,
            near_text,
            suggestion,
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
    /// Arithmetic expansion bytes were structurally malformed.
    MalformedArithmeticExpansion(LexDiagnostic),
    /// Substitution recursion depth exceeded the lexer limit.
    SubstitutionRecursionDepthExceeded(LexDiagnostic),
    /// Here-document delimiter was not found before input end.
    HereDocDelimiterNotFound(LexDiagnostic),
    /// Token bytes exceeded configured lexer token limit.
    TokenSizeLimitExceeded(LexDiagnostic),
    /// Here-document body bytes exceeded configured lexer limit.
    HereDocBodySizeLimitExceeded(LexDiagnostic),
    /// Token count exceeded configured boundary token limit.
    BoundaryTokenLimitExceeded(LexDiagnostic),
    /// Input is incomplete in a mode that requires hard failure.
    IncompleteInput(LexDiagnostic),
}

/// Extracts a short snippet between byte offsets for diagnostics.
pub(crate) fn near_text_snippet(input: &str, start: ByteOffset, end: ByteOffset) -> Option<String> {
    let start_index = start.as_usize().min(input.len());
    let end_index = end.as_usize().min(input.len());
    let slice = if start_index <= end_index {
        &input[start_index..end_index]
    } else {
        ""
    };
    let preview: String = slice.chars().take(24).collect();
    if preview.is_empty() {
        None
    } else {
        Some(preview.replace('\n', "\\n"))
    }
}

/// Builds a generic quote-closure suggestion.
pub(crate) fn suggestion_close_quote(quote_label: &str, terminator: &str) -> String {
    format!("close the {quote_label} with `{terminator}`.")
}

/// Builds a suggestion for unterminated here-document input.
pub(crate) fn suggestion_add_heredoc_delimiter(delimiter: &str) -> String {
    format!("add a line containing delimiter `{delimiter}` exactly.")
}

/// Builds a suggestion for malformed arithmetic expansion input.
pub(crate) fn suggestion_fix_arithmetic_expansion() -> String {
    "use a non-empty arithmetic expression and close it with `))`.".to_string()
}
