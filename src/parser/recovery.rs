//! Recovery contracts for parser interactive behavior.

use crate::lexer::{FatalLexError, RecoverableLexError, Span};

/// Reason why interactive parsing needs more input.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NeedMoreInputReason {
    /// A trailing backslash escapes the physical line terminator.
    TrailingBackslash,
    /// A single quote is unterminated.
    UnterminatedSingleQuote,
    /// A double quote is unterminated.
    UnterminatedDoubleQuote,
    /// A dollar-single-quoted sequence is unterminated.
    UnterminatedDollarSingleQuote,
    /// A parameter expansion is unterminated.
    UnterminatedParameterExpansion,
    /// A command substitution is unterminated.
    UnterminatedCommandSubstitution,
    /// A backquoted command substitution is unterminated.
    UnterminatedBackquotedCommandSubstitution,
    /// An arithmetic expansion is unterminated.
    UnterminatedArithmeticExpansion,
    /// A here-doc delimiter is missing.
    HereDocDelimiterNotFound,
    /// An `if` clause reached EOF before `fi`.
    UnclosedIfClause,
    /// A `case` clause reached EOF before `esac`.
    UnclosedCaseClause,
    /// A `do` group reached EOF before `done`.
    UnclosedDoGroup,
    /// A brace group reached EOF before `}`.
    UnclosedBraceGroup,
    /// A subshell reached EOF before `)`.
    UnclosedSubshell,
    /// A pipeline ended after a trailing `|`.
    TrailingPipeOperator,
    /// An AND-OR list ended after a trailing `&&`.
    TrailingAndIfOperator,
    /// An AND-OR list ended after a trailing `||`.
    TrailingOrIfOperator,
    /// Input is syntactically incomplete according to lexer diagnostics.
    IncompleteInputDiagnostic,
}

impl NeedMoreInputReason {
    /// Maps a fatal lexer error to a parser continuation reason when possible.
    pub fn from_fatal_lexer_error(error: &FatalLexError) -> Option<Self> {
        match error {
            FatalLexError::UnterminatedSingleQuote(_) => Some(Self::UnterminatedSingleQuote),
            FatalLexError::UnterminatedDoubleQuote(_) => Some(Self::UnterminatedDoubleQuote),
            FatalLexError::UnterminatedDollarSingleQuote(_) => {
                Some(Self::UnterminatedDollarSingleQuote)
            }
            FatalLexError::UnterminatedParameterExpansion(_) => {
                Some(Self::UnterminatedParameterExpansion)
            }
            FatalLexError::UnterminatedCommandSubstitution(_) => {
                Some(Self::UnterminatedCommandSubstitution)
            }
            FatalLexError::UnterminatedBackquotedCommandSubstitution(_) => {
                Some(Self::UnterminatedBackquotedCommandSubstitution)
            }
            FatalLexError::UnterminatedArithmeticExpansion(_) => {
                Some(Self::UnterminatedArithmeticExpansion)
            }
            FatalLexError::HereDocDelimiterNotFound(_) => Some(Self::HereDocDelimiterNotFound),
            FatalLexError::IncompleteInput(diagnostic) => {
                if diagnostic.near_text.as_deref() == Some("\\") {
                    Some(Self::TrailingBackslash)
                } else {
                    Some(Self::IncompleteInputDiagnostic)
                }
            }
            _ => None,
        }
    }

    /// Maps a recoverable lexer error to a parser continuation reason.
    pub fn from_recoverable_lexer_error(error: &RecoverableLexError) -> Self {
        match error {
            RecoverableLexError::IncompleteInput(diagnostic) => {
                if diagnostic.near_text.as_deref() == Some("\\") {
                    Self::TrailingBackslash
                } else {
                    Self::IncompleteInputDiagnostic
                }
            }
        }
    }
}

/// Interactive parser diagnostic severity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParseDiagnosticSeverity {
    /// Informational note.
    Note,
    /// Non-fatal warning.
    Warning,
}

/// Non-fatal diagnostic payload for interactive recovery mode.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseDiagnostic {
    /// Diagnostic message text.
    pub message: String,
    /// Optional source span associated with the note.
    pub span: Option<Span>,
    /// Severity level.
    pub severity: ParseDiagnosticSeverity,
}

impl ParseDiagnostic {
    /// Creates a new parser diagnostic.
    pub fn new(
        message: impl Into<String>,
        span: Option<Span>,
        severity: ParseDiagnosticSeverity,
    ) -> Self {
        Self {
            message: message.into(),
            span,
            severity,
        }
    }
}
