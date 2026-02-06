//! IR error contracts.

use crate::lexer::Span;

/// Stable IR error categories.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrErrorKind {
    /// The input form is not supported by the current lowering phase.
    UnsupportedForm,
    /// A configured resource limit was exceeded.
    LimitExceeded,
    /// A required IR invariant was violated.
    InvariantViolation,
    /// Operand packing or encoded representation overflowed.
    EncodingOverflow,
    /// IR lowering and source shape diverged.
    LoweringMismatch,
    /// Lowering produced unreachable control-flow shape.
    UnreachableControl,
    /// Redirect form is malformed or inconsistent.
    InvalidRedirectShape,
    /// Assignment form is malformed or inconsistent.
    InvalidAssignmentShape,
    /// Word form is malformed or inconsistent.
    InvalidWordShape,
}

/// IR error payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrError {
    /// Error category.
    pub kind: IrErrorKind,
    /// Optional source span near the failure.
    pub span: Option<Span>,
    /// Human-readable error summary.
    pub message: String,
    /// Optional additional detail.
    pub detail: Option<String>,
}

impl IrError {
    /// Creates an IR error.
    pub fn new(
        kind: IrErrorKind,
        span: Option<Span>,
        message: impl Into<String>,
        detail: Option<String>,
    ) -> Self {
        Self {
            kind,
            span,
            message: message.into(),
            detail,
        }
    }

    /// Creates a `UnsupportedForm` error.
    pub fn unsupported_form(span: Option<Span>, message: impl Into<String>) -> Self {
        Self::new(IrErrorKind::UnsupportedForm, span, message, None)
    }

    /// Creates a `LimitExceeded` error.
    pub fn limit_exceeded(
        span: Option<Span>,
        message: impl Into<String>,
        detail: impl Into<String>,
    ) -> Self {
        Self::new(
            IrErrorKind::LimitExceeded,
            span,
            message,
            Some(detail.into()),
        )
    }

    /// Creates an `InvariantViolation` error.
    pub fn invariant_violation(
        span: Option<Span>,
        message: impl Into<String>,
        detail: impl Into<String>,
    ) -> Self {
        Self::new(
            IrErrorKind::InvariantViolation,
            span,
            message,
            Some(detail.into()),
        )
    }

    /// Creates an `InvalidRedirectShape` error.
    pub fn invalid_redirect_shape(
        span: Option<Span>,
        message: impl Into<String>,
        detail: impl Into<String>,
    ) -> Self {
        Self::new(
            IrErrorKind::InvalidRedirectShape,
            span,
            message,
            Some(detail.into()),
        )
    }

    /// Creates an `InvalidAssignmentShape` error.
    pub fn invalid_assignment_shape(
        span: Option<Span>,
        message: impl Into<String>,
        detail: impl Into<String>,
    ) -> Self {
        Self::new(
            IrErrorKind::InvalidAssignmentShape,
            span,
            message,
            Some(detail.into()),
        )
    }

    /// Creates an `InvalidWordShape` error.
    pub fn invalid_word_shape(
        span: Option<Span>,
        message: impl Into<String>,
        detail: impl Into<String>,
    ) -> Self {
        Self::new(
            IrErrorKind::InvalidWordShape,
            span,
            message,
            Some(detail.into()),
        )
    }

    /// Creates an invariant violation for branch-target shape errors.
    pub fn invalid_branch_target(
        span: Option<Span>,
        message: impl Into<String>,
        detail: impl Into<String>,
    ) -> Self {
        Self::new(
            IrErrorKind::InvariantViolation,
            span,
            message,
            Some(detail.into()),
        )
    }

    /// Creates an invariant violation for duplicate label binding.
    pub fn duplicate_label_binding(
        span: Option<Span>,
        message: impl Into<String>,
        detail: impl Into<String>,
    ) -> Self {
        Self::new(
            IrErrorKind::InvariantViolation,
            span,
            message,
            Some(detail.into()),
        )
    }
}
