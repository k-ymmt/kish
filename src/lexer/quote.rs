//! Quote-state helpers for token scanning.

use crate::lexer::diagnostics::{
    DiagnosticCode, FatalLexError, LexDiagnostic, near_text_snippet, suggestion_close_quote,
};
use crate::lexer::span::{ByteOffset, SourceId, Span};
use crate::lexer::token::{QuoteMarker, QuoteProvenance, TokenOffset, TokenRange};

/// Open quote variants tracked while scanning a token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OpenQuoteKind {
    /// `'...'`
    Single,
    /// `"..."`
    Double,
    /// `$'...'`
    DollarSingle,
}

/// Metadata for an open quote region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct OpenQuote {
    pub(crate) kind: OpenQuoteKind,
    pub(crate) token_start: usize,
    pub(crate) source_start: ByteOffset,
}

/// Quote-state machine for one word token scan.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct QuoteScanner {
    open_quote: Option<OpenQuote>,
}

impl QuoteScanner {
    /// Returns true when the scanner is outside any quote construct.
    pub(crate) fn is_unquoted(self) -> bool {
        self.open_quote.is_none()
    }

    /// Returns the currently open quote, if any.
    pub(crate) fn open_quote(self) -> Option<OpenQuote> {
        self.open_quote
    }

    /// Starts a quote region at the current token/source offsets.
    pub(crate) fn open(
        &mut self,
        kind: OpenQuoteKind,
        token_start: usize,
        source_start: ByteOffset,
    ) {
        self.open_quote = Some(OpenQuote {
            kind,
            token_start,
            source_start,
        });
    }

    /// Closes the current quote and returns its metadata.
    pub(crate) fn close(&mut self, expected: OpenQuoteKind) -> Option<OpenQuote> {
        let open = self.open_quote?;
        if open.kind != expected {
            return None;
        }
        self.open_quote = None;
        Some(open)
    }
}

/// Appends one quote marker when range is non-empty.
pub(crate) fn push_quote_marker(
    markers: &mut Vec<QuoteMarker>,
    provenance: QuoteProvenance,
    start: usize,
    end: usize,
) {
    if end <= start {
        return;
    }
    markers.push(QuoteMarker::new(
        provenance,
        TokenRange::new(TokenOffset::from_usize(start), TokenOffset::from_usize(end)),
    ));
}

/// Builds a fatal lexer error for an unterminated quote.
pub(crate) fn unterminated_quote_error(
    source_id: SourceId,
    open: OpenQuote,
    input: &str,
    end: ByteOffset,
) -> FatalLexError {
    let span = Span::new(source_id, open.source_start, end);
    let near_text = near_text_snippet(input, open.source_start, end);

    match open.kind {
        OpenQuoteKind::Single => {
            FatalLexError::UnterminatedSingleQuote(LexDiagnostic::with_context(
                DiagnosticCode::UnterminatedSingleQuote,
                "unterminated single quote",
                span,
                near_text.clone(),
                Some(suggestion_close_quote("single quote", "'")),
            ))
        }
        OpenQuoteKind::Double => {
            FatalLexError::UnterminatedDoubleQuote(LexDiagnostic::with_context(
                DiagnosticCode::UnterminatedDoubleQuote,
                "unterminated double quote",
                span,
                near_text.clone(),
                Some(suggestion_close_quote("double quote", "\"")),
            ))
        }
        OpenQuoteKind::DollarSingle => {
            FatalLexError::UnterminatedDollarSingleQuote(LexDiagnostic::with_context(
                DiagnosticCode::UnterminatedDollarSingleQuote,
                "unterminated dollar-single quote",
                span,
                near_text,
                Some(suggestion_close_quote("dollar-single quote", "'")),
            ))
        }
    }
}
