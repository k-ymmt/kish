//! Here-document queue and body helper types.

use crate::lexer::diagnostics::{
    DiagnosticCode, FatalLexError, LexDiagnostic, near_text_snippet,
    suggestion_add_heredoc_delimiter,
};
use crate::lexer::span::{ByteOffset, SourceId, Span};
use crate::lexer::token::{OperatorKind, QuoteMarker, Token};

/// Pending here-document specification discovered before line-end.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PendingHereDocSpec {
    pub(crate) raw_delimiter: String,
    pub(crate) delimiter_key: String,
    pub(crate) strip_tabs: bool,
    pub(crate) quoted: bool,
    pub(crate) operator_span: Span,
    pub(crate) delimiter_span: Span,
}

/// Captured body payload for one here-document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HereDocBodyCapture {
    pub origin_operator_span: Span,
    pub delimiter_span: Span,
    pub body_span: Span,
    pub raw_delimiter: String,
    pub delimiter_key: String,
    pub strip_tabs: bool,
    pub quoted: bool,
    pub raw_body: String,
}

/// Returns true when operator kind is an io_here form.
pub(crate) fn is_io_here_operator(kind: OperatorKind) -> bool {
    matches!(kind, OperatorKind::HereDoc | OperatorKind::HereDocStripTabs)
}

/// Returns true when io_here operator implies leading-tab stripping.
pub(crate) fn is_strip_tabs_operator(kind: OperatorKind) -> bool {
    matches!(kind, OperatorKind::HereDocStripTabs)
}

/// Removes leading tab bytes for here-doc line matching (`<<-` semantics).
pub(crate) fn strip_tabs_for_match(line: &str) -> &str {
    line.trim_start_matches('\t')
}

/// Produces a pending here-doc spec from the delimiter token.
pub(crate) fn derive_delimiter_spec(
    operator_span: Span,
    token: &Token,
    strip_tabs: bool,
) -> PendingHereDocSpec {
    PendingHereDocSpec {
        raw_delimiter: token.lexeme.clone(),
        delimiter_key: quote_remove_delimiter(&token.lexeme, &token.quote_markers),
        strip_tabs,
        quoted: token.has_quote_markers(),
        operator_span,
        delimiter_span: token.span,
    }
}

/// Removes quote/backslash syntax and returns delimiter comparison key.
pub(crate) fn quote_remove_delimiter(raw: &str, quote_markers: &[QuoteMarker]) -> String {
    if quote_markers.is_empty() {
        return raw.to_string();
    }

    let bytes = raw.as_bytes();
    let mut i = 0usize;
    let mut out = Vec::with_capacity(bytes.len());

    let mut in_single = false;
    let mut in_double = false;
    let mut in_dollar_single = false;
    let mut escaped = false;

    while i < bytes.len() {
        let byte = bytes[i];
        let next = bytes.get(i + 1).copied();

        if escaped {
            out.push(byte);
            escaped = false;
            i += 1;
            continue;
        }

        if in_dollar_single {
            match byte {
                b'\\' => escaped = true,
                b'\'' => in_dollar_single = false,
                _ => out.push(byte),
            }
            i += 1;
            continue;
        }

        if in_single {
            if byte == b'\'' {
                in_single = false;
            } else {
                out.push(byte);
            }
            i += 1;
            continue;
        }

        if in_double {
            match byte {
                b'"' => in_double = false,
                b'\\' => escaped = true,
                _ => out.push(byte),
            }
            i += 1;
            continue;
        }

        match byte {
            b'\\' => escaped = true,
            b'\'' => in_single = true,
            b'"' => in_double = true,
            b'$' if next == Some(b'\'') => {
                in_dollar_single = true;
                i += 1;
            }
            _ => out.push(byte),
        }

        i += 1;
    }

    String::from_utf8(out).expect("input is valid UTF-8")
}

/// Creates a warning diagnostic for EOF-before-delimiter here-doc failures.
pub(crate) fn delimiter_not_found_diagnostic(
    source_id: SourceId,
    spec: &PendingHereDocSpec,
    input: &str,
    end: ByteOffset,
) -> LexDiagnostic {
    LexDiagnostic::with_context(
        DiagnosticCode::HereDocDelimiterNotFound,
        format!(
            "here-document delimiter `{}` was not found before end of input",
            spec.delimiter_key
        ),
        Span::new(source_id, spec.delimiter_span.start, end),
        near_text_snippet(input, spec.delimiter_span.start, end),
        Some(suggestion_add_heredoc_delimiter(&spec.delimiter_key)),
    )
}

/// Creates a fatal error for EOF-before-delimiter here-doc failures.
pub(crate) fn delimiter_not_found_error(
    source_id: SourceId,
    spec: &PendingHereDocSpec,
    input: &str,
    end: ByteOffset,
) -> FatalLexError {
    FatalLexError::HereDocDelimiterNotFound(delimiter_not_found_diagnostic(
        source_id, spec, input, end,
    ))
}

/// Returns line contents without trailing newline for delimiter matching.
pub(crate) fn line_for_match(line: &str) -> &str {
    line.strip_suffix('\n').unwrap_or(line)
}

/// Removes leading tabs for stored here-doc body lines (`<<-` semantics).
pub(crate) fn strip_tabs_for_storage(line: &str) -> &str {
    line.trim_start_matches('\t')
}
