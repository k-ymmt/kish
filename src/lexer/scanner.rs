//! Scanner helpers for lexer token-recognition utilities.

use crate::lexer::cursor::Cursor;

/// Lightweight quote state for Phase 3 boundary-aware token scanning.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct QuoteContext {
    in_single_quote: bool,
    in_double_quote: bool,
    in_dollar_single_quote: bool,
    escaped: bool,
    pending_dollar_single_quote: bool,
}

impl QuoteContext {
    /// Returns true when current scanning state is unquoted.
    pub(crate) fn is_unquoted(self) -> bool {
        !self.in_single_quote
            && !self.in_double_quote
            && !self.in_dollar_single_quote
            && !self.escaped
            && !self.pending_dollar_single_quote
    }

    /// Returns true when current state is unquoted and suitable for token-start checks.
    pub(crate) fn is_token_start_unquoted(self) -> bool {
        self.is_unquoted()
    }

    /// Updates quote/escape state after consuming one byte.
    pub(crate) fn observe_byte_for_boundary(
        &mut self,
        byte: u8,
        _prev_byte: Option<u8>,
        next_byte: Option<u8>,
    ) {
        if self.escaped {
            self.escaped = false;
            self.pending_dollar_single_quote = false;
            return;
        }

        if self.in_dollar_single_quote {
            match byte {
                b'\\' => self.escaped = true,
                b'\'' => self.in_dollar_single_quote = false,
                _ => {}
            }
            return;
        }

        if self.in_single_quote {
            if byte == b'\'' {
                self.in_single_quote = false;
            }
            return;
        }

        if self.in_double_quote {
            if byte == b'"' {
                self.in_double_quote = false;
                return;
            }
            if byte == b'\\' {
                self.escaped = true;
            }
            return;
        }

        if self.pending_dollar_single_quote {
            if byte == b'\'' {
                self.in_dollar_single_quote = true;
                self.pending_dollar_single_quote = false;
                return;
            }
            self.pending_dollar_single_quote = false;
        }

        match byte {
            b'\'' => self.in_single_quote = true,
            b'"' => self.in_double_quote = true,
            b'\\' => self.escaped = true,
            b'$' if next_byte == Some(b'\'') => self.pending_dollar_single_quote = true,
            _ => {}
        }
    }
}

/// Returns true when `#` is a token-start comment introducer.
pub(crate) fn is_comment_start(
    byte: u8,
    token_is_empty: bool,
    quote_context: QuoteContext,
) -> bool {
    byte == b'#' && token_is_empty && quote_context.is_token_start_unquoted()
}

/// Consumes an unquoted `\\\n` pair if it appears at cursor position.
///
/// Returns `true` when continuation was consumed and removed.
pub(crate) fn consume_line_continuation_if_unquoted(
    cursor: &mut Cursor,
    input: &str,
    quote_context: &mut QuoteContext,
) -> bool {
    if !quote_context.is_unquoted() {
        return false;
    }

    if cursor.peek_byte(input) != Some(b'\\') || cursor.peek_next_byte(input) != Some(b'\n') {
        return false;
    }

    let _ = cursor.advance_byte(input);
    let _ = cursor.advance_byte(input);
    true
}
