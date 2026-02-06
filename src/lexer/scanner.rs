//! Scanner helpers for lexer token-recognition utilities.

use crate::lexer::cursor::Cursor;

/// Lightweight quote state used only for Phase 2 line-continuation handling.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct QuoteContext {
    in_single_quote: bool,
    in_double_quote: bool,
    escaped: bool,
}

impl QuoteContext {
    /// Returns true when current scanning state is unquoted.
    pub(crate) fn is_unquoted(self) -> bool {
        !self.in_single_quote && !self.in_double_quote && !self.escaped
    }

    /// Updates quote/escape state after consuming one byte.
    pub(crate) fn observe_consumed_byte(&mut self, byte: u8) {
        if self.escaped {
            self.escaped = false;
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

        match byte {
            b'\'' => self.in_single_quote = true,
            b'"' => self.in_double_quote = true,
            b'\\' => self.escaped = true,
            _ => {}
        }
    }
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
