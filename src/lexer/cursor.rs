//! Minimal byte cursor for Phase 0 scanning.

use crate::lexer::span::ByteOffset;

/// Byte-position cursor over input text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Cursor {
    offset: ByteOffset,
}

impl Cursor {
    /// Creates a cursor at byte offset `0`.
    pub(crate) fn new() -> Self {
        Self {
            offset: ByteOffset::new(0),
        }
    }

    /// Returns the current byte offset.
    pub(crate) fn offset(&self) -> ByteOffset {
        self.offset
    }

    /// Returns `true` if the cursor is at or beyond input end.
    pub(crate) fn is_eof(&self, input: &str) -> bool {
        self.offset.as_usize() >= input.len()
    }

    /// Returns the current byte at cursor position.
    pub(crate) fn peek_byte(&self, input: &str) -> Option<u8> {
        input.as_bytes().get(self.offset.as_usize()).copied()
    }

    /// Advances the cursor by `count` bytes, clamped to input length.
    pub(crate) fn advance_by(&mut self, count: usize, input: &str) {
        let next = self
            .offset
            .as_usize()
            .saturating_add(count)
            .min(input.len());
        self.offset = ByteOffset::from_usize(next);
    }
}
