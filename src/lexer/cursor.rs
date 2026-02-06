//! Byte cursor utilities for lexer scanning.
//!
//! Phase 2 tracks byte offset and 1-based line/column positions while keeping
//! byte-oriented scanning primitives efficient.

use crate::lexer::span::ByteOffset;

/// Snapshot of cursor position for speculative scanning.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub(crate) struct CursorCheckpoint {
    offset: ByteOffset,
    line: u32,
    column: u32,
}

/// Byte-position cursor over input text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Cursor {
    offset: ByteOffset,
    line: u32,
    column: u32,
}

#[allow(dead_code)]
impl Cursor {
    /// Creates a cursor at byte offset `0` and line/column `1`.
    pub(crate) fn new() -> Self {
        Self {
            offset: ByteOffset::new(0),
            line: 1,
            column: 1,
        }
    }

    /// Returns the current byte offset.
    pub(crate) fn offset(&self) -> ByteOffset {
        self.offset
    }

    /// Returns the current 1-based line.
    pub(crate) fn line(&self) -> u32 {
        self.line
    }

    /// Returns the current 1-based column.
    pub(crate) fn column(&self) -> u32 {
        self.column
    }

    /// Returns `true` if the cursor is at or beyond input end.
    pub(crate) fn is_eof(&self, input: &str) -> bool {
        self.offset.as_usize() >= input.len()
    }

    /// Returns the current byte at cursor position.
    pub(crate) fn peek_byte(&self, input: &str) -> Option<u8> {
        input.as_bytes().get(self.offset.as_usize()).copied()
    }

    /// Returns the next byte at cursor position + 1.
    pub(crate) fn peek_next_byte(&self, input: &str) -> Option<u8> {
        let next_index = self.offset.as_usize().saturating_add(1);
        input.as_bytes().get(next_index).copied()
    }

    /// Returns true if cursor offset is on a UTF-8 character boundary.
    pub(crate) fn is_char_boundary(&self, input: &str) -> bool {
        input.is_char_boundary(self.offset.as_usize())
    }

    /// Returns the current character when cursor is on a UTF-8 boundary.
    pub(crate) fn peek_char(&self, input: &str) -> Option<char> {
        if !self.is_char_boundary(input) || self.is_eof(input) {
            return None;
        }
        input[self.offset.as_usize()..].chars().next()
    }

    /// Advances by one byte and returns the consumed byte.
    pub(crate) fn advance_byte(&mut self, input: &str) -> Option<u8> {
        let byte = self.peek_byte(input)?;
        let next = self.offset.as_usize().saturating_add(1).min(input.len());
        self.offset = ByteOffset::from_usize(next);
        self.update_line_column(byte);
        Some(byte)
    }

    /// Consumes while predicate returns true.
    pub(crate) fn consume_while<F>(&mut self, input: &str, mut predicate: F)
    where
        F: FnMut(u8) -> bool,
    {
        while let Some(byte) = self.peek_byte(input) {
            if !predicate(byte) {
                break;
            }
            let _ = self.advance_byte(input);
        }
    }

    /// Returns a checkpoint that can be used for rollback.
    pub(crate) fn checkpoint(&self) -> CursorCheckpoint {
        CursorCheckpoint {
            offset: self.offset,
            line: self.line,
            column: self.column,
        }
    }

    /// Restores cursor state from checkpoint.
    pub(crate) fn rollback(&mut self, checkpoint: CursorCheckpoint) {
        self.offset = checkpoint.offset;
        self.line = checkpoint.line;
        self.column = checkpoint.column;
    }

    fn update_line_column(&mut self, consumed: u8) {
        if consumed == b'\n' {
            self.line = self.line.saturating_add(1);
            self.column = 1;
            return;
        }

        // UTF-8 continuation bytes (10xxxxxx) must not increment display column.
        if (consumed & 0b1100_0000) != 0b1000_0000 {
            self.column = self.column.saturating_add(1);
        }
    }
}
