//! Immutable source location primitives.

/// Identifier for a logical input source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceId(u32);

impl SourceId {
    /// Creates a source identifier.
    pub const fn new(value: u32) -> Self {
        Self(value)
    }

    /// Returns the raw identifier value.
    pub const fn value(self) -> u32 {
        self.0
    }
}

/// Byte offset within a single source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ByteOffset(u32);

impl ByteOffset {
    /// Creates a byte offset value.
    pub const fn new(value: u32) -> Self {
        Self(value)
    }

    /// Creates an offset from `usize` with saturation.
    pub fn from_usize(value: usize) -> Self {
        match u32::try_from(value) {
            Ok(offset) => Self(offset),
            Err(_) => Self(u32::MAX),
        }
    }

    /// Returns the raw offset value.
    pub const fn value(self) -> u32 {
        self.0
    }

    /// Converts the offset to `usize`.
    pub const fn as_usize(self) -> usize {
        self.0 as usize
    }
}

/// Immutable source span using byte offsets.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// Source identity for this span.
    pub source_id: SourceId,
    /// Inclusive start byte offset.
    pub start: ByteOffset,
    /// Exclusive end byte offset.
    pub end: ByteOffset,
}

impl Span {
    /// Creates a span and normalizes offset ordering.
    pub fn new(source_id: SourceId, start: ByteOffset, end: ByteOffset) -> Self {
        if start <= end {
            Self {
                source_id,
                start,
                end,
            }
        } else {
            Self {
                source_id,
                start: end,
                end: start,
            }
        }
    }

    /// Returns the span length in bytes.
    pub fn len(self) -> u32 {
        self.end.value() - self.start.value()
    }

    /// Returns `true` when the span contains no bytes.
    pub fn is_empty(self) -> bool {
        self.start == self.end
    }
}
