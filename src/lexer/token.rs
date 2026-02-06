//! Token and boundary result contracts for the lexer.

use crate::lexer::diagnostics::RecoverableLexError;
use crate::lexer::span::{ByteOffset, Span};

/// Operators recognized by POSIX-oriented tokenization rules.
///
/// This enum is the Phase 1 lexical contract for parser-facing operator kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperatorKind {
    /// `&&`
    AndIf,
    /// `||`
    OrIf,
    /// `;;`
    DoubleSemicolon,
    /// `;&`
    SemicolonAmpersand,
    /// `<<`
    HereDoc,
    /// `<<-`
    HereDocStripTabs,
    /// `>>`
    AppendOutput,
    /// `<&`
    DupInput,
    /// `>&`
    DupOutput,
    /// `<>`
    ReadWrite,
    /// `>|`
    Clobber,
    /// `|`
    Pipe,
    /// `;`
    Semicolon,
    /// `&`
    Ampersand,
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `<`
    Less,
    /// `>`
    Greater,
}

impl OperatorKind {
    /// Complete set of operators required by Phase 1.
    pub const ALL: [Self; 18] = [
        Self::AndIf,
        Self::OrIf,
        Self::DoubleSemicolon,
        Self::SemicolonAmpersand,
        Self::HereDoc,
        Self::HereDocStripTabs,
        Self::AppendOutput,
        Self::DupInput,
        Self::DupOutput,
        Self::ReadWrite,
        Self::Clobber,
        Self::Pipe,
        Self::Semicolon,
        Self::Ampersand,
        Self::LeftParen,
        Self::RightParen,
        Self::Less,
        Self::Greater,
    ];

    /// Returns the canonical source text for this operator.
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::AndIf => "&&",
            Self::OrIf => "||",
            Self::DoubleSemicolon => ";;",
            Self::SemicolonAmpersand => ";&",
            Self::HereDoc => "<<",
            Self::HereDocStripTabs => "<<-",
            Self::AppendOutput => ">>",
            Self::DupInput => "<&",
            Self::DupOutput => ">&",
            Self::ReadWrite => "<>",
            Self::Clobber => ">|",
            Self::Pipe => "|",
            Self::Semicolon => ";",
            Self::Ampersand => "&",
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::Less => "<",
            Self::Greater => ">",
        }
    }
}

/// Token-relative byte offset.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenOffset(u32);

impl TokenOffset {
    /// Creates a token-relative offset.
    pub const fn new(value: u32) -> Self {
        Self(value)
    }

    /// Converts `usize` to offset using saturation on overflow.
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
}

/// Token-relative byte range.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TokenRange {
    /// Inclusive start offset.
    pub start: TokenOffset,
    /// Exclusive end offset.
    pub end: TokenOffset,
}

impl TokenRange {
    /// Creates a range and normalizes start/end ordering.
    pub fn new(start: TokenOffset, end: TokenOffset) -> Self {
        if start <= end {
            Self { start, end }
        } else {
            Self {
                start: end,
                end: start,
            }
        }
    }

    /// Returns the range length in bytes.
    pub fn len(self) -> u32 {
        self.end.value() - self.start.value()
    }

    /// Returns `true` when start and end offsets are equal.
    pub fn is_empty(self) -> bool {
        self.start == self.end
    }
}

/// Quote provenance kinds for token segments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum QuoteProvenance {
    /// Segment resulted from backslash escaping.
    BackslashEscaped,
    /// Segment inside single quotes.
    SingleQuoted,
    /// Segment inside double quotes.
    DoubleQuoted,
    /// Segment inside dollar-single quotes (`$'...'`).
    DollarSingleQuoted,
}

/// Quote marker attached to a token.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QuoteMarker {
    /// Quote provenance for the marked segment.
    pub provenance: QuoteProvenance,
    /// Token-relative byte range.
    pub range: TokenRange,
}

impl QuoteMarker {
    /// Creates a quote marker.
    pub fn new(provenance: QuoteProvenance, range: TokenRange) -> Self {
        Self { provenance, range }
    }
}

/// Substitution kinds tracked by token metadata.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SubstitutionKind {
    /// Parameter expansion such as `$x` or `${x}`.
    ParameterExpansion,
    /// Command substitution using `$(...)`.
    CommandSubstitution,
    /// Command substitution using backquotes.
    BackquotedCommandSubstitution,
    /// Arithmetic expansion using `$((...))`.
    ArithmeticExpansion,
}

/// Substitution marker attached to a token.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SubstitutionMarker {
    /// Substitution category for the marked segment.
    pub kind: SubstitutionKind,
    /// Token-relative byte range.
    pub range: TokenRange,
}

impl SubstitutionMarker {
    /// Creates a substitution marker.
    pub fn new(kind: SubstitutionKind, range: TokenRange) -> Self {
        Self { kind, range }
    }
}

/// Core token kinds available in Phase 1.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    /// Shell operator token.
    Operator(OperatorKind),
    /// Raw POSIX TOKEN category.
    Token,
    /// Newline delimiter token.
    Newline,
}

/// A lexical token with raw text, source span, and metadata markers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// Token category.
    pub kind: TokenKind,
    /// Token text preserved as scanned.
    pub lexeme: String,
    /// Byte-oriented source span.
    pub span: Span,
    /// Quote provenance markers for token segments.
    pub quote_markers: Vec<QuoteMarker>,
    /// Substitution markers for token segments.
    pub substitution_markers: Vec<SubstitutionMarker>,
}

impl Token {
    /// Creates a token with empty metadata.
    pub fn new(kind: TokenKind, lexeme: String, span: Span) -> Self {
        Self {
            kind,
            lexeme,
            span,
            quote_markers: Vec::new(),
            substitution_markers: Vec::new(),
        }
    }

    /// Creates a token with explicit metadata.
    pub fn with_metadata(
        kind: TokenKind,
        lexeme: String,
        span: Span,
        quote_markers: Vec<QuoteMarker>,
        substitution_markers: Vec<SubstitutionMarker>,
    ) -> Self {
        Self {
            kind,
            lexeme,
            span,
            quote_markers,
            substitution_markers,
        }
    }

    /// Returns true when this token has no quote/substitution metadata.
    pub fn is_plain(&self) -> bool {
        self.quote_markers.is_empty() && self.substitution_markers.is_empty()
    }

    /// Returns true if this token contains at least one quote marker.
    pub fn has_quote_markers(&self) -> bool {
        !self.quote_markers.is_empty()
    }

    /// Returns true if this token contains at least one substitution marker.
    pub fn has_substitution_markers(&self) -> bool {
        !self.substitution_markers.is_empty()
    }

    /// Returns `Some(fd)` when token is a plain digits-only `TOKEN`.
    ///
    /// This is a parser-side classification helper for IO number handling.
    pub fn io_number_candidate(&self) -> Option<u32> {
        if self.kind != TokenKind::Token || !self.is_plain() || self.lexeme.is_empty() {
            return None;
        }

        if !self.lexeme.bytes().all(|byte| byte.is_ascii_digit()) {
            return None;
        }

        self.lexeme.parse::<u32>().ok()
    }

    /// Returns true when token looks like an IO location candidate.
    ///
    /// Phase 1 uses a conservative shape: `{name}` where `name` follows shell
    /// identifier constraints.
    pub fn is_io_location_candidate(&self) -> bool {
        if self.kind != TokenKind::Token || !self.is_plain() {
            return false;
        }

        let bytes = self.lexeme.as_bytes();
        if bytes.len() < 3 || bytes.first() != Some(&b'{') || bytes.last() != Some(&b'}') {
            return false;
        }

        let name = &self.lexeme[1..bytes.len() - 1];
        let mut chars = name.chars();
        let Some(first) = chars.next() else {
            return false;
        };

        if !(first == '_' || first.is_ascii_alphabetic()) {
            return false;
        }

        chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
    }
}

/// One step produced by [`crate::lexer::Lexer::next_token`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexStep {
    /// A concrete token was produced.
    Token(Token),
    /// End of input was reached.
    EndOfInput,
    /// Recoverable issue requiring more data or context.
    Recoverable(RecoverableLexError),
}

/// Result of tokenization at complete-command boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoundaryResult {
    /// A complete command boundary was reached.
    Complete(CompleteCommandTokens),
    /// More input is required to continue safely.
    NeedMoreInput(NeedMoreInput),
}

/// Tokens gathered up to a command boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompleteCommandTokens {
    /// Token sequence for a single command boundary unit.
    pub tokens: Vec<Token>,
}

impl CompleteCommandTokens {
    /// Creates a boundary token container.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }
}

/// Placeholder reason for incomplete input conditions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NeedMoreReason {
    /// Line continuation marker with no following line.
    TrailingBackslash,
    /// Unterminated single quote in remaining input.
    UnterminatedSingleQuote,
    /// Unterminated double quote in remaining input.
    UnterminatedDoubleQuote,
    /// Unterminated dollar-single quote in remaining input.
    UnterminatedDollarSingleQuote,
    /// Unterminated parameter expansion in remaining input.
    UnterminatedParameterExpansion,
    /// Unterminated command substitution in remaining input.
    UnterminatedCommandSubstitution,
    /// Unterminated backquoted command substitution in remaining input.
    UnterminatedBackquotedCommandSubstitution,
    /// Unterminated arithmetic expansion in remaining input.
    UnterminatedArithmeticExpansion,
    /// Here-document delimiter was not found before input end.
    HereDocDelimiterNotFound,
    /// Incomplete state propagated from recoverable lexer diagnostics.
    Recoverable(RecoverableLexError),
}

/// Metadata for an incomplete command boundary scan.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NeedMoreInput {
    /// Byte position where scanning should resume.
    pub checkpoint: ByteOffset,
    /// The reason more input is required.
    pub reason: NeedMoreReason,
}

impl NeedMoreInput {
    /// Creates an incomplete boundary value.
    pub fn new(checkpoint: ByteOffset, reason: NeedMoreReason) -> Self {
        Self { checkpoint, reason }
    }
}
