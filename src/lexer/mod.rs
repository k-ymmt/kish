//! POSIX-oriented lexer module.
//!
//! Phase 3 adds core operator and boundary tokenization:
//! longest-match operators, token-start comments, newline preservation, and
//! quote-aware boundary protection with deferred quote semantics.

pub mod diagnostics;
pub mod span;
pub mod token;

mod alias;
mod cursor;
mod heredoc;
mod operator;
mod quote;
mod scanner;
mod substitution;

use crate::lexer::cursor::Cursor;
use crate::lexer::operator::{has_operator_prefix, scan_operator};
use crate::lexer::scanner::{
    QuoteContext, consume_line_continuation_if_unquoted, is_comment_start,
};

pub use diagnostics::{DiagnosticCode, FatalLexError, LexDiagnostic, RecoverableLexError};
pub use span::{ByteOffset, SourceId, Span};
pub use token::{
    BoundaryResult, CompleteCommandTokens, LexStep, NeedMoreInput, NeedMoreReason, OperatorKind,
    QuoteMarker, QuoteProvenance, SubstitutionKind, SubstitutionMarker, Token, TokenKind,
    TokenOffset, TokenRange,
};

/// Lexer mode used to control tokenization state.
///
/// In Phase 3 these modes are still state markers only.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerMode {
    /// Ordinary token recognition mode.
    Normal,
    /// Mode entered after one or more here-doc operators.
    HereDocPending,
    /// Mode used while reading here-doc bodies.
    HereDocBody,
}

/// A POSIX-first lexer with placeholder scanning behavior.
///
/// Phase 3 behavior:
/// - recognizes longest-match operators
/// - preserves newline tokens
/// - removes unquoted `\\\n` before token delimiting
/// - applies token-start `#` comment skipping
/// - uses quote-aware boundary protection only (full quote semantics deferred)
pub struct Lexer<'a> {
    input: &'a str,
    mode: LexerMode,
    source_id: SourceId,
    cursor: Cursor,
}

impl<'a> Lexer<'a> {
    /// Creates a lexer for the provided input and mode.
    pub fn new(input: &'a str, mode: LexerMode) -> Self {
        Self {
            input,
            mode,
            source_id: SourceId::new(0),
            cursor: Cursor::new(),
        }
    }

    /// Returns the current lexer mode.
    pub fn mode(&self) -> LexerMode {
        self.mode
    }

    /// Scans and returns the next lexical step.
    ///
    /// Phase 3 behavior:
    /// - returns [`LexStep::EndOfInput`] once all input is consumed
    /// - emits one [`TokenKind::Newline`] per `\n`
    /// - emits [`TokenKind::Operator`] using longest-match rules
    /// - emits one [`TokenKind::Token`] for contiguous non-delimiter bytes
    /// - removes unquoted line continuations (`\\\n`) while scanning
    pub fn next_token(&mut self) -> Result<LexStep, FatalLexError> {
        self.skip_horizontal_whitespace();

        if self.cursor.is_eof(self.input) {
            return Ok(LexStep::EndOfInput);
        }

        if self.try_skip_comment_until_newline() {
            if self.cursor.peek_byte(self.input) == Some(b'\n') {
                return self.emit_newline_token();
            }
            return Ok(LexStep::EndOfInput);
        }

        if self.cursor.peek_byte(self.input) == Some(b'\n') {
            return self.emit_newline_token();
        }

        if let Some(operator_scan) = scan_operator(&mut self.cursor, self.input) {
            return Ok(LexStep::Token(Token::new(
                TokenKind::Operator(operator_scan.kind),
                operator_scan.lexeme,
                Span::new(self.source_id, operator_scan.start, operator_scan.end),
            )));
        }

        self.scan_word_token()
    }

    fn emit_newline_token(&mut self) -> Result<LexStep, FatalLexError> {
        let start = self.cursor.offset();
        let _ = self.cursor.advance_byte(self.input);
        let end = self.cursor.offset();
        Ok(LexStep::Token(Token::new(
            TokenKind::Newline,
            "\n".to_string(),
            Span::new(self.source_id, start, end),
        )))
    }

    fn scan_word_token(&mut self) -> Result<LexStep, FatalLexError> {
        let start = self.cursor.offset();
        let mut quote_context = QuoteContext::default();
        let mut lexeme_bytes = Vec::new();
        let mut previous_byte: Option<u8> = None;

        while !self.cursor.is_eof(self.input) {
            if consume_line_continuation_if_unquoted(
                &mut self.cursor,
                self.input,
                &mut quote_context,
            ) {
                previous_byte = None;
                continue;
            }

            let Some(byte) = self.cursor.peek_byte(self.input) else {
                break;
            };

            if quote_context.is_unquoted()
                && (byte.is_ascii_whitespace() || has_operator_prefix(&self.cursor, self.input))
            {
                break;
            }

            let next_byte = self.cursor.peek_next_byte(self.input);
            let consumed = self
                .cursor
                .advance_byte(self.input)
                .expect("peeked byte must be consumable");
            quote_context.observe_byte_for_boundary(consumed, previous_byte, next_byte);
            lexeme_bytes.push(consumed);
            previous_byte = Some(consumed);
        }

        let end = self.cursor.offset();
        let lexeme = String::from_utf8(lexeme_bytes).expect("input is valid UTF-8");
        Ok(LexStep::Token(Token::new(
            TokenKind::Token,
            lexeme,
            Span::new(self.source_id, start, end),
        )))
    }

    fn try_skip_comment_until_newline(&mut self) -> bool {
        let Some(byte) = self.cursor.peek_byte(self.input) else {
            return false;
        };
        if !is_comment_start(byte, true, QuoteContext::default()) {
            return false;
        }

        loop {
            match self.cursor.peek_byte(self.input) {
                Some(b'\n') | None => break,
                Some(_) => {
                    let _ = self.cursor.advance_byte(self.input);
                }
            }
        }
        true
    }

    /// Tokenizes up to the current complete-command boundary.
    ///
    /// Phase 2 behavior:
    /// - returns [`BoundaryResult::NeedMoreInput`] for explicit placeholder
    ///   incomplete patterns
    /// - otherwise tokenizes until newline or end of input and returns
    ///   [`BoundaryResult::Complete`]
    pub fn tokenize_complete_command_boundary(&mut self) -> Result<BoundaryResult, FatalLexError> {
        if let Some(reason) = self.detect_incomplete_input_reason() {
            return Ok(BoundaryResult::NeedMoreInput(NeedMoreInput::new(
                self.cursor.offset(),
                reason,
            )));
        }

        let mut tokens = Vec::new();
        loop {
            match self.next_token()? {
                LexStep::Token(token) => {
                    let is_newline = token.kind == TokenKind::Newline;
                    tokens.push(token);
                    if is_newline {
                        break;
                    }
                }
                LexStep::EndOfInput => break,
                LexStep::Recoverable(error) => {
                    return Ok(BoundaryResult::NeedMoreInput(NeedMoreInput::new(
                        self.cursor.offset(),
                        NeedMoreReason::Recoverable(error),
                    )));
                }
            }
        }

        Ok(BoundaryResult::Complete(CompleteCommandTokens::new(tokens)))
    }

    fn skip_horizontal_whitespace(&mut self) {
        let mut quote_context = QuoteContext::default();
        while !self.cursor.is_eof(self.input) {
            if consume_line_continuation_if_unquoted(
                &mut self.cursor,
                self.input,
                &mut quote_context,
            ) {
                continue;
            }

            let Some(byte) = self.cursor.peek_byte(self.input) else {
                break;
            };
            if byte == b'\n' || !byte.is_ascii_whitespace() {
                break;
            }
            let next_byte = self.cursor.peek_next_byte(self.input);
            let consumed = self
                .cursor
                .advance_byte(self.input)
                .expect("peeked byte must be consumable");
            quote_context.observe_byte_for_boundary(consumed, None, next_byte);
        }
    }

    fn detect_incomplete_input_reason(&self) -> Option<NeedMoreReason> {
        let mut probe = self.cursor;
        let mut quote_context = QuoteContext::default();
        let mut previous_byte: Option<u8> = None;
        let mut saw_unquoted_trailing_backslash = false;

        while !probe.is_eof(self.input) {
            if consume_line_continuation_if_unquoted(&mut probe, self.input, &mut quote_context) {
                saw_unquoted_trailing_backslash = false;
                previous_byte = None;
                continue;
            }

            let next_byte = probe.peek_next_byte(self.input);
            let Some(byte) = probe.advance_byte(self.input) else {
                break;
            };

            saw_unquoted_trailing_backslash =
                quote_context.is_unquoted() && byte == b'\\' && probe.is_eof(self.input);
            quote_context.observe_byte_for_boundary(byte, previous_byte, next_byte);
            previous_byte = Some(byte);
        }

        if saw_unquoted_trailing_backslash {
            return Some(NeedMoreReason::TrailingBackslash);
        }

        None
    }
}
