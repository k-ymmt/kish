//! POSIX-oriented lexer module.
//!
//! Phase 2 keeps tokenization semantics minimal while adding cursor utilities:
//! byte/line/column tracking, checkpoint/rollback, and unquoted line-join
//! handling for `\\\n`.

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
use crate::lexer::scanner::{QuoteContext, consume_line_continuation_if_unquoted};

pub use diagnostics::{DiagnosticCode, FatalLexError, LexDiagnostic, RecoverableLexError};
pub use span::{ByteOffset, SourceId, Span};
pub use token::{
    BoundaryResult, CompleteCommandTokens, LexStep, NeedMoreInput, NeedMoreReason, OperatorKind,
    QuoteMarker, QuoteProvenance, SubstitutionKind, SubstitutionMarker, Token, TokenKind,
    TokenOffset, TokenRange,
};

/// Lexer mode used to control tokenization state.
///
/// In Phase 2 these modes are still state markers only.
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
/// The implementation is intentionally simple and deterministic:
/// - returns raw word tokens split on ASCII whitespace
/// - emits newline tokens
/// - removes unquoted `\\\n` before token delimiting
/// - does not implement operator, quote, substitution, or here-doc semantics yet
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
    /// Phase 2 behavior:
    /// - returns [`LexStep::EndOfInput`] once all input is consumed
    /// - emits one [`TokenKind::Newline`] per `\n`
    /// - emits one [`TokenKind::Token`] for contiguous non-whitespace bytes
    /// - removes unquoted line continuations (`\\\n`) while scanning
    pub fn next_token(&mut self) -> Result<LexStep, FatalLexError> {
        self.skip_horizontal_whitespace();

        if self.cursor.is_eof(self.input) {
            return Ok(LexStep::EndOfInput);
        }

        let start = self.cursor.offset();
        match self.cursor.peek_byte(self.input) {
            Some(b'\n') => {
                let _ = self.cursor.advance_byte(self.input);
                let end = self.cursor.offset();
                Ok(LexStep::Token(Token::new(
                    TokenKind::Newline,
                    "\n".to_string(),
                    Span::new(self.source_id, start, end),
                )))
            }
            Some(_) => {
                let mut quote_context = QuoteContext::default();
                let mut lexeme_bytes = Vec::new();

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
                    if byte.is_ascii_whitespace() {
                        break;
                    }

                    let consumed = self
                        .cursor
                        .advance_byte(self.input)
                        .expect("peeked byte must be consumable");
                    quote_context.observe_consumed_byte(consumed);
                    lexeme_bytes.push(consumed);
                }

                let end = self.cursor.offset();
                let lexeme = String::from_utf8(lexeme_bytes).expect("input is valid UTF-8");
                Ok(LexStep::Token(Token::new(
                    TokenKind::Token,
                    lexeme,
                    Span::new(self.source_id, start, end),
                )))
            }
            None => Ok(LexStep::EndOfInput),
        }
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
            let _ = self.cursor.advance_byte(self.input);
        }
    }

    fn detect_incomplete_input_reason(&self) -> Option<NeedMoreReason> {
        let mut probe = self.cursor;
        let mut quote_context = QuoteContext::default();
        let mut saw_unquoted_trailing_backslash = false;

        while !probe.is_eof(self.input) {
            if consume_line_continuation_if_unquoted(&mut probe, self.input, &mut quote_context) {
                saw_unquoted_trailing_backslash = false;
                continue;
            }

            let Some(byte) = probe.advance_byte(self.input) else {
                break;
            };

            saw_unquoted_trailing_backslash =
                quote_context.is_unquoted() && byte == b'\\' && probe.is_eof(self.input);
            quote_context.observe_consumed_byte(byte);
        }

        if saw_unquoted_trailing_backslash {
            return Some(NeedMoreReason::TrailingBackslash);
        }

        None
    }
}
