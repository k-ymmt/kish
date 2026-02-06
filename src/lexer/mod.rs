//! POSIX-oriented lexer module.
//!
//! Phase 5 adds nested substitution scanning:
//! longest-match operators, token-start comments, newline preservation, and
//! quote/substitution metadata and diagnostics for nested scanner constructs.

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
use crate::lexer::heredoc::{
    PendingHereDocSpec, delimiter_not_found_diagnostic, delimiter_not_found_error,
    derive_delimiter_spec, is_io_here_operator, is_strip_tabs_operator, line_for_match,
    strip_tabs_for_match, strip_tabs_for_storage,
};
use crate::lexer::operator::{has_operator_prefix, scan_operator};
use crate::lexer::quote::{
    OpenQuoteKind, QuoteScanner, push_quote_marker, unterminated_quote_error,
};
use crate::lexer::scanner::{
    QuoteContext, consume_line_continuation_if_unquoted, is_comment_start,
};
use crate::lexer::substitution::{
    fatal_error_from_scan_error, marker_from_scan, need_more_reason_from_scan_error,
    try_scan_backquote, try_scan_dollar,
};
use std::collections::VecDeque;
use std::mem;

pub use diagnostics::{DiagnosticCode, FatalLexError, LexDiagnostic, RecoverableLexError};
pub use heredoc::HereDocBodyCapture;
pub use span::{ByteOffset, SourceId, Span};
pub use token::{
    BoundaryResult, CompleteCommandTokens, DelimiterContext, LexStep, NeedMoreInput,
    NeedMoreReason, OperatorKind, ParserClassificationOptions, ParserTokenClass, QuoteMarker,
    QuoteProvenance, SubstitutionKind, SubstitutionMarker, Token, TokenKind, TokenOffset,
    TokenRange,
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

/// EOF policy for here-document body scanning.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HereDocEofPolicy {
    /// Emit a fatal lexical error when delimiter is not found before EOF.
    StrictError,
    /// Record a warning and keep any partial body collected before EOF.
    Warning,
}

/// Configuration options for lexer behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LexerOptions {
    /// EOF policy for here-document body scanning.
    pub here_doc_eof_policy: HereDocEofPolicy,
}

impl Default for LexerOptions {
    fn default() -> Self {
        Self {
            here_doc_eof_policy: HereDocEofPolicy::StrictError,
        }
    }
}

/// A POSIX-first lexer with placeholder scanning behavior.
///
/// Phase 5 behavior:
/// - recognizes longest-match operators
/// - preserves newline tokens
/// - removes unquoted `\\\n` before token delimiting
/// - applies token-start `#` comment skipping
/// - preserves quote bytes and records quote/substitution metadata per token
pub struct Lexer<'a> {
    input: &'a str,
    mode: LexerMode,
    source_id: SourceId,
    cursor: Cursor,
    options: LexerOptions,
    pending_heredocs: VecDeque<PendingHereDocSpec>,
    replay_tokens: VecDeque<Token>,
    collected_heredoc_bodies: Vec<HereDocBodyCapture>,
    warnings: Vec<LexDiagnostic>,
}

impl<'a> Lexer<'a> {
    /// Creates a lexer for the provided input and mode.
    pub fn new(input: &'a str, mode: LexerMode) -> Self {
        Self::with_options(input, mode, LexerOptions::default())
    }

    /// Creates a lexer with explicit options.
    pub fn with_options(input: &'a str, mode: LexerMode, options: LexerOptions) -> Self {
        Self {
            input,
            mode,
            source_id: SourceId::new(0),
            cursor: Cursor::new(),
            options,
            pending_heredocs: VecDeque::new(),
            replay_tokens: VecDeque::new(),
            collected_heredoc_bodies: Vec::new(),
            warnings: Vec::new(),
        }
    }

    /// Returns the current lexer mode.
    pub fn mode(&self) -> LexerMode {
        self.mode
    }

    /// Returns here-document body captures collected so far.
    pub fn here_doc_bodies(&self) -> &[HereDocBodyCapture] {
        &self.collected_heredoc_bodies
    }

    /// Drains and returns here-document body captures collected so far.
    pub fn drain_here_doc_bodies(&mut self) -> Vec<HereDocBodyCapture> {
        mem::take(&mut self.collected_heredoc_bodies)
    }

    /// Returns non-fatal lexer warnings collected so far.
    pub fn warnings(&self) -> &[LexDiagnostic] {
        &self.warnings
    }

    /// Drains and returns non-fatal lexer warnings collected so far.
    pub fn drain_warnings(&mut self) -> Vec<LexDiagnostic> {
        mem::take(&mut self.warnings)
    }

    /// Scans and returns the next lexical step.
    ///
    /// Phase 5 behavior:
    /// - returns [`LexStep::EndOfInput`] once all input is consumed
    /// - emits one [`TokenKind::Newline`] per `\n`
    /// - emits [`TokenKind::Operator`] using longest-match rules
    /// - emits one [`TokenKind::Token`] for contiguous non-delimiter bytes
    /// - removes unquoted line continuations (`\\\n`) while scanning
    /// - records quote/backslash and substitution markers for word tokens
    pub fn next_token(&mut self) -> Result<LexStep, FatalLexError> {
        if let Some(token) = self.replay_tokens.pop_front() {
            return Ok(LexStep::Token(token));
        }

        let step = self.scan_next_token_raw()?;
        let LexStep::Token(token) = step else {
            return Ok(step);
        };

        let TokenKind::Operator(kind) = token.kind else {
            return Ok(LexStep::Token(token));
        };

        if !is_io_here_operator(kind) {
            return Ok(LexStep::Token(token));
        }

        self.process_here_doc_line_and_queue(token)
    }

    fn scan_next_token_raw(&mut self) -> Result<LexStep, FatalLexError> {
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
        let mut quote_scanner = QuoteScanner::default();
        let mut lexeme_bytes = Vec::new();
        let mut quote_markers = Vec::new();
        let mut substitution_markers = Vec::new();

        while !self.cursor.is_eof(self.input) {
            if quote_scanner.is_unquoted()
                && self.cursor.peek_byte(self.input) == Some(b'\\')
                && self.cursor.peek_next_byte(self.input) == Some(b'\n')
            {
                let _ = self.cursor.advance_byte(self.input);
                let _ = self.cursor.advance_byte(self.input);
                continue;
            }

            let Some(byte) = self.cursor.peek_byte(self.input) else {
                break;
            };

            if quote_scanner.is_unquoted()
                && (byte.is_ascii_whitespace() || has_operator_prefix(&self.cursor, self.input))
            {
                break;
            }

            let token_start = lexeme_bytes.len();
            let source_start = self.cursor.offset();

            if let Some(open) = quote_scanner.open_quote() {
                match open.kind {
                    OpenQuoteKind::Single => {
                        let consumed = self
                            .cursor
                            .advance_byte(self.input)
                            .expect("peeked byte must be consumable");
                        lexeme_bytes.push(consumed);
                        if consumed == b'\'' {
                            let closed = quote_scanner
                                .close(OpenQuoteKind::Single)
                                .expect("open quote kind should match");
                            push_quote_marker(
                                &mut quote_markers,
                                QuoteProvenance::SingleQuoted,
                                closed.token_start,
                                lexeme_bytes.len(),
                            );
                        }
                    }
                    OpenQuoteKind::Double => {
                        if self.try_consume_substitution(
                            &mut lexeme_bytes,
                            &mut substitution_markers,
                            1,
                        )? {
                            continue;
                        }
                        if byte == b'\\' {
                            self.consume_backslash_escape(&mut lexeme_bytes, &mut quote_markers);
                            continue;
                        }
                        let consumed = self
                            .cursor
                            .advance_byte(self.input)
                            .expect("peeked byte must be consumable");
                        lexeme_bytes.push(consumed);
                        if consumed == b'"' {
                            let closed = quote_scanner
                                .close(OpenQuoteKind::Double)
                                .expect("open quote kind should match");
                            push_quote_marker(
                                &mut quote_markers,
                                QuoteProvenance::DoubleQuoted,
                                closed.token_start,
                                lexeme_bytes.len(),
                            );
                        }
                    }
                    OpenQuoteKind::DollarSingle => {
                        if byte == b'\\' {
                            self.consume_backslash_escape(&mut lexeme_bytes, &mut quote_markers);
                            continue;
                        }
                        let consumed = self
                            .cursor
                            .advance_byte(self.input)
                            .expect("peeked byte must be consumable");
                        lexeme_bytes.push(consumed);
                        if consumed == b'\'' {
                            let closed = quote_scanner
                                .close(OpenQuoteKind::DollarSingle)
                                .expect("open quote kind should match");
                            push_quote_marker(
                                &mut quote_markers,
                                QuoteProvenance::DollarSingleQuoted,
                                closed.token_start,
                                lexeme_bytes.len(),
                            );
                        }
                    }
                }
                continue;
            }

            if self.try_consume_substitution(&mut lexeme_bytes, &mut substitution_markers, 1)? {
                continue;
            }

            match byte {
                b'\\' => self.consume_backslash_escape(&mut lexeme_bytes, &mut quote_markers),
                b'\'' => {
                    let consumed = self
                        .cursor
                        .advance_byte(self.input)
                        .expect("peeked byte must be consumable");
                    lexeme_bytes.push(consumed);
                    quote_scanner.open(OpenQuoteKind::Single, token_start, source_start);
                }
                b'"' => {
                    let consumed = self
                        .cursor
                        .advance_byte(self.input)
                        .expect("peeked byte must be consumable");
                    lexeme_bytes.push(consumed);
                    quote_scanner.open(OpenQuoteKind::Double, token_start, source_start);
                }
                b'$' if self.cursor.peek_next_byte(self.input) == Some(b'\'') => {
                    let dollar = self
                        .cursor
                        .advance_byte(self.input)
                        .expect("peeked byte must be consumable");
                    lexeme_bytes.push(dollar);
                    let quote = self
                        .cursor
                        .advance_byte(self.input)
                        .expect("second byte for dollar-single quote must exist");
                    lexeme_bytes.push(quote);
                    quote_scanner.open(OpenQuoteKind::DollarSingle, token_start, source_start);
                }
                _ => {
                    let consumed = self
                        .cursor
                        .advance_byte(self.input)
                        .expect("peeked byte must be consumable");
                    lexeme_bytes.push(consumed);
                }
            }
        }

        if let Some(open) = quote_scanner.open_quote() {
            return Err(unterminated_quote_error(
                self.source_id,
                open,
                self.input,
                self.cursor.offset(),
            ));
        }

        let end = self.cursor.offset();
        let lexeme = String::from_utf8(lexeme_bytes).expect("input is valid UTF-8");
        Ok(LexStep::Token(Token::with_metadata(
            TokenKind::Token,
            lexeme,
            Span::new(self.source_id, start, end),
            quote_markers,
            substitution_markers,
        )))
    }

    fn process_here_doc_line_and_queue(
        &mut self,
        first_token: Token,
    ) -> Result<LexStep, FatalLexError> {
        self.mode = LexerMode::HereDocPending;

        let mut line_tokens = vec![first_token];
        let mut expecting_delimiter = line_tokens
            .last()
            .and_then(Self::expected_heredoc_delimiter);
        let mut saw_newline = false;

        loop {
            match self.scan_next_token_raw()? {
                LexStep::Token(token) => {
                    if let Some((strip_tabs, operator_span)) = expecting_delimiter {
                        if token.kind == TokenKind::Token {
                            self.pending_heredocs.push_back(derive_delimiter_spec(
                                operator_span,
                                &token,
                                strip_tabs,
                            ));
                            expecting_delimiter = None;
                        }
                    }

                    if let TokenKind::Operator(kind) = token.kind {
                        if is_io_here_operator(kind) {
                            expecting_delimiter =
                                Some((is_strip_tabs_operator(kind), token.span));
                        }
                    }

                    saw_newline = token.kind == TokenKind::Newline;
                    line_tokens.push(token);
                    if saw_newline {
                        break;
                    }
                }
                LexStep::EndOfInput => break,
                LexStep::Recoverable(error) => return Ok(LexStep::Recoverable(error)),
            }
        }

        if saw_newline && !self.pending_heredocs.is_empty() {
            self.mode = LexerMode::HereDocBody;
            self.consume_pending_heredoc_bodies()?;
            self.mode = LexerMode::Normal;
        } else if saw_newline {
            self.mode = LexerMode::Normal;
        }

        self.replay_tokens.extend(line_tokens);
        let replay = self
            .replay_tokens
            .pop_front()
            .expect("line replay queue must contain at least one token");
        Ok(LexStep::Token(replay))
    }

    fn expected_heredoc_delimiter(token: &Token) -> Option<(bool, Span)> {
        let TokenKind::Operator(kind) = token.kind else {
            return None;
        };
        if is_io_here_operator(kind) {
            return Some((is_strip_tabs_operator(kind), token.span));
        }
        None
    }

    fn consume_pending_heredoc_bodies(&mut self) -> Result<(), FatalLexError> {
        while let Some(spec) = self.pending_heredocs.pop_front() {
            let body_start = self.cursor.offset();
            let mut raw_body = String::new();

            let (body_end, found_delimiter) = loop {
                let line_start = self.cursor.offset();
                let (line, had_newline) = self.read_raw_line();

                if line.is_empty() && !had_newline {
                    break (self.cursor.offset(), false);
                }

                let candidate = line_for_match(&line);
                let candidate = if spec.strip_tabs {
                    strip_tabs_for_match(candidate)
                } else {
                    candidate
                };

                if candidate == spec.delimiter_key {
                    break (line_start, true);
                }

                if spec.strip_tabs {
                    raw_body.push_str(strip_tabs_for_storage(&line));
                } else {
                    raw_body.push_str(&line);
                }

                if !had_newline {
                    break (self.cursor.offset(), false);
                }
            };

            if found_delimiter {
                self.collected_heredoc_bodies.push(HereDocBodyCapture {
                    origin_operator_span: spec.operator_span,
                    delimiter_span: spec.delimiter_span,
                    body_span: Span::new(self.source_id, body_start, body_end),
                    raw_delimiter: spec.raw_delimiter,
                    delimiter_key: spec.delimiter_key,
                    strip_tabs: spec.strip_tabs,
                    quoted: spec.quoted,
                    raw_body,
                });
                continue;
            }

            match self.options.here_doc_eof_policy {
                HereDocEofPolicy::StrictError => {
                    return Err(delimiter_not_found_error(self.source_id, &spec, body_end));
                }
                HereDocEofPolicy::Warning => {
                    self.warnings.push(delimiter_not_found_diagnostic(
                        self.source_id,
                        &spec,
                        body_end,
                    ));
                    self.collected_heredoc_bodies.push(HereDocBodyCapture {
                        origin_operator_span: spec.operator_span,
                        delimiter_span: spec.delimiter_span,
                        body_span: Span::new(self.source_id, body_start, body_end),
                        raw_delimiter: spec.raw_delimiter,
                        delimiter_key: spec.delimiter_key,
                        strip_tabs: spec.strip_tabs,
                        quoted: spec.quoted,
                        raw_body,
                    });
                    self.capture_remaining_unterminated_heredocs(body_end);
                    return Ok(());
                }
            }
        }

        Ok(())
    }

    fn capture_remaining_unterminated_heredocs(&mut self, eof: ByteOffset) {
        while let Some(spec) = self.pending_heredocs.pop_front() {
            self.warnings.push(delimiter_not_found_diagnostic(
                self.source_id,
                &spec,
                eof,
            ));
            self.collected_heredoc_bodies.push(HereDocBodyCapture {
                origin_operator_span: spec.operator_span,
                delimiter_span: spec.delimiter_span,
                body_span: Span::new(self.source_id, eof, eof),
                raw_delimiter: spec.raw_delimiter,
                delimiter_key: spec.delimiter_key,
                strip_tabs: spec.strip_tabs,
                quoted: spec.quoted,
                raw_body: String::new(),
            });
        }
    }

    fn read_raw_line(&mut self) -> (String, bool) {
        let mut bytes = Vec::new();
        let mut had_newline = false;

        while let Some(byte) = self.cursor.peek_byte(self.input) {
            let consumed = self
                .cursor
                .advance_byte(self.input)
                .expect("peeked byte must be consumable");
            bytes.push(consumed);
            if byte == b'\n' {
                had_newline = true;
                break;
            }
        }

        let line = String::from_utf8(bytes).expect("input is valid UTF-8");
        (line, had_newline)
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
        if let Some(reason) = self.detect_incomplete_input_reason()? {
            return Ok(BoundaryResult::NeedMoreInput(NeedMoreInput::new(
                self.cursor.offset(),
                reason,
            )));
        }

        let mut tokens = Vec::new();
        loop {
            let step = match self.next_token() {
                Ok(step) => step,
                Err(FatalLexError::HereDocDelimiterNotFound(_)) => {
                    return Ok(BoundaryResult::NeedMoreInput(NeedMoreInput::new(
                        self.cursor.offset(),
                        NeedMoreReason::HereDocDelimiterNotFound,
                    )));
                }
                Err(error) => return Err(error),
            };

            match step {
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

    fn detect_incomplete_input_reason(&self) -> Result<Option<NeedMoreReason>, FatalLexError> {
        let mut probe = self.cursor;
        let mut quote_context = QuoteContext::default();
        let mut previous_byte: Option<u8> = None;
        let mut saw_unquoted_trailing_backslash = false;
        let mut probe_lexeme_bytes = Vec::new();
        let mut probe_markers = Vec::new();

        while !probe.is_eof(self.input) {
            if consume_line_continuation_if_unquoted(&mut probe, self.input, &mut quote_context) {
                saw_unquoted_trailing_backslash = false;
                previous_byte = None;
                continue;
            }

            if quote_context.can_start_substitution() {
                let scan = match try_scan_dollar(
                    &mut probe,
                    self.input,
                    &mut probe_lexeme_bytes,
                    &mut probe_markers,
                    1,
                ) {
                    Ok(scan) => scan,
                    Err(error) => {
                        if let Some(reason) = need_more_reason_from_scan_error(error) {
                            return Ok(Some(reason));
                        }
                        return Err(fatal_error_from_scan_error(
                            self.source_id,
                            self.input,
                            error,
                        ));
                    }
                };
                if let Some(scan) = scan {
                    probe_markers.push(marker_from_scan(scan));
                    saw_unquoted_trailing_backslash = false;
                    previous_byte = None;
                    continue;
                }

                let scan = match try_scan_backquote(
                    &mut probe,
                    self.input,
                    &mut probe_lexeme_bytes,
                    &mut probe_markers,
                    1,
                ) {
                    Ok(scan) => scan,
                    Err(error) => {
                        if let Some(reason) = need_more_reason_from_scan_error(error) {
                            return Ok(Some(reason));
                        }
                        return Err(fatal_error_from_scan_error(
                            self.source_id,
                            self.input,
                            error,
                        ));
                    }
                };
                if let Some(scan) = scan {
                    probe_markers.push(marker_from_scan(scan));
                    saw_unquoted_trailing_backslash = false;
                    previous_byte = None;
                    continue;
                }
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
            return Ok(Some(NeedMoreReason::TrailingBackslash));
        }

        if let Some(reason) = quote_context.incomplete_reason() {
            return Ok(Some(reason));
        }

        Ok(None)
    }

    fn try_consume_substitution(
        &mut self,
        lexeme_bytes: &mut Vec<u8>,
        substitution_markers: &mut Vec<SubstitutionMarker>,
        depth: u16,
    ) -> Result<bool, FatalLexError> {
        let Some(byte) = self.cursor.peek_byte(self.input) else {
            return Ok(false);
        };

        let scan = if byte == b'$' {
            try_scan_dollar(
                &mut self.cursor,
                self.input,
                lexeme_bytes,
                substitution_markers,
                depth,
            )
            .map_err(|error| fatal_error_from_scan_error(self.source_id, self.input, error))?
        } else if byte == b'`' {
            try_scan_backquote(
                &mut self.cursor,
                self.input,
                lexeme_bytes,
                substitution_markers,
                depth,
            )
            .map_err(|error| fatal_error_from_scan_error(self.source_id, self.input, error))?
        } else {
            None
        };

        if let Some(scan) = scan {
            substitution_markers.push(marker_from_scan(scan));
            return Ok(true);
        }

        Ok(false)
    }

    fn consume_backslash_escape(
        &mut self,
        lexeme_bytes: &mut Vec<u8>,
        quote_markers: &mut Vec<QuoteMarker>,
    ) {
        let marker_start = lexeme_bytes.len();
        let backslash = self
            .cursor
            .advance_byte(self.input)
            .expect("peeked backslash must be consumable");
        lexeme_bytes.push(backslash);
        if self.cursor.is_eof(self.input) {
            return;
        }
        let escaped = self
            .cursor
            .advance_byte(self.input)
            .expect("escaped byte must be consumable");
        lexeme_bytes.push(escaped);
        push_quote_marker(
            quote_markers,
            QuoteProvenance::BackslashEscaped,
            marker_start,
            lexeme_bytes.len(),
        );
    }
}
