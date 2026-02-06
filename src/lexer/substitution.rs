//! Recursive substitution scanning helpers.

use crate::lexer::cursor::Cursor;
use crate::lexer::diagnostics::{DiagnosticCode, FatalLexError, LexDiagnostic};
use crate::lexer::span::{ByteOffset, SourceId, Span};
use crate::lexer::token::{
    NeedMoreReason, SubstitutionKind, SubstitutionMarker, TokenOffset, TokenRange,
};

/// Maximum nested substitution depth accepted by the lexer.
pub(crate) const MAX_SUBSTITUTION_DEPTH: u16 = 64;

/// Scan result for one substitution segment in a token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SubstitutionScan {
    pub(crate) kind: SubstitutionKind,
    pub(crate) token_start: usize,
    pub(crate) token_end: usize,
}

/// Error categories produced by substitution scanning.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SubstitutionScanErrorKind {
    UnterminatedParameterExpansion,
    UnterminatedCommandSubstitution,
    UnterminatedBackquotedCommandSubstitution,
    UnterminatedArithmeticExpansion,
    RecursionDepthExceeded,
}

/// Structured substitution scan failure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SubstitutionScanError {
    pub(crate) kind: SubstitutionScanErrorKind,
    pub(crate) source_start: ByteOffset,
    pub(crate) source_end: ByteOffset,
    pub(crate) depth: u16,
}

/// Attempts to scan substitution forms that begin with `$`.
pub(crate) fn try_scan_dollar(
    cursor: &mut Cursor,
    input: &str,
    token_bytes: &mut Vec<u8>,
    nested_markers: &mut Vec<SubstitutionMarker>,
    depth: u16,
) -> Result<Option<SubstitutionScan>, SubstitutionScanError> {
    if cursor.peek_byte(input) != Some(b'$') {
        return Ok(None);
    }

    match (peek_at(cursor, input, 1), peek_at(cursor, input, 2)) {
        (Some(b'{'), _) => {
            scan_parameter_braced(cursor, input, token_bytes, nested_markers, depth).map(Some)
        }
        (Some(b'('), Some(b'(')) => {
            scan_arithmetic_substitution(cursor, input, token_bytes, nested_markers, depth)
                .map(Some)
        }
        (Some(b'('), _) => {
            scan_command_substitution(cursor, input, token_bytes, nested_markers, depth).map(Some)
        }
        _ => Ok(None),
    }
}

/// Attempts to scan substitution forms that begin with backquote.
pub(crate) fn try_scan_backquote(
    cursor: &mut Cursor,
    input: &str,
    token_bytes: &mut Vec<u8>,
    nested_markers: &mut Vec<SubstitutionMarker>,
    depth: u16,
) -> Result<Option<SubstitutionScan>, SubstitutionScanError> {
    if cursor.peek_byte(input) != Some(b'`') {
        return Ok(None);
    }

    scan_backquoted_substitution(cursor, input, token_bytes, nested_markers, depth).map(Some)
}

/// Converts scan result to a token marker.
pub(crate) fn marker_from_scan(scan: SubstitutionScan) -> SubstitutionMarker {
    SubstitutionMarker::new(
        scan.kind,
        TokenRange::new(
            TokenOffset::from_usize(scan.token_start),
            TokenOffset::from_usize(scan.token_end),
        ),
    )
}

/// Maps substitution scan errors to incomplete-input reasons when recoverable.
pub(crate) fn need_more_reason_from_scan_error(
    error: SubstitutionScanError,
) -> Option<NeedMoreReason> {
    match error.kind {
        SubstitutionScanErrorKind::UnterminatedParameterExpansion => {
            Some(NeedMoreReason::UnterminatedParameterExpansion)
        }
        SubstitutionScanErrorKind::UnterminatedCommandSubstitution => {
            Some(NeedMoreReason::UnterminatedCommandSubstitution)
        }
        SubstitutionScanErrorKind::UnterminatedBackquotedCommandSubstitution => {
            Some(NeedMoreReason::UnterminatedBackquotedCommandSubstitution)
        }
        SubstitutionScanErrorKind::UnterminatedArithmeticExpansion => {
            Some(NeedMoreReason::UnterminatedArithmeticExpansion)
        }
        SubstitutionScanErrorKind::RecursionDepthExceeded => None,
    }
}

/// Converts substitution scan errors into fatal lexer diagnostics.
pub(crate) fn fatal_error_from_scan_error(
    source_id: SourceId,
    input: &str,
    error: SubstitutionScanError,
) -> FatalLexError {
    let span = Span::new(source_id, error.source_start, error.source_end);
    let context = extract_near_text(input, error.source_start, error.source_end);

    match error.kind {
        SubstitutionScanErrorKind::UnterminatedParameterExpansion => {
            FatalLexError::UnterminatedParameterExpansion(LexDiagnostic::new(
                DiagnosticCode::UnterminatedParameterExpansion,
                format!("unterminated parameter expansion starting near `{context}`"),
                span,
            ))
        }
        SubstitutionScanErrorKind::UnterminatedCommandSubstitution => {
            FatalLexError::UnterminatedCommandSubstitution(LexDiagnostic::new(
                DiagnosticCode::UnterminatedCommandSubstitution,
                format!("unterminated command substitution starting near `{context}`"),
                span,
            ))
        }
        SubstitutionScanErrorKind::UnterminatedBackquotedCommandSubstitution => {
            FatalLexError::UnterminatedBackquotedCommandSubstitution(LexDiagnostic::new(
                DiagnosticCode::UnterminatedBackquotedCommandSubstitution,
                format!("unterminated backquoted substitution starting near `{context}`"),
                span,
            ))
        }
        SubstitutionScanErrorKind::UnterminatedArithmeticExpansion => {
            FatalLexError::UnterminatedArithmeticExpansion(LexDiagnostic::new(
                DiagnosticCode::UnterminatedArithmeticExpansion,
                format!("unterminated arithmetic expansion starting near `{context}`"),
                span,
            ))
        }
        SubstitutionScanErrorKind::RecursionDepthExceeded => {
            FatalLexError::SubstitutionRecursionDepthExceeded(LexDiagnostic::new(
                DiagnosticCode::SubstitutionRecursionDepthExceeded,
                format!(
                    "substitution recursion depth {} exceeded limit {} near `{context}`",
                    error.depth, MAX_SUBSTITUTION_DEPTH
                ),
                span,
            ))
        }
    }
}

fn scan_parameter_braced(
    cursor: &mut Cursor,
    input: &str,
    token_bytes: &mut Vec<u8>,
    nested_markers: &mut Vec<SubstitutionMarker>,
    depth: u16,
) -> Result<SubstitutionScan, SubstitutionScanError> {
    let token_start = token_bytes.len();
    let source_start = cursor.offset();
    ensure_depth(depth, source_start)?;

    consume_byte(cursor, input, token_bytes); // $
    consume_byte(cursor, input, token_bytes); // {

    let mut braces = 1usize;
    let mut state = SubstitutionQuoteState::default();

    while !cursor.is_eof(input) {
        if state.can_start_substitution() {
            if let Some(scan) =
                try_scan_dollar(cursor, input, token_bytes, nested_markers, depth + 1)?
            {
                nested_markers.push(marker_from_scan(scan));
                continue;
            }
            if let Some(scan) =
                try_scan_backquote(cursor, input, token_bytes, nested_markers, depth + 1)?
            {
                nested_markers.push(marker_from_scan(scan));
                continue;
            }
        }

        let byte = cursor
            .peek_byte(input)
            .expect("loop guarantees non-eof byte availability");

        if state.is_unquoted() {
            if byte == b'{' {
                let next = cursor.peek_next_byte(input);
                let consumed = consume_byte(cursor, input, token_bytes);
                state.observe(consumed, next);
                braces += 1;
                continue;
            }
            if byte == b'}' {
                let next = cursor.peek_next_byte(input);
                let consumed = consume_byte(cursor, input, token_bytes);
                state.observe(consumed, next);
                braces -= 1;
                if braces == 0 {
                    return Ok(SubstitutionScan {
                        kind: SubstitutionKind::ParameterExpansion,
                        token_start,
                        token_end: token_bytes.len(),
                    });
                }
                continue;
            }
        }

        let next = cursor.peek_next_byte(input);
        let consumed = consume_byte(cursor, input, token_bytes);
        state.observe(consumed, next);
    }

    Err(SubstitutionScanError {
        kind: SubstitutionScanErrorKind::UnterminatedParameterExpansion,
        source_start,
        source_end: cursor.offset(),
        depth,
    })
}

fn scan_command_substitution(
    cursor: &mut Cursor,
    input: &str,
    token_bytes: &mut Vec<u8>,
    nested_markers: &mut Vec<SubstitutionMarker>,
    depth: u16,
) -> Result<SubstitutionScan, SubstitutionScanError> {
    let token_start = token_bytes.len();
    let source_start = cursor.offset();
    ensure_depth(depth, source_start)?;

    consume_byte(cursor, input, token_bytes); // $
    consume_byte(cursor, input, token_bytes); // (

    let mut parens = 1usize;
    let mut state = SubstitutionQuoteState::default();

    while !cursor.is_eof(input) {
        if state.can_start_substitution() {
            if let Some(scan) =
                try_scan_dollar(cursor, input, token_bytes, nested_markers, depth + 1)?
            {
                nested_markers.push(marker_from_scan(scan));
                continue;
            }
            if let Some(scan) =
                try_scan_backquote(cursor, input, token_bytes, nested_markers, depth + 1)?
            {
                nested_markers.push(marker_from_scan(scan));
                continue;
            }
        }

        let byte = cursor
            .peek_byte(input)
            .expect("loop guarantees non-eof byte availability");

        if state.is_unquoted() {
            if byte == b'(' {
                let next = cursor.peek_next_byte(input);
                let consumed = consume_byte(cursor, input, token_bytes);
                state.observe(consumed, next);
                parens += 1;
                continue;
            }
            if byte == b')' {
                let next = cursor.peek_next_byte(input);
                let consumed = consume_byte(cursor, input, token_bytes);
                state.observe(consumed, next);
                parens -= 1;
                if parens == 0 {
                    return Ok(SubstitutionScan {
                        kind: SubstitutionKind::CommandSubstitution,
                        token_start,
                        token_end: token_bytes.len(),
                    });
                }
                continue;
            }
        }

        let next = cursor.peek_next_byte(input);
        let consumed = consume_byte(cursor, input, token_bytes);
        state.observe(consumed, next);
    }

    Err(SubstitutionScanError {
        kind: SubstitutionScanErrorKind::UnterminatedCommandSubstitution,
        source_start,
        source_end: cursor.offset(),
        depth,
    })
}

fn scan_arithmetic_substitution(
    cursor: &mut Cursor,
    input: &str,
    token_bytes: &mut Vec<u8>,
    nested_markers: &mut Vec<SubstitutionMarker>,
    depth: u16,
) -> Result<SubstitutionScan, SubstitutionScanError> {
    let token_start = token_bytes.len();
    let source_start = cursor.offset();
    ensure_depth(depth, source_start)?;

    consume_byte(cursor, input, token_bytes); // $
    consume_byte(cursor, input, token_bytes); // (
    consume_byte(cursor, input, token_bytes); // (

    let mut parens = 1usize;
    let mut state = SubstitutionQuoteState::default();

    while !cursor.is_eof(input) {
        if state.can_start_substitution() {
            if let Some(scan) =
                try_scan_dollar(cursor, input, token_bytes, nested_markers, depth + 1)?
            {
                nested_markers.push(marker_from_scan(scan));
                continue;
            }
            if let Some(scan) =
                try_scan_backquote(cursor, input, token_bytes, nested_markers, depth + 1)?
            {
                nested_markers.push(marker_from_scan(scan));
                continue;
            }
        }

        let byte = cursor
            .peek_byte(input)
            .expect("loop guarantees non-eof byte availability");

        if state.is_unquoted() {
            if byte == b'(' {
                let next = cursor.peek_next_byte(input);
                let consumed = consume_byte(cursor, input, token_bytes);
                state.observe(consumed, next);
                parens += 1;
                continue;
            }
            if byte == b')' {
                if parens == 1 && cursor.peek_next_byte(input) == Some(b')') {
                    let next = cursor.peek_next_byte(input);
                    let consumed = consume_byte(cursor, input, token_bytes);
                    state.observe(consumed, next);
                    let next = cursor.peek_next_byte(input);
                    let consumed = consume_byte(cursor, input, token_bytes);
                    state.observe(consumed, next);
                    return Ok(SubstitutionScan {
                        kind: SubstitutionKind::ArithmeticExpansion,
                        token_start,
                        token_end: token_bytes.len(),
                    });
                }
                if parens > 1 {
                    let next = cursor.peek_next_byte(input);
                    let consumed = consume_byte(cursor, input, token_bytes);
                    state.observe(consumed, next);
                    parens -= 1;
                    continue;
                }
            }
        }

        let next = cursor.peek_next_byte(input);
        let consumed = consume_byte(cursor, input, token_bytes);
        state.observe(consumed, next);
    }

    Err(SubstitutionScanError {
        kind: SubstitutionScanErrorKind::UnterminatedArithmeticExpansion,
        source_start,
        source_end: cursor.offset(),
        depth,
    })
}

fn scan_backquoted_substitution(
    cursor: &mut Cursor,
    input: &str,
    token_bytes: &mut Vec<u8>,
    nested_markers: &mut Vec<SubstitutionMarker>,
    depth: u16,
) -> Result<SubstitutionScan, SubstitutionScanError> {
    let token_start = token_bytes.len();
    let source_start = cursor.offset();
    ensure_depth(depth, source_start)?;

    consume_byte(cursor, input, token_bytes); // `
    let mut state = SubstitutionQuoteState::default();

    while !cursor.is_eof(input) {
        if state.can_start_substitution() {
            if let Some(scan) =
                try_scan_dollar(cursor, input, token_bytes, nested_markers, depth + 1)?
            {
                nested_markers.push(marker_from_scan(scan));
                continue;
            }
        }

        let byte = cursor
            .peek_byte(input)
            .expect("loop guarantees non-eof byte availability");
        if state.is_unquoted() && byte == b'`' {
            let next = cursor.peek_next_byte(input);
            let consumed = consume_byte(cursor, input, token_bytes);
            state.observe(consumed, next);
            return Ok(SubstitutionScan {
                kind: SubstitutionKind::BackquotedCommandSubstitution,
                token_start,
                token_end: token_bytes.len(),
            });
        }

        let next = cursor.peek_next_byte(input);
        let consumed = consume_byte(cursor, input, token_bytes);
        state.observe(consumed, next);
    }

    Err(SubstitutionScanError {
        kind: SubstitutionScanErrorKind::UnterminatedBackquotedCommandSubstitution,
        source_start,
        source_end: cursor.offset(),
        depth,
    })
}

fn ensure_depth(depth: u16, source_start: ByteOffset) -> Result<(), SubstitutionScanError> {
    if depth <= MAX_SUBSTITUTION_DEPTH {
        return Ok(());
    }

    Err(SubstitutionScanError {
        kind: SubstitutionScanErrorKind::RecursionDepthExceeded,
        source_start,
        source_end: source_start,
        depth,
    })
}

fn consume_byte(cursor: &mut Cursor, input: &str, token_bytes: &mut Vec<u8>) -> u8 {
    let byte = cursor
        .advance_byte(input)
        .expect("peeked byte must be consumable");
    token_bytes.push(byte);
    byte
}

fn peek_at(cursor: &Cursor, input: &str, distance: usize) -> Option<u8> {
    input
        .as_bytes()
        .get(cursor.offset().as_usize().saturating_add(distance))
        .copied()
}

fn extract_near_text(input: &str, start: ByteOffset, end: ByteOffset) -> String {
    let start_index = start.as_usize().min(input.len());
    let end_index = end.as_usize().min(input.len());
    let slice = if start_index <= end_index {
        &input[start_index..end_index]
    } else {
        ""
    };
    let preview: String = slice.chars().take(16).collect();
    preview.replace('\n', "\\n")
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct SubstitutionQuoteState {
    in_single_quote: bool,
    in_double_quote: bool,
    in_dollar_single_quote: bool,
    escaped: bool,
    pending_dollar_single_quote: bool,
}

impl SubstitutionQuoteState {
    fn is_unquoted(self) -> bool {
        !self.in_single_quote
            && !self.in_double_quote
            && !self.in_dollar_single_quote
            && !self.escaped
            && !self.pending_dollar_single_quote
    }

    fn can_start_substitution(self) -> bool {
        !self.in_single_quote
            && !self.in_dollar_single_quote
            && !self.escaped
            && !self.pending_dollar_single_quote
    }

    fn observe(&mut self, byte: u8, next_byte: Option<u8>) {
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
