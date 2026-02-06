//! Word-program lowering: translates lexer token metadata into WordProgramOp sequences.
//!
//! Phase 7 replaces the stub word programs from Phase 6 with real expansion
//! lowering that reads quote markers, substitution markers, and scans for
//! bare `$VAR` references in the lexeme text.

use crate::ir::bytecode::WordProgramOp;
use crate::ir::error::IrError;
use crate::ir::ids::WordProgramId;
use crate::ir::lower::arith_program;
use crate::ir::lower::emit::EmitContext;
use crate::ir::program::WordProgram;
use crate::lexer::{QuoteMarker, QuoteProvenance, SubstitutionKind, SubstitutionMarker};

use crate::ir::hir::{HirAssignment, HirWord};

// ---------------------------------------------------------------------------
// WordContext
// ---------------------------------------------------------------------------

/// Context in which a word is being expanded, determining trailing operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WordContext {
    /// Command argument: FieldSplit + Glob + QuoteRemoval.
    CommandArgument,
    /// Assignment value: QuoteRemoval only.
    AssignmentValue,
    /// For-loop iteration word: FieldSplit + Glob + QuoteRemoval.
    ForWord,
    /// Case subject: QuoteRemoval only.
    CaseSubject,
    /// Case pattern: QuoteRemoval only.
    CasePattern,
}

// ---------------------------------------------------------------------------
// Internal event types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WordEventKind {
    QuotedRegion(QuoteProvenance),
    ParameterExpansion,
    CommandSubstitution,
    BackquotedCommandSubstitution,
    ArithmeticExpansion,
    BareParameter,
}

#[derive(Debug, Clone)]
struct WordEvent {
    start: usize,
    end: usize,
    kind: WordEventKind,
}

// ---------------------------------------------------------------------------
// Public entry points
// ---------------------------------------------------------------------------

/// Lowers an `HirWord` into a `WordProgram` and registers it in the module.
pub(crate) fn lower_word_to_program(
    ctx: &mut EmitContext<'_>,
    word: &HirWord,
    word_ctx: WordContext,
) -> Result<WordProgramId, IrError> {
    let text = &word.token.lexeme;
    let quote_markers = &word.token.quote_markers;
    let sub_markers = &word.token.substitution_markers;

    let ops = lower_word_segments(ctx, text, quote_markers, sub_markers, word_ctx)?;

    let wp = WordProgram {
        id: WordProgramId::default(),
        ops,
    };
    ctx.module().add_word_program(wp)
}

/// Lowers an `HirAssignment` value into a `WordProgram` and registers it.
pub(crate) fn lower_assignment_value_to_program(
    ctx: &mut EmitContext<'_>,
    assignment: &HirAssignment,
) -> Result<WordProgramId, IrError> {
    let full_text = &assignment.token.lexeme;
    let value_offset = assignment.name.len() + 1; // skip "NAME="
    let value_end = full_text.len();

    if value_offset > value_end {
        // Empty assignment value like `A=`
        let str_id = ctx.module().intern_string("")?;
        let ops = vec![
            WordProgramOp::PushLiteral(str_id),
            WordProgramOp::QuoteRemoval,
        ];
        let wp = WordProgram {
            id: WordProgramId::default(),
            ops,
        };
        return ctx.module().add_word_program(wp);
    }

    let value_text = &full_text[value_offset..value_end];
    let adjusted_quotes = adjust_quote_markers(
        &assignment.token.quote_markers,
        value_offset,
        value_end,
    );
    let adjusted_subs = adjust_substitution_markers(
        &assignment.token.substitution_markers,
        value_offset,
        value_end,
    );

    let ops = lower_word_segments(
        ctx,
        value_text,
        &adjusted_quotes,
        &adjusted_subs,
        WordContext::AssignmentValue,
    )?;

    let wp = WordProgram {
        id: WordProgramId::default(),
        ops,
    };
    ctx.module().add_word_program(wp)
}

// ---------------------------------------------------------------------------
// Core algorithm
// ---------------------------------------------------------------------------

/// Builds the `WordProgramOp` sequence for a word given its text and markers.
fn lower_word_segments(
    ctx: &mut EmitContext<'_>,
    text: &str,
    quote_markers: &[QuoteMarker],
    sub_markers: &[SubstitutionMarker],
    word_ctx: WordContext,
) -> Result<Vec<WordProgramOp>, IrError> {
    let mut ops = Vec::new();

    // Build sorted event list.
    let events = build_event_list(text, quote_markers, sub_markers);

    // Check for tilde prefix at start of unquoted text.
    let tilde_end = detect_tilde_prefix(text, &events);
    let mut cursor = 0;

    if let Some(te) = tilde_end {
        let tilde_text = &text[..te];
        let str_id = ctx.module().intern_string(tilde_text)?;
        ops.push(WordProgramOp::ExpandTilde(str_id));
        cursor = te;
    }

    // Walk events left-to-right.
    for event in &events {
        if event.start < cursor {
            continue;
        }

        // Emit literal for gap before this event.
        if event.start > cursor {
            let gap = &text[cursor..event.start];
            if !gap.is_empty() {
                let str_id = ctx.module().intern_string(gap)?;
                ops.push(WordProgramOp::PushLiteral(str_id));
            }
        }

        match event.kind {
            WordEventKind::QuotedRegion(QuoteProvenance::SingleQuoted) => {
                let raw = &text[event.start..event.end];
                // Strip surrounding quotes: 'content'
                let content = strip_delimiters(raw, 1, 1);
                let str_id = ctx.module().intern_string(content)?;
                ops.push(WordProgramOp::PushLiteral(str_id));
            }
            WordEventKind::QuotedRegion(QuoteProvenance::DollarSingleQuoted) => {
                let raw = &text[event.start..event.end];
                // Strip $' and '
                let content = strip_delimiters(raw, 2, 1);
                let str_id = ctx.module().intern_string(content)?;
                ops.push(WordProgramOp::PushLiteral(str_id));
            }
            WordEventKind::QuotedRegion(QuoteProvenance::DoubleQuoted) => {
                let raw = &text[event.start..event.end];
                // Strip surrounding "
                let inner = strip_delimiters(raw, 1, 1);
                // Recursively process inner content for substitutions.
                let inner_offset = event.start + 1;
                let inner_end = event.end.saturating_sub(1);
                let inner_quotes = filter_markers_in_range(quote_markers, inner_offset, inner_end);
                let inner_subs =
                    filter_sub_markers_in_range(sub_markers, inner_offset, inner_end);
                let inner_events = build_event_list(inner, &inner_quotes, &inner_subs);
                emit_inner_segments(ctx, inner, &inner_events, &inner_quotes, &inner_subs, &mut ops)?;
            }
            WordEventKind::QuotedRegion(QuoteProvenance::BackslashEscaped) => {
                let raw = &text[event.start..event.end];
                // Backslash-escaped: emit the escaped character (skip the backslash).
                if raw.len() > 1 {
                    let escaped = &raw[1..];
                    let str_id = ctx.module().intern_string(escaped)?;
                    ops.push(WordProgramOp::PushLiteral(str_id));
                }
            }
            WordEventKind::ParameterExpansion => {
                let raw = &text[event.start..event.end];
                let name = extract_braced_param_name(raw);
                let sym = ctx.module().intern_symbol(name)?;
                ops.push(WordProgramOp::ExpandParameter(sym));
            }
            WordEventKind::BareParameter => {
                let raw = &text[event.start..event.end];
                let name = extract_bare_param_name(raw);
                let sym = ctx.module().intern_symbol(name)?;
                ops.push(WordProgramOp::ExpandParameter(sym));
            }
            WordEventKind::CommandSubstitution => {
                let co_id = create_stub_code_object(ctx)?;
                ops.push(WordProgramOp::ExpandCommandSubstitution(co_id));
            }
            WordEventKind::BackquotedCommandSubstitution => {
                let co_id = create_stub_code_object(ctx)?;
                ops.push(WordProgramOp::ExpandCommandSubstitution(co_id));
            }
            WordEventKind::ArithmeticExpansion => {
                let raw = &text[event.start..event.end];
                let ap_id = arith_program::lower_arith_expression(ctx, raw)?;
                ops.push(WordProgramOp::ExpandArithmetic(ap_id));
            }
        }

        cursor = event.end;
    }

    // Emit trailing literal after last event.
    if cursor < text.len() {
        let tail = &text[cursor..];
        if !tail.is_empty() {
            let str_id = ctx.module().intern_string(tail)?;
            ops.push(WordProgramOp::PushLiteral(str_id));
        }
    }

    // If no ops were emitted yet, push an empty literal.
    if ops.is_empty() {
        let str_id = ctx.module().intern_string("")?;
        ops.push(WordProgramOp::PushLiteral(str_id));
    }

    // Append trailing ops based on word context.
    append_trailing_ops(word_ctx, &mut ops);

    Ok(ops)
}

/// Emits inner segments for double-quoted content (handles substitutions inside quotes).
fn emit_inner_segments(
    ctx: &mut EmitContext<'_>,
    text: &str,
    events: &[WordEvent],
    _quote_markers: &[QuoteMarker],
    _sub_markers: &[SubstitutionMarker],
    ops: &mut Vec<WordProgramOp>,
) -> Result<(), IrError> {
    let mut cursor = 0;

    for event in events {
        if event.start < cursor {
            continue;
        }

        // Literal gap.
        if event.start > cursor {
            let gap = &text[cursor..event.start];
            if !gap.is_empty() {
                let str_id = ctx.module().intern_string(gap)?;
                ops.push(WordProgramOp::PushLiteral(str_id));
            }
        }

        match event.kind {
            WordEventKind::ParameterExpansion => {
                let raw = &text[event.start..event.end];
                let name = extract_braced_param_name(raw);
                let sym = ctx.module().intern_symbol(name)?;
                ops.push(WordProgramOp::ExpandParameter(sym));
            }
            WordEventKind::BareParameter => {
                let raw = &text[event.start..event.end];
                let name = extract_bare_param_name(raw);
                let sym = ctx.module().intern_symbol(name)?;
                ops.push(WordProgramOp::ExpandParameter(sym));
            }
            WordEventKind::CommandSubstitution
            | WordEventKind::BackquotedCommandSubstitution => {
                let co_id = create_stub_code_object(ctx)?;
                ops.push(WordProgramOp::ExpandCommandSubstitution(co_id));
            }
            WordEventKind::ArithmeticExpansion => {
                let raw = &text[event.start..event.end];
                let ap_id = arith_program::lower_arith_expression(ctx, raw)?;
                ops.push(WordProgramOp::ExpandArithmetic(ap_id));
            }
            WordEventKind::QuotedRegion(QuoteProvenance::BackslashEscaped) => {
                let raw = &text[event.start..event.end];
                if raw.len() > 1 {
                    let escaped = &raw[1..];
                    let str_id = ctx.module().intern_string(escaped)?;
                    ops.push(WordProgramOp::PushLiteral(str_id));
                }
            }
            _ => {
                // Other quoted regions inside double quotes are treated as literals.
                let raw = &text[event.start..event.end];
                if !raw.is_empty() {
                    let str_id = ctx.module().intern_string(raw)?;
                    ops.push(WordProgramOp::PushLiteral(str_id));
                }
            }
        }

        cursor = event.end;
    }

    // Trailing literal.
    if cursor < text.len() {
        let tail = &text[cursor..];
        if !tail.is_empty() {
            let str_id = ctx.module().intern_string(tail)?;
            ops.push(WordProgramOp::PushLiteral(str_id));
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Event list construction
// ---------------------------------------------------------------------------

/// Builds a sorted event list from quote markers, substitution markers, and bare parameters.
fn build_event_list(
    text: &str,
    quote_markers: &[QuoteMarker],
    sub_markers: &[SubstitutionMarker],
) -> Vec<WordEvent> {
    let mut events = Vec::new();

    // Add top-level quote markers.
    for qm in quote_markers {
        let start = qm.range.start.value() as usize;
        let end = qm.range.end.value() as usize;
        // Skip markers that are nested inside substitution markers.
        if is_inside_substitution(start, end, sub_markers) {
            continue;
        }
        events.push(WordEvent {
            start,
            end,
            kind: WordEventKind::QuotedRegion(qm.provenance),
        });
    }

    // Add top-level substitution markers.
    for sm in sub_markers {
        let start = sm.range.start.value() as usize;
        let end = sm.range.end.value() as usize;
        // Skip markers that are nested inside quote markers that suppress expansion
        // (single quotes).
        if is_inside_single_quote(start, end, quote_markers) {
            continue;
        }
        let kind = match sm.kind {
            SubstitutionKind::ParameterExpansion => WordEventKind::ParameterExpansion,
            SubstitutionKind::CommandSubstitution => WordEventKind::CommandSubstitution,
            SubstitutionKind::BackquotedCommandSubstitution => {
                WordEventKind::BackquotedCommandSubstitution
            }
            SubstitutionKind::ArithmeticExpansion => WordEventKind::ArithmeticExpansion,
        };
        events.push(WordEvent { start, end, kind });
    }

    // Scan for bare parameters in gaps.
    let bare_params = scan_bare_parameters(text, &events);
    events.extend(bare_params);

    // Sort by start position.
    events.sort_by_key(|e| e.start);
    events
}

/// Checks if a range is fully contained inside any substitution marker.
fn is_inside_substitution(start: usize, end: usize, sub_markers: &[SubstitutionMarker]) -> bool {
    sub_markers.iter().any(|sm| {
        let sm_start = sm.range.start.value() as usize;
        let sm_end = sm.range.end.value() as usize;
        start > sm_start && end <= sm_end
    })
}

/// Checks if a range is fully contained inside a single-quoted region.
fn is_inside_single_quote(start: usize, end: usize, quote_markers: &[QuoteMarker]) -> bool {
    quote_markers.iter().any(|qm| {
        let qm_start = qm.range.start.value() as usize;
        let qm_end = qm.range.end.value() as usize;
        qm.provenance == QuoteProvenance::SingleQuoted
            && start >= qm_start
            && end <= qm_end
    })
}

// ---------------------------------------------------------------------------
// Bare parameter scanning
// ---------------------------------------------------------------------------

/// Scans text for `$X` patterns not covered by any existing event.
fn scan_bare_parameters(text: &str, events: &[WordEvent]) -> Vec<WordEvent> {
    let mut results = Vec::new();
    let bytes = text.as_bytes();
    let len = bytes.len();
    let mut i = 0;

    while i < len {
        // Skip positions covered by existing events.
        if is_covered(i, events) {
            i += 1;
            continue;
        }

        if bytes[i] == b'$' && i + 1 < len {
            let next = bytes[i + 1];
            // Special parameters: @, *, #, ?, -, $, !, 0
            if matches!(next, b'@' | b'*' | b'#' | b'?' | b'-' | b'$' | b'!' | b'0') {
                results.push(WordEvent {
                    start: i,
                    end: i + 2,
                    kind: WordEventKind::BareParameter,
                });
                i += 2;
                continue;
            }
            // Positional: $1-$9
            if next.is_ascii_digit() {
                results.push(WordEvent {
                    start: i,
                    end: i + 2,
                    kind: WordEventKind::BareParameter,
                });
                i += 2;
                continue;
            }
            // Named: $[A-Za-z_][A-Za-z0-9_]*
            if next == b'_' || next.is_ascii_alphabetic() {
                let name_start = i + 1;
                let mut j = name_start + 1;
                while j < len
                    && (bytes[j] == b'_' || bytes[j].is_ascii_alphanumeric())
                {
                    j += 1;
                }
                results.push(WordEvent {
                    start: i,
                    end: j,
                    kind: WordEventKind::BareParameter,
                });
                i = j;
                continue;
            }
        }
        i += 1;
    }

    results
}

/// Checks if a byte position is covered by any existing event.
fn is_covered(pos: usize, events: &[WordEvent]) -> bool {
    events.iter().any(|e| pos >= e.start && pos < e.end)
}

// ---------------------------------------------------------------------------
// Parameter name extraction
// ---------------------------------------------------------------------------

/// Extracts the parameter name from a braced `${...}` expansion.
fn extract_braced_param_name(raw: &str) -> &str {
    // raw is like `${FOO}`, `${FOO:-default}`, `${#FOO}`
    let inner = if raw.starts_with("${") && raw.ends_with('}') {
        &raw[2..raw.len() - 1]
    } else {
        raw
    };

    // Handle ${#FOO} length prefix.
    let inner = if inner.starts_with('#') && inner.len() > 1 {
        &inner[1..]
    } else {
        inner
    };

    // Extract base name before operator (:-,  :+, :=, :?, %, %%, etc.)
    extract_base_name(inner)
}

/// Extracts the base parameter name, stopping at shell parameter operators.
fn extract_base_name(s: &str) -> &str {
    let bytes = s.as_bytes();
    if bytes.is_empty() {
        return s;
    }

    // Special parameters: single char
    if bytes.len() == 1 && matches!(bytes[0], b'@' | b'*' | b'#' | b'?' | b'-' | b'$' | b'!' | b'0') {
        return s;
    }

    // Positional: digits
    if bytes[0].is_ascii_digit() {
        let end = bytes.iter().position(|b| !b.is_ascii_digit()).unwrap_or(bytes.len());
        return &s[..end];
    }

    // Named: [A-Za-z_][A-Za-z0-9_]*
    if bytes[0] == b'_' || bytes[0].is_ascii_alphabetic() {
        let end = bytes
            .iter()
            .position(|b| !(b == &b'_' || b.is_ascii_alphanumeric()))
            .unwrap_or(bytes.len());
        return &s[..end];
    }

    s
}

/// Extracts the parameter name from a bare `$VAR` reference.
fn extract_bare_param_name(raw: &str) -> &str {
    // raw starts with '$'
    &raw[1..]
}

// ---------------------------------------------------------------------------
// Tilde detection
// ---------------------------------------------------------------------------

/// Detects a tilde prefix at the start of unquoted text.
/// Returns the byte offset just past the tilde prefix, or None.
fn detect_tilde_prefix(text: &str, events: &[WordEvent]) -> Option<usize> {
    let bytes = text.as_bytes();
    if bytes.is_empty() || bytes[0] != b'~' {
        return None;
    }

    // Check the ~ is not inside a quoted region or substitution.
    if events.iter().any(|e| e.start == 0 && matches!(e.kind, WordEventKind::QuotedRegion(_))) {
        return None;
    }

    // Find end of tilde prefix: up to first unquoted / or end of word.
    let mut end = 1;
    while end < bytes.len() {
        if bytes[end] == b'/' {
            break;
        }
        // Stop at any special character.
        if bytes[end] == b'$' || bytes[end] == b'`' || bytes[end] == b'\'' || bytes[end] == b'"'
        {
            break;
        }
        end += 1;
    }

    Some(end)
}

// ---------------------------------------------------------------------------
// Marker adjustment for assignment values
// ---------------------------------------------------------------------------

/// Filters and shifts quote markers for assignment value portion.
fn adjust_quote_markers(
    markers: &[QuoteMarker],
    value_offset: usize,
    value_end: usize,
) -> Vec<QuoteMarker> {
    markers
        .iter()
        .filter_map(|qm| {
            let start = qm.range.start.value() as usize;
            let end = qm.range.end.value() as usize;
            if start >= value_offset && end <= value_end {
                Some(QuoteMarker::new(
                    qm.provenance,
                    crate::lexer::TokenRange::new(
                        crate::lexer::TokenOffset::from_usize(start - value_offset),
                        crate::lexer::TokenOffset::from_usize(end - value_offset),
                    ),
                ))
            } else {
                None
            }
        })
        .collect()
}

/// Filters and shifts substitution markers for assignment value portion.
fn adjust_substitution_markers(
    markers: &[SubstitutionMarker],
    value_offset: usize,
    value_end: usize,
) -> Vec<SubstitutionMarker> {
    markers
        .iter()
        .filter_map(|sm| {
            let start = sm.range.start.value() as usize;
            let end = sm.range.end.value() as usize;
            if start >= value_offset && end <= value_end {
                Some(SubstitutionMarker::new(
                    sm.kind,
                    crate::lexer::TokenRange::new(
                        crate::lexer::TokenOffset::from_usize(start - value_offset),
                        crate::lexer::TokenOffset::from_usize(end - value_offset),
                    ),
                ))
            } else {
                None
            }
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Trailing ops
// ---------------------------------------------------------------------------

/// Appends context-appropriate trailing operations.
fn append_trailing_ops(word_ctx: WordContext, ops: &mut Vec<WordProgramOp>) {
    match word_ctx {
        WordContext::CommandArgument | WordContext::ForWord => {
            ops.push(WordProgramOp::FieldSplit);
            ops.push(WordProgramOp::Glob);
            ops.push(WordProgramOp::QuoteRemoval);
        }
        WordContext::AssignmentValue | WordContext::CaseSubject | WordContext::CasePattern => {
            ops.push(WordProgramOp::QuoteRemoval);
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Strips `prefix_len` bytes from the start and `suffix_len` bytes from the end.
fn strip_delimiters(s: &str, prefix_len: usize, suffix_len: usize) -> &str {
    let total = prefix_len + suffix_len;
    if s.len() >= total {
        &s[prefix_len..s.len() - suffix_len]
    } else {
        ""
    }
}

/// Filters quote markers that fall within a given range and shifts their offsets.
fn filter_markers_in_range(
    markers: &[QuoteMarker],
    range_start: usize,
    range_end: usize,
) -> Vec<QuoteMarker> {
    markers
        .iter()
        .filter_map(|qm| {
            let start = qm.range.start.value() as usize;
            let end = qm.range.end.value() as usize;
            if start >= range_start && end <= range_end {
                Some(QuoteMarker::new(
                    qm.provenance,
                    crate::lexer::TokenRange::new(
                        crate::lexer::TokenOffset::from_usize(start - range_start),
                        crate::lexer::TokenOffset::from_usize(end - range_start),
                    ),
                ))
            } else {
                None
            }
        })
        .collect()
}

/// Filters substitution markers that fall within a given range and shifts their offsets.
fn filter_sub_markers_in_range(
    markers: &[SubstitutionMarker],
    range_start: usize,
    range_end: usize,
) -> Vec<SubstitutionMarker> {
    markers
        .iter()
        .filter_map(|sm| {
            let start = sm.range.start.value() as usize;
            let end = sm.range.end.value() as usize;
            if start >= range_start && end <= range_end {
                Some(SubstitutionMarker::new(
                    sm.kind,
                    crate::lexer::TokenRange::new(
                        crate::lexer::TokenOffset::from_usize(start - range_start),
                        crate::lexer::TokenOffset::from_usize(end - range_start),
                    ),
                ))
            } else {
                None
            }
        })
        .collect()
}

/// Creates a stub code object as a placeholder for command substitution inner body.
fn create_stub_code_object(
    ctx: &mut EmitContext<'_>,
) -> Result<crate::ir::ids::CodeObjectId, IrError> {
    use crate::ir::ids::CodeObjectId;
    use crate::ir::program::CodeObjectBuilder;

    let placeholder = CodeObjectBuilder::new(CodeObjectId::default()).finalize()?;
    ctx.module().add_code_object(placeholder)
}

