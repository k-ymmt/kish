//! Operator scanning helpers for longest-match tokenization.

use crate::lexer::cursor::Cursor;
use crate::lexer::span::ByteOffset;
use crate::lexer::token::OperatorKind;

/// Result of scanning one operator token.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct OperatorScan {
    /// Operator category.
    pub(crate) kind: OperatorKind,
    /// Exact source lexeme.
    pub(crate) lexeme: String,
    /// Source start offset.
    pub(crate) start: ByteOffset,
    /// Source end offset.
    pub(crate) end: ByteOffset,
}

/// Scans an operator at cursor position using longest-match rules.
pub(crate) fn scan_operator(cursor: &mut Cursor, input: &str) -> Option<OperatorScan> {
    let start = cursor.offset();
    let (kind, lexeme) = match_operator_at(input, start)?;
    for _ in 0..lexeme.len() {
        let _ = cursor.advance_byte(input);
    }
    let end = cursor.offset();

    Some(OperatorScan {
        kind,
        lexeme: lexeme.to_string(),
        start,
        end,
    })
}

/// Returns true when cursor points to any operator prefix.
pub(crate) fn has_operator_prefix(cursor: &Cursor, input: &str) -> bool {
    match_operator_at(input, cursor.offset()).is_some()
}

fn match_operator_at(input: &str, offset: ByteOffset) -> Option<(OperatorKind, &'static str)> {
    let bytes = input.as_bytes();
    let start = offset.as_usize();
    if start >= bytes.len() {
        return None;
    }
    let tail = &bytes[start..];

    // Longest-match precedence.
    if tail.len() >= 3 && &tail[..3] == b"<<-" {
        return Some((OperatorKind::HereDocStripTabs, "<<-"));
    }

    if tail.len() >= 2 {
        let pair = &tail[..2];
        let matched = if pair == b"&&" {
            Some((OperatorKind::AndIf, "&&"))
        } else if pair == b"||" {
            Some((OperatorKind::OrIf, "||"))
        } else if pair == b";;" {
            Some((OperatorKind::DoubleSemicolon, ";;"))
        } else if pair == b";&" {
            Some((OperatorKind::SemicolonAmpersand, ";&"))
        } else if pair == b"<<" {
            Some((OperatorKind::HereDoc, "<<"))
        } else if pair == b">>" {
            Some((OperatorKind::AppendOutput, ">>"))
        } else if pair == b"<&" {
            Some((OperatorKind::DupInput, "<&"))
        } else if pair == b">&" {
            Some((OperatorKind::DupOutput, ">&"))
        } else if pair == b"<>" {
            Some((OperatorKind::ReadWrite, "<>"))
        } else if pair == b">|" {
            Some((OperatorKind::Clobber, ">|"))
        } else {
            None
        };
        if matched.is_some() {
            return matched;
        }
    }

    match tail[0] {
        b'|' => Some((OperatorKind::Pipe, "|")),
        b';' => Some((OperatorKind::Semicolon, ";")),
        b'&' => Some((OperatorKind::Ampersand, "&")),
        b'(' => Some((OperatorKind::LeftParen, "(")),
        b')' => Some((OperatorKind::RightParen, ")")),
        b'<' => Some((OperatorKind::Less, "<")),
        b'>' => Some((OperatorKind::Greater, ">")),
        _ => None,
    }
}
