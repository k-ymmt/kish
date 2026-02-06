//! Parser-side contextual token classification.

use crate::lexer::{
    DelimiterContext, OperatorKind, ParserClassificationOptions, ParserTokenClass, Token, TokenKind,
};

/// Reserved words recognized by POSIX grammar contexts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReservedWord {
    /// `if`
    If,
    /// `then`
    Then,
    /// `else`
    Else,
    /// `elif`
    Elif,
    /// `fi`
    Fi,
    /// `do`
    Do,
    /// `done`
    Done,
    /// `case`
    Case,
    /// `esac`
    Esac,
    /// `while`
    While,
    /// `until`
    Until,
    /// `for`
    For,
    /// `in`
    In,
}

impl ReservedWord {
    /// Parses a token lexeme into a reserved word when exact-match compatible.
    pub fn from_lexeme(lexeme: &str) -> Option<Self> {
        match lexeme {
            "if" => Some(Self::If),
            "then" => Some(Self::Then),
            "else" => Some(Self::Else),
            "elif" => Some(Self::Elif),
            "fi" => Some(Self::Fi),
            "do" => Some(Self::Do),
            "done" => Some(Self::Done),
            "case" => Some(Self::Case),
            "esac" => Some(Self::Esac),
            "while" => Some(Self::While),
            "until" => Some(Self::Until),
            "for" => Some(Self::For),
            "in" => Some(Self::In),
            _ => None,
        }
    }
}

/// Reserved-word recognition policy for a grammar position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ReservedWordPolicy {
    /// Reserved words are not recognized in this position.
    #[default]
    None,
    /// Any reserved word can be recognized.
    Any,
    /// Only `esac` is recognized.
    EsacOnly,
    /// Only `in` is recognized.
    InOnly,
    /// Only `in` and `do` are recognized.
    InOrDo,
}

/// Name classification mode for context-sensitive grammar rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum NameContext {
    /// Name conversion is disabled.
    #[default]
    None,
    /// Rule 5 name conversion (`for` loop variable).
    ForName,
    /// Rule 8 name conversion (`fname` in function definition).
    FunctionName,
}

/// Parser-controlled context for token reclassification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct ClassificationContext {
    /// IO delimiter context determined by adjacent operator lookahead.
    pub delimiter_context: Option<DelimiterContext>,
    /// Reserved-word recognition policy for the grammar position.
    pub reserved_word_policy: ReservedWordPolicy,
    /// Name conversion mode.
    pub name_context: NameContext,
    /// Enables assignment-word conversion for command prefix contexts.
    pub allow_assignment_word: bool,
    /// Enables function-body rule 9 behavior (suppress assignment conversion).
    pub function_body_rule9: bool,
}

/// Classification options configured by parser settings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ClassificationOptions {
    /// Enables optional `IO_LOCATION` conversion.
    pub allow_io_location: bool,
}

/// Classified token kind produced by parser context rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassifiedTokenKind {
    /// Ordinary word.
    Word,
    /// POSIX name.
    Name,
    /// Assignment word.
    AssignmentWord,
    /// Reserved word.
    ReservedWord(ReservedWord),
    /// IO number form such as `2` before redirect.
    IoNumber(u32),
    /// IO location form such as `{fd}` before redirect.
    IoLocation,
    /// Shell operator pass-through.
    Operator(OperatorKind),
    /// Newline pass-through.
    Newline,
}

/// Classified token with original lexer token payload preserved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassifiedToken {
    /// Classified kind in parser context.
    pub kind: ClassifiedTokenKind,
    /// Original token.
    pub token: Token,
}

/// POSIX contextual token classifier.
#[derive(Debug, Default, Clone, Copy)]
pub struct Classifier;

impl Classifier {
    /// Creates a classifier.
    pub const fn new() -> Self {
        Self
    }

    /// Classifies one lexer token under explicit parser context.
    pub fn classify(
        &self,
        token: &Token,
        context: ClassificationContext,
        options: ClassificationOptions,
    ) -> ClassifiedToken {
        let kind = self.classify_kind(token, context, options);
        ClassifiedToken {
            kind,
            token: token.clone(),
        }
    }

    /// Classifies and returns only the contextual token kind.
    ///
    /// This is a parser hot path that avoids cloning token payloads when only
    /// reclassification output is needed.
    pub fn classify_kind_only(
        &self,
        token: &Token,
        context: ClassificationContext,
        options: ClassificationOptions,
    ) -> ClassifiedTokenKind {
        self.classify_kind(token, context, options)
    }

    fn classify_kind(
        &self,
        token: &Token,
        context: ClassificationContext,
        options: ClassificationOptions,
    ) -> ClassifiedTokenKind {
        match token.kind {
            TokenKind::Operator(kind) => return ClassifiedTokenKind::Operator(kind),
            TokenKind::Newline => return ClassifiedTokenKind::Newline,
            TokenKind::Token => {}
        }

        let parser_class = token.classify_for_parser_with_options(
            context.delimiter_context,
            ParserClassificationOptions {
                allow_io_location: options.allow_io_location,
            },
        );
        match parser_class {
            ParserTokenClass::IoNumber => {
                if let Some(fd) = token.io_number_candidate() {
                    return ClassifiedTokenKind::IoNumber(fd);
                }
            }
            ParserTokenClass::IoLocation => return ClassifiedTokenKind::IoLocation,
            ParserTokenClass::Token => {}
        }

        if context.function_body_rule9 {
            return ClassifiedTokenKind::Word;
        }

        if let Some(reserved) = classify_reserved_word(token, context.reserved_word_policy) {
            return ClassifiedTokenKind::ReservedWord(reserved);
        }

        match context.name_context {
            NameContext::ForName => {
                return if is_posix_name(&token.lexeme) {
                    ClassifiedTokenKind::Name
                } else {
                    ClassifiedTokenKind::Word
                };
            }
            NameContext::FunctionName => {
                if is_posix_name(&token.lexeme) {
                    return ClassifiedTokenKind::Name;
                }
            }
            NameContext::None => {}
        }

        if context.allow_assignment_word {
            return classify_assignment_word(token);
        }

        ClassifiedTokenKind::Word
    }
}

fn classify_reserved_word(token: &Token, policy: ReservedWordPolicy) -> Option<ReservedWord> {
    if !token.is_plain() {
        return None;
    }

    let reserved = ReservedWord::from_lexeme(&token.lexeme)?;
    match policy {
        ReservedWordPolicy::None => None,
        ReservedWordPolicy::Any => Some(reserved),
        ReservedWordPolicy::EsacOnly => (reserved == ReservedWord::Esac).then_some(reserved),
        ReservedWordPolicy::InOnly => (reserved == ReservedWord::In).then_some(reserved),
        ReservedWordPolicy::InOrDo => {
            (reserved == ReservedWord::In || reserved == ReservedWord::Do).then_some(reserved)
        }
    }
}

fn classify_assignment_word(token: &Token) -> ClassifiedTokenKind {
    if split_assignment_word(token).is_some() {
        ClassifiedTokenKind::AssignmentWord
    } else {
        ClassifiedTokenKind::Word
    }
}

/// Splits an assignment token into `name` and `value` when rule-7 compatible.
pub(crate) fn split_assignment_word(token: &Token) -> Option<(&str, &str)> {
    let eq_index = first_unquoted_unsubstituted_equals(token)?;
    if eq_index == 0 {
        return None;
    }

    let name = &token.lexeme[..eq_index];
    if !is_posix_name(name) {
        return None;
    }

    Some((name, &token.lexeme[eq_index + 1..]))
}

fn first_unquoted_unsubstituted_equals(token: &Token) -> Option<usize> {
    for (index, byte) in token.lexeme.as_bytes().iter().copied().enumerate() {
        if byte != b'=' {
            continue;
        }

        if byte_index_is_in_quote_or_substitution(token, index) {
            continue;
        }

        return Some(index);
    }

    None
}

fn byte_index_is_in_quote_or_substitution(token: &Token, index: usize) -> bool {
    let index = index as u32;
    token.quote_markers.iter().any(|marker| {
        range_contains_index(marker.range.start.value(), marker.range.end.value(), index)
    }) || token.substitution_markers.iter().any(|marker| {
        range_contains_index(marker.range.start.value(), marker.range.end.value(), index)
    })
}

fn range_contains_index(start: u32, end: u32, index: u32) -> bool {
    start <= index && index < end
}

/// Returns true when lexeme matches POSIX portable name form.
pub fn is_posix_name(lexeme: &str) -> bool {
    let mut chars = lexeme.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }

    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}
