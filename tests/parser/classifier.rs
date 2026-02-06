use kish::lexer::{
    ByteOffset, DelimiterContext, QuoteMarker, QuoteProvenance, SourceId, Span, Token, TokenKind,
    TokenOffset, TokenRange,
};
use kish::parser::{
    ClassificationContext, ClassificationOptions, ClassifiedTokenKind, Classifier, NameContext,
    ReservedWord, ReservedWordPolicy,
};

fn token(lexeme: &str) -> Token {
    let span = Span::new(
        SourceId::new(0),
        ByteOffset::new(0),
        ByteOffset::from_usize(lexeme.len()),
    );
    Token::new(TokenKind::Token, lexeme.to_string(), span)
}

#[test]
fn reserved_words_only_match_when_policy_allows() {
    let classifier = Classifier::new();
    let if_token = token("if");

    let none = classifier.classify(
        &if_token,
        ClassificationContext {
            reserved_word_policy: ReservedWordPolicy::None,
            ..Default::default()
        },
        ClassificationOptions::default(),
    );
    assert_eq!(none.kind, ClassifiedTokenKind::Word);

    let any = classifier.classify(
        &if_token,
        ClassificationContext {
            reserved_word_policy: ReservedWordPolicy::Any,
            ..Default::default()
        },
        ClassificationOptions::default(),
    );
    assert_eq!(
        any.kind,
        ClassifiedTokenKind::ReservedWord(ReservedWord::If)
    );
}

#[test]
fn quoted_tokens_are_not_reserved_words() {
    let classifier = Classifier::new();
    let mut quoted = token("if");
    quoted.quote_markers.push(QuoteMarker::new(
        QuoteProvenance::SingleQuoted,
        TokenRange::new(TokenOffset::new(0), TokenOffset::new(2)),
    ));

    let result = classifier.classify(
        &quoted,
        ClassificationContext {
            reserved_word_policy: ReservedWordPolicy::Any,
            ..Default::default()
        },
        ClassificationOptions::default(),
    );
    assert_eq!(result.kind, ClassifiedTokenKind::Word);
}

#[test]
fn for_name_context_maps_valid_name_only() {
    let classifier = Classifier::new();
    let valid = token("var_name0");
    let invalid = token("0name");

    let valid_result = classifier.classify(
        &valid,
        ClassificationContext {
            name_context: NameContext::ForName,
            ..Default::default()
        },
        ClassificationOptions::default(),
    );
    assert_eq!(valid_result.kind, ClassifiedTokenKind::Name);

    let invalid_result = classifier.classify(
        &invalid,
        ClassificationContext {
            name_context: NameContext::ForName,
            ..Default::default()
        },
        ClassificationOptions::default(),
    );
    assert_eq!(invalid_result.kind, ClassifiedTokenKind::Word);
}

#[test]
fn assignment_word_rule_handles_valid_and_invalid_prefixes() {
    let classifier = Classifier::new();
    let a_eq_b = token("a=b");
    let eq_x = token("=x");
    let invalid_name = token("a-b=c");

    let context = ClassificationContext {
        allow_assignment_word: true,
        ..Default::default()
    };
    let options = ClassificationOptions::default();

    assert_eq!(
        classifier.classify(&a_eq_b, context, options).kind,
        ClassifiedTokenKind::AssignmentWord
    );
    assert_eq!(
        classifier.classify(&eq_x, context, options).kind,
        ClassifiedTokenKind::Word
    );
    assert_eq!(
        classifier.classify(&invalid_name, context, options).kind,
        ClassifiedTokenKind::Word
    );
}

#[test]
fn io_number_and_io_location_classification_follow_context_and_options() {
    let classifier = Classifier::new();

    let io_number = token("2");
    let number_result = classifier.classify(
        &io_number,
        ClassificationContext {
            delimiter_context: Some(DelimiterContext::Greater),
            ..Default::default()
        },
        ClassificationOptions::default(),
    );
    assert_eq!(number_result.kind, ClassifiedTokenKind::IoNumber(2));

    let io_location = token("{fd}");
    let location_disabled = classifier.classify(
        &io_location,
        ClassificationContext {
            delimiter_context: Some(DelimiterContext::Greater),
            ..Default::default()
        },
        ClassificationOptions {
            allow_io_location: false,
        },
    );
    assert_eq!(location_disabled.kind, ClassifiedTokenKind::Word);

    let location_enabled = classifier.classify(
        &io_location,
        ClassificationContext {
            delimiter_context: Some(DelimiterContext::Greater),
            ..Default::default()
        },
        ClassificationOptions {
            allow_io_location: true,
        },
    );
    assert_eq!(location_enabled.kind, ClassifiedTokenKind::IoLocation);
}

#[test]
fn rule9_function_body_bypasses_assignment_conversion() {
    let classifier = Classifier::new();
    let assignment_like = token("name=value");

    let result = classifier.classify(
        &assignment_like,
        ClassificationContext {
            allow_assignment_word: true,
            function_body_rule9: true,
            ..Default::default()
        },
        ClassificationOptions::default(),
    );
    assert_eq!(result.kind, ClassifiedTokenKind::Word);
}
