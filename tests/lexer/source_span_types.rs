use kish::lexer::{ByteOffset, SourceId, Span};

#[test]
fn source_and_span_contracts_hold_ordering() {
    let source = SourceId::new(7);
    assert_eq!(source.value(), 7);

    let high = ByteOffset::new(10);
    let low = ByteOffset::new(2);
    let span = Span::new(source, high, low);

    assert_eq!(span.start, low);
    assert_eq!(span.end, high);
    assert_eq!(span.len(), 8);
    assert!(!span.is_empty());
}
