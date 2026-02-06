#[allow(dead_code)]
#[path = "../../src/lexer/cursor.rs"]
mod cursor;

use cursor::Cursor;

#[test]
fn initial_position_is_one_based() {
    let cursor = Cursor::new();
    assert_eq!(cursor.offset().as_usize(), 0);
    assert_eq!(cursor.line(), 1);
    assert_eq!(cursor.column(), 1);
}

#[test]
fn ascii_advance_updates_column_and_newline_resets_column() {
    let mut cursor = Cursor::new();
    let input = "ab\nc";

    assert_eq!(cursor.advance_byte(input), Some(b'a'));
    assert_eq!(cursor.line(), 1);
    assert_eq!(cursor.column(), 2);
    assert_eq!(cursor.advance_byte(input), Some(b'b'));
    assert_eq!(cursor.column(), 3);
    assert_eq!(cursor.advance_byte(input), Some(b'\n'));
    assert_eq!(cursor.line(), 2);
    assert_eq!(cursor.column(), 1);
}

#[test]
fn utf8_continuation_byte_does_not_increment_column() {
    let mut cursor = Cursor::new();
    let input = "é";

    assert_eq!(input.len(), 2);
    assert_eq!(cursor.advance_byte(input), Some(0xC3));
    assert_eq!(cursor.column(), 2);
    assert_eq!(cursor.advance_byte(input), Some(0xA9));
    assert_eq!(cursor.column(), 2);
}

#[test]
fn checkpoint_and_rollback_restore_all_position_fields() {
    let mut cursor = Cursor::new();
    let input = "x\ny";
    let checkpoint = cursor.checkpoint();

    let _ = cursor.advance_byte(input);
    let _ = cursor.advance_byte(input);
    assert_eq!(cursor.offset().as_usize(), 2);
    assert_eq!(cursor.line(), 2);
    assert_eq!(cursor.column(), 1);

    cursor.rollback(checkpoint);
    assert_eq!(cursor.offset().as_usize(), 0);
    assert_eq!(cursor.line(), 1);
    assert_eq!(cursor.column(), 1);
}
