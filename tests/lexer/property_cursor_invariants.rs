#[allow(dead_code)]
#[path = "../../src/lexer/cursor.rs"]
mod cursor;

use cursor::Cursor;
use proptest::prelude::*;

const MAX_INPUT_BYTES: usize = 256;

proptest! {
    #[test]
    fn checkpoint_and_rollback_match_fresh_cursor_position(
        bytes in proptest::collection::vec(any::<u8>(), 0..=MAX_INPUT_BYTES),
        checkpoint_step in 0usize..=MAX_INPUT_BYTES,
        extra_steps in 0usize..=MAX_INPUT_BYTES,
    ) {
        let input = String::from_utf8_lossy(&bytes).into_owned();
        let checkpoint_step = checkpoint_step.min(input.len());
        let mut cursor = Cursor::new();

        for _ in 0..checkpoint_step {
            let _ = cursor.advance_byte(&input);
        }

        let checkpoint = cursor.checkpoint();

        for _ in 0..extra_steps {
            if cursor.advance_byte(&input).is_none() {
                break;
            }
        }

        cursor.rollback(checkpoint);

        let mut expected = Cursor::new();
        for _ in 0..checkpoint_step {
            let _ = expected.advance_byte(&input);
        }

        prop_assert_eq!(cursor.offset(), expected.offset());
        prop_assert_eq!(cursor.line(), expected.line());
        prop_assert_eq!(cursor.column(), expected.column());
    }

    #[test]
    fn rollback_is_idempotent_for_same_checkpoint(
        bytes in proptest::collection::vec(any::<u8>(), 0..=MAX_INPUT_BYTES),
        checkpoint_step in 0usize..=MAX_INPUT_BYTES,
    ) {
        let input = String::from_utf8_lossy(&bytes).into_owned();
        let checkpoint_step = checkpoint_step.min(input.len());
        let mut cursor = Cursor::new();

        for _ in 0..checkpoint_step {
            let _ = cursor.advance_byte(&input);
        }

        let checkpoint = cursor.checkpoint();

        while cursor.advance_byte(&input).is_some() {}
        cursor.rollback(checkpoint);

        let first_offset = cursor.offset();
        let first_line = cursor.line();
        let first_column = cursor.column();

        while cursor.advance_byte(&input).is_some() {}
        cursor.rollback(checkpoint);

        prop_assert_eq!(cursor.offset(), first_offset);
        prop_assert_eq!(cursor.line(), first_line);
        prop_assert_eq!(cursor.column(), first_column);
    }
}
