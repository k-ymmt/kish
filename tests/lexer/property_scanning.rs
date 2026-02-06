use kish::lexer::{LexStep, Lexer, LexerMode};
use proptest::prelude::*;

const MAX_INPUT_BYTES: usize = 256;
const MAX_STEPS: usize = 2048;

proptest! {
    #[test]
    fn next_token_handles_lossy_utf8_inputs_without_panicking(
        bytes in proptest::collection::vec(any::<u8>(), 0..=MAX_INPUT_BYTES)
    ) {
        let input = String::from_utf8_lossy(&bytes).into_owned();
        let mut lexer = Lexer::new(&input, LexerMode::Normal);
        let mut finished = false;

        for _ in 0..MAX_STEPS {
            match lexer.next_token() {
                Ok(LexStep::Token(_)) => {}
                Ok(LexStep::Recoverable(_)) | Ok(LexStep::EndOfInput) | Err(_) => {
                    finished = true;
                    break;
                }
            }
        }

        prop_assert!(
            finished,
            "lexer did not terminate within {MAX_STEPS} steps for input length {}",
            input.len()
        );
    }
}
