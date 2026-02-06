use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};
use proptest::prelude::*;

const MAX_INPUT_BYTES: usize = 256;
const MAX_STEPS: usize = 2048;

fn parser_for(input: &str, options: ParseOptions) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), options, token_stream)
}

proptest! {
    #[test]
    fn parse_program_handles_lossy_utf8_inputs_without_panicking(
        bytes in proptest::collection::vec(any::<u8>(), 0..=MAX_INPUT_BYTES)
    ) {
        let input = String::from_utf8_lossy(&bytes).into_owned();
        let mut parser = parser_for(&input, ParseOptions::default());
        let _ = parser.parse_program();
    }

    #[test]
    fn parse_complete_command_loop_terminates_without_panicking(
        bytes in proptest::collection::vec(any::<u8>(), 0..=MAX_INPUT_BYTES)
    ) {
        let input = String::from_utf8_lossy(&bytes).into_owned();
        let mut parser = parser_for(&input, ParseOptions::default());
        let mut terminated = false;

        for _ in 0..MAX_STEPS {
            match parser.parse_complete_command() {
                Ok(ParseStep::Complete(_)) => {}
                Ok(ParseStep::EndOfInput) | Err(_) => {
                    terminated = true;
                    break;
                }
                Ok(ParseStep::NeedMoreInput(_)) => {
                    terminated = true;
                    break;
                }
            }
        }

        prop_assert!(
            terminated,
            "parser did not terminate within {MAX_STEPS} steps for input length {}",
            input.len()
        );
    }

    #[test]
    fn interactive_complete_command_loop_handles_need_more_without_panicking(
        bytes in proptest::collection::vec(any::<u8>(), 0..=MAX_INPUT_BYTES)
    ) {
        let input = String::from_utf8_lossy(&bytes).into_owned();
        let mut parser = parser_for(
            &input,
            ParseOptions {
                interactive: true,
                ..Default::default()
            },
        );
        let mut terminated = false;

        for _ in 0..MAX_STEPS {
            match parser.parse_complete_command() {
                Ok(ParseStep::Complete(_)) => {}
                Ok(ParseStep::EndOfInput) | Ok(ParseStep::NeedMoreInput(_)) | Err(_) => {
                    terminated = true;
                    break;
                }
            }
        }

        prop_assert!(
            terminated,
            "interactive parser did not terminate within {MAX_STEPS} steps for input length {}",
            input.len()
        );
    }
}
