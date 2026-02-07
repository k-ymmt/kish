//! Property-based tests for the IR pipeline using `proptest`.

use kish::ir::{
    IrModule, IrOptions, LoweringContext, encode_module, verify_module,
    decode_code_object_words, decode_word_program_words,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};
use proptest::prelude::*;

// ---------------------------------------------------------------------------
// Strategies
// ---------------------------------------------------------------------------

/// A curated set of valid shell fragments for property testing.
fn arb_shell_input() -> impl Strategy<Value = String> {
    prop_oneof![
        // Simple commands
        Just("echo hello\n".to_string()),
        Just("true\n".to_string()),
        Just("false\n".to_string()),
        Just("ls -la\n".to_string()),
        Just("echo a b c\n".to_string()),
        // Assignments
        Just("FOO=bar\n".to_string()),
        Just("X=1 Y=2 cmd\n".to_string()),
        // Pipelines
        Just("echo a | cat\n".to_string()),
        Just("ls | grep foo | wc -l\n".to_string()),
        // And/or
        Just("true && echo yes\n".to_string()),
        Just("false || echo fallback\n".to_string()),
        Just("true && echo a || echo b\n".to_string()),
        // Lists
        Just("echo a; echo b\n".to_string()),
        Just("echo a; echo b; echo c\n".to_string()),
        // Background
        Just("echo bg &\n".to_string()),
        // Subshell
        Just("(echo sub)\n".to_string()),
        // Brace group
        Just("{ echo hello; }\n".to_string()),
        // If
        Just("if true; then echo y; fi\n".to_string()),
        Just("if true; then echo y; else echo n; fi\n".to_string()),
        // For
        Just("for i in a b c; do echo $i; done\n".to_string()),
        // While / Until
        Just("while true; do echo loop; done\n".to_string()),
        Just("until false; do echo loop; done\n".to_string()),
        // Case
        Just("case x in a) echo a;; esac\n".to_string()),
        // Function
        Just("f() { echo hello; }\n".to_string()),
        // Redirect
        Just("echo out >file\n".to_string()),
        // Arithmetic
        Just("echo $((1+2))\n".to_string()),
        Just("echo $((3*4+5))\n".to_string()),
        // Negated pipeline
        Just("! echo fail\n".to_string()),
    ]
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Parse a single complete command from input.
fn try_parse_and_lower(input: &str) -> Option<IrModule> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    let mut parser = Parser::new(SourceId::new(0), ParseOptions::default(), stream);
    let step = parser.parse_complete_command().ok()?;
    let ParseStep::Complete(command) = step else {
        return None;
    };
    let mut ctx = LoweringContext::new(IrOptions::default());
    ctx.lower_complete_command(&command).ok()
}

// ---------------------------------------------------------------------------
// Pool deduplication determinism
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn pool_string_dedup_determinism(input in arb_shell_input()) {
        let module = try_parse_and_lower(&input);
        if let Some(module) = module {
            // Check that all strings in the pool are unique (deduplication works).
            let pool = &module.string_pool;
            for (i, s) in pool.iter().enumerate() {
                for (j, t) in pool.iter().enumerate() {
                    if i != j {
                        prop_assert!(
                            s != t,
                            "string pool has duplicates at indices {i} and {j}: {s:?}"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn pool_symbol_dedup_determinism(input in arb_shell_input()) {
        let module = try_parse_and_lower(&input);
        if let Some(module) = module {
            let pool = &module.symbol_pool;
            for (i, s) in pool.iter().enumerate() {
                for (j, t) in pool.iter().enumerate() {
                    if i != j {
                        prop_assert!(
                            s != t,
                            "symbol pool has duplicates at indices {i} and {j}: {s:?}"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn pool_const_dedup_determinism(input in arb_shell_input()) {
        let module = try_parse_and_lower(&input);
        if let Some(module) = module {
            let pool = &module.const_pool;
            for (i, c) in pool.iter().enumerate() {
                for (j, d) in pool.iter().enumerate() {
                    if i != j {
                        prop_assert!(
                            c != d,
                            "const pool has duplicates at indices {i} and {j}: {c:?}"
                        );
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Determinism properties
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn encode_is_deterministic(input in arb_shell_input()) {
        let module = try_parse_and_lower(&input);
        if let Some(module) = module {
            let encoded1 = encode_module(&module).expect("first encode should succeed");
            let encoded2 = encode_module(&module).expect("second encode should succeed");
            prop_assert_eq!(encoded1, encoded2, "encoding should be deterministic");
        }
    }

    #[test]
    fn lowering_determinism(input in arb_shell_input()) {
        let module1 = try_parse_and_lower(&input);
        let module2 = try_parse_and_lower(&input);
        prop_assert_eq!(module1, module2, "lowering should be deterministic");
    }
}

// ---------------------------------------------------------------------------
// Pipeline composition properties
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn verify_accepts_all_lowered_programs(input in arb_shell_input()) {
        let module = try_parse_and_lower(&input);
        if let Some(module) = module {
            let result = verify_module(&module);
            prop_assert!(
                result.is_ok(),
                "verify_module should accept all lowered programs, got: {:?}",
                result.err()
            );
        }
    }

    #[test]
    fn encode_accepts_all_verified_modules(input in arb_shell_input()) {
        let module = try_parse_and_lower(&input);
        if let Some(module) = module {
            verify_module(&module).expect("verify should succeed");
            let result = encode_module(&module);
            prop_assert!(
                result.is_ok(),
                "encode_module should accept all verified modules, got: {:?}",
                result.err()
            );
        }
    }

    #[test]
    fn decode_roundtrip_word_count(input in arb_shell_input()) {
        let module = try_parse_and_lower(&input);
        if let Some(module) = module {
            let encoded = encode_module(&module).expect("encode should succeed");

            // Check that all code objects decode and have the right word count.
            for (i, co) in encoded.code_objects.iter().enumerate() {
                let decoded = decode_code_object_words(&co.words);
                prop_assert!(
                    decoded.is_ok(),
                    "code object {i} should decode, got: {:?}",
                    decoded.err()
                );
                let decoded = decoded.unwrap();
                // Total words consumed by decoded instructions should equal the words length.
                let total_words: u32 = decoded.iter().map(|d| d.word_count).sum();
                prop_assert_eq!(
                    total_words as usize,
                    co.words.len(),
                    "code object {}: decoded word count mismatch", i
                );
            }

            // Check word programs.
            for (i, words) in encoded.word_programs.iter().enumerate() {
                let decoded = decode_word_program_words(words);
                prop_assert!(
                    decoded.is_ok(),
                    "word program {i} should decode, got: {:?}",
                    decoded.err()
                );
                let decoded = decoded.unwrap();
                let total_words: u32 = decoded.iter().map(|d| d.word_count).sum();
                prop_assert_eq!(
                    total_words as usize,
                    words.len(),
                    "word program {}: decoded word count mismatch", i
                );
            }
        }
    }
}
