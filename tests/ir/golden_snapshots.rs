//! Golden snapshot tests: verify exact disassembly output for regression protection.
//!
//! Each test parses → lowers → encodes → decodes all streams and compares the
//! formatted disassembly output against a hard-coded expected string.

use kish::ir::{
    IrOptions, LoweringContext, decode_arith_program_words, decode_code_object_words,
    decode_redirect_program_words, decode_word_program_words, encode_module, verify_module,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};

// ---------------------------------------------------------------------------
// Disassembly helper
// ---------------------------------------------------------------------------

fn parser_for(input: &str) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), ParseOptions::default(), stream)
}

/// Full disassembly of a single complete command: parse → lower → verify →
/// encode → decode → format as deterministic string.
fn disassemble(input: &str) -> String {
    let mut parser = parser_for(input);
    let step = parser
        .parse_complete_command()
        .expect("parse should succeed");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command, got {step:?}");
    };

    let mut ctx = LoweringContext::new(IrOptions::default());
    let module = ctx
        .lower_complete_command(&command)
        .expect("lowering should succeed");

    verify_module(&module).expect("verification should succeed");

    let encoded = encode_module(&module).expect("encoding should succeed");

    format_encoded(&encoded)
}

/// Disassemble a full program (multiple complete commands).
fn disassemble_program(input: &str) -> String {
    let mut parser = parser_for(input);
    let program = parser.parse_program().expect("parse should succeed");

    let mut ctx = LoweringContext::new(IrOptions::default());
    let module = ctx
        .lower_program(&program)
        .expect("lowering should succeed");

    verify_module(&module).expect("verification should succeed");

    let encoded = encode_module(&module).expect("encoding should succeed");

    format_encoded(&encoded)
}

fn format_encoded(encoded: &kish::ir::EncodedModule) -> String {
    let mut out = String::new();

    if !encoded.string_pool.is_empty() {
        out.push_str("=== string_pool ===\n");
        for (i, s) in encoded.string_pool.iter().enumerate() {
            out.push_str(&format!("  {i}: {s:?}\n"));
        }
    }
    if !encoded.symbol_pool.is_empty() {
        out.push_str("=== symbol_pool ===\n");
        for (i, s) in encoded.symbol_pool.iter().enumerate() {
            out.push_str(&format!("  {i}: {s:?}\n"));
        }
    }
    if !encoded.const_pool.is_empty() {
        out.push_str("=== const_pool ===\n");
        for (i, c) in encoded.const_pool.iter().enumerate() {
            out.push_str(&format!("  {i}: {c:?}\n"));
        }
    }

    for (i, co) in encoded.code_objects.iter().enumerate() {
        out.push_str(&format!("=== code_object[{i}] ===\n"));
        let decoded = decode_code_object_words(&co.words).expect("decode CO");
        for d in &decoded {
            out.push_str(&format!("  {}: {}\n", d.offset, d.display));
        }
    }

    for (i, words) in encoded.word_programs.iter().enumerate() {
        out.push_str(&format!("=== word_program[{i}] ===\n"));
        let decoded = decode_word_program_words(words).expect("decode WP");
        for d in &decoded {
            out.push_str(&format!("  {}: {}\n", d.offset, d.display));
        }
    }

    for (i, words) in encoded.redirect_programs.iter().enumerate() {
        out.push_str(&format!("=== redirect_program[{i}] ===\n"));
        let decoded = decode_redirect_program_words(words).expect("decode RP");
        for d in &decoded {
            out.push_str(&format!("  {}: {}\n", d.offset, d.display));
        }
    }

    for (i, words) in encoded.arith_programs.iter().enumerate() {
        out.push_str(&format!("=== arith_program[{i}] ===\n"));
        let decoded = decode_arith_program_words(words).expect("decode AP");
        for d in &decoded {
            out.push_str(&format!("  {}: {}\n", d.offset, d.display));
        }
    }

    out
}

// ---------------------------------------------------------------------------
// Snapshot tests
// ---------------------------------------------------------------------------

#[test]
fn snapshot_echo() {
    let actual = disassemble("echo hello\n");
    let expected = "\
=== string_pool ===
  0: \"echo\"
  1: \"hello\"
=== code_object[0] ===
  0: BeginSimple
  1: AddArg 0
  2: AddArg 1
  3: EndSimple
  4: ExecSimple Standard
  5: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_assignment() {
    let actual = disassemble("FOO=bar\n");
    let expected = "\
=== string_pool ===
  0: \"bar\"
=== symbol_pool ===
  0: \"FOO\"
=== code_object[0] ===
  0: BeginSimple
  1: AddAssign sym=0 wp=0
  3: EndSimple
  4: ExecSimple NoCommand
  5: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_pipeline() {
    let actual = disassemble("echo a | cat\n");
    let expected = "\
=== string_pool ===
  0: \"echo\"
  1: \"a\"
  2: \"cat\"
=== code_object[0] ===
  0: BeginPipeline 2
  1: AddPipelineStage 1
  2: AddPipelineStage 2
  3: ExecPipeline
  4: Ret
=== code_object[1] ===
  0: BeginSimple
  1: AddArg 0
  2: AddArg 1
  3: EndSimple
  4: ExecSimple Standard
  5: Ret
=== code_object[2] ===
  0: BeginSimple
  1: AddArg 2
  2: EndSimple
  3: ExecSimple Standard
  4: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[2] ===
  0: PushLiteral 2
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_if_else() {
    let actual = disassemble("if true; then echo y; else echo n; fi\n");
    let expected = "\
=== string_pool ===
  0: \"true\"
  1: \"echo\"
  2: \"y\"
  3: \"n\"
=== code_object[0] ===
  0: BeginSimple
  1: AddArg 0
  2: EndSimple
  3: ExecSimple Standard
  4: JmpIfNonZero 11
  5: BeginSimple
  6: AddArg 1
  7: AddArg 2
  8: EndSimple
  9: ExecSimple Standard
  10: Jmp 16
  11: BeginSimple
  12: AddArg 3
  13: AddArg 4
  14: EndSimple
  15: ExecSimple Standard
  16: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[2] ===
  0: PushLiteral 2
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[3] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[4] ===
  0: PushLiteral 3
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_for_loop() {
    let actual = disassemble("for i in a b c; do echo $i; done\n");
    let expected = "\
=== string_pool ===
  0: \"a\"
  1: \"b\"
  2: \"c\"
  3: \"echo\"
=== symbol_pool ===
  0: \"i\"
=== code_object[0] ===
  0: PushInt 0
  3: ForSetup var=0 word_count=3
  5: ForAddWord 0
  6: ForAddWord 1
  7: ForAddWord 2
  8: ForNext
  9: JmpIfZero 14
  10: Drop
  11: BeginSimple
  12: AddArg 3
  13: AddArg 4
  14: EndSimple
  15: ExecSimple Standard
  16: Jmp 5
  17: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[2] ===
  0: PushLiteral 2
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[3] ===
  0: PushLiteral 3
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[4] ===
  0: ExpandParameter 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_while_loop() {
    let actual = disassemble("while true; do echo loop; done\n");
    let expected = "\
=== string_pool ===
  0: \"true\"
  1: \"echo\"
  2: \"loop\"
=== code_object[0] ===
  0: PushInt 0
  3: BeginSimple
  4: AddArg 0
  5: EndSimple
  6: ExecSimple Standard
  7: JmpIfNonZero 13
  8: Drop
  9: BeginSimple
  10: AddArg 1
  11: AddArg 2
  12: EndSimple
  13: ExecSimple Standard
  14: Jmp 1
  15: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[2] ===
  0: PushLiteral 2
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_case() {
    let actual = disassemble("case x in a) echo a;; b) echo b;; esac\n");
    let expected = "\
=== string_pool ===
  0: \"x\"
  1: \"a\"
  2: \"echo\"
  3: \"b\"
=== code_object[0] ===
  0: CaseSetSubject 0
  1: PushInt 0
  4: CaseTestPattern 1
  5: JmpIfNonZero 5
  6: Jmp 12
  7: Drop
  8: BeginSimple
  9: AddArg 2
  10: AddArg 3
  11: EndSimple
  12: ExecSimple Standard
  13: Jmp 23
  14: CaseTestPattern 4
  15: JmpIfNonZero 15
  16: Jmp 22
  17: Drop
  18: BeginSimple
  19: AddArg 5
  20: AddArg 6
  21: EndSimple
  22: ExecSimple Standard
  23: Jmp 23
  24: CaseClear
  25: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: QuoteRemoval
=== word_program[2] ===
  0: PushLiteral 2
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[3] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[4] ===
  0: PushLiteral 3
  1: QuoteRemoval
=== word_program[5] ===
  0: PushLiteral 2
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[6] ===
  0: PushLiteral 3
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_function() {
    let actual = disassemble("f() { echo hello; }\n");
    let expected = "\
=== string_pool ===
  0: \"echo\"
  1: \"hello\"
=== symbol_pool ===
  0: \"f\"
=== code_object[0] ===
  0: DefineFunction name=0 body=1
  2: Ret
=== code_object[1] ===
  0: BeginSimple
  1: AddArg 0
  2: AddArg 1
  3: EndSimple
  4: ExecSimple Standard
  5: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_subshell() {
    let actual = disassemble("(echo sub)\n");
    let expected = "\
=== string_pool ===
  0: \"echo\"
  1: \"sub\"
=== code_object[0] ===
  0: ExecSubshell 1
  1: Ret
=== code_object[1] ===
  0: BeginSimple
  1: AddArg 0
  2: AddArg 1
  3: EndSimple
  4: ExecSimple Standard
  5: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_background() {
    let actual = disassemble("echo bg &\n");
    let expected = "\
=== string_pool ===
  0: \"echo\"
  1: \"bg\"
=== code_object[0] ===
  0: ExecBackground 1
  1: Ret
=== code_object[1] ===
  0: BeginSimple
  1: AddArg 0
  2: AddArg 1
  3: EndSimple
  4: ExecSimple Standard
  5: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_redirect() {
    let actual = disassemble("cmd >out 2>&1\n");
    let expected = "\
=== string_pool ===
  0: \"cmd\"
  1: \"out\"
=== code_object[0] ===
  0: BeginSimple
  1: AddArg 0
  2: AddRedir 0
  3: AddRedir 1
  4: EndSimple
  5: ExecSimple Standard
  6: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== redirect_program[0] ===
  0: Open fd=1 mode=WriteCreate target=1
=== redirect_program[1] ===
  0: Dup from=1 to=2
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_arithmetic() {
    let actual = disassemble("echo $((1+2*3))\n");
    let expected = "\
=== string_pool ===
  0: \"echo\"
=== code_object[0] ===
  0: BeginSimple
  1: AddArg 0
  2: AddArg 1
  3: EndSimple
  4: ExecSimple Standard
  5: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: ExpandArithmetic 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== arith_program[0] ===
  0: PushLiteral 1
  3: PushLiteral 2
  6: PushLiteral 3
  9: Multiply
  10: Add
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_and_or() {
    let actual = disassemble("true && echo yes || echo no\n");
    let expected = "\
=== string_pool ===
  0: \"true\"
  1: \"echo\"
  2: \"yes\"
  3: \"no\"
=== code_object[0] ===
  0: BeginSimple
  1: AddArg 0
  2: EndSimple
  3: ExecSimple Standard
  4: Dup
  5: JmpIfNonZero 12
  6: Drop
  7: BeginSimple
  8: AddArg 1
  9: AddArg 2
  10: EndSimple
  11: ExecSimple Standard
  12: Dup
  13: JmpIfZero 20
  14: Drop
  15: BeginSimple
  16: AddArg 3
  17: AddArg 4
  18: EndSimple
  19: ExecSimple Standard
  20: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[2] ===
  0: PushLiteral 2
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[3] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[4] ===
  0: PushLiteral 3
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
";
    assert_eq!(actual, expected);
}

#[test]
fn snapshot_complex() {
    let actual = disassemble_program("x=1 cmd >out\nif true; then echo y; fi\n");
    let expected = "\
=== string_pool ===
  0: \"1\"
  1: \"cmd\"
  2: \"out\"
  3: \"true\"
  4: \"echo\"
  5: \"y\"
=== symbol_pool ===
  0: \"x\"
=== code_object[0] ===
  0: BeginSimple
  1: AddAssign sym=0 wp=0
  3: AddArg 1
  4: AddRedir 0
  5: EndSimple
  6: ExecSimple Standard
  7: Drop
  8: BeginSimple
  9: AddArg 3
  10: EndSimple
  11: ExecSimple Standard
  12: JmpIfNonZero 18
  13: BeginSimple
  14: AddArg 4
  15: AddArg 5
  16: EndSimple
  17: ExecSimple Standard
  18: Jmp 19
  19: PushInt 0
  22: Ret
=== word_program[0] ===
  0: PushLiteral 0
  1: QuoteRemoval
=== word_program[1] ===
  0: PushLiteral 1
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[2] ===
  0: PushLiteral 2
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[3] ===
  0: PushLiteral 3
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[4] ===
  0: PushLiteral 4
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== word_program[5] ===
  0: PushLiteral 5
  1: FieldSplit
  2: Glob
  3: QuoteRemoval
=== redirect_program[0] ===
  0: Open fd=1 mode=WriteCreate target=2
";
    assert_eq!(actual, expected);
}
