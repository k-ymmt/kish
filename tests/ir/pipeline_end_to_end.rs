//! End-to-end pipeline tests: parse → lower → verify → encode → decode round-trip.
//!
//! Each test exercises the full IR pipeline to ensure all phases compose correctly.

use kish::ir::{
    DecodedInstruction, EncodedModule, IrModule, IrOptions, LoweringContext, VerifyWarningKind,
    decode_arith_program_words, decode_code_object_words, decode_redirect_program_words,
    decode_word_program_words, encode_module, verify_module, verify_module_debug,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

fn parser_for(input: &str) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), ParseOptions::default(), stream)
}

/// Full pipeline for a single complete command: parse → lower → verify → encode.
fn full_pipeline(input: &str) -> (IrModule, EncodedModule) {
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

    (module, encoded)
}

/// Full pipeline for a program (multiple complete commands).
fn full_pipeline_program(input: &str) -> (IrModule, EncodedModule) {
    let mut parser = parser_for(input);
    let program = parser.parse_program().expect("parse should succeed");

    let mut ctx = LoweringContext::new(IrOptions::default());
    let module = ctx
        .lower_program(&program)
        .expect("lowering should succeed");

    verify_module(&module).expect("verification should succeed");

    let encoded = encode_module(&module).expect("encoding should succeed");

    (module, encoded)
}

/// Decode all code objects in the encoded module, returning one vec per code object.
fn decode_all_code_objects(
    encoded: &EncodedModule,
) -> Vec<Vec<DecodedInstruction>> {
    encoded
        .code_objects
        .iter()
        .map(|co| {
            decode_code_object_words(&co.words).expect("code object decode should succeed")
        })
        .collect()
}

/// Decode all word programs in the encoded module.
fn decode_all_word_programs(
    encoded: &EncodedModule,
) -> Vec<Vec<DecodedInstruction>> {
    encoded
        .word_programs
        .iter()
        .map(|words| decode_word_program_words(words).expect("word program decode should succeed"))
        .collect()
}

/// Decode all redirect programs in the encoded module.
fn decode_all_redirect_programs(
    encoded: &EncodedModule,
) -> Vec<Vec<DecodedInstruction>> {
    encoded
        .redirect_programs
        .iter()
        .map(|words| {
            decode_redirect_program_words(words)
                .expect("redirect program decode should succeed")
        })
        .collect()
}

/// Decode all arith programs in the encoded module.
fn decode_all_arith_programs(
    encoded: &EncodedModule,
) -> Vec<Vec<DecodedInstruction>> {
    encoded
        .arith_programs
        .iter()
        .map(|words| {
            decode_arith_program_words(words).expect("arith program decode should succeed")
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Simple command tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_simple_command() {
    let (module, encoded) = full_pipeline("echo hello\n");
    assert_eq!(module.code_objects.len(), 1);
    assert!(!module.word_programs.is_empty());
    let decoded = decode_all_code_objects(&encoded);
    assert!(!decoded[0].is_empty());
    // Verify decode produces at least BeginSimple, AddArg, EndSimple, ExecSimple, Ret
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.contains(&"BeginSimple"));
    assert!(displays.contains(&"EndSimple"));
    assert!(displays.iter().any(|d| d.starts_with("ExecSimple")));
    assert!(displays.contains(&"Ret"));
}

#[test]
fn e2e_simple_command_with_assignments() {
    let (module, encoded) = full_pipeline("FOO=bar cmd\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.contains(&"BeginSimple"));
    assert!(displays.iter().any(|d| d.starts_with("AddAssign")));
    assert!(displays.iter().any(|d| d.starts_with("ExecSimple")));
}

// ---------------------------------------------------------------------------
// Pipeline tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_pipeline() {
    let (module, encoded) = full_pipeline("echo a | cat\n");
    // Pipeline creates child code objects for each stage plus top-level.
    assert!(module.code_objects.len() >= 3);
    let decoded = decode_all_code_objects(&encoded);
    let top_displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(top_displays.iter().any(|d| d.starts_with("BeginPipeline")));
    assert!(top_displays.iter().any(|d| d.starts_with("AddPipelineStage")));
    assert!(top_displays.contains(&"ExecPipeline"));
}

#[test]
fn e2e_negated_pipeline() {
    let (module, encoded) = full_pipeline("! echo fail\n");
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.contains(&"NegateStatus"));
    // Module should still verify and encode cleanly.
    assert!(!encoded.code_objects.is_empty());
    drop(module);
}

// ---------------------------------------------------------------------------
// And/Or chain tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_and_or_chain() {
    let (module, encoded) = full_pipeline("true && echo yes || echo no\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    // Should contain conditional jumps for && and ||.
    assert!(displays.iter().any(|d| d.starts_with("JmpIfNonZero")));
    assert!(displays.iter().any(|d| d.starts_with("JmpIfZero")));
}

// ---------------------------------------------------------------------------
// List tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_list_with_semicolons() {
    let (module, encoded) = full_pipeline("echo a; echo b; echo c\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    // Multiple commands should produce multiple BeginSimple/EndSimple pairs.
    let begin_count = decoded[0]
        .iter()
        .filter(|d| d.display == "BeginSimple")
        .count();
    assert!(begin_count >= 3);
}

// ---------------------------------------------------------------------------
// Background command tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_background_command() {
    let (module, encoded) = full_pipeline("echo bg &\n");
    // Background creates a child code object.
    assert!(module.code_objects.len() >= 2);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.iter().any(|d| d.starts_with("ExecBackground")));
}

// ---------------------------------------------------------------------------
// Compound command tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_subshell() {
    let (module, encoded) = full_pipeline("(echo sub)\n");
    assert!(module.code_objects.len() >= 2);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.iter().any(|d| d.starts_with("ExecSubshell")));
}

#[test]
fn e2e_brace_group() {
    let (module, encoded) = full_pipeline("{ echo hello; }\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    assert!(!decoded[0].is_empty());
}

// ---------------------------------------------------------------------------
// If/then/else tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_if_then() {
    let (module, encoded) = full_pipeline("if true; then echo y; fi\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.iter().any(|d| d.starts_with("JmpIfNonZero")));
}

#[test]
fn e2e_if_then_else() {
    let (module, encoded) = full_pipeline("if true; then echo y; else echo n; fi\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.iter().any(|d| d.starts_with("JmpIfNonZero")));
    assert!(displays.iter().any(|d| d.starts_with("Jmp ")));
}

#[test]
fn e2e_if_elif_else() {
    let (module, encoded) = full_pipeline(
        "if true; then echo a; elif false; then echo b; else echo c; fi\n",
    );
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    // Multiple conditional jumps from if + elif.
    let jmp_count = decoded[0]
        .iter()
        .filter(|d| d.display.starts_with("JmpIfNonZero"))
        .count();
    assert!(jmp_count >= 2);
}

// ---------------------------------------------------------------------------
// Loop tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_for_loop() {
    let (module, encoded) = full_pipeline("for i in a b c; do echo $i; done\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.iter().any(|d| d.starts_with("ForSetup")));
    assert!(displays.iter().any(|d| d.starts_with("ForAddWord")));
    assert!(displays.contains(&"ForNext"));
}

#[test]
fn e2e_while_loop() {
    let (module, encoded) = full_pipeline("while true; do echo loop; done\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.iter().any(|d| d.starts_with("JmpIfNonZero")));
    assert!(displays.iter().any(|d| d.starts_with("Jmp ")));
}

#[test]
fn e2e_until_loop() {
    let (module, encoded) = full_pipeline("until false; do echo loop; done\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    // Until loop uses JmpIfZero (opposite of while).
    assert!(displays.iter().any(|d| d.starts_with("JmpIfZero")));
}

// ---------------------------------------------------------------------------
// Case command tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_case_command() {
    let (module, encoded) =
        full_pipeline("case x in a) echo a;; b) echo b;; esac\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.iter().any(|d| d.starts_with("CaseSetSubject")));
    assert!(displays.iter().any(|d| d.starts_with("CaseTestPattern")));
    assert!(displays.contains(&"CaseClear"));
}

#[test]
fn e2e_case_fallthrough() {
    let (module, encoded) =
        full_pipeline("case x in a) echo a;& b) echo b;; esac\n");
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    // Fallthrough (;&) still produces case instructions.
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.iter().any(|d| d.starts_with("CaseSetSubject")));
}

// ---------------------------------------------------------------------------
// Function definition tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_function_definition() {
    let (module, encoded) = full_pipeline("f() { echo hello; }\n");
    // Function creates a body code object.
    assert!(module.code_objects.len() >= 2);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    assert!(displays.iter().any(|d| d.starts_with("DefineFunction")));
}

// ---------------------------------------------------------------------------
// Subprogram tests
// ---------------------------------------------------------------------------

#[test]
fn e2e_all_subprogram_types() {
    // This exercises word programs, redirect programs, and arith programs simultaneously.
    let (module, encoded) = full_pipeline("FOO=$((1+2)) echo $HOME >out\n");
    assert!(!module.word_programs.is_empty(), "should have word programs");
    assert!(
        !module.redirect_programs.is_empty(),
        "should have redirect programs"
    );
    assert!(
        !module.arith_programs.is_empty(),
        "should have arith programs"
    );

    // All subprogram streams should decode cleanly.
    let wp_decoded = decode_all_word_programs(&encoded);
    assert!(!wp_decoded.is_empty());
    for wp in &wp_decoded {
        assert!(!wp.is_empty(), "each word program should have ops");
    }

    let rp_decoded = decode_all_redirect_programs(&encoded);
    assert!(!rp_decoded.is_empty());

    let ap_decoded = decode_all_arith_programs(&encoded);
    assert!(!ap_decoded.is_empty());
}

// ---------------------------------------------------------------------------
// Nested compound commands
// ---------------------------------------------------------------------------

#[test]
fn e2e_nested_compound() {
    let (module, encoded) = full_pipeline(
        "if true; then for i in a b; do echo $i; done; fi\n",
    );
    assert_eq!(module.code_objects.len(), 1);
    let decoded = decode_all_code_objects(&encoded);
    let displays: Vec<&str> = decoded[0].iter().map(|d| d.display.as_str()).collect();
    // Should have both if-related jumps and for-related instructions.
    assert!(displays.iter().any(|d| d.starts_with("JmpIfNonZero")));
    assert!(displays.iter().any(|d| d.starts_with("ForSetup")));
    assert!(displays.iter().any(|d| d.starts_with("ForAddWord")));
}

// ---------------------------------------------------------------------------
// Multi-command program
// ---------------------------------------------------------------------------

#[test]
fn e2e_multi_command_program() {
    let (module, encoded) = full_pipeline_program("echo a\necho b\necho c\n");
    // Program may create one code object per complete command or merge them.
    assert!(!module.code_objects.is_empty());
    let decoded = decode_all_code_objects(&encoded);
    // Every code object should decode cleanly.
    for (i, co) in decoded.iter().enumerate() {
        assert!(!co.is_empty(), "code object {i} should have instructions");
    }
}

// ---------------------------------------------------------------------------
// Debug verifier
// ---------------------------------------------------------------------------

#[test]
fn e2e_debug_verifier_no_warnings() {
    // A straightforward program should produce no debug warnings.
    let (module, _encoded) = full_pipeline("echo hello\n");
    let warnings = verify_module_debug(&module);
    let dead_code_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.kind == VerifyWarningKind::DeadCodeObject)
        .collect();
    assert!(
        dead_code_warnings.is_empty(),
        "expected no DeadCodeObject warnings, got {dead_code_warnings:?}"
    );
}
