//! Linear-scaling validation tests for IR infrastructure.
//!
//! Each test measures wall-clock time at two input sizes and asserts the
//! ratio stays within linear bounds (size_ratio * tolerance).

use std::time::{Duration, Instant};

use kish::ir::{
    CodeObject, CodeObjectBuilder, CodeObjectId, Instruction, IrModuleBuilder, IrOptions,
    LoweringContext, encode_module,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, Parser, TokenStream};

// ---------------------------------------------------------------------------
// Helper: assert linear scaling
// ---------------------------------------------------------------------------

/// Runs `f(small_n)` and `f(large_n)`, asserts the time ratio stays within
/// `(large_n / small_n) * tolerance`.
fn assert_linear_scaling<F>(f: F, small_n: usize, large_n: usize, tolerance: f64)
where
    F: Fn(usize),
{
    let size_ratio = large_n as f64 / small_n as f64;

    // Warm up.
    f(small_n);

    let start = Instant::now();
    f(small_n);
    let small_time = start.elapsed();

    let start = Instant::now();
    f(large_n);
    let large_time = start.elapsed();

    // Guard against near-zero small_time (sub-microsecond).
    let min_time = Duration::from_micros(1);
    let effective_small = small_time.max(min_time);

    let actual_ratio = large_time.as_secs_f64() / effective_small.as_secs_f64();
    let max_ratio = size_ratio * tolerance;

    assert!(
        actual_ratio <= max_ratio,
        "scaling violation: large/small time ratio {actual_ratio:.2} exceeds \
         max allowed {max_ratio:.2} (size_ratio={size_ratio:.1}, tolerance={tolerance:.1})"
    );
}

// ---------------------------------------------------------------------------
// Helpers: script generators
// ---------------------------------------------------------------------------

fn generate_simple_commands(n: usize) -> String {
    let mut script = String::new();
    for i in 0..n {
        script.push_str(&format!("cmd_{i} arg_{i}\n"));
    }
    script
}

fn lower_script(input: &str) -> kish::ir::IrModule {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    let mut parser = Parser::new(SourceId::new(0), ParseOptions::default(), stream);
    let program = parser.parse_program().expect("parse should succeed");
    let mut context = LoweringContext::new(IrOptions::default());
    context.lower_program(&program).expect("lowering should succeed")
}

// ---------------------------------------------------------------------------
// 1. Pool interning scales linearly
// ---------------------------------------------------------------------------

#[test]
fn pool_interning_scales_linearly() {
    assert_linear_scaling(
        |n| {
            let mut builder = IrModuleBuilder::new(IrOptions::default());
            for i in 0..n {
                builder
                    .intern_string(format!("string_{i}"))
                    .expect("intern should succeed");
            }
        },
        1_000,
        4_000,
        3.0,
    );
}

// ---------------------------------------------------------------------------
// 2. Branch patching scales linearly
// ---------------------------------------------------------------------------

#[test]
fn branch_patching_scales_linearly() {
    assert_linear_scaling(
        |n| {
            let mut builder = CodeObjectBuilder::new(CodeObjectId::new(0));
            for _ in 0..n {
                let label = builder.new_label();
                builder.emit_jmp(label).expect("jmp should succeed");
                builder.bind_label(label).expect("bind should succeed");
            }
            let _co = builder.finalize().expect("finalize should succeed");
        },
        500,
        2_000,
        3.0,
    );
}

// ---------------------------------------------------------------------------
// 3. Encode pass scales linearly
// ---------------------------------------------------------------------------

#[test]
fn encode_pass_scales_linearly() {
    assert_linear_scaling(
        |n| {
            let options = IrOptions {
                max_instructions: n + 10,
                max_code_objects: 10,
                ..IrOptions::default()
            };
            let mut builder = IrModuleBuilder::new(options);
            let co = CodeObject {
                id: CodeObjectId::new(0),
                instructions: vec![Instruction::Nop; n],
                locals_count: 0,
                max_stack_depth: 0,
            };
            builder
                .add_code_object(co)
                .expect("add_code_object should succeed");
            let module = builder.finish();
            let _encoded = encode_module(&module).expect("encode should succeed");
        },
        1_000,
        4_000,
        3.0,
    );
}

// ---------------------------------------------------------------------------
// 4. Full lowering scales linearly
// ---------------------------------------------------------------------------

#[test]
fn full_lowering_scales_linearly() {
    // Pre-generate scripts to avoid measuring string generation.
    let small_script = generate_simple_commands(100);
    let large_script = generate_simple_commands(400);

    // Warm up.
    lower_script(&small_script);

    let start = Instant::now();
    lower_script(&small_script);
    let small_time = start.elapsed();

    let start = Instant::now();
    lower_script(&large_script);
    let large_time = start.elapsed();

    let min_time = Duration::from_micros(1);
    let effective_small = small_time.max(min_time);
    let actual_ratio = large_time.as_secs_f64() / effective_small.as_secs_f64();
    let size_ratio = 4.0;
    let max_ratio = size_ratio * 3.0;

    assert!(
        actual_ratio <= max_ratio,
        "full lowering scaling violation: ratio {actual_ratio:.2} exceeds max {max_ratio:.2}"
    );
}
