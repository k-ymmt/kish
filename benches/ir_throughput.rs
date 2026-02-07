//! Criterion benchmarks for IR lowering, encoding, and interning throughput.

use criterion::{Criterion, criterion_group, criterion_main};

use kish::ir::{
    CodeObjectBuilder, CodeObjectId, IrModuleBuilder, IrOptions, LoweringContext, encode_module,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, Parser, TokenStream};

// ---------------------------------------------------------------------------
// Script generators
// ---------------------------------------------------------------------------

fn generate_simple_commands(n: usize) -> String {
    let mut script = String::new();
    for i in 0..n {
        script.push_str(&format!("cmd_{i} arg_{i}\n"));
    }
    script
}

fn generate_mixed_script(n: usize) -> String {
    let mut script = String::new();
    for i in 0..n {
        match i % 4 {
            0 => script.push_str(&format!("cmd_{i} arg_{i}\n")),
            1 => script.push_str(&format!("VAR_{i}=val_{i}\n")),
            2 => script.push_str(&format!("cmd_{i} >out_{i}\n")),
            3 => script.push_str(&format!("cmd_{i} arg_{i} && cmd_{i}b\n")),
            _ => unreachable!(),
        }
    }
    script
}

fn lower_script(input: &str) -> kish::ir::IrModule {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    let mut parser = Parser::new(SourceId::new(0), ParseOptions::default(), stream);
    let program = parser.parse_program().expect("parse should succeed");
    let mut context = LoweringContext::new(IrOptions::default());
    context
        .lower_program(&program)
        .expect("lowering should succeed")
}

// ---------------------------------------------------------------------------
// Lowering benchmarks
// ---------------------------------------------------------------------------

fn bench_lowering(c: &mut Criterion) {
    let small = generate_simple_commands(10);
    let medium = generate_mixed_script(100);
    let large = generate_mixed_script(1000);

    let mut group = c.benchmark_group("lowering");

    group.bench_function("small", |b| {
        b.iter(|| lower_script(&small));
    });

    group.bench_function("medium", |b| {
        b.iter(|| lower_script(&medium));
    });

    group.bench_function("large", |b| {
        b.iter(|| lower_script(&large));
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Encode benchmarks
// ---------------------------------------------------------------------------

fn bench_encode(c: &mut Criterion) {
    let small_module = lower_script(&generate_simple_commands(10));
    let medium_module = lower_script(&generate_mixed_script(100));
    let large_module = lower_script(&generate_mixed_script(1000));

    let mut group = c.benchmark_group("encode");

    group.bench_function("small", |b| {
        b.iter(|| encode_module(&small_module).expect("encode"));
    });

    group.bench_function("medium", |b| {
        b.iter(|| encode_module(&medium_module).expect("encode"));
    });

    group.bench_function("large", |b| {
        b.iter(|| encode_module(&large_module).expect("encode"));
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Interning benchmarks
// ---------------------------------------------------------------------------

fn bench_interning(c: &mut Criterion) {
    let mut group = c.benchmark_group("interning");

    let strings: Vec<String> = (0..10_000).map(|i| format!("string_{i}")).collect();
    let symbols: Vec<String> = (0..10_000).map(|i| format!("symbol_{i}")).collect();

    group.bench_function("strings_10k", |b| {
        b.iter(|| {
            let mut builder = IrModuleBuilder::new(IrOptions::default());
            for s in &strings {
                builder.intern_string(s.as_str()).expect("intern");
            }
        });
    });

    group.bench_function("symbols_10k", |b| {
        b.iter(|| {
            let mut builder = IrModuleBuilder::new(IrOptions::default());
            for s in &symbols {
                builder.intern_symbol(s.as_str()).expect("intern");
            }
        });
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Branch patching benchmarks
// ---------------------------------------------------------------------------

fn bench_branch_patching(c: &mut Criterion) {
    let mut group = c.benchmark_group("branch_patching");

    group.bench_function("1000_labels", |b| {
        b.iter(|| {
            let mut builder = CodeObjectBuilder::new(CodeObjectId::new(0));
            for _ in 0..1000 {
                let label = builder.new_label();
                builder.emit_jmp(label).expect("jmp");
                builder.bind_label(label).expect("bind");
            }
            builder.finalize().expect("finalize")
        });
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

criterion_group!(
    benches,
    bench_lowering,
    bench_encode,
    bench_interning,
    bench_branch_patching
);
criterion_main!(benches);
