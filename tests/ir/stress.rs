//! Stress tests for IR resource limits added in phase 13.
//!
//! Each test exercises one of the new guardrails (string pool, symbol pool,
//! word/redirect/arith program count, nesting depth) using restrictive
//! `IrOptions` and verifies that `LimitExceeded` fires correctly.

use kish::ir::{
    ArithProgram, ArithProgramOp, IrErrorKind, IrModule, IrModuleBuilder, IrOptions,
    LoweringContext, RedirectProgram, RedirectProgramOp, WordProgram, WordProgramOp,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, Parser, TokenStream};

// ---------------------------------------------------------------------------
// Helpers (mirror negative_lowering.rs)
// ---------------------------------------------------------------------------

fn try_lower_program_with(
    input: &str,
    options: IrOptions,
) -> Result<IrModule, kish::ir::IrError> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    let mut parser = Parser::new(SourceId::new(0), ParseOptions::default(), stream);
    let program = parser.parse_program().expect("parse should succeed");
    let mut context = LoweringContext::new(options);
    context.lower_program(&program)
}

fn assert_limit_exceeded(result: Result<IrModule, kish::ir::IrError>) {
    let err = result.expect_err("lowering should fail with LimitExceeded");
    assert_eq!(
        err.kind,
        IrErrorKind::LimitExceeded,
        "expected LimitExceeded, got {err:?}"
    );
}

// ---------------------------------------------------------------------------
// 1. String pool limit
// ---------------------------------------------------------------------------

#[test]
fn string_pool_limit_enforced() {
    let options = IrOptions {
        max_strings: 100,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    for i in 0..100 {
        builder
            .intern_string(format!("str_{i}"))
            .expect("string should fit");
    }

    let err = builder
        .intern_string("overflow")
        .expect_err("101st unique string must exceed limit");
    assert_eq!(err.kind, IrErrorKind::LimitExceeded);
    assert_eq!(err.message, "string pool limit exceeded");
    assert_eq!(
        err.detail.as_deref(),
        Some("max_strings=100, attempted_count=101")
    );
}

// ---------------------------------------------------------------------------
// 2. Symbol pool limit
// ---------------------------------------------------------------------------

#[test]
fn symbol_pool_limit_enforced() {
    let options = IrOptions {
        max_symbols: 100,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    for i in 0..100 {
        builder
            .intern_symbol(format!("sym_{i}"))
            .expect("symbol should fit");
    }

    let err = builder
        .intern_symbol("overflow")
        .expect_err("101st unique symbol must exceed limit");
    assert_eq!(err.kind, IrErrorKind::LimitExceeded);
    assert_eq!(err.message, "symbol pool limit exceeded");
    assert_eq!(
        err.detail.as_deref(),
        Some("max_symbols=100, attempted_count=101")
    );
}

// ---------------------------------------------------------------------------
// 3. Word program count limit
// ---------------------------------------------------------------------------

#[test]
fn word_program_count_limit_enforced() {
    let options = IrOptions {
        max_word_programs: 10,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    for _ in 0..10 {
        builder
            .add_word_program(WordProgram {
                ops: vec![WordProgramOp::FieldSplit],
                ..WordProgram::default()
            })
            .expect("word program should fit");
    }

    let err = builder
        .add_word_program(WordProgram {
            ops: vec![WordProgramOp::FieldSplit],
            ..WordProgram::default()
        })
        .expect_err("11th word program must exceed limit");
    assert_eq!(err.kind, IrErrorKind::LimitExceeded);
    assert_eq!(err.message, "word program count limit exceeded");
    assert_eq!(
        err.detail.as_deref(),
        Some("max_word_programs=10, attempted_count=11")
    );
}

// ---------------------------------------------------------------------------
// 4. Redirect program count limit
// ---------------------------------------------------------------------------

#[test]
fn redirect_program_count_limit_enforced() {
    let options = IrOptions {
        max_redirect_programs: 10,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    for _ in 0..10 {
        builder
            .add_redirect_program(RedirectProgram {
                ops: vec![RedirectProgramOp::Close { fd: 1 }],
                ..RedirectProgram::default()
            })
            .expect("redirect program should fit");
    }

    let err = builder
        .add_redirect_program(RedirectProgram {
            ops: vec![RedirectProgramOp::Close { fd: 1 }],
            ..RedirectProgram::default()
        })
        .expect_err("11th redirect program must exceed limit");
    assert_eq!(err.kind, IrErrorKind::LimitExceeded);
    assert_eq!(err.message, "redirect program count limit exceeded");
    assert_eq!(
        err.detail.as_deref(),
        Some("max_redirect_programs=10, attempted_count=11")
    );
}

// ---------------------------------------------------------------------------
// 5. Arith program count limit
// ---------------------------------------------------------------------------

#[test]
fn arith_program_count_limit_enforced() {
    let options = IrOptions {
        max_arith_programs: 10,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    for _ in 0..10 {
        builder
            .add_arith_program(ArithProgram {
                ops: vec![ArithProgramOp::PushLiteral(1)],
                ..ArithProgram::default()
            })
            .expect("arith program should fit");
    }

    let err = builder
        .add_arith_program(ArithProgram {
            ops: vec![ArithProgramOp::PushLiteral(1)],
            ..ArithProgram::default()
        })
        .expect_err("11th arith program must exceed limit");
    assert_eq!(err.kind, IrErrorKind::LimitExceeded);
    assert_eq!(err.message, "arith program count limit exceeded");
    assert_eq!(
        err.detail.as_deref(),
        Some("max_arith_programs=10, attempted_count=11")
    );
}

// ---------------------------------------------------------------------------
// 6. Nesting depth limit
// ---------------------------------------------------------------------------

#[test]
fn nesting_depth_limit_enforced() {
    // Generate a script with 10 nested `if` commands and set max_nesting_depth=5.
    let depth = 10;
    let mut script = String::new();
    for _ in 0..depth {
        script.push_str("if true; then ");
    }
    script.push_str("echo deep");
    for _ in 0..depth {
        script.push_str("; fi");
    }
    script.push('\n');

    let opts = IrOptions {
        max_nesting_depth: 5,
        ..IrOptions::default()
    };
    let result = try_lower_program_with(&script, opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// 7. Many unique strings via lowering
// ---------------------------------------------------------------------------

#[test]
fn many_unique_strings_via_lowering() {
    // Each unique command name creates at least one unique string.
    // With max_strings=50, a script with 100 unique command names should overflow.
    let mut script = String::new();
    for i in 0..100 {
        script.push_str(&format!("cmd_{i}\n"));
    }

    let opts = IrOptions {
        max_strings: 50,
        ..IrOptions::default()
    };
    let result = try_lower_program_with(&script, opts);
    assert_limit_exceeded(result);
}

// ---------------------------------------------------------------------------
// 8. Deduplicated strings do not count against limit
// ---------------------------------------------------------------------------

#[test]
fn deduplicated_strings_do_not_count_against_limit() {
    let options = IrOptions {
        max_strings: 10,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    // Intern 10 unique strings.
    for i in 0..10 {
        builder
            .intern_string(format!("str_{i}"))
            .expect("string should fit");
    }

    // Re-intern same 10 strings — should succeed (dedup).
    for i in 0..10 {
        builder
            .intern_string(format!("str_{i}"))
            .expect("deduplicated string should not count against limit");
    }

    // 11th unique string must fail.
    let err = builder
        .intern_string("overflow")
        .expect_err("11th unique string must exceed limit");
    assert_eq!(err.kind, IrErrorKind::LimitExceeded);
}

// ---------------------------------------------------------------------------
// 9. Deduplicated symbols do not count against limit
// ---------------------------------------------------------------------------

#[test]
fn deduplicated_symbols_do_not_count_against_limit() {
    let options = IrOptions {
        max_symbols: 10,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    // Intern 10 unique symbols.
    for i in 0..10 {
        builder
            .intern_symbol(format!("sym_{i}"))
            .expect("symbol should fit");
    }

    // Re-intern same 10 symbols — should succeed (dedup).
    for i in 0..10 {
        builder
            .intern_symbol(format!("sym_{i}"))
            .expect("deduplicated symbol should not count against limit");
    }

    // 11th unique symbol must fail.
    let err = builder
        .intern_symbol("overflow")
        .expect_err("11th unique symbol must exceed limit");
    assert_eq!(err.kind, IrErrorKind::LimitExceeded);
}
