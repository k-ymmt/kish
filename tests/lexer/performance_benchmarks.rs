use kish::lexer::{LexStep, Lexer, LexerMode};
use std::time::Instant;

#[test]
#[ignore]
fn bench_long_line_tokenization_ignored() {
    let input = format!("{}\n", "a".repeat(512_000));
    let iterations = 20usize;

    let started = Instant::now();
    for _ in 0..iterations {
        let mut lexer = Lexer::new(&input, LexerMode::Normal);
        let mut token_count = 0usize;
        loop {
            match lexer.next_token().expect("scan should succeed") {
                LexStep::Token(_) => token_count = token_count.saturating_add(1),
                LexStep::Recoverable(_) => panic!("unexpected recoverable state"),
                LexStep::EndOfInput => break,
            }
        }
        assert!(token_count >= 2);
    }
    eprintln!(
        "bench_long_line_tokenization_ignored: iterations={iterations}, elapsed_ms={}",
        started.elapsed().as_millis()
    );
}

#[test]
#[ignore]
fn bench_nested_substitution_tokenization_ignored() {
    let mut nested = "x".to_string();
    for _ in 0..48 {
        nested = format!("$({nested})");
    }
    let input = format!("{nested}\n");
    let iterations = 200usize;

    let started = Instant::now();
    for _ in 0..iterations {
        let mut lexer = Lexer::new(&input, LexerMode::Normal);
        loop {
            match lexer.next_token().expect("scan should succeed") {
                LexStep::Token(_) => {}
                LexStep::Recoverable(_) => panic!("unexpected recoverable state"),
                LexStep::EndOfInput => break,
            }
        }
    }
    eprintln!(
        "bench_nested_substitution_tokenization_ignored: iterations={iterations}, elapsed_ms={}",
        started.elapsed().as_millis()
    );
}
