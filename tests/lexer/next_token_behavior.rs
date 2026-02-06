use kish::lexer::{LexStep, Lexer, LexerMode};

#[test]
fn next_token_smoke_does_not_panic() {
    let mut lexer = Lexer::new("echo hi\n", LexerMode::Normal);

    loop {
        let step = lexer.next_token().expect("Phase 0 scan should not fail");
        match step {
            LexStep::Token(_) => {}
            LexStep::Recoverable(_) => {}
            LexStep::EndOfInput => break,
        }
    }
}
