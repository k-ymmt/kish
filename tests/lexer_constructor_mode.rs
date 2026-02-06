use kish::lexer::{Lexer, LexerMode};

#[test]
fn lexer_new_sets_mode() {
    let lexer = Lexer::new("echo hi", LexerMode::HereDocPending);
    assert_eq!(lexer.mode(), LexerMode::HereDocPending);
}
