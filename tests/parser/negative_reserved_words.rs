use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{CommandAst, ParseErrorKind, ParseOptions, ParseStep, Parser, TokenStream};

fn parser_for(input: &str, options: ParseOptions) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let token_stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), options, token_stream)
}

fn assert_reserved_misuse_fails(input: &str) {
    let mut parser = parser_for(input, ParseOptions::default());
    let error = parser
        .parse_complete_command()
        .expect_err("invalid reserved-word usage should fail");
    assert!(
        matches!(
            error.kind,
            ParseErrorKind::UnexpectedToken | ParseErrorKind::UnexpectedEndOfInput
        ),
        "input: {input}, error: {error:?}"
    );
}

#[test]
fn standalone_reserved_words_fail_at_top_level() {
    let cases = [
        "then echo\n",
        "else echo\n",
        "fi\n",
        "do echo\n",
        "done\n",
        "esac\n",
        "in echo\n",
    ];

    for input in cases {
        assert_reserved_misuse_fails(input);
    }
}

#[test]
fn invalid_reserved_words_in_compound_contexts_fail() {
    let cases = [
        "if echo a; then else echo b; fi\n",
        "for i in do echo x; done\n",
    ];

    for input in cases {
        assert_reserved_misuse_fails(input);
    }
}

#[test]
fn reserved_lexemes_in_suffix_position_stay_plain_words() {
    let mut parser = parser_for(
        "echo then else fi do done in esac\n",
        ParseOptions::default(),
    );
    let step = parser
        .parse_complete_command()
        .expect("reserved-like suffix words should parse");

    let ParseStep::Complete(command) = step else {
        panic!("expected ParseStep::Complete");
    };
    let and_or = command.list.and_ors.first().expect("and_or exists");
    let pipeline = &and_or.head;
    let simple = match pipeline.commands.first().expect("command exists") {
        CommandAst::Simple(simple) => simple,
        other => panic!("expected simple command, got {other:?}"),
    };

    let words: Vec<&str> = simple
        .words
        .iter()
        .map(|word| word.token.lexeme.as_str())
        .collect();
    assert_eq!(
        words,
        vec!["echo", "then", "else", "fi", "do", "done", "in", "esac"]
    );
}
