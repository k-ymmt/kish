use kish::lexer::{
    LexStep, Lexer, LexerMode, OperatorKind, QuoteMarker, QuoteProvenance, SubstitutionKind,
    SubstitutionMarker, Token, TokenKind,
};
use std::fs;
use std::path::PathBuf;

fn fixture_path(case: &str, extension: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("lexer")
        .join("golden")
        .join(format!("{case}.{extension}"))
}

fn load_case_input(case: &str) -> String {
    let path = fixture_path(case, "input");
    fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("failed to read input fixture {}: {error}", path.display()))
}

fn load_case_expected(case: &str) -> String {
    let path = fixture_path(case, "golden");
    fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("failed to read golden fixture {}: {error}", path.display()))
}

fn render_operator_kind(kind: OperatorKind) -> &'static str {
    match kind {
        OperatorKind::AndIf => "AndIf",
        OperatorKind::OrIf => "OrIf",
        OperatorKind::DoubleSemicolon => "DoubleSemicolon",
        OperatorKind::SemicolonAmpersand => "SemicolonAmpersand",
        OperatorKind::HereDoc => "HereDoc",
        OperatorKind::HereDocStripTabs => "HereDocStripTabs",
        OperatorKind::AppendOutput => "AppendOutput",
        OperatorKind::DupInput => "DupInput",
        OperatorKind::DupOutput => "DupOutput",
        OperatorKind::ReadWrite => "ReadWrite",
        OperatorKind::Clobber => "Clobber",
        OperatorKind::Pipe => "Pipe",
        OperatorKind::Semicolon => "Semicolon",
        OperatorKind::Ampersand => "Ampersand",
        OperatorKind::LeftParen => "LeftParen",
        OperatorKind::RightParen => "RightParen",
        OperatorKind::Less => "Less",
        OperatorKind::Greater => "Greater",
    }
}

fn render_token_kind(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Token => "Token".to_string(),
        TokenKind::Newline => "Newline".to_string(),
        TokenKind::Operator(kind) => format!("Operator({})", render_operator_kind(*kind)),
    }
}

fn render_quote_provenance(provenance: QuoteProvenance) -> &'static str {
    match provenance {
        QuoteProvenance::BackslashEscaped => "BackslashEscaped",
        QuoteProvenance::SingleQuoted => "SingleQuoted",
        QuoteProvenance::DoubleQuoted => "DoubleQuoted",
        QuoteProvenance::DollarSingleQuoted => "DollarSingleQuoted",
    }
}

fn render_substitution_kind(kind: SubstitutionKind) -> &'static str {
    match kind {
        SubstitutionKind::ParameterExpansion => "ParameterExpansion",
        SubstitutionKind::CommandSubstitution => "CommandSubstitution",
        SubstitutionKind::BackquotedCommandSubstitution => "BackquotedCommandSubstitution",
        SubstitutionKind::ArithmeticExpansion => "ArithmeticExpansion",
    }
}

fn render_lexeme(token: &Token) -> String {
    token
        .lexeme
        .chars()
        .flat_map(|ch| ch.escape_default())
        .collect()
}

fn render_quote_markers(markers: &[QuoteMarker]) -> String {
    if markers.is_empty() {
        return "-".to_string();
    }

    let mut entries: Vec<(u32, u32, &'static str)> = markers
        .iter()
        .map(|marker| {
            (
                marker.range.start.value(),
                marker.range.end.value(),
                render_quote_provenance(marker.provenance),
            )
        })
        .collect();
    entries.sort_by(|left, right| (left.0, left.1, left.2).cmp(&(right.0, right.1, right.2)));

    entries
        .into_iter()
        .map(|(start, end, provenance)| format!("{provenance}@{start}..{end}"))
        .collect::<Vec<_>>()
        .join(";")
}

fn render_substitution_markers(markers: &[SubstitutionMarker]) -> String {
    if markers.is_empty() {
        return "-".to_string();
    }

    let mut entries: Vec<(u32, u32, &'static str)> = markers
        .iter()
        .map(|marker| {
            (
                marker.range.start.value(),
                marker.range.end.value(),
                render_substitution_kind(marker.kind),
            )
        })
        .collect();
    entries.sort_by(|left, right| (left.0, left.1, left.2).cmp(&(right.0, right.1, right.2)));

    entries
        .into_iter()
        .map(|(start, end, kind)| format!("{kind}@{start}..{end}"))
        .collect::<Vec<_>>()
        .join(";")
}

fn render_tokens(input: &str) -> String {
    let mut lexer = Lexer::new(input, LexerMode::Normal);
    let mut lines = Vec::new();
    let mut index = 0usize;

    loop {
        match lexer.next_token() {
            Ok(LexStep::Token(token)) => {
                let line = format!(
                    "{index}\t{}\t{}\t{}\t{}",
                    render_token_kind(&token.kind),
                    render_lexeme(&token),
                    render_quote_markers(&token.quote_markers),
                    render_substitution_markers(&token.substitution_markers),
                );
                lines.push(line);
                index = index.saturating_add(1);
            }
            Ok(LexStep::Recoverable(error)) => {
                panic!("unexpected recoverable lexer state: {error:?}")
            }
            Ok(LexStep::EndOfInput) => break,
            Err(error) => panic!("unexpected fatal lexer error: {error:?}"),
        }
    }

    if lines.is_empty() {
        String::new()
    } else {
        format!("{}\n", lines.join("\n"))
    }
}

fn assert_case(case: &str) {
    let input = load_case_input(case);
    let expected = load_case_expected(case);
    let actual = render_tokens(&input);

    assert_eq!(actual, expected, "golden mismatch for case `{case}`");
}

#[test]
fn simple_command_case_matches_golden() {
    assert_case("simple_command");
}

#[test]
fn pipeline_list_case_matches_golden() {
    assert_case("pipeline_list");
}

#[test]
fn nested_substitution_case_matches_golden() {
    assert_case("nested_substitution");
}

#[test]
fn mixed_quotes_case_matches_golden() {
    assert_case("mixed_quotes");
}

#[test]
fn multiple_heredocs_case_matches_golden() {
    assert_case("multiple_heredocs");
}
