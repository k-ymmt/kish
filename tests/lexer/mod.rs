#[allow(dead_code)]
#[path = "../../src/lexer/span.rs"]
pub mod span;

mod boundary_api;
mod alias_substitution;
mod comment_handling;
mod constructor_mode;
mod cursor_position;
mod error_types;
mod heredoc_mode_machine;
mod line_continuation;
mod newline_preservation;
mod next_token_behavior;
mod operator_scanning;
mod quote_scanning;
mod source_span_types;
mod substitution_scanning;
mod token_model;
mod word_boundaries;
