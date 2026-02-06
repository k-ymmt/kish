#[allow(dead_code)]
#[path = "../../src/lexer/span.rs"]
pub mod span;

mod alias_substitution;
mod boundary_api;
mod comment_handling;
mod constructor_mode;
mod cursor_position;
mod diagnostic_stability;
mod error_types;
mod golden_tokenization;
mod guardrails;
mod heredoc_mode_machine;
mod line_continuation;
mod newline_preservation;
mod next_token_behavior;
mod operator_scanning;
mod parser_handoff_contract;
mod performance_benchmarks;
mod property_cursor_invariants;
mod property_scanning;
mod quote_scanning;
mod source_span_types;
mod substitution_scanning;
mod token_model;
mod word_boundaries;
