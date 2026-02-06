#[allow(dead_code)]
#[path = "../../src/lexer/span.rs"]
pub mod span;

mod boundary_api;
mod constructor_mode;
mod cursor_position;
mod error_types;
mod line_continuation;
mod next_token_behavior;
mod source_span_types;
mod token_model;
