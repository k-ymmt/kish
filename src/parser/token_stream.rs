//! Lexer-backed parser token stream with bounded lookahead.

use std::collections::VecDeque;

use crate::lexer::{LexStep, Lexer, RecoverableLexError, Token};

use crate::parser::error::ParseError;
use crate::parser::recovery::NeedMoreInputReason;

/// Maximum supported lookahead index (`peek(0..=3)`).
pub const MAX_LOOKAHEAD: usize = 3;

/// Token stream failures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenStreamError {
    /// Input is incomplete and may continue interactively.
    NeedMoreInput(NeedMoreInputReason),
    /// Non-recoverable parse error.
    Parse(ParseError),
}

/// Fixed-lookahead token stream over the lexer.
pub struct TokenStream<'a> {
    lexer: Lexer<'a>,
    lookahead: VecDeque<Token>,
    reached_end_of_input: bool,
}

impl<'a> TokenStream<'a> {
    /// Creates a stream from an existing lexer.
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            lookahead: VecDeque::new(),
            reached_end_of_input: false,
        }
    }

    /// Peeks a token by bounded lookahead index.
    ///
    /// `peek(0)` is the next token to be consumed by [`Self::next`].
    pub fn peek(&mut self, n: usize) -> Result<Option<&Token>, TokenStreamError> {
        if n > MAX_LOOKAHEAD {
            return Err(TokenStreamError::Parse(ParseError::lookahead_exceeded(
                n,
                MAX_LOOKAHEAD,
            )));
        }

        self.fill_to(n)?;
        Ok(self.lookahead.get(n))
    }

    /// Consumes and returns the next token from the stream.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<Option<Token>, TokenStreamError> {
        self.fill_to(0)?;
        Ok(self.lookahead.pop_front())
    }

    /// Consumes and returns the next token when predicate matches.
    pub fn consume_if<P>(&mut self, predicate: P) -> Result<Option<Token>, TokenStreamError>
    where
        P: Fn(&Token) -> bool,
    {
        let should_consume = match self.peek(0)? {
            Some(token) => predicate(token),
            None => false,
        };

        if should_consume {
            self.next()
        } else {
            Ok(None)
        }
    }

    /// Consumes one token and validates it with a predicate.
    pub fn expect<P>(
        &mut self,
        predicate: P,
        expected_label: impl Into<String>,
    ) -> Result<Token, TokenStreamError>
    where
        P: Fn(&Token) -> bool,
    {
        let expected_label = expected_label.into();
        let Some(token) = self.next()? else {
            return Err(TokenStreamError::Parse(
                ParseError::unexpected_end_of_input([expected_label]),
            ));
        };

        if predicate(&token) {
            return Ok(token);
        }

        Err(TokenStreamError::Parse(ParseError::unexpected_token(
            &token,
            [expected_label],
        )))
    }

    fn fill_to(&mut self, index: usize) -> Result<(), TokenStreamError> {
        while self.lookahead.len() <= index && !self.reached_end_of_input {
            match self.lexer.next_token() {
                Ok(LexStep::Token(token)) => self.lookahead.push_back(token),
                Ok(LexStep::EndOfInput) => self.reached_end_of_input = true,
                Ok(LexStep::Recoverable(error)) => {
                    return Err(TokenStreamError::NeedMoreInput(
                        NeedMoreInputReason::from_recoverable_lexer_error(&error),
                    ));
                }
                Err(error) => {
                    if let Some(reason) = NeedMoreInputReason::from_fatal_lexer_error(&error) {
                        return Err(TokenStreamError::NeedMoreInput(reason));
                    }
                    return Err(TokenStreamError::Parse(ParseError::lexer_error(&error)));
                }
            }
        }

        Ok(())
    }
}

impl From<RecoverableLexError> for TokenStreamError {
    fn from(value: RecoverableLexError) -> Self {
        Self::NeedMoreInput(NeedMoreInputReason::from_recoverable_lexer_error(&value))
    }
}
