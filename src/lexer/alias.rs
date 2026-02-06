//! Alias substitution transformer for parser-facing token streams.

use crate::lexer::diagnostics::FatalLexError;
use crate::lexer::span::{SourceId, Span};
use crate::lexer::token::{CompleteCommandTokens, LexStep, OperatorKind, Token, TokenKind};
use crate::lexer::{Lexer, LexerMode};
use std::collections::HashMap;

/// Alias expansion result with source mapping metadata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasExpansionResult {
    /// Expanded tokens in deterministic order.
    pub tokens: Vec<AliasExpandedToken>,
}

/// Token plus expansion-origin metadata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasExpandedToken {
    /// Expanded lexical token.
    pub token: Token,
    /// Origin mapping for this token.
    pub origin: AliasTokenOrigin,
}

/// Source origin for expanded tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AliasTokenOrigin {
    /// Token came directly from input stream.
    Original {
        /// Original input span.
        span: Span,
    },
    /// Token came from alias value re-tokenization.
    AliasExpansion {
        /// Alias name used for this expansion.
        alias_name: String,
        /// Span where alias expansion was triggered.
        expansion_site: Span,
        /// Synthetic source identifier assigned to alias value.
        synthetic_source_id: SourceId,
        /// Span inside synthetic source.
        synthetic_span: Span,
    },
}

/// Alias expansion options.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AliasExpansionOptions {
    /// Enables alias-blank continuation behavior.
    ///
    /// When enabled, if an alias value ends with a blank (`' '` or `'\t'`),
    /// the next token is eligible for alias expansion even if it is not in
    /// command-name position.
    pub allow_trailing_blank: bool,
    /// Maximum active alias expansion depth.
    pub max_expansion_depth: usize,
}

impl Default for AliasExpansionOptions {
    fn default() -> Self {
        Self {
            allow_trailing_blank: true,
            max_expansion_depth: 128,
        }
    }
}

/// Fatal alias-expansion failures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AliasExpansionError {
    /// Lexing alias value failed while re-tokenizing.
    Lex(FatalLexError),
    /// Alias expansion depth exceeded configured limit.
    ExpansionDepthExceeded { alias_name: String, limit: usize },
}

/// Alias substitution component.
#[derive(Debug, Clone, Default)]
pub struct AliasExpander {
    aliases: HashMap<String, String>,
    options: AliasExpansionOptions,
}

#[derive(Debug, Clone)]
struct ExpansionState {
    tokens: Vec<AliasExpandedToken>,
    command_position: bool,
}

impl AliasExpander {
    /// Creates an alias expander with default options.
    pub fn new(aliases: HashMap<String, String>) -> Self {
        Self::with_options(aliases, AliasExpansionOptions::default())
    }

    /// Creates an alias expander with explicit options.
    pub fn with_options(aliases: HashMap<String, String>, options: AliasExpansionOptions) -> Self {
        Self { aliases, options }
    }

    /// Returns configured alias map.
    pub fn aliases(&self) -> &HashMap<String, String> {
        &self.aliases
    }

    /// Expands aliases for complete-command tokens.
    pub fn expand_complete_command_tokens(
        &self,
        tokens: &CompleteCommandTokens,
    ) -> Result<AliasExpansionResult, AliasExpansionError> {
        self.expand_tokens(&tokens.tokens)
    }

    /// Expands aliases for token slices.
    pub fn expand_tokens(
        &self,
        tokens: &[Token],
    ) -> Result<AliasExpansionResult, AliasExpansionError> {
        let input_tokens: Vec<AliasExpandedToken> = tokens
            .iter()
            .cloned()
            .map(|token| {
                let span = token.span;
                AliasExpandedToken {
                    token,
                    origin: AliasTokenOrigin::Original { span },
                }
            })
            .collect();

        let mut active_aliases = Vec::new();
        let mut next_synthetic_source_id = 1u32;
        let expanded = self.expand_stream(
            &input_tokens,
            true,
            &mut active_aliases,
            &mut next_synthetic_source_id,
        )?;
        Ok(AliasExpansionResult {
            tokens: expanded.tokens,
        })
    }

    fn expand_stream(
        &self,
        input: &[AliasExpandedToken],
        initial_command_position: bool,
        active_aliases: &mut Vec<String>,
        next_synthetic_source_id: &mut u32,
    ) -> Result<ExpansionState, AliasExpansionError> {
        let mut output = Vec::new();
        let mut command_position = initial_command_position;
        let mut alias_continuation = false;

        for item in input {
            let token = &item.token;
            let allow_alias = command_position || alias_continuation;
            alias_continuation = false;

            if allow_alias
                && self.is_alias_candidate_token(token)
                && self.is_valid_alias_name(&token.lexeme)
            {
                if let Some(alias_value) = self.aliases.get(&token.lexeme) {
                    let alias_name = token.lexeme.clone();

                    if !active_aliases.iter().any(|active| active == &alias_name) {
                        if active_aliases.len() >= self.options.max_expansion_depth {
                            return Err(AliasExpansionError::ExpansionDepthExceeded {
                                alias_name,
                                limit: self.options.max_expansion_depth,
                            });
                        }

                        active_aliases.push(alias_name.clone());
                        let synthetic_source_id = SourceId::new(*next_synthetic_source_id);
                        *next_synthetic_source_id = next_synthetic_source_id.saturating_add(1);

                        let alias_tokens = self.retokenize_alias_value(
                            alias_value,
                            &alias_name,
                            token.span,
                            synthetic_source_id,
                        )?;
                        let nested = self.expand_stream(
                            &alias_tokens,
                            true,
                            active_aliases,
                            next_synthetic_source_id,
                        )?;
                        let _ = active_aliases.pop();

                        output.extend(nested.tokens);
                        command_position = nested.command_position;
                        if self.options.allow_trailing_blank && ends_with_blank(alias_value) {
                            alias_continuation = true;
                        }
                        continue;
                    }
                }
            }

            output.push(item.clone());
            command_position = command_position_after(&item.token);
        }

        Ok(ExpansionState {
            tokens: output,
            command_position,
        })
    }

    fn retokenize_alias_value(
        &self,
        value: &str,
        alias_name: &str,
        expansion_site: Span,
        synthetic_source_id: SourceId,
    ) -> Result<Vec<AliasExpandedToken>, AliasExpansionError> {
        let mut lexer = Lexer::new(value, LexerMode::Normal);
        let mut out = Vec::new();

        loop {
            match lexer.next_token() {
                Ok(LexStep::Token(mut token)) => {
                    let synthetic_span =
                        Span::new(synthetic_source_id, token.span.start, token.span.end);
                    token.span = synthetic_span;
                    out.push(AliasExpandedToken {
                        token,
                        origin: AliasTokenOrigin::AliasExpansion {
                            alias_name: alias_name.to_string(),
                            expansion_site,
                            synthetic_source_id,
                            synthetic_span,
                        },
                    });
                }
                Ok(LexStep::EndOfInput) => break,
                Ok(LexStep::Recoverable(_)) => continue,
                Err(error) => return Err(AliasExpansionError::Lex(error)),
            }
        }

        Ok(out)
    }

    fn is_alias_candidate_token(&self, token: &Token) -> bool {
        token.kind == TokenKind::Token && token.is_plain() && !token.lexeme.is_empty()
    }

    fn is_valid_alias_name(&self, name: &str) -> bool {
        let mut chars = name.chars();
        let Some(first) = chars.next() else {
            return false;
        };
        if !(first == '_' || first.is_ascii_alphabetic()) {
            return false;
        }
        chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
    }
}

fn ends_with_blank(value: &str) -> bool {
    value.ends_with(' ') || value.ends_with('\t')
}

fn command_position_after(token: &Token) -> bool {
    match token.kind {
        TokenKind::Newline => true,
        TokenKind::Operator(kind) => matches!(
            kind,
            OperatorKind::Pipe
                | OperatorKind::AndIf
                | OperatorKind::OrIf
                | OperatorKind::Semicolon
                | OperatorKind::DoubleSemicolon
                | OperatorKind::SemicolonAmpersand
                | OperatorKind::Ampersand
                | OperatorKind::LeftParen
        ),
        TokenKind::Token => false,
    }
}
