//! HIR builder utilities.

use crate::lexer::Span;

use crate::ir::hir::{HirCommand, HirModule};

/// Lightweight builder for assembling one HIR module.
#[derive(Debug, Default)]
pub struct HirBuilder {
    commands: Vec<HirCommand>,
    span: Option<Span>,
}

impl HirBuilder {
    /// Creates an empty builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets module-wide source span.
    pub fn set_span(&mut self, span: Span) {
        self.span = Some(span);
    }

    /// Appends one command in source order.
    pub fn push_command(&mut self, command: HirCommand) {
        self.commands.push(command);
    }

    /// Finalizes a HIR module.
    pub fn build(self) -> HirModule {
        HirModule {
            commands: self.commands,
            span: self.span,
        }
    }
}
