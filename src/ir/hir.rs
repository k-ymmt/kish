//! High-level IR (HIR) contracts used before VM instruction lowering.

use crate::lexer::Span;

/// High-level IR module container.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct HirModule {
    /// Top-level commands in source order.
    pub commands: Vec<HirCommand>,
    /// Optional module-wide source span.
    pub span: Option<Span>,
}

/// HIR command family.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirCommand {
    /// Simple command form.
    Simple(HirSimpleCommand),
    /// Compound command form.
    Compound(HirCompoundCommand),
    /// Function definition form.
    Function(HirFunctionDefinition),
}

/// HIR simple command node.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct HirSimpleCommand {
    /// Assignment prefixes in source order.
    pub assignments: Vec<HirAssignment>,
    /// Command words in source order.
    pub words: Vec<HirWord>,
    /// Redirections in source order.
    pub redirects: Vec<HirRedirect>,
    /// Source span for this command.
    pub span: Option<Span>,
}

/// HIR compound command placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirCompoundCommand {
    /// Source span for this compound command.
    pub span: Span,
}

/// HIR function definition placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirFunctionDefinition {
    /// Function symbol name.
    pub name: String,
    /// Source span for this function definition.
    pub span: Span,
}

/// HIR word node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirWord {
    /// Source lexeme text.
    pub text: String,
    /// Source span for this word.
    pub span: Span,
}

/// HIR assignment node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirAssignment {
    /// Assignment variable name.
    pub name: String,
    /// Assignment value text.
    pub value: String,
    /// Source span for this assignment.
    pub span: Span,
}

/// HIR redirect node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirRedirect {
    /// Optional leading file descriptor.
    pub fd: Option<u16>,
    /// Redirect operator text.
    pub operator: String,
    /// Redirect target text.
    pub target: String,
    /// Source span for this redirect.
    pub span: Span,
}
