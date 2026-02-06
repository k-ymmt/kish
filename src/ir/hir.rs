//! High-level IR (HIR) contracts used before VM instruction lowering.

use crate::lexer::{Span, Token};

/// Source-origin metadata for one HIR node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirOrigin {
    /// Origin family of the source parser node.
    pub kind: HirOriginKind,
}

impl HirOrigin {
    /// Creates an origin marker.
    pub const fn new(kind: HirOriginKind) -> Self {
        Self { kind }
    }
}

/// Source-origin families tracked on HIR nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HirOriginKind {
    /// Originates from `ProgramAst`.
    ProgramAst,
    /// Originates from `CompleteCommandAst`.
    CompleteCommandAst,
    /// Originates from `ListAst`.
    ListAst,
    /// Originates from `AndOrAst`.
    AndOrAst,
    /// Originates from `PipelineAst`.
    PipelineAst,
    /// Originates from `CommandAst`.
    CommandAst,
    /// Originates from `SimpleCommandAst`.
    SimpleCommandAst,
    /// Originates from `CompoundCommandNodeAst`.
    CompoundCommandNodeAst,
    /// Originates from `CompoundCommandAst`.
    CompoundCommandAst,
    /// Originates from `FunctionDefinitionAst`.
    FunctionDefinitionAst,
    /// Originates from `RedirectAst`.
    RedirectAst,
    /// Originates from `WordAst`.
    WordAst,
    /// Originates from `AssignmentWordAst`.
    AssignmentWordAst,
    /// Originates from `IfClauseAst`.
    IfClauseAst,
    /// Originates from `ForClauseAst`.
    ForClauseAst,
    /// Originates from `WhileClauseAst`.
    WhileClauseAst,
    /// Originates from `UntilClauseAst`.
    UntilClauseAst,
    /// Originates from `CaseClauseAst`.
    CaseClauseAst,
    /// Originates from `CaseItemAst`.
    CaseItemAst,
    /// Originates from `CaseTerminatorAst`.
    CaseTerminatorAst,
}

/// List terminator semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HirListTerminator {
    /// Sequential execution (`;`).
    Semicolon,
    /// Asynchronous execution (`&`).
    Ampersand,
}

/// Connector semantics for AND-OR chains.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HirAndOrConnector {
    /// Short-circuit on non-zero status (`&&`).
    AndIf,
    /// Short-circuit on zero status (`||`).
    OrIf,
}

/// Case item terminator semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HirCaseTerminator {
    /// Stop `case` evaluation (`;;`).
    Break,
    /// Fall through to next item (`;&`).
    Fallthrough,
}

/// Redirect operator semantics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HirRedirectOperator {
    /// Redirect input from file (`<`).
    Input,
    /// Redirect output to file (`>`).
    Output,
    /// Redirect output append (`>>`).
    AppendOutput,
    /// Duplicate input fd (`<&`).
    DupInput,
    /// Duplicate output fd (`>&`).
    DupOutput,
    /// Open file for read/write (`<>`).
    ReadWrite,
    /// Force output clobber (`>|`).
    Clobber,
    /// Here-document (`<<`).
    HereDoc,
    /// Here-document with tab stripping (`<<-`).
    HereDocStripTabs,
}

/// Root HIR output for full-program lowering.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirProgram {
    /// Complete commands in source order.
    pub complete_commands: Vec<HirCompleteCommand>,
    /// Span covering the full parsed program, when available.
    pub span: Option<Span>,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

impl Default for HirProgram {
    fn default() -> Self {
        Self {
            complete_commands: Vec::new(),
            span: None,
            origin: HirOrigin::new(HirOriginKind::ProgramAst),
        }
    }
}

/// Backward-compatible alias kept from Phase 0 scaffolding.
pub type HirModule = HirProgram;

/// One complete command as defined by POSIX grammar.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirCompleteCommand {
    /// Main command list.
    pub list: HirList,
    /// Optional list terminator (`;` or `&`).
    pub separator_op: Option<HirListTerminator>,
    /// Source span for this command.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// Command list (`and_or` separated by list separators).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirList {
    /// Sequence of `and_or` groups in source order.
    pub and_ors: Vec<HirAndOr>,
    /// Source span for this list.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// AND-OR command chain node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirAndOr {
    /// First pipeline in the chain.
    pub head: HirPipeline,
    /// Remaining linked pipelines with connective operator.
    pub tail: Vec<(HirAndOrConnector, HirPipeline)>,
    /// Source span for this `and_or` group.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// Pipeline node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirPipeline {
    /// Whether this pipeline is prefixed by logical negation (`!`).
    pub negated: bool,
    /// Commands in pipeline order.
    pub commands: Vec<HirCommand>,
    /// Source span for this pipeline.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// Command node family.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirCommand {
    /// Simple command form.
    Simple(HirSimpleCommand),
    /// Compound command form.
    Compound(HirCompoundCommandNode),
    /// Function definition form.
    FunctionDefinition(HirFunctionDefinition),
}

/// Simple command node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirSimpleCommand {
    /// Assignment prefix words.
    pub assignments: Vec<HirAssignment>,
    /// Word arguments in command order.
    pub words: Vec<HirWord>,
    /// Redirects associated with this command in encounter order.
    pub redirects: Vec<HirRedirect>,
    /// Source span for this command.
    pub span: Option<Span>,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// Compound command node family.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirCompoundCommand {
    /// Subshell command (`(...)`).
    Subshell(HirList),
    /// Brace-group command (`{ ... }`).
    BraceGroup(HirList),
    /// `if` command.
    If(HirIfClause),
    /// `for` command.
    For(HirForClause),
    /// `while` command.
    While(HirWhileClause),
    /// `until` command.
    Until(HirUntilClause),
    /// `case` command.
    Case(HirCaseClause),
}

/// Compound command wrapper with redirects and source span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirCompoundCommandNode {
    /// Compound command kind payload.
    pub kind: HirCompoundCommand,
    /// Redirects associated with this command in encounter order.
    pub redirects: Vec<HirRedirect>,
    /// Source span for the full compound command with redirects.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// Function definition node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirFunctionDefinition {
    /// Function name as lexical word.
    pub name: HirWord,
    /// Function body command.
    pub body: Box<HirCompoundCommandNode>,
    /// Redirects attached to function body.
    pub redirects: Vec<HirRedirect>,
    /// Source span for the full definition.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// Redirect node with preserved source ordering metadata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirRedirect {
    /// Optional leading file descriptor token.
    pub fd_or_location: Option<Token>,
    /// Parsed file descriptor when present.
    pub fd: Option<u16>,
    /// Redirect operator.
    pub operator: HirRedirectOperator,
    /// Redirect target word.
    pub target: HirWord,
    /// Source span for this redirect.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// Word node with lexical metadata preserved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirWord {
    /// Original lexer token preserving quote/substitution metadata.
    pub token: Token,
    /// Source span for this word.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// Assignment word node (`name=value`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirAssignment {
    /// Original lexer token.
    pub token: Token,
    /// Assignment name.
    pub name: String,
    /// Assignment value bytes as a UTF-8 string.
    pub value: String,
    /// Source span for this assignment.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// `if` clause node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirIfClause {
    /// Condition command list.
    pub condition: HirList,
    /// Then-body command list.
    pub then_body: HirList,
    /// Zero or more `elif` arms in encounter order.
    pub elif_arms: Vec<(HirList, HirList)>,
    /// Optional else-body command list.
    pub else_body: Option<HirList>,
    /// Source span for this `if` clause.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// `for` clause node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirForClause {
    /// Loop variable name.
    pub name: HirWord,
    /// Optional explicit iteration word list.
    pub words: Vec<HirWord>,
    /// Loop body.
    pub body: HirList,
    /// Source span for this `for` clause.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// `while` clause node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirWhileClause {
    /// Loop condition list.
    pub condition: HirList,
    /// Loop body list.
    pub body: HirList,
    /// Source span for this `while` clause.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// `until` clause node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirUntilClause {
    /// Loop condition list.
    pub condition: HirList,
    /// Loop body list.
    pub body: HirList,
    /// Source span for this `until` clause.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// `case` clause node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirCaseClause {
    /// Subject word.
    pub word: HirWord,
    /// Case items in source order.
    pub items: Vec<HirCaseItem>,
    /// Source span for this `case` clause.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}

/// `case` item payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirCaseItem {
    /// Pattern words (`a | b`).
    pub patterns: Vec<HirWord>,
    /// Optional body command list.
    pub body: Option<HirList>,
    /// Optional item terminator (`;;` or `;&`).
    pub terminator: Option<HirCaseTerminator>,
    /// Source span for this case item.
    pub span: Span,
    /// Source-origin metadata.
    pub origin: HirOrigin,
}
