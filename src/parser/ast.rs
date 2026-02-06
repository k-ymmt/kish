//! AST contracts for parser phases.
//!
//! These nodes are intentionally lightweight in Phase 0-2. They provide stable
//! public shapes for later grammar and lowering phases.

use crate::lexer::{OperatorKind, Span, Token};
use crate::parser::arena::AstArena;
use crate::parser::error::ParseError;

/// Root parser output for full-program parsing.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ProgramAst {
    /// Complete commands parsed from input.
    pub complete_commands: Vec<CompleteCommandAst>,
    /// Span covering the full parsed program, when available.
    pub span: Option<Span>,
}

/// One complete command as defined by POSIX grammar.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompleteCommandAst {
    /// Main command list.
    pub list: ListAst,
    /// Optional list terminator (`;` or `&`).
    pub separator_op: Option<OperatorKind>,
    /// Source span for this command.
    pub span: Span,
}

/// Command list (`and_or` separated by list separators).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ListAst {
    /// Sequence of `and_or` groups in source order.
    pub and_ors: Vec<AndOrAst>,
    /// Source span for this list.
    pub span: Span,
}

/// AND-OR command chain node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AndOrAst {
    /// First pipeline in the chain.
    pub head: PipelineAst,
    /// Remaining linked pipelines with connective operator.
    pub tail: Vec<(OperatorKind, PipelineAst)>,
    /// Source span for this `and_or` group.
    pub span: Span,
}

/// Pipeline node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PipelineAst {
    /// Whether this pipeline is prefixed by logical negation (`!`).
    pub negated: bool,
    /// Commands in pipeline order.
    pub commands: Vec<CommandAst>,
    /// Source span for this pipeline.
    pub span: Span,
}

/// Command node family.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommandAst {
    /// Simple command form.
    Simple(SimpleCommandAst),
    /// Compound command form.
    Compound(CompoundCommandAst),
    /// Function definition form.
    FunctionDefinition(FunctionDefinitionAst),
}

/// Simple command placeholder node.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SimpleCommandAst {
    /// Assignment prefix words.
    pub assignments: Vec<AssignmentWordAst>,
    /// Word arguments in command order.
    pub words: Vec<WordAst>,
    /// Redirects associated with this command in encounter order.
    pub redirects: Vec<RedirectAst>,
    /// Source span for this command.
    pub span: Option<Span>,
}

/// Compound command node family.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompoundCommandAst {
    /// Subshell command (`(...)`).
    Subshell(ListAst),
    /// Brace-group command (`{ ... }`).
    BraceGroup(ListAst),
    /// `if` command.
    If(IfClauseAst),
    /// `for` command.
    For(ForClauseAst),
    /// `while` command.
    While(WhileClauseAst),
    /// `until` command.
    Until(UntilClauseAst),
    /// `case` command.
    Case(CaseClauseAst),
}

/// Function definition node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinitionAst {
    /// Function name as lexical word.
    pub name: WordAst,
    /// Function body command.
    pub body: Box<CommandAst>,
    /// Redirects attached to function body.
    pub redirects: Vec<RedirectAst>,
    /// Source span for the full definition.
    pub span: Span,
}

/// Redirect node with preserved source ordering metadata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RedirectAst {
    /// Optional leading file descriptor (or future location designator).
    pub fd_or_location: Option<Token>,
    /// Redirect operator.
    pub operator: OperatorKind,
    /// Redirect target word.
    pub target: WordAst,
    /// Source span for this redirect.
    pub span: Span,
}

/// Word node with lexical metadata preserved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WordAst {
    /// Original lexer token preserving quote/substitution metadata.
    pub token: Token,
    /// Source span for this word.
    pub span: Span,
}

/// Assignment word node (`name=value`) placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignmentWordAst {
    /// Original lexer token.
    pub token: Token,
    /// Assignment name.
    pub name: String,
    /// Assignment value bytes as a UTF-8 string.
    pub value: String,
    /// Source span for this assignment.
    pub span: Span,
}

/// `if` clause placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfClauseAst {
    /// Condition command list.
    pub condition: ListAst,
    /// Then-body command list.
    pub then_body: ListAst,
    /// Optional else-body command list.
    pub else_body: Option<ListAst>,
    /// Source span for this `if` clause.
    pub span: Span,
}

/// `for` clause placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForClauseAst {
    /// Loop variable name.
    pub name: WordAst,
    /// Optional explicit iteration word list.
    pub words: Vec<WordAst>,
    /// Loop body.
    pub body: ListAst,
    /// Source span for this `for` clause.
    pub span: Span,
}

/// `while` clause placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileClauseAst {
    /// Loop condition list.
    pub condition: ListAst,
    /// Loop body list.
    pub body: ListAst,
    /// Source span for this `while` clause.
    pub span: Span,
}

/// `until` clause placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UntilClauseAst {
    /// Loop condition list.
    pub condition: ListAst,
    /// Loop body list.
    pub body: ListAst,
    /// Source span for this `until` clause.
    pub span: Span,
}

/// `case` clause placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseClauseAst {
    /// Subject word.
    pub word: WordAst,
    /// Source span for this `case` clause.
    pub span: Span,
}

/// Lightweight AST builder backed by [`AstArena`] accounting.
pub struct AstBuilder<'a> {
    arena: &'a mut AstArena,
}

impl<'a> AstBuilder<'a> {
    /// Creates an AST builder from an arena allocator.
    pub fn new(arena: &'a mut AstArena) -> Self {
        Self { arena }
    }

    /// Builds a word node from a lexer token.
    pub fn word_from_token(&mut self, token: Token) -> Result<WordAst, ParseError> {
        self.reserve_node()?;
        Ok(WordAst {
            span: token.span,
            token,
        })
    }

    /// Builds an assignment node from a lexer token and split payload.
    pub fn assignment_word_from_parts(
        &mut self,
        token: Token,
        name: impl Into<String>,
        value: impl Into<String>,
    ) -> Result<AssignmentWordAst, ParseError> {
        self.reserve_node()?;
        Ok(AssignmentWordAst {
            span: token.span,
            token,
            name: name.into(),
            value: value.into(),
        })
    }

    /// Builds a redirect node preserving left-to-right encounter order.
    pub fn redirect(
        &mut self,
        fd_or_location: Option<Token>,
        operator: OperatorKind,
        target: WordAst,
        span: Span,
    ) -> Result<RedirectAst, ParseError> {
        self.reserve_node()?;
        Ok(RedirectAst {
            fd_or_location,
            operator,
            target,
            span,
        })
    }

    /// Builds a simple-command node.
    pub fn simple_command(
        &mut self,
        assignments: Vec<AssignmentWordAst>,
        words: Vec<WordAst>,
        redirects: Vec<RedirectAst>,
        span: Option<Span>,
    ) -> Result<SimpleCommandAst, ParseError> {
        self.reserve_node()?;
        Ok(SimpleCommandAst {
            assignments,
            words,
            redirects,
            span,
        })
    }

    /// Wraps a simple command into a command node.
    pub fn command_simple(&mut self, command: SimpleCommandAst) -> Result<CommandAst, ParseError> {
        self.reserve_node()?;
        Ok(CommandAst::Simple(command))
    }

    /// Builds a pipeline node.
    pub fn pipeline(
        &mut self,
        negated: bool,
        commands: Vec<CommandAst>,
        span: Span,
    ) -> Result<PipelineAst, ParseError> {
        self.reserve_node()?;
        Ok(PipelineAst {
            negated,
            commands,
            span,
        })
    }

    /// Builds an and-or node.
    pub fn and_or(
        &mut self,
        head: PipelineAst,
        tail: Vec<(OperatorKind, PipelineAst)>,
        span: Span,
    ) -> Result<AndOrAst, ParseError> {
        self.reserve_node()?;
        Ok(AndOrAst { head, tail, span })
    }

    /// Builds a list node.
    pub fn list(&mut self, and_ors: Vec<AndOrAst>, span: Span) -> Result<ListAst, ParseError> {
        self.reserve_node()?;
        Ok(ListAst { and_ors, span })
    }

    /// Builds a complete-command node.
    pub fn complete_command(
        &mut self,
        list: ListAst,
        separator_op: Option<OperatorKind>,
        span: Span,
    ) -> Result<CompleteCommandAst, ParseError> {
        self.reserve_node()?;
        Ok(CompleteCommandAst {
            list,
            separator_op,
            span,
        })
    }

    /// Builds a program node.
    pub fn program(
        &mut self,
        complete_commands: Vec<CompleteCommandAst>,
        span: Option<Span>,
    ) -> Result<ProgramAst, ParseError> {
        self.reserve_node()?;
        Ok(ProgramAst {
            complete_commands,
            span,
        })
    }

    /// Returns current number of allocated AST nodes.
    pub fn allocated_nodes(&self) -> usize {
        self.arena.allocated_nodes()
    }

    fn reserve_node(&mut self) -> Result<(), ParseError> {
        self.arena.allocate().map(|_| ()).map_err(ParseError::from)
    }
}
