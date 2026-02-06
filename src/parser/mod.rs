//! POSIX-first parser scaffolding and contracts.
//!
//! Phase 0-2 provides parser-facing contracts, a lexer-backed token stream,
//! and contextual token reclassification.

pub mod arena;
pub mod ast;
pub mod classifier;
pub mod error;
pub mod parser;
pub mod recovery;
pub mod token_stream;

pub use arena::{ArenaError, AstArena, AstNodeId};
pub use ast::{
    AndOrAst, AssignmentWordAst, CaseClauseAst, CommandAst, CompleteCommandAst, CompoundCommandAst,
    ForClauseAst, FunctionDefinitionAst, IfClauseAst, ListAst, PipelineAst, ProgramAst,
    RedirectAst, SimpleCommandAst, UntilClauseAst, WhileClauseAst, WordAst,
};
pub use classifier::{
    ClassificationContext, ClassificationOptions, ClassifiedToken, ClassifiedTokenKind, Classifier,
    NameContext, ReservedWord, ReservedWordPolicy,
};
pub use error::{ParseError, ParseErrorKind};
pub use parser::{ParseOptions, ParseStep, Parser};
pub use recovery::{NeedMoreInputReason, ParseDiagnostic, ParseDiagnosticSeverity};
pub use token_stream::{MAX_LOOKAHEAD, TokenStream, TokenStreamError};
