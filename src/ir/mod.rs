//! Intermediate representation (IR) contracts for parser-to-VM lowering.
//!
//! Phase 0 provides compile-safe scaffolding and public signatures.

pub mod bytecode;
pub mod encode;
pub mod error;
pub mod hir;
pub mod hir_builder;
pub mod ids;
pub mod lower;
pub mod program;
pub mod verify;

pub use bytecode::{
    ArithCompoundOp, ArithProgramOp, BranchTarget, CommandDispatchHint, EncodedCodeObject,
    EncodedModule, Instruction, OpenMode, RedirectProgramOp, SourceMapEntry, WordProgramOp,
};
pub use encode::{
    DecodedInstruction, decode_arith_program_words, decode_code_object_words,
    decode_redirect_program_words, decode_word_program_words, encode_arith_program,
    encode_code_object, encode_module, encode_redirect_program, encode_word_program,
};
pub use error::{IrError, IrErrorKind};
pub use hir::{
    HirAndOr, HirAndOrConnector, HirAssignment, HirCaseClause, HirCaseItem, HirCaseTerminator,
    HirCommand, HirCompleteCommand, HirCompoundCommand, HirCompoundCommandNode, HirForClause,
    HirFunctionDefinition, HirIfClause, HirList, HirListTerminator, HirModule, HirOrigin,
    HirOriginKind, HirPipeline, HirProgram, HirRedirect, HirRedirectOperator, HirSimpleCommand,
    HirUntilClause, HirWhileClause, HirWord,
};
pub use hir_builder::HirBuilder;
pub use ids::{
    ArithProgramId, CodeObjectId, ConstId, LabelId, LocalId, RedirectProgramId, StringId, SymbolId,
    WordProgramId,
};
pub use lower::LoweringContext;
pub use program::{
    ArithProgram, CodeObject, CodeObjectBuilder, ConstValue, IrModule, IrModuleBuilder, IrOptions,
    RedirectProgram, WordProgram,
};
pub use verify::{VerifyWarning, VerifyWarningKind, verify_module, verify_module_debug};
