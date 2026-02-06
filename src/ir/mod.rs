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
    ArithProgramOp, EncodedCodeObject, EncodedModule, Instruction, RedirectProgramOp, WordProgramOp,
};
pub use encode::encode_module;
pub use error::{IrError, IrErrorKind};
pub use ids::{
    ArithProgramId, CodeObjectId, ConstId, LabelId, LocalId, RedirectProgramId, StringId, SymbolId,
    WordProgramId,
};
pub use lower::LoweringContext;
pub use program::{
    ArithProgram, CodeObject, ConstValue, IrModule, IrOptions, RedirectProgram, WordProgram,
};
pub use verify::verify_module;
