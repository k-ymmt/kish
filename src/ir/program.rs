//! VM-facing IR module containers and options.

use crate::ir::bytecode::{ArithProgramOp, Instruction, RedirectProgramOp, WordProgramOp};
use crate::ir::ids::{
    ArithProgramId, CodeObjectId, ConstId, RedirectProgramId, StringId, SymbolId, WordProgramId,
};

/// Resource guardrails for IR lowering and construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrOptions {
    /// Maximum number of VM instructions across all code objects.
    pub max_instructions: usize,
    /// Maximum entries in the constant pool.
    pub max_consts: usize,
    /// Maximum code-object count in one module.
    pub max_code_objects: usize,
    /// Maximum operations in one word-expansion subprogram.
    pub max_word_program_ops: usize,
    /// Maximum operations in one redirect subprogram.
    pub max_redirect_ops: usize,
    /// Maximum command arity accepted during lowering.
    pub max_arity: usize,
}

impl Default for IrOptions {
    fn default() -> Self {
        Self {
            max_instructions: 1_000_000,
            max_consts: 100_000,
            max_code_objects: 10_000,
            max_word_program_ops: 10_000,
            max_redirect_ops: 2_000,
            max_arity: 8_192,
        }
    }
}

/// Constant-pool entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    /// Reference to an interned string.
    String(StringId),
    /// Reference to an interned symbol.
    Symbol(SymbolId),
    /// Immediate signed integer literal.
    Integer(i64),
}

/// One VM code object before bytecode packing.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CodeObject {
    /// Stable code-object id.
    pub id: CodeObjectId,
    /// Typed instruction stream.
    pub instructions: Vec<Instruction>,
}

/// Word-expansion subprogram.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct WordProgram {
    /// Stable subprogram id.
    pub id: WordProgramId,
    /// Typed word-expansion operations.
    pub ops: Vec<WordProgramOp>,
}

/// Redirect-evaluation subprogram.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct RedirectProgram {
    /// Stable subprogram id.
    pub id: RedirectProgramId,
    /// Typed redirect operations.
    pub ops: Vec<RedirectProgramOp>,
}

/// Arithmetic-expansion subprogram.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ArithProgram {
    /// Stable subprogram id.
    pub id: ArithProgramId,
    /// Typed arithmetic operations.
    pub ops: Vec<ArithProgramOp>,
}

/// Whole typed IR module.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct IrModule {
    /// Interned string pool.
    pub string_pool: Vec<String>,
    /// Interned symbol pool.
    pub symbol_pool: Vec<String>,
    /// Constant pool.
    pub const_pool: Vec<ConstValue>,
    /// Typed VM code objects.
    pub code_objects: Vec<CodeObject>,
    /// Word-expansion subprograms.
    pub word_programs: Vec<WordProgram>,
    /// Redirect subprograms.
    pub redirect_programs: Vec<RedirectProgram>,
    /// Arithmetic subprograms.
    pub arith_programs: Vec<ArithProgram>,
}

impl IrModule {
    /// Creates an empty module.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns `true` when no code or pools are present.
    pub fn is_empty(&self) -> bool {
        self.string_pool.is_empty()
            && self.symbol_pool.is_empty()
            && self.const_pool.is_empty()
            && self.code_objects.is_empty()
            && self.word_programs.is_empty()
            && self.redirect_programs.is_empty()
            && self.arith_programs.is_empty()
    }

    /// Resolves a constant-pool handle.
    pub fn const_at(&self, id: ConstId) -> Option<&ConstValue> {
        self.const_pool.get(id.value() as usize)
    }
}
