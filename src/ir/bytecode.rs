//! Typed VM IR instruction contracts and encoded bytecode containers.

use crate::ir::ids::{
    ArithProgramId, CodeObjectId, ConstId, LabelId, LocalId, RedirectProgramId, StringId, SymbolId,
    WordProgramId,
};
use crate::ir::program::ConstValue;

/// Typed VM instruction stream used before packing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    /// No-op placeholder.
    Nop,
    /// Pushes a constant onto the VM stack.
    PushConst(ConstId),
    /// Pops one stack value.
    Drop,
    /// Duplicates the stack top.
    Dup,
    /// Loads a local onto the stack.
    LocalGet(LocalId),
    /// Stores stack top into a local.
    LocalSet(LocalId),
    /// Unconditional branch.
    Jmp(LabelId),
    /// Branch if top-of-stack is zero.
    JmpIfZero(LabelId),
    /// Branch if top-of-stack is non-zero.
    JmpIfNonZero(LabelId),
    /// Calls another code object.
    Call(CodeObjectId),
    /// Returns from current code object.
    Ret,
    /// Starts simple command assembly.
    BeginSimple,
    /// Adds one argument expansion program.
    AddArg(WordProgramId),
    /// Adds one assignment expansion program.
    AddAssign(SymbolId, WordProgramId),
    /// Adds one redirect subprogram.
    AddRedir(RedirectProgramId),
    /// Executes assembled simple command.
    ExecSimple,
}

/// Word-expansion subprogram operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WordProgramOp {
    /// Appends literal bytes from string pool.
    PushLiteral(StringId),
    /// Expands one shell parameter.
    ExpandParameter(SymbolId),
    /// Expands one command substitution.
    ExpandCommandSubstitution(CodeObjectId),
    /// Expands one arithmetic expression.
    ExpandArithmetic(ArithProgramId),
    /// Applies POSIX field splitting.
    FieldSplit,
    /// Applies pathname expansion.
    Glob,
    /// Applies quote removal.
    QuoteRemoval,
}

/// Redirect-evaluation subprogram operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RedirectProgramOp {
    /// Opens a path and binds it to a file descriptor.
    Open {
        /// Destination file descriptor.
        fd: u16,
        /// Expansion program for path operand.
        target: WordProgramId,
    },
    /// Duplicates one descriptor to another.
    Dup {
        /// Source file descriptor.
        from: u16,
        /// Destination file descriptor.
        to: u16,
    },
    /// Closes a descriptor.
    Close {
        /// File descriptor to close.
        fd: u16,
    },
    /// Installs one here-document body.
    HereDoc {
        /// Destination file descriptor.
        fd: u16,
        /// Here-document body storage handle.
        body: StringId,
        /// Whether expansions should be applied.
        expand: bool,
    },
}

/// Arithmetic-expansion subprogram operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArithProgramOp {
    /// Pushes integer literal.
    PushLiteral(i64),
    /// Loads variable value by symbol id.
    LoadVariable(SymbolId),
    /// Applies addition.
    Add,
    /// Applies subtraction.
    Subtract,
}

/// Encoded code object blob.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EncodedCodeObject {
    /// Stable source code-object id.
    pub id: CodeObjectId,
    /// Packed instruction words.
    pub words: Vec<u32>,
}

/// Packed bytecode module produced by encode pass.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EncodedModule {
    /// Interned string pool.
    pub string_pool: Vec<String>,
    /// Interned symbol pool.
    pub symbol_pool: Vec<String>,
    /// Constant pool used by encoded code objects.
    pub const_pool: Vec<ConstValue>,
    /// Encoded VM code objects.
    pub code_objects: Vec<EncodedCodeObject>,
    /// Encoded word programs.
    pub word_programs: Vec<Vec<u32>>,
    /// Encoded redirect programs.
    pub redirect_programs: Vec<Vec<u32>>,
    /// Encoded arithmetic programs.
    pub arith_programs: Vec<Vec<u32>>,
}
