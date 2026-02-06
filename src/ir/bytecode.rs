//! Typed VM IR instruction contracts and encoded bytecode containers.

use crate::ir::ids::{
    ArithProgramId, CodeObjectId, ConstId, LocalId, RedirectProgramId, StringId, SymbolId,
    WordProgramId,
};
use crate::ir::program::ConstValue;

/// Absolute branch destination represented as instruction index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BranchTarget(u32);

impl BranchTarget {
    /// Creates a branch target from an absolute instruction index.
    pub const fn new(index: u32) -> Self {
        Self(index)
    }

    /// Returns the absolute instruction index.
    pub const fn index(self) -> u32 {
        self.0
    }
}

/// Hint for runtime command dispatch per POSIX search order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum CommandDispatchHint {
    /// No command name present; only assignments and/or redirects.
    /// Assignments affect the current shell environment directly.
    NoCommand,
    /// First word is a known POSIX special builtin name (literal, unquoted).
    /// Runtime should bypass function lookup for these.
    SpecialBuiltin,
    /// Standard dispatch: runtime applies full POSIX command search order
    /// (special builtins -> functions -> regular builtins -> PATH).
    #[default]
    Standard,
}

/// Typed VM instruction stream used before packing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    /// No-op placeholder.
    Nop,
    /// Pushes a constant onto the VM stack.
    PushConst(ConstId),
    /// Pushes an immediate integer onto the VM stack.
    PushInt(i64),
    /// Pushes an interned string reference onto the VM stack.
    PushString(StringId),
    /// Pushes an interned symbol reference onto the VM stack.
    PushSymbol(SymbolId),
    /// Pops one stack value.
    Drop,
    /// Duplicates the stack top.
    Dup,
    /// Loads a local onto the stack.
    LocalGet(LocalId),
    /// Stores stack top into a local.
    LocalSet(LocalId),
    /// Unconditional branch.
    Jmp(BranchTarget),
    /// Branch if top-of-stack is zero.
    JmpIfZero(BranchTarget),
    /// Branch if top-of-stack is non-zero.
    JmpIfNonZero(BranchTarget),
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
    /// Ends simple command assembly.
    EndSimple,
    /// Executes assembled simple command.
    ExecSimple(CommandDispatchHint),
    /// Negates top-of-stack exit status (0 -> 1, nonzero -> 0).
    NegateStatus,
    /// Forks a subshell, executes code object, pushes exit status.
    ExecSubshell(CodeObjectId),
    /// Registers a function definition, pushes 0.
    DefineFunction { name: SymbolId, body: CodeObjectId },
    /// Executes code object in background, pushes 0.
    ExecBackground(CodeObjectId),
    /// Begins pipeline assembly with N stages.
    BeginPipeline(u32),
    /// Adds one pipeline stage.
    AddPipelineStage(CodeObjectId),
    /// Executes assembled pipeline, pushes exit status of last stage.
    ExecPipeline,
    /// Sets up for-loop: stores var name, expects word_count ForAddWord instructions.
    ForSetup { var: SymbolId, word_count: u32 },
    /// Adds one word expansion program to for-loop iteration list.
    ForAddWord(WordProgramId),
    /// Advances for-loop iterator. Pushes 1 (value available, var set) or 0 (done).
    ForNext,
    /// Pops top-of-stack and stores as case subject.
    CaseSetSubject,
    /// Tests pattern against stored case subject. Pushes 1 (match) or 0 (no match).
    CaseTestPattern(WordProgramId),
    /// Clears stored case subject.
    CaseClear,
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
