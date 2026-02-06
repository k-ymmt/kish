//! VM-facing IR module containers and options.

use std::collections::{BTreeMap, BTreeSet};

use crate::ir::bytecode::{
    ArithProgramOp, BranchTarget, Instruction, RedirectProgramOp, WordProgramOp,
};
use crate::ir::error::IrError;
use crate::ir::ids::{
    ArithProgramId, CodeObjectId, ConstId, LabelId, RedirectProgramId, StringId, SymbolId,
    WordProgramId,
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
    /// Number of locals required by this code object.
    pub locals_count: u32,
    /// Maximum stack depth observed for this code object.
    pub max_stack_depth: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum PendingInstruction {
    Concrete(Instruction),
    Jmp(LabelId),
    JmpIfZero(LabelId),
    JmpIfNonZero(LabelId),
}

/// Deterministic builder for one [`CodeObject`].
#[derive(Debug, Clone)]
pub struct CodeObjectBuilder {
    id: CodeObjectId,
    instructions: Vec<PendingInstruction>,
    max_instructions: Option<usize>,
    next_label_value: u32,
    known_labels: BTreeSet<LabelId>,
    label_bindings: BTreeMap<LabelId, u32>,
    locals_count: u32,
    max_stack_depth: u32,
}

impl CodeObjectBuilder {
    /// Creates a builder without an explicit instruction cap.
    pub fn new(id: CodeObjectId) -> Self {
        Self {
            id,
            instructions: Vec::new(),
            max_instructions: None,
            next_label_value: 0,
            known_labels: BTreeSet::new(),
            label_bindings: BTreeMap::new(),
            locals_count: 0,
            max_stack_depth: 0,
        }
    }

    /// Creates a builder with an explicit instruction cap.
    pub fn with_limits(id: CodeObjectId, max_instructions: usize) -> Self {
        let mut builder = Self::new(id);
        builder.max_instructions = Some(max_instructions);
        builder
    }

    /// Allocates a new label handle.
    pub fn new_label(&mut self) -> LabelId {
        let label = LabelId::new(self.next_label_value);
        self.next_label_value = self.next_label_value.saturating_add(1);
        self.known_labels.insert(label);
        label
    }

    /// Binds a label to the current instruction index.
    pub fn bind_label(&mut self, label: LabelId) -> Result<(), IrError> {
        self.ensure_known_label(label, "bind")?;
        if self.label_bindings.contains_key(&label) {
            return Err(IrError::duplicate_label_binding(
                None,
                "label is already bound",
                format!("label {} is bound more than once", label.value()),
            ));
        }

        let index = self.current_instruction_index()?;
        self.label_bindings.insert(label, index);
        Ok(())
    }

    /// Emits one concrete instruction.
    pub fn emit(&mut self, instruction: Instruction) -> Result<u32, IrError> {
        self.ensure_instruction_capacity()?;
        let index = self.current_instruction_index()?;
        self.instructions
            .push(PendingInstruction::Concrete(instruction));
        Ok(index)
    }

    /// Emits one unconditional jump to a label.
    pub fn emit_jmp(&mut self, label: LabelId) -> Result<u32, IrError> {
        self.ensure_known_label(label, "jump")?;
        self.ensure_instruction_capacity()?;
        let index = self.current_instruction_index()?;
        self.instructions.push(PendingInstruction::Jmp(label));
        Ok(index)
    }

    /// Emits one conditional jump (zero) to a label.
    pub fn emit_jmp_if_zero(&mut self, label: LabelId) -> Result<u32, IrError> {
        self.ensure_known_label(label, "jump_if_zero")?;
        self.ensure_instruction_capacity()?;
        let index = self.current_instruction_index()?;
        self.instructions.push(PendingInstruction::JmpIfZero(label));
        Ok(index)
    }

    /// Emits one conditional jump (non-zero) to a label.
    pub fn emit_jmp_if_non_zero(&mut self, label: LabelId) -> Result<u32, IrError> {
        self.ensure_known_label(label, "jump_if_non_zero")?;
        self.ensure_instruction_capacity()?;
        let index = self.current_instruction_index()?;
        self.instructions
            .push(PendingInstruction::JmpIfNonZero(label));
        Ok(index)
    }

    /// Sets the number of locals needed by this code object.
    pub fn set_locals_count(&mut self, locals: u32) {
        self.locals_count = locals;
    }

    /// Notes one observed stack depth and tracks maximum.
    pub fn note_stack_depth(&mut self, depth: u32) {
        if depth > self.max_stack_depth {
            self.max_stack_depth = depth;
        }
    }

    /// Finalizes this builder into a [`CodeObject`] with patched branch targets.
    pub fn finalize(self) -> Result<CodeObject, IrError> {
        let CodeObjectBuilder {
            id,
            instructions,
            label_bindings,
            locals_count,
            max_stack_depth,
            ..
        } = self;

        let mut finalized = Vec::with_capacity(instructions.len());

        for pending in instructions {
            let instruction = match pending {
                PendingInstruction::Concrete(instruction) => instruction,
                PendingInstruction::Jmp(label) => {
                    let target = Self::resolve_bound_target(&label_bindings, label)?;
                    Instruction::Jmp(target)
                }
                PendingInstruction::JmpIfZero(label) => {
                    let target = Self::resolve_bound_target(&label_bindings, label)?;
                    Instruction::JmpIfZero(target)
                }
                PendingInstruction::JmpIfNonZero(label) => {
                    let target = Self::resolve_bound_target(&label_bindings, label)?;
                    Instruction::JmpIfNonZero(target)
                }
            };
            finalized.push(instruction);
        }

        Ok(CodeObject {
            id,
            instructions: finalized,
            locals_count,
            max_stack_depth,
        })
    }

    fn ensure_instruction_capacity(&self) -> Result<(), IrError> {
        if let Some(max) = self.max_instructions
            && self.instructions.len() >= max
        {
            return Err(IrError::limit_exceeded(
                None,
                "code object instruction limit exceeded",
                format!(
                    "max_instructions={max}, attempted_index={}",
                    self.instructions.len()
                ),
            ));
        }
        Ok(())
    }

    fn current_instruction_index(&self) -> Result<u32, IrError> {
        u32::try_from(self.instructions.len()).map_err(|_| {
            IrError::limit_exceeded(
                None,
                "instruction index overflow",
                format!(
                    "instruction count {} exceeds u32::MAX",
                    self.instructions.len()
                ),
            )
        })
    }

    fn ensure_known_label(&self, label: LabelId, action: &str) -> Result<(), IrError> {
        if !self.known_labels.contains(&label) {
            return Err(IrError::invalid_branch_target(
                None,
                "branch uses unknown label",
                format!("cannot {action} using undeclared label {}", label.value()),
            ));
        }
        Ok(())
    }

    fn resolve_bound_target(
        label_bindings: &BTreeMap<LabelId, u32>,
        label: LabelId,
    ) -> Result<BranchTarget, IrError> {
        let Some(target_index) = label_bindings.get(&label).copied() else {
            return Err(IrError::invalid_branch_target(
                None,
                "branch target label is not bound",
                format!("label {} was never bound", label.value()),
            ));
        };

        Ok(BranchTarget::new(target_index))
    }
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
