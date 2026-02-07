//! VM call-frame: activation record carrying operand stack and register file.

use crate::ir::ids::{CodeObjectId, LocalId, SymbolId};
use crate::vm::command::CommandBuilder;
use crate::vm::error::VmError;
use crate::vm::value::Value;

/// For-loop iteration state.
pub struct ForLoopState {
    /// Variable receiving each word.
    pub var: SymbolId,
    /// Words to iterate over.
    pub words: Vec<String>,
    /// Current iteration index.
    pub index: usize,
}

/// Case-statement matching state.
pub struct CaseState {
    /// The subject string being matched.
    pub subject: String,
}

/// Pipeline assembly state.
pub struct PipelineBuilder {
    /// Number of pipeline stages expected.
    pub expected_stages: u32,
    /// Code objects for each stage collected so far.
    pub stages: Vec<CodeObjectId>,
}

/// A single activation record on the VM call stack.
pub struct Frame {
    code_object_id: CodeObjectId,
    pc: usize,
    registers: Vec<Value>,
    stack: Vec<Value>,
    max_stack: usize,
    /// Transient command builder (used during command assembly).
    pub command_builder: Option<CommandBuilder>,
    /// Transient pipeline builder (used during pipeline assembly).
    pub pipeline_builder: Option<PipelineBuilder>,
    /// Transient for-loop state.
    pub for_state: Option<ForLoopState>,
    /// Transient case-statement state.
    pub case_state: Option<CaseState>,
}

impl Frame {
    /// Creates a new frame for the given code object.
    ///
    /// `locals_count` determines the register file size (all initialized to
    /// `Value::Uninitialized`).  `max_stack` sets the operand stack depth limit.
    pub fn new(code_object_id: CodeObjectId, locals_count: u32, max_stack: u32) -> Self {
        Self {
            code_object_id,
            pc: 0,
            registers: vec![Value::Uninitialized; locals_count as usize],
            stack: Vec::with_capacity(max_stack as usize),
            max_stack: max_stack as usize,
            command_builder: None,
            pipeline_builder: None,
            for_state: None,
            case_state: None,
        }
    }

    // -- PC accessors --

    /// Returns the current program counter.
    pub fn pc(&self) -> usize {
        self.pc
    }

    /// Sets the program counter to `pc`.
    pub fn set_pc(&mut self, pc: usize) {
        self.pc = pc;
    }

    /// Returns the current program counter and then increments it.
    pub fn advance_pc(&mut self) -> usize {
        let current = self.pc;
        self.pc += 1;
        current
    }

    /// Returns the code object ID this frame is executing.
    pub fn code_object_id(&self) -> CodeObjectId {
        self.code_object_id
    }

    // -- Stack operations --

    /// Pushes a value onto the operand stack.
    ///
    /// Returns `Err(VmError)` with `StackOverflow` if the stack has reached its
    /// configured depth limit.
    pub fn push(&mut self, value: Value) -> Result<(), VmError> {
        if self.stack.len() >= self.max_stack {
            return Err(VmError::stack_overflow());
        }
        self.stack.push(value);
        Ok(())
    }

    /// Pops the top value from the operand stack.
    ///
    /// Returns `Err(VmError)` with `StackUnderflow` if the stack is empty.
    pub fn pop(&mut self) -> Result<Value, VmError> {
        self.stack.pop().ok_or_else(VmError::stack_underflow)
    }

    /// Returns a reference to the top value without removing it.
    ///
    /// Returns `Err(VmError)` with `StackUnderflow` if the stack is empty.
    pub fn peek(&self) -> Result<&Value, VmError> {
        self.stack.last().ok_or_else(VmError::stack_underflow)
    }

    /// Duplicates the top value on the operand stack.
    ///
    /// Returns `Err(VmError)` on underflow (empty stack) or overflow (stack
    /// already at its depth limit).
    pub fn dup(&mut self) -> Result<(), VmError> {
        let value = self.peek()?.clone();
        self.push(value)
    }

    /// Swaps the top two values on the operand stack.
    ///
    /// Returns `Err(VmError)` with `StackUnderflow` if the stack contains
    /// fewer than two elements.
    pub fn swap(&mut self) -> Result<(), VmError> {
        let len = self.stack.len();
        if len < 2 {
            return Err(VmError::stack_underflow());
        }
        self.stack.swap(len - 1, len - 2);
        Ok(())
    }

    /// Returns the current operand stack depth.
    pub fn stack_depth(&self) -> usize {
        self.stack.len()
    }

    // -- Register (local) operations --

    /// Reads a register (local variable).
    ///
    /// Returns `Err(VmError)` with `RegisterOutOfBounds` if `id` exceeds the
    /// register file size.
    pub fn local_get(&self, id: LocalId) -> Result<&Value, VmError> {
        let index = id.value() as usize;
        self.registers
            .get(index)
            .ok_or_else(|| VmError::register_out_of_bounds(index, self.registers.len()))
    }

    /// Writes a register (local variable).
    ///
    /// Returns `Err(VmError)` with `RegisterOutOfBounds` if `id` exceeds the
    /// register file size.
    pub fn local_set(&mut self, id: LocalId, value: Value) -> Result<(), VmError> {
        let index = id.value() as usize;
        if index >= self.registers.len() {
            return Err(VmError::register_out_of_bounds(index, self.registers.len()));
        }
        self.registers[index] = value;
        Ok(())
    }
}
