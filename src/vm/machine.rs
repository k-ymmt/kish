//! Top-level VM execution engine with minimal dispatch loop.

use std::sync::Arc;

use crate::ir::bytecode::Instruction;
use crate::ir::ids::{CodeObjectId, ConstId, StringId, SymbolId};
use crate::ir::program::{CodeObject, ConstValue, IrModule};
use crate::vm::error::VmError;
use crate::vm::frame::Frame;
use crate::vm::value::Value;

/// Placeholder shell state; replaced in Phase 3 when `src/runtime/` is created.
pub struct ShellState {
    _private: (),
}

impl ShellState {
    /// Creates a new default shell state.
    pub fn new() -> Self {
        Self { _private: () }
    }
}

impl Default for ShellState {
    fn default() -> Self {
        Self::new()
    }
}

/// Default maximum call depth.
const DEFAULT_MAX_CALL_DEPTH: usize = 1024;

/// Main VM interpreter that dispatches bytecode instructions.
pub struct VmMachine {
    call_stack: Vec<Frame>,
    module: Arc<IrModule>,
    #[allow(dead_code)]
    state: ShellState,
    max_call_depth: usize,
}

impl VmMachine {
    /// Creates a new VM with default settings.
    pub fn new(module: Arc<IrModule>) -> Self {
        Self {
            call_stack: Vec::new(),
            module,
            state: ShellState::new(),
            max_call_depth: DEFAULT_MAX_CALL_DEPTH,
        }
    }

    /// Creates a new VM with a custom call depth limit.
    pub fn with_call_depth_limit(module: Arc<IrModule>, limit: usize) -> Self {
        Self {
            call_stack: Vec::new(),
            module,
            state: ShellState::new(),
            max_call_depth: limit,
        }
    }

    /// Returns a reference to the IR module.
    pub fn module(&self) -> &IrModule {
        &self.module
    }

    /// Returns the current call stack depth.
    pub fn call_depth(&self) -> usize {
        self.call_stack.len()
    }

    /// Executes a code object by its ID and returns the final value.
    pub fn execute_code_object(&mut self, id: CodeObjectId) -> Result<Value, VmError> {
        let co = self.lookup_code_object(id)?;
        let frame = Frame::new(id, co.locals_count, co.max_stack_depth);
        self.call_stack.push(frame);
        self.run()
    }

    /// Main dispatch loop.
    fn run(&mut self) -> Result<Value, VmError> {
        loop {
            let frame = match self.call_stack.last() {
                Some(_) => self.call_stack.last().unwrap(),
                None => return Ok(Value::ExitStatus(0)),
            };

            let co_id = frame.code_object_id();
            let pc = frame.pc();

            // Look up instructions for current code object.
            let co = self.lookup_code_object(co_id)?;

            // Check if PC has run past the end of instructions (implicit return).
            if pc >= co.instructions.len() {
                let return_value = {
                    let frame = self.call_stack.last_mut().unwrap();
                    if frame.stack_depth() > 0 {
                        frame.pop()?
                    } else {
                        Value::ExitStatus(0)
                    }
                };
                self.call_stack.pop();

                if self.call_stack.is_empty() {
                    return Ok(return_value);
                }

                // Push return value onto caller's stack.
                let caller = self.call_stack.last_mut().unwrap();
                caller.push(return_value)?;
                continue;
            }

            // Clone instruction to release borrow on self.module before mutating.
            let instruction = co.instructions[pc].clone();

            // Advance PC.
            self.call_stack.last_mut().unwrap().advance_pc();

            // Dispatch.
            match instruction {
                Instruction::Nop => {}

                Instruction::PushConst(id) => {
                    let value = self.resolve_const(id)?;
                    self.current_frame_mut()?.push(value)?;
                }

                Instruction::PushInt(n) => {
                    self.current_frame_mut()?.push(Value::Integer(n))?;
                }

                Instruction::PushString(id) => {
                    let s = self.resolve_string(id)?;
                    self.current_frame_mut()?.push(Value::String(s))?;
                }

                Instruction::PushSymbol(id) => {
                    let s = self.resolve_symbol(id)?;
                    self.current_frame_mut()?.push(Value::String(s))?;
                }

                Instruction::Drop => {
                    self.current_frame_mut()?.pop()?;
                }

                Instruction::Dup => {
                    self.current_frame_mut()?.dup()?;
                }

                Instruction::LocalGet(id) => {
                    let value = self.current_frame()?.local_get(id)?.clone();
                    self.current_frame_mut()?.push(value)?;
                }

                Instruction::LocalSet(id) => {
                    let value = self.current_frame_mut()?.pop()?;
                    self.current_frame_mut()?.local_set(id, value)?;
                }

                Instruction::Jmp(target) => {
                    self.current_frame_mut()?.set_pc(target.index() as usize);
                }

                Instruction::JmpIfZero(target) => {
                    let value = self.current_frame_mut()?.pop()?;
                    if value.is_zero() {
                        self.current_frame_mut()?.set_pc(target.index() as usize);
                    }
                }

                Instruction::JmpIfNonZero(target) => {
                    let value = self.current_frame_mut()?.pop()?;
                    if !value.is_zero() {
                        self.current_frame_mut()?.set_pc(target.index() as usize);
                    }
                }

                Instruction::Call(callee_id) => {
                    if self.call_stack.len() >= self.max_call_depth {
                        return Err(VmError::call_depth_exceeded(
                            self.call_stack.len(),
                            self.max_call_depth,
                        ));
                    }
                    let co = self.lookup_code_object(callee_id)?;
                    let frame = Frame::new(callee_id, co.locals_count, co.max_stack_depth);
                    self.call_stack.push(frame);
                }

                Instruction::Ret => {
                    let return_value = self.current_frame_mut()?.pop()?;
                    self.call_stack.pop();

                    if self.call_stack.is_empty() {
                        return Ok(return_value);
                    }

                    self.current_frame_mut()?.push(return_value)?;
                }

                Instruction::NegateStatus => {
                    let value = self.current_frame_mut()?.pop()?;
                    let status = value.as_exit_status();
                    let negated = if status == 0 { 1 } else { 0 };
                    self.current_frame_mut()?.push(Value::ExitStatus(negated))?;
                }

                _ => {
                    return Err(VmError::invalid_instruction(format!(
                        "unimplemented instruction: {instruction:?}"
                    )));
                }
            }
        }
    }

    // -- Internal helpers --

    fn current_frame(&self) -> Result<&Frame, VmError> {
        self.call_stack
            .last()
            .ok_or_else(|| VmError::internal("no active frame"))
    }

    fn current_frame_mut(&mut self) -> Result<&mut Frame, VmError> {
        self.call_stack
            .last_mut()
            .ok_or_else(|| VmError::internal("no active frame"))
    }

    fn lookup_code_object(&self, id: CodeObjectId) -> Result<&CodeObject, VmError> {
        self.module
            .code_objects
            .get(id.value() as usize)
            .ok_or_else(|| VmError::invalid_code_object(id.value()))
    }

    fn resolve_const(&self, id: ConstId) -> Result<Value, VmError> {
        let cv = self
            .module
            .const_at(id)
            .ok_or_else(|| VmError::invalid_instruction(format!("invalid const id {}", id.value())))?;
        match cv {
            ConstValue::String(sid) => {
                let s = self.resolve_string(*sid)?;
                Ok(Value::String(s))
            }
            ConstValue::Symbol(sid) => {
                let s = self.resolve_symbol(*sid)?;
                Ok(Value::String(s))
            }
            ConstValue::Integer(n) => Ok(Value::Integer(*n)),
        }
    }

    fn resolve_string(&self, id: StringId) -> Result<String, VmError> {
        self.module
            .string_at(id)
            .map(|s| s.to_owned())
            .ok_or_else(|| {
                VmError::invalid_instruction(format!("invalid string id {}", id.value()))
            })
    }

    fn resolve_symbol(&self, id: SymbolId) -> Result<String, VmError> {
        self.module
            .symbol_at(id)
            .map(|s| s.to_owned())
            .ok_or_else(|| {
                VmError::invalid_instruction(format!("invalid symbol id {}", id.value()))
            })
    }
}
