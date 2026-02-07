//! VM error contracts.

use std::fmt;

/// Stable VM error categories.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VmErrorKind {
    /// VM operand stack exceeded its configured depth limit.
    StackOverflow,
    /// Pop or peek on an empty operand stack.
    StackUnderflow,
    /// Register index exceeds the frame's register file size.
    RegisterOutOfBounds,
    /// Code-object ID does not map to a valid code object.
    InvalidCodeObject,
    /// Instruction opcode or operand encoding is invalid.
    InvalidInstruction,
    /// Operand type does not match the instruction's expectation.
    TypeMismatch,
    /// Integer division or modulo by zero.
    DivisionByZero,
    /// Arithmetic overflow or invalid arithmetic operation.
    InvalidArithmetic,
    /// Named command was not found in PATH or builtins.
    CommandNotFound(String),
    /// Named command exists but lacks execute permission.
    CommandNotExecutable(String),
    /// File-descriptor redirect could not be completed.
    RedirectFailed,
    /// File-descriptor manipulation error.
    FdError,
    /// Subshell exited with a non-zero status.
    SubshellFailed,
    /// Pipeline setup or teardown failed.
    PipelineFailed,
    /// Trap registration or dispatch error.
    TrapError,
    /// Signal delivery or handling error.
    SignalError,
    /// Word or parameter expansion error.
    ExpansionError,
    /// Filename pattern compilation or matching error.
    PatternError,
    /// Unexpected internal VM state; indicates a VM bug.
    InternalError,
}

impl fmt::Display for VmErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StackOverflow => write!(f, "stack overflow"),
            Self::StackUnderflow => write!(f, "stack underflow"),
            Self::RegisterOutOfBounds => write!(f, "register out of bounds"),
            Self::InvalidCodeObject => write!(f, "invalid code object"),
            Self::InvalidInstruction => write!(f, "invalid instruction"),
            Self::TypeMismatch => write!(f, "type mismatch"),
            Self::DivisionByZero => write!(f, "division by zero"),
            Self::InvalidArithmetic => write!(f, "invalid arithmetic"),
            Self::CommandNotFound(name) => write!(f, "command not found: {name}"),
            Self::CommandNotExecutable(name) => write!(f, "command not executable: {name}"),
            Self::RedirectFailed => write!(f, "redirect failed"),
            Self::FdError => write!(f, "fd error"),
            Self::SubshellFailed => write!(f, "subshell failed"),
            Self::PipelineFailed => write!(f, "pipeline failed"),
            Self::TrapError => write!(f, "trap error"),
            Self::SignalError => write!(f, "signal error"),
            Self::ExpansionError => write!(f, "expansion error"),
            Self::PatternError => write!(f, "pattern error"),
            Self::InternalError => write!(f, "internal error"),
        }
    }
}

/// VM error payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VmError {
    /// Error category.
    pub kind: VmErrorKind,
    /// Human-readable error message.
    pub message: String,
}

impl VmError {
    /// Creates a VM error.
    pub fn new(kind: VmErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
        }
    }

    /// Creates a `StackOverflow` error.
    pub fn stack_overflow() -> Self {
        Self::new(VmErrorKind::StackOverflow, "stack overflow")
    }

    /// Creates a `StackUnderflow` error.
    pub fn stack_underflow() -> Self {
        Self::new(VmErrorKind::StackUnderflow, "stack underflow")
    }

    /// Creates a `RegisterOutOfBounds` error.
    pub fn register_out_of_bounds(index: usize, limit: usize) -> Self {
        Self::new(
            VmErrorKind::RegisterOutOfBounds,
            format!("register index {index} out of bounds (limit {limit})"),
        )
    }

    /// Creates an `InvalidCodeObject` error.
    pub fn invalid_code_object(id: u32) -> Self {
        Self::new(
            VmErrorKind::InvalidCodeObject,
            format!("invalid code object id {id}"),
        )
    }

    /// Creates a `TypeMismatch` error.
    pub fn type_mismatch(message: impl Into<String>) -> Self {
        Self::new(VmErrorKind::TypeMismatch, message)
    }

    /// Creates a `DivisionByZero` error.
    pub fn division_by_zero() -> Self {
        Self::new(VmErrorKind::DivisionByZero, "division by zero")
    }

    /// Creates a `CommandNotFound` error.
    pub fn command_not_found(name: impl Into<String>) -> Self {
        let name = name.into();
        let message = format!("{name}: command not found");
        Self::new(VmErrorKind::CommandNotFound(name), message)
    }

    /// Creates a `CommandNotExecutable` error.
    pub fn command_not_executable(name: impl Into<String>) -> Self {
        let name = name.into();
        let message = format!("{name}: permission denied");
        Self::new(VmErrorKind::CommandNotExecutable(name), message)
    }

    /// Creates an `InternalError` error.
    pub fn internal(message: impl Into<String>) -> Self {
        Self::new(VmErrorKind::InternalError, message)
    }
}

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for VmError {}
