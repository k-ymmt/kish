//! Virtual machine runtime for kish bytecode execution.

pub mod arith_vm;
pub mod command;
pub mod error;
pub mod frame;
pub mod machine;
pub mod pattern;
pub mod redir_vm;
pub mod value;
pub mod word_vm;

pub use error::{VmError, VmErrorKind};
pub use frame::{CaseState, ForLoopState, Frame, PipelineBuilder};
pub use machine::{ShellState, VmMachine};
pub use value::Value;
