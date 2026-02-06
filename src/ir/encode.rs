//! Typed IR to packed bytecode encode pass.

use crate::ir::bytecode::EncodedModule;
use crate::ir::error::IrError;
use crate::ir::program::IrModule;

/// Encodes a typed IR module to a packed bytecode module.
pub fn encode_module(_module: &IrModule) -> Result<EncodedModule, IrError> {
    Err(IrError::unsupported_form(
        None,
        "encode_module is not implemented in Phase 0",
    ))
}
