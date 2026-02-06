//! IR verification pass contracts.

use crate::ir::error::IrError;
use crate::ir::program::IrModule;

/// Verifies structural invariants for a typed IR module.
pub fn verify_module(_module: &IrModule) -> Result<(), IrError> {
    Err(IrError::unsupported_form(
        None,
        "verify_module is not implemented in Phase 0",
    ))
}
