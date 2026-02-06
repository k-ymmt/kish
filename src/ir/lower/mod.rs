//! Parser AST to IR lowering entrypoints.

pub(crate) mod from_parser;

pub mod arith_program;
pub mod compound;
pub mod control_flow;
pub mod function;
pub mod redirect_program;
pub mod simple_command;
pub mod word_program;

use crate::ir::error::IrError;
use crate::ir::hir::{HirCompleteCommand, HirProgram};
use crate::ir::program::{IrModule, IrModuleBuilder, IrOptions};
use crate::parser::ast::{CompleteCommandAst, ProgramAst};

/// Stateful lowering context for parser AST to IR conversion.
#[derive(Debug, Clone)]
pub struct LoweringContext {
    options: IrOptions,
}

impl LoweringContext {
    /// Creates a lowering context.
    pub fn new(options: IrOptions) -> Self {
        Self { options }
    }

    /// Returns lowering options.
    pub fn options(&self) -> IrOptions {
        self.options
    }

    /// Creates a module builder with this lowering context's options.
    pub fn module_builder(&self) -> IrModuleBuilder {
        IrModuleBuilder::new(self.options)
    }

    /// Lowers one complete command to HIR.
    pub fn lower_complete_command_to_hir(
        &mut self,
        command: &CompleteCommandAst,
    ) -> Result<HirCompleteCommand, IrError> {
        from_parser::lower_complete_command_to_hir(self, command)
    }

    /// Lowers a whole parsed program to HIR.
    pub fn lower_program_to_hir(
        &mut self,
        program: &ProgramAst,
    ) -> Result<HirProgram, IrError> {
        from_parser::lower_program_to_hir(self, program)
    }

    /// Lowers one complete command.
    pub fn lower_complete_command(
        &mut self,
        command: &CompleteCommandAst,
    ) -> Result<IrModule, IrError> {
        from_parser::lower_complete_command_entry(self, command)
    }

    /// Lowers a whole parsed program.
    pub fn lower_program(&mut self, program: &ProgramAst) -> Result<IrModule, IrError> {
        from_parser::lower_program_entry(self, program)
    }
}
