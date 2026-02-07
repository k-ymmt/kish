//! IR verification pass — structural validation of a typed `IrModule`.
//!
//! Catches bugs in lowering passes (invalid pool references, unbalanced blocks,
//! out-of-bounds branch targets) early with actionable error messages.

use crate::ir::bytecode::{ArithProgramOp, Instruction, RedirectProgramOp, WordProgramOp};
use crate::ir::error::IrError;
use crate::ir::ids::{
    ArithProgramId, CodeObjectId, ConstId, LocalId, RedirectProgramId, StringId, SymbolId,
    WordProgramId,
};
use crate::ir::program::{ConstValue, IrModule};

// ===========================================================================
// Public API
// ===========================================================================

/// Verifies structural invariants for a typed IR module.
///
/// Returns the first error found, or `Ok(())` when the module is well-formed.
pub fn verify_module(module: &IrModule) -> Result<(), IrError> {
    ModuleVerifier::new(module).verify()
}

/// Diagnostic warning emitted by the debug verifier.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyWarning {
    /// Warning category.
    pub kind: VerifyWarningKind,
    /// Human-readable description.
    pub message: String,
}

/// Warning categories for the debug verifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VerifyWarningKind {
    /// Instructions that are never reached from the entry point.
    UnreachableCode,
    /// A code object that is never referenced.
    DeadCodeObject,
}

/// Runs extended diagnostics on a verified module, returning non-fatal warnings.
///
/// The module should pass [`verify_module`] first; this function assumes
/// structural validity.
pub fn verify_module_debug(module: &IrModule) -> Vec<VerifyWarning> {
    let mut warnings = Vec::new();
    check_unreachable_code(module, &mut warnings);
    check_dead_code_objects(module, &mut warnings);
    warnings
}

// ===========================================================================
// ModuleVerifier
// ===========================================================================

struct ModuleVerifier<'a> {
    module: &'a IrModule,
    string_count: u32,
    symbol_count: u32,
    const_count: u32,
    code_object_count: u32,
    word_program_count: u32,
    redirect_program_count: u32,
    arith_program_count: u32,
}

impl<'a> ModuleVerifier<'a> {
    fn new(module: &'a IrModule) -> Self {
        Self {
            module,
            string_count: module.string_pool.len() as u32,
            symbol_count: module.symbol_pool.len() as u32,
            const_count: module.const_pool.len() as u32,
            code_object_count: module.code_objects.len() as u32,
            word_program_count: module.word_programs.len() as u32,
            redirect_program_count: module.redirect_programs.len() as u32,
            arith_program_count: module.arith_programs.len() as u32,
        }
    }

    fn verify(&self) -> Result<(), IrError> {
        self.verify_const_pool()?;
        self.verify_code_objects()?;
        self.verify_word_programs()?;
        self.verify_redirect_programs()?;
        self.verify_arith_programs()?;
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Const pool
    // -----------------------------------------------------------------------

    fn verify_const_pool(&self) -> Result<(), IrError> {
        for (i, cv) in self.module.const_pool.iter().enumerate() {
            match cv {
                ConstValue::String(id) => {
                    self.check_string_id(
                        *id,
                        &format!("const_pool[{i}] ConstValue::String"),
                    )?;
                }
                ConstValue::Symbol(id) => {
                    self.check_symbol_id(
                        *id,
                        &format!("const_pool[{i}] ConstValue::Symbol"),
                    )?;
                }
                ConstValue::Integer(_) => {}
            }
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Code objects
    // -----------------------------------------------------------------------

    fn verify_code_objects(&self) -> Result<(), IrError> {
        for (i, co) in self.module.code_objects.iter().enumerate() {
            // Identity check
            if co.id.value() as usize != i {
                return Err(IrError::invariant_violation(
                    None,
                    "code object id mismatch",
                    format!(
                        "code_object[{i}] has id={}, expected {i}",
                        co.id.value()
                    ),
                ));
            }

            let instruction_count = co.instructions.len();

            // Structural balancing state
            let mut simple_depth: u32 = 0;
            let mut pipeline_state: Option<(u32, u32)> = None; // (expected, seen)
            let mut for_state: Option<(u32, u32)> = None; // (expected, seen)
            let mut redirect_scope_depth: u32 = 0;

            for (j, instr) in co.instructions.iter().enumerate() {
                let ctx = format!("code_object[{i}] instruction[{j}]");

                // Per-instruction pool reference checks
                match instr {
                    Instruction::PushConst(id) => {
                        self.check_const_id(*id, &format!("{ctx} PushConst"))?;
                    }
                    Instruction::PushString(id) => {
                        self.check_string_id(*id, &format!("{ctx} PushString"))?;
                    }
                    Instruction::PushSymbol(id) => {
                        self.check_symbol_id(*id, &format!("{ctx} PushSymbol"))?;
                    }
                    Instruction::LocalGet(id) => {
                        self.check_local_id(*id, co.locals_count, &format!("{ctx} LocalGet"))?;
                    }
                    Instruction::LocalSet(id) => {
                        self.check_local_id(*id, co.locals_count, &format!("{ctx} LocalSet"))?;
                    }
                    Instruction::Jmp(target) => {
                        self.check_branch_target(
                            *target,
                            instruction_count,
                            &format!("{ctx} Jmp"),
                        )?;
                    }
                    Instruction::JmpIfZero(target) => {
                        self.check_branch_target(
                            *target,
                            instruction_count,
                            &format!("{ctx} JmpIfZero"),
                        )?;
                    }
                    Instruction::JmpIfNonZero(target) => {
                        self.check_branch_target(
                            *target,
                            instruction_count,
                            &format!("{ctx} JmpIfNonZero"),
                        )?;
                    }
                    Instruction::Call(id) => {
                        self.check_code_object_id(*id, &format!("{ctx} Call"))?;
                    }
                    Instruction::ExecSubshell(id) => {
                        self.check_code_object_id(*id, &format!("{ctx} ExecSubshell"))?;
                    }
                    Instruction::ExecBackground(id) => {
                        self.check_code_object_id(*id, &format!("{ctx} ExecBackground"))?;
                    }
                    Instruction::AddPipelineStage(id) => {
                        self.check_code_object_id(
                            *id,
                            &format!("{ctx} AddPipelineStage"),
                        )?;
                    }
                    Instruction::DefineFunction { name, body } => {
                        self.check_symbol_id(*name, &format!("{ctx} DefineFunction.name"))?;
                        self.check_code_object_id(
                            *body,
                            &format!("{ctx} DefineFunction.body"),
                        )?;
                    }
                    Instruction::AddArg(id) => {
                        self.check_word_program_id(*id, &format!("{ctx} AddArg"))?;
                    }
                    Instruction::ForAddWord(id) => {
                        self.check_word_program_id(*id, &format!("{ctx} ForAddWord"))?;
                    }
                    Instruction::CaseSetSubject(id) => {
                        self.check_word_program_id(*id, &format!("{ctx} CaseSetSubject"))?;
                    }
                    Instruction::CaseTestPattern(id) => {
                        self.check_word_program_id(*id, &format!("{ctx} CaseTestPattern"))?;
                    }
                    Instruction::AddAssign(sym, wp) => {
                        self.check_symbol_id(*sym, &format!("{ctx} AddAssign.symbol"))?;
                        self.check_word_program_id(*wp, &format!("{ctx} AddAssign.word_program"))?;
                    }
                    Instruction::AddRedir(id) => {
                        self.check_redirect_program_id(*id, &format!("{ctx} AddRedir"))?;
                    }
                    Instruction::ForSetup { var, .. } => {
                        self.check_symbol_id(*var, &format!("{ctx} ForSetup.var"))?;
                    }
                    Instruction::Nop
                    | Instruction::PushInt(_)
                    | Instruction::Drop
                    | Instruction::Dup
                    | Instruction::Ret
                    | Instruction::BeginSimple
                    | Instruction::EndSimple
                    | Instruction::ExecSimple(_)
                    | Instruction::NegateStatus
                    | Instruction::BeginPipeline(_)
                    | Instruction::ExecPipeline
                    | Instruction::ForNext
                    | Instruction::CaseClear
                    | Instruction::PushRedirectScope
                    | Instruction::PopRedirectScope => {}
                }

                // Structural balancing checks
                match instr {
                    Instruction::BeginSimple => {
                        simple_depth += 1;
                    }
                    Instruction::EndSimple => {
                        if simple_depth == 0 {
                            return Err(IrError::invariant_violation(
                                None,
                                "unbalanced simple command assembly",
                                format!("{ctx}: EndSimple without matching BeginSimple"),
                            ));
                        }
                        simple_depth -= 1;
                    }
                    Instruction::BeginPipeline(n) => {
                        if pipeline_state.is_some() {
                            return Err(IrError::invariant_violation(
                                None,
                                "nested pipeline assembly",
                                format!("{ctx}: BeginPipeline while another pipeline is open"),
                            ));
                        }
                        pipeline_state = Some((*n, 0));
                    }
                    Instruction::AddPipelineStage(_) => {
                        match &mut pipeline_state {
                            Some((_, seen)) => {
                                *seen += 1;
                            }
                            None => {
                                return Err(IrError::invariant_violation(
                                    None,
                                    "orphan pipeline stage",
                                    format!(
                                        "{ctx}: AddPipelineStage without BeginPipeline"
                                    ),
                                ));
                            }
                        }
                    }
                    Instruction::ExecPipeline => {
                        match pipeline_state.take() {
                            Some((expected, seen)) => {
                                if seen != expected {
                                    return Err(IrError::invariant_violation(
                                        None,
                                        "pipeline stage count mismatch",
                                        format!(
                                            "{ctx}: ExecPipeline expected {expected} stages, got {seen}"
                                        ),
                                    ));
                                }
                            }
                            None => {
                                return Err(IrError::invariant_violation(
                                    None,
                                    "orphan pipeline exec",
                                    format!(
                                        "{ctx}: ExecPipeline without BeginPipeline"
                                    ),
                                ));
                            }
                        }
                    }
                    Instruction::ForSetup { word_count, .. } => {
                        for_state = Some((*word_count, 0));
                    }
                    Instruction::ForAddWord(_) => {
                        match &mut for_state {
                            Some((expected, seen)) => {
                                *seen += 1;
                                if *seen == *expected {
                                    for_state = None;
                                }
                            }
                            None => {
                                return Err(IrError::invariant_violation(
                                    None,
                                    "orphan for-add-word",
                                    format!("{ctx}: ForAddWord without ForSetup"),
                                ));
                            }
                        }
                    }
                    Instruction::PushRedirectScope => {
                        redirect_scope_depth += 1;
                    }
                    Instruction::PopRedirectScope => {
                        if redirect_scope_depth == 0 {
                            return Err(IrError::invariant_violation(
                                None,
                                "unbalanced redirect scope",
                                format!(
                                    "{ctx}: PopRedirectScope without matching PushRedirectScope"
                                ),
                            ));
                        }
                        redirect_scope_depth -= 1;
                    }
                    _ => {}
                }
            }

            // End-of-code-object structural checks
            if simple_depth != 0 {
                return Err(IrError::invariant_violation(
                    None,
                    "unbalanced simple command assembly",
                    format!(
                        "code_object[{i}]: {simple_depth} unclosed BeginSimple at end of code object"
                    ),
                ));
            }
            if let Some((expected, seen)) = pipeline_state {
                return Err(IrError::invariant_violation(
                    None,
                    "unclosed pipeline assembly",
                    format!(
                        "code_object[{i}]: pipeline with {expected} expected stages ({seen} added) never executed"
                    ),
                ));
            }
            if let Some((expected, seen)) = for_state {
                return Err(IrError::invariant_violation(
                    None,
                    "incomplete for-loop setup",
                    format!(
                        "code_object[{i}]: ForSetup expected {expected} words, only {seen} added"
                    ),
                ));
            }
            if redirect_scope_depth != 0 {
                return Err(IrError::invariant_violation(
                    None,
                    "unbalanced redirect scope",
                    format!(
                        "code_object[{i}]: {redirect_scope_depth} unclosed PushRedirectScope at end of code object"
                    ),
                ));
            }
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Word programs
    // -----------------------------------------------------------------------

    fn verify_word_programs(&self) -> Result<(), IrError> {
        for (i, wp) in self.module.word_programs.iter().enumerate() {
            // Identity check
            if wp.id.value() as usize != i {
                return Err(IrError::invariant_violation(
                    None,
                    "word program id mismatch",
                    format!(
                        "word_program[{i}] has id={}, expected {i}",
                        wp.id.value()
                    ),
                ));
            }

            for (j, op) in wp.ops.iter().enumerate() {
                let ctx = format!("word_program[{i}] op[{j}]");
                match op {
                    WordProgramOp::PushLiteral(id) => {
                        self.check_string_id(*id, &format!("{ctx} PushLiteral"))?;
                    }
                    WordProgramOp::ExpandTilde(id) => {
                        self.check_string_id(*id, &format!("{ctx} ExpandTilde"))?;
                    }
                    WordProgramOp::ExpandParameter(id) => {
                        self.check_symbol_id(*id, &format!("{ctx} ExpandParameter"))?;
                    }
                    WordProgramOp::ExpandCommandSubstitution(id) => {
                        self.check_code_object_id(
                            *id,
                            &format!("{ctx} ExpandCommandSubstitution"),
                        )?;
                    }
                    WordProgramOp::ExpandArithmetic(id) => {
                        self.check_arith_program_id(
                            *id,
                            &format!("{ctx} ExpandArithmetic"),
                        )?;
                    }
                    WordProgramOp::FieldSplit
                    | WordProgramOp::Glob
                    | WordProgramOp::QuoteRemoval => {}
                }
            }
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Redirect programs
    // -----------------------------------------------------------------------

    fn verify_redirect_programs(&self) -> Result<(), IrError> {
        for (i, rp) in self.module.redirect_programs.iter().enumerate() {
            // Identity check
            if rp.id.value() as usize != i {
                return Err(IrError::invariant_violation(
                    None,
                    "redirect program id mismatch",
                    format!(
                        "redirect_program[{i}] has id={}, expected {i}",
                        rp.id.value()
                    ),
                ));
            }

            for (j, op) in rp.ops.iter().enumerate() {
                let ctx = format!("redirect_program[{i}] op[{j}]");
                match op {
                    RedirectProgramOp::Open { target, .. } => {
                        self.check_word_program_id(
                            *target,
                            &format!("{ctx} Open.target"),
                        )?;
                    }
                    RedirectProgramOp::HereDoc { body, .. } => {
                        self.check_string_id(*body, &format!("{ctx} HereDoc.body"))?;
                    }
                    RedirectProgramOp::Dup { .. } | RedirectProgramOp::Close { .. } => {}
                }
            }
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Arith programs
    // -----------------------------------------------------------------------

    fn verify_arith_programs(&self) -> Result<(), IrError> {
        for (i, ap) in self.module.arith_programs.iter().enumerate() {
            // Identity check
            if ap.id.value() as usize != i {
                return Err(IrError::invariant_violation(
                    None,
                    "arith program id mismatch",
                    format!(
                        "arith_program[{i}] has id={}, expected {i}",
                        ap.id.value()
                    ),
                ));
            }

            let ops_len = ap.ops.len();
            for (j, op) in ap.ops.iter().enumerate() {
                let ctx = format!("arith_program[{i}] op[{j}]");
                match op {
                    ArithProgramOp::LoadVariable(id) => {
                        self.check_symbol_id(*id, &format!("{ctx} LoadVariable"))?;
                    }
                    ArithProgramOp::Assign(id) => {
                        self.check_symbol_id(*id, &format!("{ctx} Assign"))?;
                    }
                    ArithProgramOp::CompoundAssign(id, _) => {
                        self.check_symbol_id(*id, &format!("{ctx} CompoundAssign"))?;
                    }
                    ArithProgramOp::PreIncrement(id) => {
                        self.check_symbol_id(*id, &format!("{ctx} PreIncrement"))?;
                    }
                    ArithProgramOp::PreDecrement(id) => {
                        self.check_symbol_id(*id, &format!("{ctx} PreDecrement"))?;
                    }
                    ArithProgramOp::PostIncrement(id) => {
                        self.check_symbol_id(*id, &format!("{ctx} PostIncrement"))?;
                    }
                    ArithProgramOp::PostDecrement(id) => {
                        self.check_symbol_id(*id, &format!("{ctx} PostDecrement"))?;
                    }
                    ArithProgramOp::Jmp(target) => {
                        self.check_arith_jump_target(
                            *target,
                            ops_len,
                            &format!("{ctx} Jmp"),
                        )?;
                    }
                    ArithProgramOp::JmpIfZero(target) => {
                        self.check_arith_jump_target(
                            *target,
                            ops_len,
                            &format!("{ctx} JmpIfZero"),
                        )?;
                    }
                    ArithProgramOp::JmpIfNonZero(target) => {
                        self.check_arith_jump_target(
                            *target,
                            ops_len,
                            &format!("{ctx} JmpIfNonZero"),
                        )?;
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Bounds-check helpers
    // -----------------------------------------------------------------------

    fn check_string_id(&self, id: StringId, context: &str) -> Result<(), IrError> {
        if id.value() >= self.string_count {
            return Err(IrError::invariant_violation(
                None,
                "invalid string pool reference",
                format!(
                    "{context}: string_id={} is out of range (string_pool_len={})",
                    id.value(),
                    self.string_count
                ),
            ));
        }
        Ok(())
    }

    fn check_symbol_id(&self, id: SymbolId, context: &str) -> Result<(), IrError> {
        if id.value() >= self.symbol_count {
            return Err(IrError::invariant_violation(
                None,
                "invalid symbol pool reference",
                format!(
                    "{context}: symbol_id={} is out of range (symbol_pool_len={})",
                    id.value(),
                    self.symbol_count
                ),
            ));
        }
        Ok(())
    }

    fn check_const_id(&self, id: ConstId, context: &str) -> Result<(), IrError> {
        if id.value() >= self.const_count {
            return Err(IrError::invariant_violation(
                None,
                "invalid const pool reference",
                format!(
                    "{context}: const_id={} is out of range (const_pool_len={})",
                    id.value(),
                    self.const_count
                ),
            ));
        }
        Ok(())
    }

    fn check_code_object_id(&self, id: CodeObjectId, context: &str) -> Result<(), IrError> {
        if id.value() >= self.code_object_count {
            return Err(IrError::invariant_violation(
                None,
                "invalid code object reference",
                format!(
                    "{context}: code_object_id={} is out of range (code_objects_len={})",
                    id.value(),
                    self.code_object_count
                ),
            ));
        }
        Ok(())
    }

    fn check_word_program_id(&self, id: WordProgramId, context: &str) -> Result<(), IrError> {
        if id.value() >= self.word_program_count {
            return Err(IrError::invariant_violation(
                None,
                "invalid word program reference",
                format!(
                    "{context}: word_program_id={} is out of range (word_programs_len={})",
                    id.value(),
                    self.word_program_count
                ),
            ));
        }
        Ok(())
    }

    fn check_redirect_program_id(
        &self,
        id: RedirectProgramId,
        context: &str,
    ) -> Result<(), IrError> {
        if id.value() >= self.redirect_program_count {
            return Err(IrError::invariant_violation(
                None,
                "invalid redirect program reference",
                format!(
                    "{context}: redirect_program_id={} is out of range (redirect_programs_len={})",
                    id.value(),
                    self.redirect_program_count
                ),
            ));
        }
        Ok(())
    }

    fn check_arith_program_id(&self, id: ArithProgramId, context: &str) -> Result<(), IrError> {
        if id.value() >= self.arith_program_count {
            return Err(IrError::invariant_violation(
                None,
                "invalid arith program reference",
                format!(
                    "{context}: arith_program_id={} is out of range (arith_programs_len={})",
                    id.value(),
                    self.arith_program_count
                ),
            ));
        }
        Ok(())
    }

    fn check_local_id(&self, id: LocalId, locals_count: u32, context: &str) -> Result<(), IrError> {
        if id.value() >= locals_count {
            return Err(IrError::invariant_violation(
                None,
                "invalid local variable reference",
                format!(
                    "{context}: local_id={} is out of range (locals_count={})",
                    id.value(),
                    locals_count
                ),
            ));
        }
        Ok(())
    }

    fn check_branch_target(
        &self,
        target: crate::ir::bytecode::BranchTarget,
        instruction_count: usize,
        context: &str,
    ) -> Result<(), IrError> {
        // One-past-end is valid for loop exits (consistent with CodeObjectBuilder::bind_label)
        if target.index() as usize > instruction_count {
            return Err(IrError::invariant_violation(
                None,
                "invalid branch target",
                format!(
                    "{context}: branch_target={} is out of range (instruction_count={})",
                    target.index(),
                    instruction_count
                ),
            ));
        }
        Ok(())
    }

    fn check_arith_jump_target(
        &self,
        target: u32,
        ops_len: usize,
        context: &str,
    ) -> Result<(), IrError> {
        if target as usize >= ops_len {
            return Err(IrError::invariant_violation(
                None,
                "invalid arith jump target",
                format!(
                    "{context}: target={target} is out of range (ops_len={ops_len})"
                ),
            ));
        }
        Ok(())
    }
}

// ===========================================================================
// Debug verifier helpers
// ===========================================================================

fn check_unreachable_code(module: &IrModule, warnings: &mut Vec<VerifyWarning>) {
    use std::collections::VecDeque;

    for (i, co) in module.code_objects.iter().enumerate() {
        if co.instructions.is_empty() {
            continue;
        }

        let len = co.instructions.len();
        let mut reachable = vec![false; len];
        let mut queue = VecDeque::new();
        queue.push_back(0usize);

        while let Some(idx) = queue.pop_front() {
            if idx >= len || reachable[idx] {
                continue;
            }
            reachable[idx] = true;

            match &co.instructions[idx] {
                Instruction::Jmp(target) => {
                    queue.push_back(target.index() as usize);
                }
                Instruction::JmpIfZero(target) | Instruction::JmpIfNonZero(target) => {
                    queue.push_back(target.index() as usize);
                    queue.push_back(idx + 1);
                }
                Instruction::Ret => {
                    // No fall-through
                }
                _ => {
                    queue.push_back(idx + 1);
                }
            }
        }

        // Find unreachable ranges
        let mut start: Option<usize> = None;
        for (idx, &is_reachable) in reachable.iter().enumerate() {
            if !is_reachable {
                if start.is_none() {
                    start = Some(idx);
                }
            } else if let Some(s) = start.take() {
                warnings.push(VerifyWarning {
                    kind: VerifyWarningKind::UnreachableCode,
                    message: format!(
                        "code_object[{i}]: instructions [{s}..{}] are unreachable",
                        idx - 1
                    ),
                });
            }
        }
        if let Some(s) = start {
            warnings.push(VerifyWarning {
                kind: VerifyWarningKind::UnreachableCode,
                message: format!(
                    "code_object[{i}]: instructions [{s}..{}] are unreachable",
                    len - 1
                ),
            });
        }
    }
}

fn check_dead_code_objects(module: &IrModule, warnings: &mut Vec<VerifyWarning>) {
    use std::collections::HashSet;

    let mut referenced = HashSet::new();

    // code_objects[0] is the entry point — always considered alive
    if !module.code_objects.is_empty() {
        referenced.insert(0u32);
    }

    // Scan instructions in all code objects
    for co in &module.code_objects {
        for instr in &co.instructions {
            match instr {
                Instruction::Call(id)
                | Instruction::ExecSubshell(id)
                | Instruction::ExecBackground(id)
                | Instruction::AddPipelineStage(id) => {
                    referenced.insert(id.value());
                }
                Instruction::DefineFunction { body, .. } => {
                    referenced.insert(body.value());
                }
                _ => {}
            }
        }
    }

    // Scan word programs
    for wp in &module.word_programs {
        for op in &wp.ops {
            if let WordProgramOp::ExpandCommandSubstitution(id) = op {
                referenced.insert(id.value());
            }
        }
    }

    for (i, co) in module.code_objects.iter().enumerate() {
        if !referenced.contains(&co.id.value()) {
            warnings.push(VerifyWarning {
                kind: VerifyWarningKind::DeadCodeObject,
                message: format!("code_object[{i}] (id={}) is never referenced", co.id.value()),
            });
        }
    }
}
