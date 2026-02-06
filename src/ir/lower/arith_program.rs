//! Arithmetic-program lowering: parses arithmetic expressions inside `$((...))` and
//! emits `ArithProgramOp` sequences.
//!
//! Phase 9 replaces the stub arithmetic programs from Phase 7 with real
//! arithmetic expression parsing and lowering.

use crate::ir::bytecode::{ArithCompoundOp, ArithProgramOp};
use crate::ir::error::IrError;
use crate::ir::ids::ArithProgramId;
use crate::ir::lower::emit::EmitContext;
use crate::ir::program::ArithProgram;

// ---------------------------------------------------------------------------
// ArithToken
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
enum ArithToken {
    Integer(i64),
    Identifier(String),
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Tilde,
    Bang,
    Ampersand,
    Pipe,
    Caret,
    LParen,
    RParen,
    Question,
    Colon,
    Comma,
    // Multi-char operators
    PlusPlus,
    MinusMinus,
    ShiftLeft,
    ShiftRight,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    BangEqual,
    AmpAmp,
    PipePipe,
    Less,
    Greater,
    // Assignment
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    AmpAssign,
    PipeAssign,
    CaretAssign,
    // End
    Eof,
}

// ---------------------------------------------------------------------------
// ArithLexer
// ---------------------------------------------------------------------------

struct ArithLexer<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> ArithLexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    fn advance(&mut self) -> u8 {
        let b = self.input[self.pos];
        self.pos += 1;
        b
    }

    fn next_token(&mut self) -> Result<ArithToken, IrError> {
        self.skip_whitespace();

        let Some(ch) = self.peek() else {
            return Ok(ArithToken::Eof);
        };

        // Integer literals
        if ch.is_ascii_digit() {
            return self.scan_integer();
        }

        // Identifiers
        if ch == b'_' || ch.is_ascii_alphabetic() {
            return Ok(self.scan_identifier());
        }

        self.advance();

        match ch {
            b'+' => {
                if self.peek() == Some(b'+') {
                    self.advance();
                    Ok(ArithToken::PlusPlus)
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::PlusAssign)
                } else {
                    Ok(ArithToken::Plus)
                }
            }
            b'-' => {
                if self.peek() == Some(b'-') {
                    self.advance();
                    Ok(ArithToken::MinusMinus)
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::MinusAssign)
                } else {
                    Ok(ArithToken::Minus)
                }
            }
            b'*' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::StarAssign)
                } else {
                    Ok(ArithToken::Star)
                }
            }
            b'/' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::SlashAssign)
                } else {
                    Ok(ArithToken::Slash)
                }
            }
            b'%' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::PercentAssign)
                } else {
                    Ok(ArithToken::Percent)
                }
            }
            b'~' => Ok(ArithToken::Tilde),
            b'!' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::BangEqual)
                } else {
                    Ok(ArithToken::Bang)
                }
            }
            b'&' => {
                if self.peek() == Some(b'&') {
                    self.advance();
                    Ok(ArithToken::AmpAmp)
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::AmpAssign)
                } else {
                    Ok(ArithToken::Ampersand)
                }
            }
            b'|' => {
                if self.peek() == Some(b'|') {
                    self.advance();
                    Ok(ArithToken::PipePipe)
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::PipeAssign)
                } else {
                    Ok(ArithToken::Pipe)
                }
            }
            b'^' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::CaretAssign)
                } else {
                    Ok(ArithToken::Caret)
                }
            }
            b'<' => {
                if self.peek() == Some(b'<') {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        Ok(ArithToken::ShiftLeftAssign)
                    } else {
                        Ok(ArithToken::ShiftLeft)
                    }
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::LessEqual)
                } else {
                    Ok(ArithToken::Less)
                }
            }
            b'>' => {
                if self.peek() == Some(b'>') {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        Ok(ArithToken::ShiftRightAssign)
                    } else {
                        Ok(ArithToken::ShiftRight)
                    }
                } else if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::GreaterEqual)
                } else {
                    Ok(ArithToken::Greater)
                }
            }
            b'=' => {
                if self.peek() == Some(b'=') {
                    self.advance();
                    Ok(ArithToken::EqualEqual)
                } else {
                    Ok(ArithToken::Assign)
                }
            }
            b'(' => Ok(ArithToken::LParen),
            b')' => Ok(ArithToken::RParen),
            b'?' => Ok(ArithToken::Question),
            b':' => Ok(ArithToken::Colon),
            b',' => Ok(ArithToken::Comma),
            _ => Err(IrError::invalid_arithmetic_expression(
                None,
                "unexpected character in arithmetic expression",
                format!("character '{}' at position {}", ch as char, self.pos - 1),
            )),
        }
    }

    fn scan_integer(&mut self) -> Result<ArithToken, IrError> {
        let start = self.pos;

        if self.peek() == Some(b'0') {
            self.advance();
            match self.peek() {
                Some(b'x') | Some(b'X') => {
                    // Hexadecimal
                    self.advance();
                    let hex_start = self.pos;
                    while self.pos < self.input.len()
                        && self.input[self.pos].is_ascii_hexdigit()
                    {
                        self.pos += 1;
                    }
                    if self.pos == hex_start {
                        return Err(IrError::invalid_arithmetic_expression(
                            None,
                            "invalid hex literal",
                            format!("expected hex digits after '0x' at position {start}"),
                        ));
                    }
                    let text = std::str::from_utf8(&self.input[hex_start..self.pos])
                        .expect("hex digits are valid UTF-8");
                    let value = i64::from_str_radix(text, 16).map_err(|_| {
                        IrError::invalid_arithmetic_expression(
                            None,
                            "hex literal overflow",
                            format!("hex literal '0x{text}' overflows i64"),
                        )
                    })?;
                    return Ok(ArithToken::Integer(value));
                }
                Some(b'0'..=b'7') => {
                    // Octal
                    while self.pos < self.input.len()
                        && matches!(self.input[self.pos], b'0'..=b'7')
                    {
                        self.pos += 1;
                    }
                    let text = std::str::from_utf8(&self.input[start..self.pos])
                        .expect("octal digits are valid UTF-8");
                    let value = i64::from_str_radix(text, 8).map_err(|_| {
                        IrError::invalid_arithmetic_expression(
                            None,
                            "octal literal overflow",
                            format!("octal literal '{text}' overflows i64"),
                        )
                    })?;
                    return Ok(ArithToken::Integer(value));
                }
                _ => {
                    // Just zero
                    return Ok(ArithToken::Integer(0));
                }
            }
        }

        // Decimal
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_digit() {
            self.pos += 1;
        }
        let text = std::str::from_utf8(&self.input[start..self.pos])
            .expect("digits are valid UTF-8");
        let value: i64 = text.parse().map_err(|_| {
            IrError::invalid_arithmetic_expression(
                None,
                "integer literal overflow",
                format!("integer literal '{text}' overflows i64"),
            )
        })?;
        Ok(ArithToken::Integer(value))
    }

    fn scan_identifier(&mut self) -> ArithToken {
        let start = self.pos;
        while self.pos < self.input.len()
            && (self.input[self.pos] == b'_' || self.input[self.pos].is_ascii_alphanumeric())
        {
            self.pos += 1;
        }
        let text = std::str::from_utf8(&self.input[start..self.pos])
            .expect("identifier is valid UTF-8");
        ArithToken::Identifier(text.to_owned())
    }
}

// ---------------------------------------------------------------------------
// ArithEmitter
// ---------------------------------------------------------------------------

struct ArithEmitter<'a, 'b> {
    ctx: &'a mut EmitContext<'b>,
    tokens: Vec<ArithToken>,
    pos: usize,
    ops: Vec<ArithProgramOp>,
}

impl<'a, 'b> ArithEmitter<'a, 'b> {
    fn new(ctx: &'a mut EmitContext<'b>, tokens: Vec<ArithToken>) -> Self {
        Self {
            ctx,
            tokens,
            pos: 0,
            ops: Vec::new(),
        }
    }

    fn current(&self) -> &ArithToken {
        self.tokens.get(self.pos).unwrap_or(&ArithToken::Eof)
    }

    fn advance(&mut self) -> ArithToken {
        let tok = self.tokens.get(self.pos).cloned().unwrap_or(ArithToken::Eof);
        self.pos += 1;
        tok
    }

    fn expect(&mut self, expected: &ArithToken) -> Result<(), IrError> {
        if self.current() == expected {
            self.advance();
            Ok(())
        } else {
            Err(IrError::invalid_arithmetic_expression(
                None,
                "unexpected token in arithmetic expression",
                format!("expected {expected:?}, got {:?}", self.current()),
            ))
        }
    }

    fn emit(&mut self, op: ArithProgramOp) {
        self.ops.push(op);
    }

    fn current_op_index(&self) -> u32 {
        self.ops.len() as u32
    }

    fn patch_jump(&mut self, index: u32, target: u32) {
        match &mut self.ops[index as usize] {
            ArithProgramOp::JmpIfZero(t) => *t = target,
            ArithProgramOp::JmpIfNonZero(t) => *t = target,
            ArithProgramOp::Jmp(t) => *t = target,
            _ => {}
        }
    }

    // -----------------------------------------------------------------------
    // Recursive descent: operator precedence (lowest → highest)
    //   comma, assignment, ternary, ||, &&, |, ^, &,
    //   ==  !=, < > <= >=, << >>, + -, * / %, unary prefix, postfix, primary
    // -----------------------------------------------------------------------

    /// Top-level: comma expression.
    fn parse_expression(&mut self) -> Result<(), IrError> {
        self.parse_assignment()?;
        while *self.current() == ArithToken::Comma {
            self.advance();
            self.emit(ArithProgramOp::Pop);
            self.parse_assignment()?;
        }
        Ok(())
    }

    /// Assignment (right-associative): `x = expr`, `x += expr`, etc.
    fn parse_assignment(&mut self) -> Result<(), IrError> {
        // Speculatively try to parse as assignment if we see an identifier.
        if let ArithToken::Identifier(name) = self.current().clone() {
            let saved_pos = self.pos;

            // Check for simple assignment or compound assignment.
            self.advance(); // skip identifier
            let tok = self.current().clone();

            match tok {
                ArithToken::Assign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?; // right-associative
                    self.emit(ArithProgramOp::Assign(sym));
                    return Ok(());
                }
                ArithToken::PlusAssign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?;
                    self.emit(ArithProgramOp::CompoundAssign(sym, ArithCompoundOp::Add));
                    return Ok(());
                }
                ArithToken::MinusAssign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?;
                    self.emit(ArithProgramOp::CompoundAssign(sym, ArithCompoundOp::Subtract));
                    return Ok(());
                }
                ArithToken::StarAssign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?;
                    self.emit(ArithProgramOp::CompoundAssign(sym, ArithCompoundOp::Multiply));
                    return Ok(());
                }
                ArithToken::SlashAssign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?;
                    self.emit(ArithProgramOp::CompoundAssign(sym, ArithCompoundOp::Divide));
                    return Ok(());
                }
                ArithToken::PercentAssign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?;
                    self.emit(ArithProgramOp::CompoundAssign(sym, ArithCompoundOp::Modulo));
                    return Ok(());
                }
                ArithToken::ShiftLeftAssign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?;
                    self.emit(ArithProgramOp::CompoundAssign(sym, ArithCompoundOp::ShiftLeft));
                    return Ok(());
                }
                ArithToken::ShiftRightAssign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?;
                    self.emit(ArithProgramOp::CompoundAssign(sym, ArithCompoundOp::ShiftRight));
                    return Ok(());
                }
                ArithToken::AmpAssign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?;
                    self.emit(ArithProgramOp::CompoundAssign(sym, ArithCompoundOp::BitwiseAnd));
                    return Ok(());
                }
                ArithToken::PipeAssign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?;
                    self.emit(ArithProgramOp::CompoundAssign(sym, ArithCompoundOp::BitwiseOr));
                    return Ok(());
                }
                ArithToken::CaretAssign => {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.parse_assignment()?;
                    self.emit(ArithProgramOp::CompoundAssign(sym, ArithCompoundOp::BitwiseXor));
                    return Ok(());
                }
                _ => {
                    // Not an assignment; backtrack.
                    self.pos = saved_pos;
                }
            }
        }

        self.parse_ternary()
    }

    /// Ternary: `expr ? expr : expr`
    fn parse_ternary(&mut self) -> Result<(), IrError> {
        self.parse_logical_or()?;

        if *self.current() == ArithToken::Question {
            self.advance();
            // Condition is on stack. Emit JmpIfZero to else branch.
            let jmp_else = self.current_op_index();
            self.emit(ArithProgramOp::JmpIfZero(0)); // patched later

            self.parse_assignment()?; // true branch (allows assignment)
            let jmp_end = self.current_op_index();
            self.emit(ArithProgramOp::Jmp(0)); // patched later

            self.expect(&ArithToken::Colon)?;
            let else_target = self.current_op_index();
            self.patch_jump(jmp_else, else_target);

            self.parse_assignment()?; // false branch
            let end_target = self.current_op_index();
            self.patch_jump(jmp_end, end_target);
        }

        Ok(())
    }

    /// Logical OR: `||` (short-circuit)
    fn parse_logical_or(&mut self) -> Result<(), IrError> {
        self.parse_logical_and()?;

        while *self.current() == ArithToken::PipePipe {
            self.advance();
            // Short-circuit: if non-zero, skip right operand.
            let jmp_short = self.current_op_index();
            self.emit(ArithProgramOp::JmpIfNonZero(0)); // patched later
            self.emit(ArithProgramOp::Pop);
            self.parse_logical_and()?;
            let end_target = self.current_op_index();
            self.patch_jump(jmp_short, end_target);
            self.emit(ArithProgramOp::LogicalOr);
        }

        Ok(())
    }

    /// Logical AND: `&&` (short-circuit)
    fn parse_logical_and(&mut self) -> Result<(), IrError> {
        self.parse_bitwise_or()?;

        while *self.current() == ArithToken::AmpAmp {
            self.advance();
            // Short-circuit: if zero, skip right operand.
            let jmp_short = self.current_op_index();
            self.emit(ArithProgramOp::JmpIfZero(0)); // patched later
            self.emit(ArithProgramOp::Pop);
            self.parse_bitwise_or()?;
            let end_target = self.current_op_index();
            self.patch_jump(jmp_short, end_target);
            self.emit(ArithProgramOp::LogicalAnd);
        }

        Ok(())
    }

    /// Bitwise OR: `|`
    fn parse_bitwise_or(&mut self) -> Result<(), IrError> {
        self.parse_bitwise_xor()?;
        while *self.current() == ArithToken::Pipe {
            self.advance();
            self.parse_bitwise_xor()?;
            self.emit(ArithProgramOp::BitwiseOr);
        }
        Ok(())
    }

    /// Bitwise XOR: `^`
    fn parse_bitwise_xor(&mut self) -> Result<(), IrError> {
        self.parse_bitwise_and()?;
        while *self.current() == ArithToken::Caret {
            self.advance();
            self.parse_bitwise_and()?;
            self.emit(ArithProgramOp::BitwiseXor);
        }
        Ok(())
    }

    /// Bitwise AND: `&`
    fn parse_bitwise_and(&mut self) -> Result<(), IrError> {
        self.parse_equality()?;
        while *self.current() == ArithToken::Ampersand {
            self.advance();
            self.parse_equality()?;
            self.emit(ArithProgramOp::BitwiseAnd);
        }
        Ok(())
    }

    /// Equality: `==`, `!=`
    fn parse_equality(&mut self) -> Result<(), IrError> {
        self.parse_relational()?;
        loop {
            match self.current().clone() {
                ArithToken::EqualEqual => {
                    self.advance();
                    self.parse_relational()?;
                    self.emit(ArithProgramOp::Equal);
                }
                ArithToken::BangEqual => {
                    self.advance();
                    self.parse_relational()?;
                    self.emit(ArithProgramOp::NotEqual);
                }
                _ => break,
            }
        }
        Ok(())
    }

    /// Relational: `<`, `>`, `<=`, `>=`
    fn parse_relational(&mut self) -> Result<(), IrError> {
        self.parse_shift()?;
        loop {
            match self.current().clone() {
                ArithToken::Less => {
                    self.advance();
                    self.parse_shift()?;
                    self.emit(ArithProgramOp::LessThan);
                }
                ArithToken::Greater => {
                    self.advance();
                    self.parse_shift()?;
                    self.emit(ArithProgramOp::GreaterThan);
                }
                ArithToken::LessEqual => {
                    self.advance();
                    self.parse_shift()?;
                    self.emit(ArithProgramOp::LessEqual);
                }
                ArithToken::GreaterEqual => {
                    self.advance();
                    self.parse_shift()?;
                    self.emit(ArithProgramOp::GreaterEqual);
                }
                _ => break,
            }
        }
        Ok(())
    }

    /// Shift: `<<`, `>>`
    fn parse_shift(&mut self) -> Result<(), IrError> {
        self.parse_additive()?;
        loop {
            match self.current().clone() {
                ArithToken::ShiftLeft => {
                    self.advance();
                    self.parse_additive()?;
                    self.emit(ArithProgramOp::ShiftLeft);
                }
                ArithToken::ShiftRight => {
                    self.advance();
                    self.parse_additive()?;
                    self.emit(ArithProgramOp::ShiftRight);
                }
                _ => break,
            }
        }
        Ok(())
    }

    /// Additive: `+`, `-`
    fn parse_additive(&mut self) -> Result<(), IrError> {
        self.parse_multiplicative()?;
        loop {
            match self.current().clone() {
                ArithToken::Plus => {
                    self.advance();
                    self.parse_multiplicative()?;
                    self.emit(ArithProgramOp::Add);
                }
                ArithToken::Minus => {
                    self.advance();
                    self.parse_multiplicative()?;
                    self.emit(ArithProgramOp::Subtract);
                }
                _ => break,
            }
        }
        Ok(())
    }

    /// Multiplicative: `*`, `/`, `%`
    fn parse_multiplicative(&mut self) -> Result<(), IrError> {
        self.parse_unary_prefix()?;
        loop {
            match self.current().clone() {
                ArithToken::Star => {
                    self.advance();
                    self.parse_unary_prefix()?;
                    self.emit(ArithProgramOp::Multiply);
                }
                ArithToken::Slash => {
                    self.advance();
                    self.parse_unary_prefix()?;
                    self.emit(ArithProgramOp::Divide);
                }
                ArithToken::Percent => {
                    self.advance();
                    self.parse_unary_prefix()?;
                    self.emit(ArithProgramOp::Modulo);
                }
                _ => break,
            }
        }
        Ok(())
    }

    /// Unary prefix: `+x`, `-x`, `~x`, `!x`, `++x`, `--x`
    fn parse_unary_prefix(&mut self) -> Result<(), IrError> {
        match self.current().clone() {
            ArithToken::Plus => {
                self.advance();
                self.parse_unary_prefix()?;
                self.emit(ArithProgramOp::UnaryPlus);
                Ok(())
            }
            ArithToken::Minus => {
                self.advance();
                self.parse_unary_prefix()?;
                self.emit(ArithProgramOp::UnaryMinus);
                Ok(())
            }
            ArithToken::Tilde => {
                self.advance();
                self.parse_unary_prefix()?;
                self.emit(ArithProgramOp::BitwiseNot);
                Ok(())
            }
            ArithToken::Bang => {
                self.advance();
                self.parse_unary_prefix()?;
                self.emit(ArithProgramOp::LogicalNot);
                Ok(())
            }
            ArithToken::PlusPlus => {
                self.advance();
                if let ArithToken::Identifier(name) = self.current().clone() {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.emit(ArithProgramOp::PreIncrement(sym));
                    Ok(())
                } else {
                    Err(IrError::invalid_arithmetic_expression(
                        None,
                        "expected identifier after '++'",
                        format!("got {:?}", self.current()),
                    ))
                }
            }
            ArithToken::MinusMinus => {
                self.advance();
                if let ArithToken::Identifier(name) = self.current().clone() {
                    self.advance();
                    let sym = self.ctx.module().intern_symbol(&name)?;
                    self.emit(ArithProgramOp::PreDecrement(sym));
                    Ok(())
                } else {
                    Err(IrError::invalid_arithmetic_expression(
                        None,
                        "expected identifier after '--'",
                        format!("got {:?}", self.current()),
                    ))
                }
            }
            _ => self.parse_postfix(),
        }
    }

    /// Postfix: `x++`, `x--`
    fn parse_postfix(&mut self) -> Result<(), IrError> {
        // If we see identifier followed by ++ or --, emit postfix.
        if let ArithToken::Identifier(name) = self.current().clone() {
            if self.tokens.get(self.pos + 1) == Some(&ArithToken::PlusPlus) {
                self.advance(); // identifier
                self.advance(); // ++
                let sym = self.ctx.module().intern_symbol(&name)?;
                self.emit(ArithProgramOp::PostIncrement(sym));
                return Ok(());
            }
            if self.tokens.get(self.pos + 1) == Some(&ArithToken::MinusMinus) {
                self.advance(); // identifier
                self.advance(); // --
                let sym = self.ctx.module().intern_symbol(&name)?;
                self.emit(ArithProgramOp::PostDecrement(sym));
                return Ok(());
            }
        }
        self.parse_primary()
    }

    /// Primary: integer, identifier, parenthesized expression.
    fn parse_primary(&mut self) -> Result<(), IrError> {
        match self.current().clone() {
            ArithToken::Integer(value) => {
                self.advance();
                self.emit(ArithProgramOp::PushLiteral(value));
                Ok(())
            }
            ArithToken::Identifier(name) => {
                self.advance();
                let sym = self.ctx.module().intern_symbol(&name)?;
                self.emit(ArithProgramOp::LoadVariable(sym));
                Ok(())
            }
            ArithToken::LParen => {
                self.advance();
                self.parse_expression()?;
                self.expect(&ArithToken::RParen)?;
                Ok(())
            }
            other => Err(IrError::invalid_arithmetic_expression(
                None,
                "unexpected token in arithmetic expression",
                format!("expected integer, identifier, or '(', got {other:?}"),
            )),
        }
    }
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Lowers an arithmetic expression into an `ArithProgram` and registers it.
///
/// `raw_text` includes the `$((` and `))` delimiters.
pub(crate) fn lower_arith_expression(
    ctx: &mut EmitContext<'_>,
    raw_text: &str,
) -> Result<ArithProgramId, IrError> {
    // Strip $((  )) delimiters.
    let inner = strip_arith_delimiters(raw_text);
    let trimmed = inner.trim();

    // Empty expression: POSIX says evaluates to 0.
    if trimmed.is_empty() {
        let program = ArithProgram {
            id: ArithProgramId::default(),
            ops: vec![ArithProgramOp::PushLiteral(0)],
        };
        return ctx.module().add_arith_program(program);
    }

    // Tokenize.
    let mut lexer = ArithLexer::new(trimmed);
    let mut tokens = Vec::new();
    loop {
        let tok = lexer.next_token()?;
        if tok == ArithToken::Eof {
            tokens.push(tok);
            break;
        }
        tokens.push(tok);
    }

    // Parse and emit.
    let mut emitter = ArithEmitter::new(ctx, tokens);
    emitter.parse_expression()?;

    // Verify we consumed all tokens.
    if *emitter.current() != ArithToken::Eof {
        return Err(IrError::invalid_arithmetic_expression(
            None,
            "trailing tokens in arithmetic expression",
            format!("unexpected {:?} after expression", emitter.current()),
        ));
    }

    let ops = emitter.ops;
    let program = ArithProgram {
        id: ArithProgramId::default(),
        ops,
    };
    ctx.module().add_arith_program(program)
}

/// Strips `$((` prefix and `))` suffix from arithmetic expression text.
fn strip_arith_delimiters(raw: &str) -> &str {
    let s = raw.strip_prefix("$((").unwrap_or(raw);
    s.strip_suffix("))").unwrap_or(s)
}
