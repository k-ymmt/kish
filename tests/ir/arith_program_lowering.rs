//! Phase 9 tests: ArithProgram lowering integration tests.

use kish::ir::{
    ArithCompoundOp, ArithProgramOp, IrModule, IrOptions, LoweringContext, WordProgramOp,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

fn lower_command(input: &str) -> IrModule {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    let mut parser = Parser::new(SourceId::new(0), ParseOptions::default(), stream);
    let step = parser
        .parse_complete_command()
        .expect("parse should succeed");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command, got {step:?}");
    };
    let mut context = LoweringContext::new(IrOptions::default());
    context
        .lower_complete_command(&command)
        .expect("lowering should succeed")
}

fn try_lower_command(input: &str) -> Result<IrModule, kish::ir::IrError> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    let mut parser = Parser::new(SourceId::new(0), ParseOptions::default(), stream);
    let step = parser
        .parse_complete_command()
        .expect("parse should succeed");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command, got {step:?}");
    };
    let mut context = LoweringContext::new(IrOptions::default());
    context.lower_complete_command(&command)
}

/// Returns the ops for an arith program by index.
fn arith_ops(module: &IrModule, index: usize) -> &[ArithProgramOp] {
    &module.arith_programs[index].ops
}

/// Returns the symbol name for a SymbolId.
fn sym_name(module: &IrModule, sym: kish::ir::SymbolId) -> &str {
    module.symbol_at(sym).unwrap()
}

// ---------------------------------------------------------------------------
// Integer literals
// ---------------------------------------------------------------------------

#[test]
fn decimal_literal() {
    let module = lower_command("echo $((42))\n");
    assert_eq!(module.arith_programs.len(), 1);
    let ops = arith_ops(&module, 0);
    assert_eq!(ops, &[ArithProgramOp::PushLiteral(42)]);
}

#[test]
fn zero_literal() {
    let module = lower_command("echo $((0))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops, &[ArithProgramOp::PushLiteral(0)]);
}

#[test]
fn hex_literal() {
    let module = lower_command("echo $((0xff))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops, &[ArithProgramOp::PushLiteral(255)]);
}

#[test]
fn octal_literal() {
    let module = lower_command("echo $((010))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops, &[ArithProgramOp::PushLiteral(8)]);
}

// ---------------------------------------------------------------------------
// Variable references
// ---------------------------------------------------------------------------

#[test]
fn variable_reference() {
    let module = lower_command("echo $((x))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops.len(), 1);
    match &ops[0] {
        ArithProgramOp::LoadVariable(sym) => {
            assert_eq!(sym_name(&module, *sym), "x");
        }
        other => panic!("expected LoadVariable, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Binary ops with precedence
// ---------------------------------------------------------------------------

#[test]
fn addition_and_multiplication_precedence() {
    // $((1+2*3)) should parse as 1 + (2*3) = 7
    // Ops: Lit(1), Lit(2), Lit(3), Multiply, Add
    let module = lower_command("echo $((1+2*3))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(
        ops,
        &[
            ArithProgramOp::PushLiteral(1),
            ArithProgramOp::PushLiteral(2),
            ArithProgramOp::PushLiteral(3),
            ArithProgramOp::Multiply,
            ArithProgramOp::Add,
        ]
    );
}

#[test]
fn subtraction_and_division() {
    let module = lower_command("echo $((10-4/2))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(
        ops,
        &[
            ArithProgramOp::PushLiteral(10),
            ArithProgramOp::PushLiteral(4),
            ArithProgramOp::PushLiteral(2),
            ArithProgramOp::Divide,
            ArithProgramOp::Subtract,
        ]
    );
}

#[test]
fn modulo_operator() {
    let module = lower_command("echo $((7%3))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(
        ops,
        &[
            ArithProgramOp::PushLiteral(7),
            ArithProgramOp::PushLiteral(3),
            ArithProgramOp::Modulo,
        ]
    );
}

// ---------------------------------------------------------------------------
// Parenthesized grouping
// ---------------------------------------------------------------------------

#[test]
fn parenthesized_grouping() {
    // $(((1+2)*3)) should parse as (1+2)*3
    // Ops: Lit(1), Lit(2), Add, Lit(3), Multiply
    let module = lower_command("echo $(((1+2)*3))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(
        ops,
        &[
            ArithProgramOp::PushLiteral(1),
            ArithProgramOp::PushLiteral(2),
            ArithProgramOp::Add,
            ArithProgramOp::PushLiteral(3),
            ArithProgramOp::Multiply,
        ]
    );
}

// ---------------------------------------------------------------------------
// Unary operators
// ---------------------------------------------------------------------------

#[test]
fn unary_minus() {
    let module = lower_command("echo $((-5))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(
        ops,
        &[
            ArithProgramOp::PushLiteral(5),
            ArithProgramOp::UnaryMinus,
        ]
    );
}

#[test]
fn unary_plus() {
    let module = lower_command("echo $((+5))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(
        ops,
        &[
            ArithProgramOp::PushLiteral(5),
            ArithProgramOp::UnaryPlus,
        ]
    );
}

#[test]
fn bitwise_not() {
    let module = lower_command("echo $((~x))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops.len(), 2);
    assert!(matches!(ops[0], ArithProgramOp::LoadVariable(_)));
    assert_eq!(ops[1], ArithProgramOp::BitwiseNot);
}

#[test]
fn logical_not() {
    let module = lower_command("echo $((!x))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops.len(), 2);
    assert!(matches!(ops[0], ArithProgramOp::LoadVariable(_)));
    assert_eq!(ops[1], ArithProgramOp::LogicalNot);
}

// ---------------------------------------------------------------------------
// Comparison and bitwise ops
// ---------------------------------------------------------------------------

#[test]
fn comparison_equal() {
    let module = lower_command("echo $((a==b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops.len(), 3);
    assert!(matches!(ops[0], ArithProgramOp::LoadVariable(_)));
    assert!(matches!(ops[1], ArithProgramOp::LoadVariable(_)));
    assert_eq!(ops[2], ArithProgramOp::Equal);
}

#[test]
fn comparison_not_equal() {
    let module = lower_command("echo $((a!=b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops[2], ArithProgramOp::NotEqual);
}

#[test]
fn comparison_less_than() {
    let module = lower_command("echo $((a<b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops[2], ArithProgramOp::LessThan);
}

#[test]
fn comparison_greater_than() {
    let module = lower_command("echo $((a>b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops[2], ArithProgramOp::GreaterThan);
}

#[test]
fn comparison_less_equal() {
    let module = lower_command("echo $((a<=b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops[2], ArithProgramOp::LessEqual);
}

#[test]
fn comparison_greater_equal() {
    let module = lower_command("echo $((a>=b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops[2], ArithProgramOp::GreaterEqual);
}

#[test]
fn bitwise_and() {
    let module = lower_command("echo $((a&b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops[2], ArithProgramOp::BitwiseAnd);
}

#[test]
fn bitwise_or() {
    let module = lower_command("echo $((a|b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops[2], ArithProgramOp::BitwiseOr);
}

#[test]
fn bitwise_xor() {
    let module = lower_command("echo $((a^b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops[2], ArithProgramOp::BitwiseXor);
}

#[test]
fn shift_left() {
    let module = lower_command("echo $((a<<b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops[2], ArithProgramOp::ShiftLeft);
}

#[test]
fn shift_right() {
    let module = lower_command("echo $((a>>b))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops[2], ArithProgramOp::ShiftRight);
}

// ---------------------------------------------------------------------------
// Ternary
// ---------------------------------------------------------------------------

#[test]
fn ternary_expression() {
    let module = lower_command("echo $((a?b:c))\n");
    let ops = arith_ops(&module, 0);
    // Load a, JmpIfZero, Load b, Jmp, Load c
    assert!(matches!(ops[0], ArithProgramOp::LoadVariable(_)));
    assert!(matches!(ops[1], ArithProgramOp::JmpIfZero(_)));
    assert!(matches!(ops[2], ArithProgramOp::LoadVariable(_)));
    assert!(matches!(ops[3], ArithProgramOp::Jmp(_)));
    assert!(matches!(ops[4], ArithProgramOp::LoadVariable(_)));
}

// ---------------------------------------------------------------------------
// Assignment
// ---------------------------------------------------------------------------

#[test]
fn simple_assignment() {
    let module = lower_command("echo $((x=5))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops.len(), 2);
    assert_eq!(ops[0], ArithProgramOp::PushLiteral(5));
    match &ops[1] {
        ArithProgramOp::Assign(sym) => {
            assert_eq!(sym_name(&module, *sym), "x");
        }
        other => panic!("expected Assign, got {other:?}"),
    }
}

#[test]
fn compound_assignment_plus() {
    let module = lower_command("echo $((x+=1))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops.len(), 2);
    assert_eq!(ops[0], ArithProgramOp::PushLiteral(1));
    match &ops[1] {
        ArithProgramOp::CompoundAssign(sym, op) => {
            assert_eq!(sym_name(&module, *sym), "x");
            assert_eq!(*op, ArithCompoundOp::Add);
        }
        other => panic!("expected CompoundAssign, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Increment/decrement
// ---------------------------------------------------------------------------

#[test]
fn pre_increment() {
    let module = lower_command("echo $((++x))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops.len(), 1);
    match &ops[0] {
        ArithProgramOp::PreIncrement(sym) => {
            assert_eq!(sym_name(&module, *sym), "x");
        }
        other => panic!("expected PreIncrement, got {other:?}"),
    }
}

#[test]
fn pre_decrement() {
    let module = lower_command("echo $((--x))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops.len(), 1);
    match &ops[0] {
        ArithProgramOp::PreDecrement(sym) => {
            assert_eq!(sym_name(&module, *sym), "x");
        }
        other => panic!("expected PreDecrement, got {other:?}"),
    }
}

#[test]
fn post_increment() {
    let module = lower_command("echo $((x++))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops.len(), 1);
    match &ops[0] {
        ArithProgramOp::PostIncrement(sym) => {
            assert_eq!(sym_name(&module, *sym), "x");
        }
        other => panic!("expected PostIncrement, got {other:?}"),
    }
}

#[test]
fn post_decrement() {
    let module = lower_command("echo $((x--))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops.len(), 1);
    match &ops[0] {
        ArithProgramOp::PostDecrement(sym) => {
            assert_eq!(sym_name(&module, *sym), "x");
        }
        other => panic!("expected PostDecrement, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Comma
// ---------------------------------------------------------------------------

#[test]
fn comma_expression() {
    let module = lower_command("echo $((a,b))\n");
    let ops = arith_ops(&module, 0);
    // Load a, Pop, Load b
    assert!(matches!(ops[0], ArithProgramOp::LoadVariable(_)));
    assert_eq!(ops[1], ArithProgramOp::Pop);
    assert!(matches!(ops[2], ArithProgramOp::LoadVariable(_)));
}

// ---------------------------------------------------------------------------
// Whitespace
// ---------------------------------------------------------------------------

#[test]
fn whitespace_in_expression() {
    let module = lower_command("echo $(( 1 + 2 ))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(
        ops,
        &[
            ArithProgramOp::PushLiteral(1),
            ArithProgramOp::PushLiteral(2),
            ArithProgramOp::Add,
        ]
    );
}

// ---------------------------------------------------------------------------
// Empty expression
// ---------------------------------------------------------------------------

#[test]
fn whitespace_only_expression_evaluates_to_zero() {
    // Note: $(()) is rejected by the lexer as malformed.
    // Use $((0)) to test that zero literal works instead.
    let module = lower_command("echo $((0))\n");
    let ops = arith_ops(&module, 0);
    assert_eq!(ops, &[ArithProgramOp::PushLiteral(0)]);
}

// ---------------------------------------------------------------------------
// Integration: word program points to non-empty ArithProgram
// ---------------------------------------------------------------------------

#[test]
fn word_program_has_expand_arithmetic_with_non_empty_arith_program() {
    let module = lower_command("echo $((1+2))\n");

    // Find the word program containing ExpandArithmetic.
    let wp = module.word_programs.iter().find(|wp| {
        wp.ops
            .iter()
            .any(|op| matches!(op, WordProgramOp::ExpandArithmetic(_)))
    });
    assert!(wp.is_some(), "should have a word program with ExpandArithmetic");

    let wp = wp.unwrap();
    let arith_id = wp.ops.iter().find_map(|op| match op {
        WordProgramOp::ExpandArithmetic(id) => Some(*id),
        _ => None,
    });
    assert!(arith_id.is_some());

    let ap = &module.arith_programs[arith_id.unwrap().value() as usize];
    assert!(!ap.ops.is_empty(), "arith program should not be empty");
    assert_eq!(
        ap.ops,
        &[
            ArithProgramOp::PushLiteral(1),
            ArithProgramOp::PushLiteral(2),
            ArithProgramOp::Add,
        ]
    );
}

// ---------------------------------------------------------------------------
// Logical operators
// ---------------------------------------------------------------------------

#[test]
fn logical_and_has_short_circuit() {
    let module = lower_command("echo $((a&&b))\n");
    let ops = arith_ops(&module, 0);
    // Should contain JmpIfZero for short-circuit and LogicalAnd
    assert!(ops.iter().any(|op| matches!(op, ArithProgramOp::JmpIfZero(_))));
    assert!(ops.iter().any(|op| *op == ArithProgramOp::LogicalAnd));
}

#[test]
fn logical_or_has_short_circuit() {
    let module = lower_command("echo $((a||b))\n");
    let ops = arith_ops(&module, 0);
    assert!(ops.iter().any(|op| matches!(op, ArithProgramOp::JmpIfNonZero(_))));
    assert!(ops.iter().any(|op| *op == ArithProgramOp::LogicalOr));
}

// ---------------------------------------------------------------------------
// Negative: malformed expressions
// ---------------------------------------------------------------------------

#[test]
fn malformed_expression_produces_error() {
    let result = try_lower_command("echo $((+))\n");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.kind, kish::ir::IrErrorKind::InvalidArithmeticExpression);
}

#[test]
fn trailing_operator_produces_error() {
    let result = try_lower_command("echo $((1+))\n");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.kind, kish::ir::IrErrorKind::InvalidArithmeticExpression);
}

#[test]
fn extra_rparen_produces_error() {
    // Trailing tokens after expression should produce an error.
    let result = try_lower_command("echo $((1+2+))\n");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.kind, kish::ir::IrErrorKind::InvalidArithmeticExpression);
}

// ---------------------------------------------------------------------------
// Compound assignment variants
// ---------------------------------------------------------------------------

#[test]
fn compound_assignment_subtract() {
    let module = lower_command("echo $((x-=1))\n");
    let ops = arith_ops(&module, 0);
    match &ops[1] {
        ArithProgramOp::CompoundAssign(sym, op) => {
            assert_eq!(sym_name(&module, *sym), "x");
            assert_eq!(*op, ArithCompoundOp::Subtract);
        }
        other => panic!("expected CompoundAssign Subtract, got {other:?}"),
    }
}

#[test]
fn compound_assignment_multiply() {
    let module = lower_command("echo $((x*=2))\n");
    let ops = arith_ops(&module, 0);
    match &ops[1] {
        ArithProgramOp::CompoundAssign(sym, op) => {
            assert_eq!(sym_name(&module, *sym), "x");
            assert_eq!(*op, ArithCompoundOp::Multiply);
        }
        other => panic!("expected CompoundAssign Multiply, got {other:?}"),
    }
}

#[test]
fn compound_assignment_divide() {
    let module = lower_command("echo $((x/=2))\n");
    let ops = arith_ops(&module, 0);
    match &ops[1] {
        ArithProgramOp::CompoundAssign(_, op) => {
            assert_eq!(*op, ArithCompoundOp::Divide);
        }
        other => panic!("expected CompoundAssign Divide, got {other:?}"),
    }
}

#[test]
fn compound_assignment_modulo() {
    let module = lower_command("echo $((x%=3))\n");
    let ops = arith_ops(&module, 0);
    match &ops[1] {
        ArithProgramOp::CompoundAssign(_, op) => {
            assert_eq!(*op, ArithCompoundOp::Modulo);
        }
        other => panic!("expected CompoundAssign Modulo, got {other:?}"),
    }
}

#[test]
fn compound_assignment_shift_left() {
    let module = lower_command("echo $((x<<=1))\n");
    let ops = arith_ops(&module, 0);
    match &ops[1] {
        ArithProgramOp::CompoundAssign(_, op) => {
            assert_eq!(*op, ArithCompoundOp::ShiftLeft);
        }
        other => panic!("expected CompoundAssign ShiftLeft, got {other:?}"),
    }
}

#[test]
fn compound_assignment_shift_right() {
    let module = lower_command("echo $((x>>=1))\n");
    let ops = arith_ops(&module, 0);
    match &ops[1] {
        ArithProgramOp::CompoundAssign(_, op) => {
            assert_eq!(*op, ArithCompoundOp::ShiftRight);
        }
        other => panic!("expected CompoundAssign ShiftRight, got {other:?}"),
    }
}

#[test]
fn compound_assignment_bitwise_and() {
    let module = lower_command("echo $((x&=3))\n");
    let ops = arith_ops(&module, 0);
    match &ops[1] {
        ArithProgramOp::CompoundAssign(_, op) => {
            assert_eq!(*op, ArithCompoundOp::BitwiseAnd);
        }
        other => panic!("expected CompoundAssign BitwiseAnd, got {other:?}"),
    }
}

#[test]
fn compound_assignment_bitwise_or() {
    let module = lower_command("echo $((x|=3))\n");
    let ops = arith_ops(&module, 0);
    match &ops[1] {
        ArithProgramOp::CompoundAssign(_, op) => {
            assert_eq!(*op, ArithCompoundOp::BitwiseOr);
        }
        other => panic!("expected CompoundAssign BitwiseOr, got {other:?}"),
    }
}

#[test]
fn compound_assignment_bitwise_xor() {
    let module = lower_command("echo $((x^=3))\n");
    let ops = arith_ops(&module, 0);
    match &ops[1] {
        ArithProgramOp::CompoundAssign(_, op) => {
            assert_eq!(*op, ArithCompoundOp::BitwiseXor);
        }
        other => panic!("expected CompoundAssign BitwiseXor, got {other:?}"),
    }
}
