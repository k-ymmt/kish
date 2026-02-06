use kish::ir::{
    BranchTarget, CodeObjectBuilder, CodeObjectId, IrErrorKind, IrModule, IrOptions,
    LoweringContext, encode_module, verify_module,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};

fn parser_for(input: &str) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), ParseOptions::default(), stream)
}

fn parse_complete_command(input: &str) -> kish::parser::CompleteCommandAst {
    let mut parser = parser_for(input);
    let step = parser
        .parse_complete_command()
        .expect("Phase 0 parser fixture should parse");

    let ParseStep::Complete(command) = step else {
        panic!("expected one complete command, got {step:?}");
    };

    command
}

#[test]
fn lowering_context_retains_options_contract() {
    let options = IrOptions {
        max_instructions: 10,
        max_consts: 11,
        max_code_objects: 12,
        max_word_program_ops: 13,
        max_redirect_ops: 14,
        max_arity: 15,
    };

    let context = LoweringContext::new(options);
    assert_eq!(context.options(), options);
}

#[test]
fn lower_complete_command_returns_phase0_stub_error() {
    let command = parse_complete_command("echo hello\n");
    let mut context = LoweringContext::new(IrOptions::default());

    let error = context
        .lower_complete_command(&command)
        .expect_err("Phase 0 lowering should be stubbed");

    assert_eq!(error.kind, IrErrorKind::UnsupportedForm);
    assert_eq!(error.span, Some(command.span));
    assert!(error.message.contains("Phase 0"));
}

#[test]
fn lower_program_returns_phase0_stub_error() {
    let mut parser = parser_for("echo a\necho b\n");
    let program = parser
        .parse_program()
        .expect("program fixture should parse");

    let mut context = LoweringContext::new(IrOptions::default());
    let error = context
        .lower_program(&program)
        .expect_err("Phase 0 lowering should be stubbed");

    assert_eq!(error.kind, IrErrorKind::UnsupportedForm);
    assert_eq!(error.span, program.span);
    assert!(error.message.contains("Phase 0"));
}

#[test]
fn encode_and_verify_public_contracts_exist() {
    let module = IrModule::default();

    let encode_error = encode_module(&module).expect_err("Phase 0 encoder should be stubbed");
    assert_eq!(encode_error.kind, IrErrorKind::UnsupportedForm);

    let verify_error = verify_module(&module).expect_err("Phase 0 verifier should be stubbed");
    assert_eq!(verify_error.kind, IrErrorKind::UnsupportedForm);
}

#[test]
fn vm_ir_builder_api_is_reachable() {
    let _target = BranchTarget::new(0);
    let _builder = CodeObjectBuilder::new(CodeObjectId::new(0));
}
