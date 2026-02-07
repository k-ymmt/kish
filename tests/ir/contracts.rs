use kish::ir::{
    BranchTarget, CodeObjectBuilder, CodeObjectId, IrModule, IrOptions, LoweringContext,
    encode_module, verify_module,
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
        max_arith_program_ops: 16,
        max_arity: 15,
        max_strings: 17,
        max_symbols: 18,
        max_word_programs: 19,
        max_redirect_programs: 20,
        max_arith_programs: 21,
        max_nesting_depth: 22,
    };

    let context = LoweringContext::new(options);
    assert_eq!(context.options(), options);
}

#[test]
fn lower_complete_command_succeeds() {
    let command = parse_complete_command("echo hello\n");
    let mut context = LoweringContext::new(IrOptions::default());

    let module = context
        .lower_complete_command(&command)
        .expect("lowering should succeed");

    assert!(!module.code_objects.is_empty());
}

#[test]
fn lower_program_succeeds() {
    let mut parser = parser_for("echo a\necho b\n");
    let program = parser
        .parse_program()
        .expect("program fixture should parse");

    let mut context = LoweringContext::new(IrOptions::default());
    let module = context
        .lower_program(&program)
        .expect("lowering should succeed");

    assert!(!module.code_objects.is_empty());
}

#[test]
fn encode_and_verify_public_contracts_exist() {
    let module = IrModule::default();

    let encoded = encode_module(&module).expect("empty module should encode successfully");
    assert!(encoded.code_objects.is_empty());
    assert!(encoded.word_programs.is_empty());
    assert!(encoded.redirect_programs.is_empty());
    assert!(encoded.arith_programs.is_empty());

    verify_module(&module).expect("empty module should verify successfully");
}

#[test]
fn vm_ir_builder_api_is_reachable() {
    let _target = BranchTarget::new(0);
    let _builder = CodeObjectBuilder::new(CodeObjectId::new(0));
}
