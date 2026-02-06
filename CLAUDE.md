# Important Rules

- Write in English when writing words in files.
- Commit at the end of work, except when there were no questions or changes.

# Project

This project is a shell that prioritizes POSIX compatibility.
This project uses Rust.

## Architecture

```
src/
├── main.rs              # Entry point
├── lib.rs               # Crate root, re-exports public modules
├── lexer/               # Tokenizer (Phase 1)
│   ├── scanner.rs       # Core scanning loop
│   ├── cursor.rs        # Byte-level source cursor
│   ├── token.rs         # Token, TokenKind, QuoteMarker, SubstitutionMarker
│   ├── operator.rs      # Operator recognition
│   ├── quote.rs         # Quote handling (single, double, backslash, $'')
│   ├── substitution.rs  # $(), ``, $(()) recognition
│   ├── heredoc.rs       # Here-document scanning
│   ├── alias.rs         # Alias expansion hooks
│   ├── span.rs          # Byte-offset source spans
│   └── diagnostics.rs   # Lexer error reporting
├── parser/              # AST construction (Phase 2-3)
│   ├── parser.rs        # Recursive-descent parser
│   ├── ast.rs           # AST node types (ProgramAst, CommandAst, etc.)
│   ├── classifier.rs    # Token classification (reserved words, assignments)
│   ├── token_stream.rs  # Buffered token stream with lookahead
│   ├── arena.rs         # Node arena for AST allocation
│   ├── recovery.rs      # Error recovery strategies
│   └── error.rs         # Parse error types
└── ir/                  # Intermediate representation (Phase 4-8)
    ├── hir.rs           # High-level IR types (HirProgram, HirSimpleCommand, etc.)
    ├── hir_builder.rs   # HIR construction helpers
    ├── bytecode.rs      # VM instruction set, CommandDispatchHint
    ├── ids.rs           # Typed ID newtypes (CodeObjectId, SymbolId, etc.)
    ├── program.rs       # CodeObject, IrModule, IrModuleBuilder, pool interning
    ├── error.rs         # IR error types
    ├── encode.rs        # IR-to-bytecode encoding (stub)
    ├── verify.rs        # IR verification pass (stub)
    └── lower/           # Lowering passes
        ├── from_parser.rs      # Parser AST -> HIR (Phase 4)
        ├── emit.rs             # HIR -> VM IR emission (Phase 5-6)
        ├── simple_command.rs   # Simple command lowering helpers
        ├── compound.rs         # Compound command lowering helpers
        ├── control_flow.rs     # Control flow lowering helpers
        ├── function.rs         # Function definition lowering
        ├── word_program.rs     # Word expansion subprogram lowering (stub)
        ├── redirect_program.rs # Redirect subprogram lowering (stub)
        └── arith_program.rs    # Arithmetic subprogram lowering (stub)

tests/
├── lexer/       # Lexer tests
├── parser/      # Parser tests
├── ir/          # IR and lowering tests
└── fixtures/    # Test input files
```

## Pipeline

Source text → Lexer (tokens) → Parser (AST) → HIR lowering (Phase 4) → VM IR emission (Phase 5-6) → Encode (Phase 9, stub) → VM execution (future).

## Commands

- `cargo build` — Build the project
- `cargo test` — Run all tests
- `cargo clippy` — Lint check
