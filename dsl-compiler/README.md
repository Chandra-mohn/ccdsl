# Credit Card DSL Compiler

**Version**: 0.1.0
**Status**: Initial Development

Compiler for the Credit Card Domain-Specific Language, transforming business-friendly DSL code into production Rust code.

---

## Overview

This compiler implements the complete toolchain for the Credit Card DSL:

1. **Lexer & Parser** (ANTLR4) - Tokenizes and parses `.dsl` files
2. **AST Construction** - Builds Abstract Syntax Tree from parse tree
3. **Semantic Analysis** - Type checking, validation, constraint verification
4. **Code Generation** - Pattern-based Rust code generation
5. **CLI Tool** - Command-line interface for compilation

---

## Quick Start

### Prerequisites

```bash
# Install Rust (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install Java (for ANTLR4)
# macOS:
brew install openjdk

# Download ANTLR4
curl -O https://www.antlr.org/download/antlr-4.13.1-complete.jar

# Set environment variable
export ANTLR4_JAR=/path/to/antlr-4.13.1-complete.jar
```

### Build

```bash
# Build the compiler
cargo build --release

# Run tests
cargo test

# Install CLI tool
cargo install --path .
```

### Usage

```bash
# Compile a DSL file
dslc compile examples/simple_entity.dsl

# Compile with output directory
dslc compile examples/simple_entity.dsl --output ./generated

# Compile entire directory
dslc compile examples/ --output ./generated

# Check syntax only (no code generation)
dslc check examples/simple_entity.dsl

# Show AST
dslc ast examples/simple_entity.dsl
```

---

## Project Structure

```
dsl-compiler/
├── Cargo.toml              # Rust project configuration
├── build.rs                # Build script (ANTLR4 code generation)
├── README.md               # This file
│
├── grammar/
│   └── CreditCardDSL.g4    # ANTLR4 grammar definition
│
├── src/
│   ├── main.rs             # CLI entry point
│   │
│   ├── ast/                # Abstract Syntax Tree
│   │   ├── mod.rs          # AST module root
│   │   ├── entity.rs       # Entity definitions
│   │   ├── workflow.rs     # Workflow definitions
│   │   ├── rule.rs         # Rule definitions
│   │   ├── parameter.rs    # Parameter definitions
│   │   ├── types.rs        # Type system
│   │   └── expressions.rs  # Expression AST nodes
│   │
│   ├── parser/             # Parser integration
│   │   ├── mod.rs          # Parser module (ANTLR4 generated code)
│   │   └── ast_builder.rs  # Parse tree → AST conversion
│   │
│   ├── semantic/           # Semantic analysis
│   │   ├── mod.rs
│   │   ├── type_checker.rs # Type checking
│   │   ├── validator.rs    # Constraint validation
│   │   └── pattern_validator.rs  # Pattern-specific validation
│   │
│   ├── codegen/            # Code generation
│   │   ├── mod.rs
│   │   ├── rust_generator.rs  # Main Rust code generator
│   │   │
│   │   ├── templates/      # Handlebars templates
│   │   │   ├── entity.rs.hbs
│   │   │   ├── workflow.rs.hbs
│   │   │   └── rule.rs.hbs
│   │   │
│   │   └── pattern_generators/  # Pattern-specific generators
│   │       ├── master_data.rs
│   │       ├── immutable_ledger.rs
│   │       ├── versioned_configuration.rs
│   │       ├── operational_parameters.rs
│   │       ├── event_log.rs
│   │       ├── state_machine.rs
│   │       ├── temporal_data.rs
│   │       └── reference_data.rs
│   │
│   └── errors/             # Error handling
│       ├── mod.rs
│       └── diagnostics.rs  # User-friendly error messages
│
├── examples/               # Example DSL files
│   ├── simple_entity.dsl
│   ├── simple_workflow.dsl
│   ├── simple_rule.dsl
│   └── simple_parameter.dsl
│
└── tests/                  # Integration tests
    ├── parser_tests.rs
    ├── semantic_tests.rs
    └── codegen_tests.rs
```

---

## Grammar Features

The ANTLR4 grammar (`grammar/CreditCardDSL.g4`) supports:

### Entity Definitions
- ✅ Pattern declarations (9 patterns)
- ✅ BIAN service domain mapping
- ✅ Identity and field groups
- ✅ Field types and qualifiers
- ✅ Relationships (belongs_to, has_many, etc.)
- ✅ Constraints (must block)
- ✅ State machines
- ✅ Versioning and temporal data

### Workflow Definitions
- ✅ Inputs and outputs
- ✅ Steps with actions
- ✅ Conditional routing (when/otherwise)
- ✅ Error handling (on_error)
- ✅ Wait for events with timeout
- ✅ All action types (load, create, update, etc.)

### Rule Definitions
- ✅ Given clause (inputs)
- ✅ When/then conditions
- ✅ Calculate clause with expressions
- ✅ Return values
- ✅ Otherwise clause

### Parameter Definitions (PCF)
- ✅ Type specification
- ✅ Default values
- ✅ Constraints
- ✅ Hot reload support
- ✅ Validation and documentation

### Expression System
- ✅ Arithmetic operations (+, -, *, /)
- ✅ Logical operations (and, or, not)
- ✅ Comparisons (==, !=, <, <=, >, >=)
- ✅ Field references (account.balance)
- ✅ Function calls
- ✅ Membership tests (is one of)
- ✅ Literals (strings, numbers, money, booleans, dates)

---

## AST Structure

The AST is defined in `src/ast/` with strong typing:

```rust
// Example: Entity AST
pub struct EntityDefinition {
    pub name: String,
    pub pattern: Pattern,
    pub business_domain: Option<String>,
    pub bian_mapping: Option<BianMapping>,
    pub identity: Option<IdentityClause>,
    pub field_groups: Vec<FieldGroup>,
    pub relationships: Vec<Relationship>,
    pub constraints: Vec<Constraint>,
    pub state_machine: Option<StateMachine>,
    pub versioning: Option<VersioningClause>,
    pub temporal: Option<TemporalClause>,
    pub location: SourceLocation,
}
```

Key features:
- **Type-safe**: All AST nodes are strongly typed
- **Location tracking**: Every node includes source location for error reporting
- **Pattern-aware**: Pattern-specific fields (state_machine, versioning, temporal)
- **Serializable**: Can be serialized to JSON for debugging

---

## Development Workflow

### 1. Modify Grammar

Edit `grammar/CreditCardDSL.g4`:

```antlr
// Add new construct
new_construct
    : NEW_KEYWORD IDENTIFIER NEWLINE
    ;
```

### 2. Regenerate Parser

```bash
# Build will automatically regenerate parser
cargo build
```

### 3. Update AST

Add corresponding AST node in `src/ast/`:

```rust
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NewConstruct {
    pub name: String,
    pub location: SourceLocation,
}
```

### 4. Implement AST Builder

Update `src/parser/ast_builder.rs` to convert parse tree → AST.

### 5. Add Semantic Analysis

Add validation in `src/semantic/`:

```rust
fn validate_new_construct(construct: &NewConstruct) -> Result<(), SemanticError> {
    // Validation logic
}
```

### 6. Implement Code Generation

Add generator in `src/codegen/`:

```rust
fn generate_new_construct(construct: &NewConstruct) -> String {
    // Code generation logic
}
```

### 7. Add Tests

```rust
#[test]
fn test_new_construct_parsing() {
    let input = "new my_construct";
    let ast = parse(input).unwrap();
    // Assertions
}
```

---

## Testing

### Unit Tests

```bash
# Run all tests
cargo test

# Run specific test module
cargo test parser_tests

# Run with output
cargo test -- --nocapture

# Run with specific test name
cargo test test_entity_parsing
```

### Integration Tests

```bash
# Test complete compilation pipeline
cargo test --test integration_tests

# Test specific example file
cargo run -- check examples/simple_entity.dsl
```

### Snapshot Testing

Using `insta` for generated code snapshot testing:

```rust
#[test]
fn test_entity_code_generation() {
    let code = generate_entity(&entity_def);
    insta::assert_snapshot!(code);
}

# Review snapshots
cargo insta review
```

---

## Code Generation

### Pattern-Based Generation

Each pattern has a dedicated generator:

```rust
// Example: master_data pattern
pub struct MasterDataGenerator;

impl PatternGenerator for MasterDataGenerator {
    fn generate(&self, entity: &EntityDefinition) -> Result<PatternCode> {
        let mut code = PatternCode::new();

        // Generate main entity struct
        code.add_struct(&self.generate_entity_struct(entity));

        // Generate history table struct
        code.add_struct(&self.generate_history_struct(entity));

        // Generate CRUD operations
        code.add_impl(&self.generate_crud_impl(entity));

        // Generate database migrations
        code.add_migration(&self.generate_migration(entity));

        Ok(code)
    }
}
```

### Template System

Uses Handlebars for code templates:

```handlebars
{{! templates/entity.rs.hbs }}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{struct_name}} {
    {{#each fields}}
    pub {{this.name}}: {{this.rust_type}},
    {{/each}}
}

impl {{struct_name}} {
    {{#each methods}}
    {{this.code}}
    {{/each}}
}
```

---

## Error Handling

Using `miette` for beautiful error messages:

```rust
// Example error
Error: Invalid field type for immutable_ledger pattern
   ┌─ examples/invalid.dsl:5:5
   │
 5 │     balance: money
   │     ^^^^^^^ cannot be modified in immutable_ledger pattern
   │
   = help: Use 'cannot_change' qualifier or choose different pattern
```

Features:
- **Source location tracking**: Precise error locations
- **Helpful messages**: Clear explanations of errors
- **Suggestions**: Hints for fixing errors
- **Color coding**: Terminal colors for readability

---

## Performance

### Compilation Speed

Target performance (for typical file):
- **Parsing**: < 50ms
- **Semantic analysis**: < 100ms
- **Code generation**: < 500ms
- **Total**: < 1 second

### Memory Usage

- **Small file** (< 100 lines): < 10MB
- **Medium file** (< 1000 lines): < 50MB
- **Large file** (< 10000 lines): < 200MB

---

## Debugging

### Print AST

```bash
# View AST in JSON format
dslc ast examples/simple_entity.dsl --format json

# View AST with pretty printing
dslc ast examples/simple_entity.dsl --format pretty
```

### Enable Debug Logging

```bash
# Set log level
RUST_LOG=debug dslc compile examples/simple_entity.dsl

# Trace-level logging
RUST_LOG=trace dslc compile examples/simple_entity.dsl
```

### Grammar Debugging

```bash
# Use ANTLR4's TestRig for grammar debugging
java -jar $ANTLR4_JAR -Dlanguage=Java CreditCardDSL.g4
javac CreditCardDSL*.java
java org.antlr.v4.gui.TestRig CreditCardDSL compilation_unit -gui examples/simple_entity.dsl
```

---

## Contributing

### Before Submitting Changes

1. **Format code**: `cargo fmt`
2. **Run linter**: `cargo clippy`
3. **Run tests**: `cargo test`
4. **Update documentation**: Update this README if needed

### Commit Message Format

```
<type>(<scope>): <subject>

<body>

<footer>
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

Example:
```
feat(parser): add support for pattern composition

- Allow combining multiple patterns with + operator
- Update grammar and AST nodes
- Add tests for pattern composition

Closes #42
```

---

## License

MIT License - See LICENSE file for details

---

## Resources

- **ANTLR4 Documentation**: https://github.com/antlr/antlr4/blob/master/doc/index.md
- **antlr4rust**: https://github.com/rrevenantt/antlr4rust
- **Rust Book**: https://doc.rust-lang.org/book/
- **DSL Language Specification**: ../DSL_LANGUAGE_SPECIFICATION.md
- **DSL Examples**: ../DSL_SAMPLES.md
