# Credit Card DSL - Toolchain Architecture

**Date**: 2025-11-11
**Status**: Architecture Decision & Implementation Plan

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Technology Comparison](#technology-comparison)
3. [Recommended Approach](#recommended-approach)
4. [Implementation Phases](#implementation-phases)
5. [Technical Stack](#technical-stack)
6. [Architecture Details](#architecture-details)
7. [Development Roadmap](#development-roadmap)

---

## Executive Summary

### Decision: ANTLR4 + tree-sitter Hybrid Approach

**Primary toolchain: ANTLR4**
- Grammar definition and parser generation
- AST construction and semantic analysis
- Code generation (DSL → Rust)
- Compiler infrastructure

**Secondary toolchain: tree-sitter**
- Editor integration and LSP
- Syntax highlighting
- Incremental parsing for IDE features

### Rationale

Your DSL is **text-based, YAML-inspired, business-friendly** - this perfectly matches ANTLR4's strengths:
✅ Plain text files (git-friendly)
✅ Indentation-based syntax (ANTLR4 handles well)
✅ Business users edit actual text
✅ Full control over Rust code generation
✅ Editor-agnostic via LSP
✅ Mature, production-proven

JetBrains MPS **does not fit** because:
❌ Projectional editing (not text-based)
❌ Complex version control
❌ Forces IntelliJ dependency
❌ Harder to achieve YAML-like appearance
❌ Business users confused by non-text editing

---

## Technology Comparison

### ANTLR4 (ANother Tool for Language Recognition)

**What it is**: Parser generator that produces parsers from grammar definitions

**Strengths**:
- ✅ Mature (20+ years), battle-tested in production
- ✅ Text-based grammar files (`.g4` format)
- ✅ Generates parsers in Java, C#, Python, JavaScript, Go, C++, **Rust**
- ✅ Excellent error recovery and diagnostics
- ✅ Strong community support and documentation
- ✅ Full control over AST structure
- ✅ Works with any editor via LSP
- ✅ Straightforward code generation pipeline
- ✅ Handles indentation-based syntax well
- ✅ Version control friendly

**Weaknesses**:
- ⚠️ Manual LSP implementation required
- ⚠️ Need to build editor tooling yourself
- ⚠️ Grammar debugging can be complex
- ⚠️ AST → Code generation is manual
- ⚠️ Type checking requires separate passes

**Best for**:
- Text-based DSLs (like yours)
- Teams comfortable with traditional compiler development
- Projects needing multi-language support
- When you want full control over every layer

**Current Status (2025)**:
- `antlr4rust` runtime stable on Rust stable compiler
- Active development and maintenance
- Available as `antlr-rust` crate on crates.io
- Generate parsers: `java -jar antlr4.jar -Dlanguage=Rust MyGrammar.g4`

---

### JetBrains MPS (Meta Programming System)

**What it is**: Language workbench with projectional editing

**Strengths**:
- ✅ All-in-one language workbench
- ✅ Built-in projectional editor
- ✅ Type system built-in
- ✅ Code generation templates included
- ✅ Automatic editor with syntax highlighting, completion, validation
- ✅ Constraint checking integrated
- ✅ Testing framework included
- ✅ Refactoring support out-of-box

**Weaknesses**:
- ❌ Steep learning curve (very different paradigm)
- ❌ **Projectional editing** (not text-based) - **critical mismatch** for your YAML-like syntax
- ❌ Heavy dependency on IntelliJ ecosystem
- ❌ Harder to integrate with non-JetBrains editors
- ❌ Version control of projectional models is complex
- ❌ Community much smaller than ANTLR
- ❌ Java-centric (Rust code generation less common)
- ❌ Business users may be confused (not editing text directly)

**Best for**:
- Complex DSLs with sophisticated type systems
- When projectional editing is desired
- Teams already using IntelliJ platform
- Internal enterprise DSLs
- When you want batteries-included approach

**Critical issue for your DSL**:
- Your DSL is designed to look like YAML and be business-friendly
- MPS uses projectional editing - users don't edit text, they edit an AST projection
- This fundamentally conflicts with your "business-friendly text" design goal

---

### tree-sitter (Incremental Parser)

**What it is**: Parser generator and incremental parsing library

**Strengths**:
- ✅ Modern, gaining rapid adoption
- ✅ Used by Neovim, GitHub, Atom, Helix
- ✅ **Incremental parsing** (excellent for editors)
- ✅ **Error-tolerant by design** (better IDE experience)
- ✅ Generates C parsers (easy FFI to Rust)
- ✅ Query system for syntax highlighting/navigation
- ✅ Fast (millisecond response times)
- ✅ Growing ecosystem
- ✅ Excellent for LSP implementations

**Weaknesses**:
- ⚠️ Less mature than ANTLR4
- ⚠️ Smaller community
- ⚠️ GLR parsing (more complex grammar design)
- ⚠️ Better for editor integration than compilation

**Best for**:
- Editor/IDE integration
- LSP server implementation
- Syntax highlighting
- Code navigation and folding

**Complementary to ANTLR4**:
- ANTLR4 for compiler/codegen
- tree-sitter for editor experience
- Best of both worlds

---

## Recommended Approach

### Hybrid Architecture: ANTLR4 + tree-sitter

```
┌─────────────────────────────────────────────────────────────────┐
│                       DSL Source Files (.dsl)                   │
│                   (Plain text, YAML-inspired)                   │
└────────────────────┬──────────────────────┬────────────────────┘
                     │                       │
                     │                       │
        ┌────────────▼─────────────┐    ┌───▼──────────────────┐
        │   ANTLR4 Parser          │    │  tree-sitter Parser  │
        │   (Compiler Pipeline)    │    │  (Editor/LSP)        │
        └────────────┬─────────────┘    └───┬──────────────────┘
                     │                       │
                     │                       │
        ┌────────────▼─────────────┐    ┌───▼──────────────────┐
        │   AST + Semantic         │    │  Syntax Tree         │
        │   Analysis               │    │  + Queries           │
        └────────────┬─────────────┘    └───┬──────────────────┘
                     │                       │
                     │                       │
        ┌────────────▼─────────────┐    ┌───▼──────────────────┐
        │   Rust Code Generator    │    │  LSP Server          │
        │   (Pattern-based)        │    │  (Completion, etc)   │
        └────────────┬─────────────┘    └───┬──────────────────┘
                     │                       │
                     │                       │
        ┌────────────▼─────────────┐    ┌───▼──────────────────┐
        │   Generated Rust Code    │    │  VS Code/Editors     │
        │   (Backend Runtime)      │    │  (Great UX)          │
        └──────────────────────────┘    └──────────────────────┘
```

### Why This Approach?

**ANTLR4 for compilation** because:
1. ✅ Your DSL is text-based YAML-inspired syntax
2. ✅ Business users need to edit plain text files
3. ✅ Version control (git) works naturally
4. ✅ Full control over Rust code generation
5. ✅ Mature, proven for production DSLs
6. ✅ Excellent error messages for business users

**tree-sitter for editor integration** because:
1. ✅ Superior editor experience
2. ✅ Incremental parsing (instant feedback)
3. ✅ Better syntax highlighting
4. ✅ Modern LSP integration
5. ✅ Works across editors (VS Code, Neovim, Helix)

**NOT JetBrains MPS** because:
1. ❌ Projectional editing conflicts with text-based design
2. ❌ Business users expect to edit YAML-like text
3. ❌ Version control complexity
4. ❌ Forces IntelliJ dependency

---

## Implementation Phases

### Phase 1: Core Compiler (ANTLR4) - 4-6 weeks

**Goal**: DSL → Rust code generation

**Deliverables**:
1. ANTLR4 grammar (`.g4` file)
2. Rust parser via antlr4rust
3. AST definition (Rust structs)
4. Semantic analyzer (type checking, validation)
5. Code generator (pattern-based templates)
6. CLI compiler tool

**Milestones**:
- Week 1-2: Grammar definition and parser
- Week 3-4: AST and semantic analysis
- Week 5-6: Code generation and testing

---

### Phase 2: Editor Support (tree-sitter) - 2-3 weeks

**Goal**: Great editing experience in VS Code and other editors

**Deliverables**:
1. tree-sitter grammar (parallel to ANTLR4)
2. Syntax highlighting queries
3. Code folding and navigation
4. VS Code extension (basic)

**Milestones**:
- Week 1: tree-sitter grammar
- Week 2: Queries and highlighting
- Week 3: VS Code extension

---

### Phase 3: LSP Server - 3-4 weeks

**Goal**: IDE features (completion, diagnostics, go-to-definition)

**Deliverables**:
1. LSP server (Rust-based)
2. Auto-completion (entities, fields, keywords)
3. Diagnostics (errors, warnings)
4. Go-to-definition
5. Hover documentation
6. Formatting

**Milestones**:
- Week 1: LSP server skeleton
- Week 2: Completion and diagnostics
- Week 3: Navigation features
- Week 4: Polish and testing

---

### Phase 4: Advanced Features - 4-6 weeks

**Goal**: Production-ready tooling

**Deliverables**:
1. BIAN service domain validation
2. Pattern-specific validation
3. Refactoring support
4. Debugging support
5. Performance optimization
6. Documentation generator

---

## Technical Stack

### Core Compiler (ANTLR4)

```rust
// Project structure
dsl-compiler/
├── Cargo.toml
├── build.rs              // ANTLR4 code generation
├── grammar/
│   └── CreditCardDSL.g4  // ANTLR4 grammar
├── src/
│   ├── main.rs           // CLI entry point
│   ├── parser/           // Generated parser code
│   ├── ast/              // AST definitions
│   │   ├── mod.rs
│   │   ├── entity.rs
│   │   ├── workflow.rs
│   │   ├── rule.rs
│   │   └── parameter.rs
│   ├── semantic/         // Semantic analysis
│   │   ├── mod.rs
│   │   ├── type_checker.rs
│   │   ├── validator.rs
│   │   └── pattern_validator.rs
│   ├── codegen/          // Code generation
│   │   ├── mod.rs
│   │   ├── rust_generator.rs
│   │   ├── templates/
│   │   │   ├── entity.rs.hbs
│   │   │   ├── workflow.rs.hbs
│   │   │   └── rule.rs.hbs
│   │   └── pattern_generators/
│   │       ├── master_data.rs
│   │       ├── immutable_ledger.rs
│   │       └── ...
│   └── errors/           // Error handling
│       ├── mod.rs
│       └── diagnostics.rs
└── tests/
    ├── parser_tests.rs
    ├── semantic_tests.rs
    └── codegen_tests.rs
```

### Dependencies

```toml
[dependencies]
# ANTLR4 runtime
antlr-rust = "0.3"

# Code generation
handlebars = "5.0"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# Error handling
miette = { version = "7.0", features = ["fancy"] }
thiserror = "1.0"

# CLI
clap = { version = "4.0", features = ["derive"] }

# Utilities
anyhow = "1.0"
log = "0.4"
env_logger = "0.11"

[build-dependencies]
# For running ANTLR4 during build
antlr4rust-build = "0.3"
```

---

### Editor Support (tree-sitter)

```
dsl-tree-sitter/
├── package.json
├── grammar.js           // tree-sitter grammar
├── src/
│   ├── parser.c         // Generated parser
│   └── scanner.c        // Custom scanner (for indentation)
├── queries/
│   ├── highlights.scm   // Syntax highlighting
│   ├── locals.scm       // Local bindings
│   └── injections.scm   // Language injections
└── test/
    └── corpus/          // Test cases
```

---

### LSP Server

```rust
dsl-lsp/
├── Cargo.toml
├── src/
│   ├── main.rs          // LSP server entry
│   ├── server.rs        // LSP protocol handling
│   ├── completion.rs    // Auto-completion
│   ├── diagnostics.rs   // Error/warning reporting
│   ├── hover.rs         // Hover documentation
│   ├── goto.rs          // Go-to-definition
│   ├── formatting.rs    // Code formatting
│   └── tree_manager.rs  // tree-sitter integration
└── tests/
    └── lsp_tests.rs
```

### LSP Dependencies

```toml
[dependencies]
# LSP protocol
tower-lsp = "0.20"
tokio = { version = "1.0", features = ["full"] }

# tree-sitter integration
tree-sitter = "0.20"
tree-sitter-creditcard-dsl = { path = "../dsl-tree-sitter" }

# Compiler integration (for semantic analysis)
dsl-compiler = { path = "../dsl-compiler" }

# Utilities
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

---

### VS Code Extension

```
dsl-vscode/
├── package.json
├── tsconfig.json
├── src/
│   ├── extension.ts     // Extension entry point
│   └── client.ts        // LSP client
└── syntaxes/
    └── dsl.tmGrammar.json  // TextMate grammar (fallback)
```

---

## Architecture Details

### ANTLR4 Grammar Structure

```antlr
grammar CreditCardDSL;

// Parser rules
compilation_unit
    : definition* EOF
    ;

definition
    : entity_definition
    | workflow_definition
    | rule_definition
    | parameter_definition
    ;

entity_definition
    : DEFINE ENTITY COLON IDENTIFIER NEWLINE
      INDENT
      entity_body
      DEDENT
    ;

entity_body
    : (pattern_clause
      | identity_clause
      | fields_clause
      | relationships_clause
      | constraints_clause)*
    ;

pattern_clause
    : PATTERN COLON pattern_name NEWLINE
    ;

pattern_name
    : MASTER_DATA
    | IMMUTABLE_LEDGER
    | VERSIONED_CONFIGURATION
    | OPERATIONAL_PARAMETERS
    | EVENT_LOG
    | STATE_MACHINE
    | TEMPORAL_DATA
    | REFERENCE_DATA
    ;

// Lexer rules
DEFINE: 'define';
ENTITY: 'entity';
WORKFLOW: 'workflow';
RULE: 'rule';
PATTERN: 'pattern';
COLON: ':';

MASTER_DATA: 'master_data';
IMMUTABLE_LEDGER: 'immutable_ledger';
// ... more patterns

IDENTIFIER: [a-z][a-z0-9_]*;

// Indentation handling
NEWLINE: '\r'? '\n' SPACES?;
INDENT: { indentation increases };
DEDENT: { indentation decreases };
SPACES: [ \t]+;

WS: [ \t]+ -> skip;
COMMENT: '#' ~[\r\n]* -> skip;
```

**Indentation Handling**:
- Custom lexer actions track indentation levels
- INDENT/DEDENT tokens emitted on indentation changes
- Similar to Python's approach

---

### AST Structure

```rust
// ast/mod.rs
#[derive(Debug, Clone, PartialEq)]
pub struct CompilationUnit {
    pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Entity(EntityDefinition),
    Workflow(WorkflowDefinition),
    Rule(RuleDefinition),
    Parameter(ParameterDefinition),
}

// ast/entity.rs
#[derive(Debug, Clone, PartialEq)]
pub struct EntityDefinition {
    pub name: String,
    pub pattern: Pattern,
    pub business_domain: Option<String>,
    pub bian_mapping: Option<BianMapping>,
    pub identity: IdentityClause,
    pub fields: Vec<FieldGroup>,
    pub relationships: Vec<Relationship>,
    pub constraints: Vec<Constraint>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    MasterData,
    ImmutableLedger,
    VersionedConfiguration,
    OperationalParameters,
    EventLog,
    StateMachine,
    TemporalData,
    ReferenceData,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldGroup {
    pub group_name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub field_type: FieldType,
    pub qualifiers: Vec<FieldQualifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldType {
    Text,
    Number,
    Money,
    Date,
    Timestamp,
    Boolean,
    Email,
    Phone,
    Percentage,
    Duration,
    Time,
    Entity(String), // Reference to another entity
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldQualifier {
    Unique,
    Required,
    CannotChange,
    Default(Value),
    Between { min: Value, max: Value },
    Values(Vec<String>),
}
```

---

### Code Generation Pipeline

```rust
// codegen/mod.rs
pub struct CodeGenerator {
    handlebars: Handlebars<'static>,
    pattern_generators: HashMap<Pattern, Box<dyn PatternGenerator>>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        let mut handlebars = Handlebars::new();

        // Register templates
        handlebars.register_template_file("entity", "templates/entity.rs.hbs")?;
        handlebars.register_template_file("workflow", "templates/workflow.rs.hbs")?;

        // Register pattern generators
        let mut pattern_generators = HashMap::new();
        pattern_generators.insert(
            Pattern::MasterData,
            Box::new(MasterDataGenerator::new()),
        );
        pattern_generators.insert(
            Pattern::ImmutableLedger,
            Box::new(ImmutableLedgerGenerator::new()),
        );
        // ... more patterns

        Self { handlebars, pattern_generators }
    }

    pub fn generate(&self, ast: &CompilationUnit) -> Result<GeneratedCode> {
        let mut generated = GeneratedCode::new();

        for definition in &ast.definitions {
            match definition {
                Definition::Entity(entity) => {
                    let code = self.generate_entity(entity)?;
                    generated.add_module(&entity.name, code);
                }
                Definition::Workflow(workflow) => {
                    let code = self.generate_workflow(workflow)?;
                    generated.add_module(&workflow.name, code);
                }
                // ... more definition types
            }
        }

        Ok(generated)
    }

    fn generate_entity(&self, entity: &EntityDefinition) -> Result<String> {
        // Get pattern-specific generator
        let generator = self.pattern_generators
            .get(&entity.pattern)
            .ok_or_else(|| anyhow!("Unknown pattern: {:?}", entity.pattern))?;

        // Generate pattern-specific code
        let pattern_code = generator.generate(entity)?;

        // Apply template
        let context = EntityContext::from(entity, pattern_code);
        let code = self.handlebars.render("entity", &context)?;

        Ok(code)
    }
}

// codegen/pattern_generators/master_data.rs
pub struct MasterDataGenerator;

impl PatternGenerator for MasterDataGenerator {
    fn generate(&self, entity: &EntityDefinition) -> Result<PatternCode> {
        let mut code = PatternCode::new();

        // Generate main table struct
        code.add_struct(&self.generate_entity_struct(entity));

        // Generate history table struct
        code.add_struct(&self.generate_history_struct(entity));

        // Generate CRUD operations
        code.add_impl(&self.generate_crud_impl(entity));

        // Generate audit triggers
        code.add_migration(&self.generate_audit_migration(entity));

        Ok(code)
    }

    fn generate_entity_struct(&self, entity: &EntityDefinition) -> String {
        format!(
            r#"
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {name} {{
    {fields}
    // Audit fields
    pub version: i32,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}}
"#,
            name = to_pascal_case(&entity.name),
            fields = self.generate_fields(&entity.fields),
        )
    }
}
```

---

### tree-sitter Grammar

```javascript
// grammar.js
module.exports = grammar({
  name: 'creditcard_dsl',

  externals: $ => [
    $.indent,
    $.dedent,
    $.newline,
  ],

  rules: {
    compilation_unit: $ => repeat($.definition),

    definition: $ => choice(
      $.entity_definition,
      $.workflow_definition,
      $.rule_definition,
      $.parameter_definition,
    ),

    entity_definition: $ => seq(
      'define',
      'entity',
      ':',
      $.identifier,
      $.newline,
      $.indent,
      $.entity_body,
      $.dedent,
    ),

    entity_body: $ => repeat1(choice(
      $.pattern_clause,
      $.identity_clause,
      $.fields_clause,
      $.relationships_clause,
      $.constraints_clause,
    )),

    pattern_clause: $ => seq(
      'pattern',
      ':',
      $.pattern_name,
      $.newline,
    ),

    pattern_name: $ => choice(
      'master_data',
      'immutable_ledger',
      'versioned_configuration',
      'operational_parameters',
      'event_log',
      'state_machine',
      'temporal_data',
      'reference_data',
    ),

    identifier: $ => /[a-z][a-z0-9_]*/,

    // ... more rules
  },
});
```

**Custom Scanner** (for indentation):
```c
// src/scanner.c
#include <tree_sitter/parser.h>
#include <wctype.h>

enum TokenType {
  INDENT,
  DEDENT,
  NEWLINE,
};

typedef struct {
  int indent_length;
  int indent_stack[256];
  int indent_stack_size;
} Scanner;

void *tree_sitter_creditcard_dsl_external_scanner_create() {
  Scanner *scanner = malloc(sizeof(Scanner));
  scanner->indent_length = 0;
  scanner->indent_stack[0] = 0;
  scanner->indent_stack_size = 1;
  return scanner;
}

bool tree_sitter_creditcard_dsl_external_scanner_scan(
  void *payload,
  TSLexer *lexer,
  const bool *valid_symbols
) {
  Scanner *scanner = (Scanner *)payload;

  // Count leading spaces
  int indent_level = 0;
  while (lexer->lookahead == ' ') {
    indent_level++;
    lexer->advance(lexer, false);
  }

  // Emit INDENT/DEDENT tokens based on stack
  // ... (Python-style indentation handling)
}
```

---

### LSP Server Implementation

```rust
// server.rs
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

pub struct DslLanguageServer {
    client: Client,
    tree_manager: TreeManager,
    compiler: Compiler,
}

#[tower_lsp::async_trait]
impl LanguageServer for DslLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ".".to_string(),
                        ":".to_string(),
                    ]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions::default(),
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        // Update tree-sitter tree incrementally
        for change in params.content_changes {
            self.tree_manager.apply_change(&change).await;
        }

        // Run diagnostics
        let diagnostics = self.compute_diagnostics(&params.text_document.uri).await;
        self.client.publish_diagnostics(
            params.text_document.uri,
            diagnostics,
            None,
        ).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        // Get tree-sitter tree
        let tree = self.tree_manager.get_tree(uri).await?;

        // Find node at cursor position
        let node = tree.root_node()
            .descendant_for_point_range(
                Point::new(position.line as usize, position.character as usize),
                Point::new(position.line as usize, position.character as usize),
            )
            .ok_or_else(|| "No node at position")?;

        // Generate completions based on context
        let completions = self.generate_completions(&node, position).await?;

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        // Get symbol at position
        let symbol = self.get_symbol_at_position(uri, position).await?;

        // Generate documentation
        let documentation = self.generate_documentation(&symbol).await?;

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: documentation,
            }),
            range: None,
        }))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;

        // Get current content
        let content = self.tree_manager.get_content(uri).await?;

        // Format using compiler's formatter
        let formatted = self.compiler.format(&content)?;

        // Calculate diff
        let edits = compute_text_edits(&content, &formatted);

        Ok(Some(edits))
    }
}
```

---

## Development Roadmap

### Milestone 1: Proof of Concept (2 weeks)

**Goal**: Validate approach with minimal working example

**Tasks**:
1. Create ANTLR4 grammar for subset (entity definitions only)
2. Generate Rust parser via antlr4rust
3. Build simple AST
4. Generate basic Rust struct from one entity
5. Create tree-sitter grammar for same subset
6. Basic syntax highlighting in VS Code

**Success Criteria**:
- Can parse simple entity definition
- Generates valid Rust struct
- Syntax highlighting works in VS Code

---

### Milestone 2: Core Compiler (6 weeks)

**Goal**: Complete DSL → Rust compilation

**Tasks**:
1. Complete ANTLR4 grammar (all constructs)
2. Full AST implementation
3. Semantic analysis (type checking, validation)
4. Pattern-based code generation (all 9 patterns)
5. Workflow code generation
6. Rule compilation
7. CLI tool
8. Comprehensive test suite

**Success Criteria**:
- Can compile late fee calculation example
- Generated code compiles and runs
- All patterns generate correct code
- Good error messages for business users

---

### Milestone 3: Editor Support (4 weeks)

**Goal**: Professional editing experience

**Tasks**:
1. Complete tree-sitter grammar
2. Syntax highlighting queries
3. Code folding, indentation
4. Basic LSP server (diagnostics, formatting)
5. VS Code extension
6. Neovim plugin (optional)

**Success Criteria**:
- Instant syntax highlighting
- Good error messages in editor
- Auto-formatting works
- Comfortable editing experience

---

### Milestone 4: Advanced LSP (4 weeks)

**Goal**: IDE-quality features

**Tasks**:
1. Auto-completion (context-aware)
2. Go-to-definition
3. Find references
4. Hover documentation
5. Rename refactoring
6. BIAN validation integration

**Success Criteria**:
- Completion feels natural
- Navigation works seamlessly
- Refactoring is safe
- BIAN validation catches errors

---

### Milestone 5: Production Ready (4 weeks)

**Goal**: Enterprise-grade tooling

**Tasks**:
1. Performance optimization
2. Error recovery improvements
3. Documentation generator
4. Debugging support
5. CI/CD integration
6. Packaging and distribution

**Success Criteria**:
- Fast compilation (< 1s for typical file)
- Great error messages
- Good documentation
- Easy to install and use

---

## Next Steps

### Immediate Actions (This Week)

1. **Set up project structure**
   ```bash
   mkdir dsl-toolchain
   cd dsl-toolchain
   cargo new dsl-compiler
   cargo new dsl-lsp
   npm init dsl-tree-sitter
   npm init dsl-vscode
   ```

2. **Install ANTLR4**
   ```bash
   # Download ANTLR4
   curl -O https://www.antlr.org/download/antlr-4.13.1-complete.jar

   # Add to .bashrc/.zshrc
   export CLASSPATH=".:/path/to/antlr-4.13.1-complete.jar:$CLASSPATH"
   alias antlr4='java -jar /path/to/antlr-4.13.1-complete.jar'
   ```

3. **Start with grammar POC**
   - Create minimal grammar for entity definitions
   - Test with late fee schedule example
   - Validate indentation handling

4. **Validate antlr4rust integration**
   - Create simple Rust project
   - Generate parser from minimal grammar
   - Ensure it compiles and works

### Week 1-2 Priorities

1. Complete minimal ANTLR4 grammar (entity subset)
2. Generate and test Rust parser
3. Define AST structures
4. Create simple code generator
5. Test with one complete entity example

---

## Questions to Resolve

Before proceeding, let's clarify:

1. **Target Rust version**: What Rust version should generated code target? (stable? specific MSRV?)

2. **Database backend**: What database will the generated code use? (PostgreSQL? MySQL? SQLite? configurable?)

3. **Web framework**: Will you need REST APIs? GraphQL? What framework? (Axum? Actix? Rocket?)

4. **Editor priorities**: Which editors are most important? (VS Code? IntelliJ? Neovim? All?)

5. **Deployment model**: How will the compiler be distributed? (CLI tool? VS Code extension includes compiler? Separate installation?)

6. **Testing strategy**: What level of testing for generated code? (Unit tests auto-generated? Integration tests? Contract tests?)

7. **Performance targets**: What compilation speed is acceptable? (< 1s? < 5s? < 30s?)

8. **Error message style**: Prefer verbose helpful messages or concise technical messages?

---

## Conclusion

### Recommended Path Forward

**Primary toolchain: ANTLR4**
- Perfect fit for your text-based YAML-inspired DSL
- Full control over Rust code generation
- Business users edit plain text files
- Mature, production-proven

**Secondary toolchain: tree-sitter**
- Superior editor experience
- Modern LSP integration
- Works across all editors

**NOT JetBrains MPS**
- Projectional editing conflicts with your design goals
- Unnecessary complexity for your use case

### Estimated Timeline

- **POC**: 2 weeks
- **Core compiler**: 6 weeks
- **Editor support**: 4 weeks
- **Advanced LSP**: 4 weeks
- **Production ready**: 4 weeks
- **Total**: ~20 weeks (5 months)

### Resources Needed

- 1 full-time developer (or equivalent)
- ANTLR4 knowledge (or learning time)
- Rust expertise (moderate to advanced)
- tree-sitter knowledge (or learning time)
- LSP protocol understanding

### Risk Factors

1. **Indentation parsing complexity** - Mitigated by ANTLR4's proven Python grammar techniques
2. **Code generation complexity** - Mitigated by pattern-based approach with templates
3. **LSP protocol complexity** - Mitigated by tower-lsp crate (mature Rust LSP framework)
4. **Business user adoption** - Mitigated by text-based approach with excellent error messages

---

**Ready to start? Let's build the grammar definition!**
