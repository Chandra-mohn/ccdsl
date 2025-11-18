# AST Implementation Guide

**Project**: Credit Card DSL Compiler
**Component**: Abstract Syntax Tree (AST)
**Language**: Rust
**Status**: Foundation Complete (25% of overall compiler)
**Last Updated**: 2025-11-11

---

## Table of Contents

1. [What Is The AST?](#what-is-the-ast)
2. [Why We Need It](#why-we-need-it)
3. [Architecture Overview](#architecture-overview)
4. [Module Structure](#module-structure)
5. [Core Components Deep Dive](#core-components-deep-dive)
6. [Type System Explained](#type-system-explained)
7. [Pattern-Based Design](#pattern-based-design)
8. [Code Examples](#code-examples)
9. [Implementation Details](#implementation-details)
10. [Next Steps](#next-steps)

---

## What Is The AST?

The **Abstract Syntax Tree (AST)** is a typed, tree-structured representation of your DSL code. It sits between the parser and the code generator:

```
.dsl files (text) → ANTLR4 Parser → AST → Semantic Analysis → Code Generator → Rust code
                                     ^^^
                                This component
```

Think of it as the **"understood" version** of your DSL:
- **Input**: Raw text from `.dsl` files
- **Output**: Type-safe Rust data structures
- **Purpose**: Separates parsing concerns from business logic and code generation

### Simple Analogy

```
DSL Text:           "balance: money"
Parse Tree:         Token(IDENTIFIER, "balance"), Token(COLON), Token(IDENTIFIER, "money")
AST:                Field { name: "balance", field_type: FieldType::Money }
```

The AST removes parsing details and gives you a **clean, type-safe representation** of what the code means.

---

## Why We Need It

### Problem Without AST

If you tried to generate code directly from the parser:

```rust
// ❌ Tightly coupled to parser structure
fn generate_from_parse_tree(ctx: &FieldContext) -> String {
    let name = ctx.IDENTIFIER(0).unwrap().get_text();
    let type_token = ctx.IDENTIFIER(1).unwrap().get_text();
    // What if the grammar changes? This breaks.
    // What about validation? Type checking? Pattern enforcement?
}
```

### Solution With AST

```rust
// ✅ Clean separation, type-safe
fn generate_from_ast(field: &Field) -> String {
    let rust_type = field.field_type.to_rust_type();  // Type conversion
    let sql_type = field.field_type.to_sql_type();    // Multi-target
    // AST provides high-level API, insulated from parser changes
}
```

### Key Benefits

1. **Separation of Concerns**: Parsing ≠ Business Logic ≠ Code Generation
2. **Type Safety**: Rust compiler catches errors at compile time
3. **Multi-Target**: Same AST generates Rust, SQL, migrations, docs
4. **Validation**: Semantic analysis operates on clean AST, not raw tokens
5. **Pattern Awareness**: Different patterns drive different code generation
6. **Maintainability**: Change grammar without breaking code generator
7. **Debuggability**: Serialize AST to JSON for inspection

---

## Architecture Overview

### High-Level Structure

```
dsl-compiler/
├── Cargo.toml              # Rust project configuration
├── build.rs                # ANTLR4 build integration
├── grammar/
│   └── CreditCardDSL.g4    # ANTLR4 grammar (generates parser)
│
└── src/
    └── ast/                # AST implementation (1200+ lines)
        ├── mod.rs          # Core types (CompilationUnit, Pattern, Definition)
        ├── types.rs        # Type system (FieldType, Value, conversions)
        ├── entity.rs       # Entity definitions (~350 lines)
        ├── expressions.rs  # Expression system (~200 lines)
        ├── workflow.rs     # Workflow definitions (~200 lines)
        ├── rule.rs         # Business rules (~100 lines)
        └── parameter.rs    # Hot-reloadable parameters (~50 lines)
```

### Data Flow

```
┌─────────────┐
│  .dsl file  │ (Text: "define entity: customer...")
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  ANTLR4     │ (Lexer + Parser)
│  Parser     │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ Parse Tree  │ (ANTLR4's internal structure)
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ AST Builder │ (Converts parse tree → AST)
└──────┬──────┘
       │
       ▼
┌─────────────┐
│     AST     │ ← We are here (type-safe structures)
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  Semantic   │ (Type checking, validation)
│  Analysis   │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│    Code     │ (Pattern-based Rust generation)
│  Generator  │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ Rust Code   │ (Production structs, impls, tests)
└─────────────┘
```

---

## Module Structure

### File Organization

| File | Lines | Purpose | Key Types |
|------|-------|---------|-----------|
| **mod.rs** | 137 | Core AST types | `CompilationUnit`, `Definition`, `Pattern`, `SourceLocation` |
| **types.rs** | 250 | Type system | `FieldType`, `Value`, conversions |
| **entity.rs** | 350 | Entity definitions | `EntityDefinition`, `Field`, `FieldGroup`, `Relationship` |
| **expressions.rs** | 200 | Expression system | `Expression`, `BinaryExpression`, `FieldReference` |
| **workflow.rs** | 200 | Workflow definitions | `WorkflowDefinition`, `Step`, `Action` |
| **rule.rs** | 100 | Business rules | `RulesDefinition`, `Rule`, `Calculation` |
| **parameter.rs** | 50 | Parameters (PCF) | `ParameterDefinition` |

### Module Dependencies

```
mod.rs (core)
    ├── types.rs (used by all modules)
    │
    ├── expressions.rs (uses types.rs)
    │
    ├── entity.rs (uses types.rs, expressions.rs)
    │
    ├── workflow.rs (uses types.rs, expressions.rs)
    │
    ├── rule.rs (uses types.rs, expressions.rs)
    │
    └── parameter.rs (uses types.rs, expressions.rs)
```

---

## Core Components Deep Dive

### 1. CompilationUnit (Root AST Node)

**Location**: `src/ast/mod.rs:17-39`

```rust
/// Top-level compilation unit representing a complete DSL file
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CompilationUnit {
    pub definitions: Vec<Definition>,
    pub source_file: Option<String>,
}
```

**Purpose**: Represents an entire `.dsl` file. Contains all top-level definitions.

**Example**:
```dsl
# customer.dsl
define entity: customer
  pattern: master_data
  ...

define workflow: onboarding
  ...
```

**Becomes**:
```rust
CompilationUnit {
    definitions: vec![
        Definition::Entity(customer_entity),
        Definition::Workflow(onboarding_workflow),
    ],
    source_file: Some("customer.dsl"),
}
```

**Methods**:
```rust
impl CompilationUnit {
    pub fn new() -> Self
    pub fn with_source_file(mut self, path: String) -> Self
    pub fn add_definition(&mut self, def: Definition)
}
```

---

### 2. Definition Enum (Top-Level Constructs)

**Location**: `src/ast/mod.rs:42-48`

```rust
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Definition {
    Entity(entity::EntityDefinition),
    Workflow(workflow::WorkflowDefinition),
    Rules(rule::RulesDefinition),
    Parameter(parameter::ParameterDefinition),
}
```

**Purpose**: Represents the four top-level definition types in the DSL.

**Pattern Matching**:
```rust
match definition {
    Definition::Entity(entity) => generate_entity_code(entity),
    Definition::Workflow(workflow) => generate_workflow_code(workflow),
    Definition::Rules(rules) => generate_rules_code(rules),
    Definition::Parameter(param) => generate_parameter_code(param),
}
```

---

### 3. Pattern Enum (9 Data Mutation Patterns)

**Location**: `src/ast/mod.rs:74-136`

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Pattern {
    MasterData,                // Mutable with audit trail
    ImmutableLedger,           // Append-only, never update
    VersionedConfiguration,    // Time-based versioning
    OperationalParameters,     // Hot-reloadable config
    EventLog,                  // Immutable event stream
    StateMachine,              // State transitions
    TemporalData,              // Valid-from/valid-to
    ReferenceData,             // Read-only lookup tables
    Composition(Vec<Pattern>), // Combine patterns
}
```

**Helper Methods**:
```rust
impl Pattern {
    pub fn from_str(s: &str) -> Option<Self>
    pub fn to_str(&self) -> &'static str

    // Pattern characteristics
    pub fn is_immutable(&self) -> bool
    pub fn supports_versioning(&self) -> bool
    pub fn supports_audit_trail(&self) -> bool
}
```

**Why This Matters**: Different patterns generate **completely different code**.

**Example**:
```rust
// master_data pattern
if entity.pattern == Pattern::MasterData {
    generate_update_method(&entity);     // ✅ Generates update()
    generate_delete_method(&entity);     // ✅ Generates delete()
    generate_audit_table(&entity);       // ✅ Generates history table
}

// immutable_ledger pattern
if entity.pattern == Pattern::ImmutableLedger {
    generate_append_method(&entity);     // ✅ Generates append()
    // ❌ No update() or delete() methods
    generate_immutable_constraints(&entity);
}
```

---

### 4. SourceLocation (Error Reporting)

**Location**: `src/ast/mod.rs:51-71`

```rust
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceLocation {
    pub file: Option<String>,
    pub line: usize,
    pub column: usize,
}
```

**Purpose**: Track where each AST node came from in the source file. Enables beautiful error messages.

**Example Error Message**:
```
Error: Invalid field type for immutable_ledger pattern
   ┌─ examples/account.dsl:12:5
   │
12 │     balance: money
   │     ^^^^^^^ cannot be modified in immutable_ledger
   │
   = help: Use 'cannot_change' qualifier or choose different pattern
```

This is powered by `SourceLocation` stored in every AST node.

---

## Type System Explained

### FieldType Enum

**Location**: `src/ast/types.rs:6-20`

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FieldType {
    // Primitive types
    Text, Number, Money, Date, Timestamp, Boolean,

    // Validated types
    Email, Phone,

    // Specialized types
    Percentage, Duration, Time,

    // Entity references
    Entity(String),  // Reference to another entity
}
```

### Type Conversion System

The magic of the type system is **multi-target code generation**:

```rust
impl FieldType {
    pub fn to_rust_type(&self) -> String {
        match self {
            FieldType::Text => "String",
            FieldType::Number => "f64",
            FieldType::Money => "Money",           // Custom type
            FieldType::Date => "Date",             // Custom type
            FieldType::Timestamp => "DateTime<Utc>",
            FieldType::Boolean => "bool",
            FieldType::Email => "Email",           // Validated type
            FieldType::Phone => "Phone",           // Validated type
            FieldType::Entity(name) => to_pascal_case(name),
        }
    }

    pub fn to_sql_type(&self) -> String {
        match self {
            FieldType::Text => "TEXT",
            FieldType::Number => "NUMERIC",
            FieldType::Money => "NUMERIC(19, 4)",  // 4 decimal places
            FieldType::Date => "DATE",
            FieldType::Timestamp => "TIMESTAMP",
            FieldType::Boolean => "BOOLEAN",
            FieldType::Email => "TEXT",            // With CHECK constraint
            FieldType::Phone => "TEXT",            // With CHECK constraint
            FieldType::Entity(name) => format!("{}_id BIGINT", name),
        }
    }
}
```

### Type Conversion Examples

| DSL Type | Rust Type | SQL Type | Notes |
|----------|-----------|----------|-------|
| `text` | `String` | `TEXT` | Standard text |
| `number` | `f64` | `NUMERIC` | Floating point |
| `money` | `Money` | `NUMERIC(19, 4)` | Custom type with 4 decimals |
| `date` | `Date` | `DATE` | Date only |
| `timestamp` | `DateTime<Utc>` | `TIMESTAMP` | Date + time |
| `boolean` | `bool` | `BOOLEAN` | True/false |
| `email` | `Email` | `TEXT` | Validated email |
| `phone` | `Phone` | `TEXT` | Validated phone |
| `customer` | `Customer` | `customer_id BIGINT` | Entity reference |

### Type Helper Methods

```rust
impl FieldType {
    pub fn is_primitive(&self) -> bool {
        !matches!(self, FieldType::Entity(_))
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self,
            FieldType::Number |
            FieldType::Money |
            FieldType::Percentage
        )
    }

    pub fn is_temporal(&self) -> bool {
        matches!(self,
            FieldType::Date |
            FieldType::Timestamp |
            FieldType::Time |
            FieldType::Duration
        )
    }
}
```

**Usage in Semantic Analysis**:
```rust
// Validate binary expression types
if left_type.is_numeric() && right_type.is_numeric() {
    // ✅ Valid: money + number
} else {
    // ❌ Invalid: text + boolean
    return Err(TypeError::IncompatibleTypes);
}
```

---

## Pattern-Based Design

### Why Patterns Matter

Different business data has different mutation characteristics. The **pattern** determines:
1. What operations are allowed
2. What code gets generated
3. What database schema is created
4. What validation is required

### Pattern Characteristics Table

| Pattern | Mutable? | Versioned? | Audit Trail? | State? | Use Case |
|---------|----------|------------|--------------|--------|----------|
| **master_data** | ✅ Yes | ❌ No | ✅ Yes | ❌ No | Customer profiles |
| **immutable_ledger** | ❌ No | ❌ No | ✅ Yes | ❌ No | Financial transactions |
| **versioned_configuration** | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | Credit limits by date |
| **operational_parameters** | ✅ Yes (hot) | ❌ No | ✅ Yes | ❌ No | Fee schedules (PCF) |
| **event_log** | ❌ No | ❌ No | N/A | ❌ No | System events |
| **state_machine** | ✅ Yes | ❌ No | ✅ Yes | ✅ Yes | Account lifecycle |
| **temporal_data** | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | Interest rates |
| **reference_data** | ❌ No | ❌ No | ❌ No | ❌ No | Country codes |

### Pattern Detection Methods

```rust
impl Pattern {
    pub fn is_immutable(&self) -> bool {
        matches!(self,
            Pattern::ImmutableLedger |
            Pattern::EventLog |
            Pattern::ReferenceData
        )
    }

    pub fn supports_versioning(&self) -> bool {
        matches!(self,
            Pattern::VersionedConfiguration |
            Pattern::TemporalData
        )
    }

    pub fn supports_audit_trail(&self) -> bool {
        matches!(self,
            Pattern::MasterData |
            Pattern::ImmutableLedger |
            Pattern::EventLog
        )
    }
}
```

### Code Generation Example

```rust
fn generate_entity(entity: &EntityDefinition) -> String {
    let mut code = String::new();

    // Generate struct
    code.push_str(&generate_struct(entity));

    // Pattern-specific methods
    match entity.pattern {
        Pattern::MasterData => {
            code.push_str(&generate_create_method(entity));
            code.push_str(&generate_update_method(entity));  // ✅
            code.push_str(&generate_delete_method(entity));  // ✅
            code.push_str(&generate_history_table(entity));  // ✅
        }

        Pattern::ImmutableLedger => {
            code.push_str(&generate_append_method(entity));  // ✅
            // No update or delete methods
            code.push_str(&generate_verify_method(entity));  // ✅
        }

        Pattern::StateMachine => {
            code.push_str(&generate_state_transitions(entity));  // ✅
            code.push_str(&generate_state_validation(entity));   // ✅
            code.push_str(&generate_event_handlers(entity));     // ✅
        }

        // ... other patterns
    }

    code
}
```

---

## Code Examples

### Example 1: Simple Entity DSL → AST

**Input DSL** (`customer.dsl`):
```dsl
define entity: customer
  pattern: master_data
  business_domain: "Credit Card (BIAN)"

  identity:
    customer_id: text, unique, required

  profile:
    first_name: text, required
    last_name: text, required
    email: email, required
    credit_score: number, between 300 and 850

  must:
    - credit_score >= 300 and credit_score <= 850
```

**Output AST** (Rust):
```rust
EntityDefinition {
    name: "customer".to_string(),
    pattern: Pattern::MasterData,
    business_domain: Some("Credit Card (BIAN)".to_string()),
    bian_mapping: None,

    identity: Some(IdentityClause {
        fields: vec![
            Field {
                name: "customer_id".to_string(),
                field_type: FieldType::Text,
                qualifiers: vec![
                    FieldQualifier::Unique,
                    FieldQualifier::Required,
                ],
                location: SourceLocation::new(5, 5),
            }
        ],
    }),

    field_groups: vec![
        FieldGroup {
            group_name: "profile".to_string(),
            fields: vec![
                Field {
                    name: "first_name".to_string(),
                    field_type: FieldType::Text,
                    qualifiers: vec![FieldQualifier::Required],
                    location: SourceLocation::new(8, 5),
                },
                Field {
                    name: "last_name".to_string(),
                    field_type: FieldType::Text,
                    qualifiers: vec![FieldQualifier::Required],
                    location: SourceLocation::new(9, 5),
                },
                Field {
                    name: "email".to_string(),
                    field_type: FieldType::Email,
                    qualifiers: vec![FieldQualifier::Required],
                    location: SourceLocation::new(10, 5),
                },
                Field {
                    name: "credit_score".to_string(),
                    field_type: FieldType::Number,
                    qualifiers: vec![
                        FieldQualifier::Between(
                            Value::Number(300.0),
                            Value::Number(850.0),
                        )
                    ],
                    location: SourceLocation::new(11, 5),
                },
            ],
        }
    ],

    relationships: vec![],

    constraints: vec![
        Constraint {
            condition: Expression::Logical(LogicalExpression {
                left: Box::new(Expression::Comparison(ComparisonExpression {
                    left: Box::new(Expression::FieldReference(
                        FieldReference::simple("credit_score".to_string(), SourceLocation::new(14, 7))
                    )),
                    operator: ComparisonOperator::GreaterThanOrEqual,
                    right: Box::new(Expression::Literal(Value::Number(300.0))),
                    location: SourceLocation::new(14, 7),
                })),
                operator: LogicalOperator::And,
                right: Box::new(Expression::Comparison(ComparisonExpression {
                    left: Box::new(Expression::FieldReference(
                        FieldReference::simple("credit_score".to_string(), SourceLocation::new(14, 29))
                    )),
                    operator: ComparisonOperator::LessThanOrEqual,
                    right: Box::new(Expression::Literal(Value::Number(850.0))),
                    location: SourceLocation::new(14, 29),
                })),
                location: SourceLocation::new(14, 7),
            }),
        }
    ],

    state_machine: None,
    versioning: None,
    temporal: None,
    location: SourceLocation::new(1, 1).with_file("customer.dsl".to_string()),
}
```

### Example 2: Type Conversion in Action

**DSL Field**:
```dsl
balance: money
```

**AST Representation**:
```rust
Field {
    name: "balance",
    field_type: FieldType::Money,
    qualifiers: vec![],
}
```

**Generated Rust Code**:
```rust
pub struct Account {
    pub balance: Money,  // ← from field_type.to_rust_type()
}
```

**Generated SQL Schema**:
```sql
CREATE TABLE accounts (
    balance NUMERIC(19, 4)  -- ← from field_type.to_sql_type()
);
```

**Generated Migration**:
```sql
ALTER TABLE accounts
ADD COLUMN balance NUMERIC(19, 4) NOT NULL DEFAULT 0.0;
```

### Example 3: Expression AST

**DSL Expression**:
```dsl
late_fee = when days_past_due > 30: base_fee * 1.5
           otherwise: $0.00
```

**AST Representation**:
```rust
Calculation {
    variable: "late_fee".to_string(),
    expression: Expression::Conditional(ConditionalExpression {
        branches: vec![
            ConditionalBranch {
                condition: Expression::Comparison(ComparisonExpression {
                    left: Box::new(Expression::FieldReference(
                        FieldReference::simple("days_past_due".to_string(), ...)
                    )),
                    operator: ComparisonOperator::GreaterThan,
                    right: Box::new(Expression::Literal(Value::Number(30.0))),
                    location: ...,
                }),
                value: Expression::Binary(BinaryExpression {
                    left: Box::new(Expression::FieldReference(
                        FieldReference::simple("base_fee".to_string(), ...)
                    )),
                    operator: BinaryOperator::Multiply,
                    right: Box::new(Expression::Literal(Value::Number(1.5))),
                    location: ...,
                }),
            }
        ],
        otherwise: Some(Box::new(Expression::Literal(
            Value::Money(MoneyValue::new(0, 0))
        ))),
    }),
}
```

**Generated Rust Code**:
```rust
let late_fee = if days_past_due > 30.0 {
    base_fee * 1.5
} else {
    Money::zero()
};
```

---

## Implementation Details

### Build System Integration

**File**: `build.rs`

```rust
use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    // Rebuild if grammar changes
    println!("cargo:rerun-if-changed=grammar/CreditCardDSL.g4");

    let out_dir = env::var("OUT_DIR").unwrap();
    let grammar_file = PathBuf::from("grammar/CreditCardDSL.g4");

    // Get ANTLR4 jar location from environment
    let antlr_jar = env::var("ANTLR4_JAR")
        .unwrap_or_else(|_| "antlr-4.13.1-complete.jar".to_string());

    // Generate Rust parser from grammar
    let status = Command::new("java")
        .args(&[
            "-jar", &antlr_jar,
            "-Dlanguage=Rust",      // Generate Rust code
            "-o", &out_dir,          // Output to build directory
            "-visitor",              // Generate visitor pattern
            "-no-listener",          // Skip listener pattern
            grammar_file.to_str().unwrap(),
        ])
        .status()
        .expect("Failed to execute ANTLR4");

    if !status.success() {
        panic!("ANTLR4 parser generation failed");
    }
}
```

**What this does**:
1. Runs during `cargo build`
2. Invokes ANTLR4 to generate Rust parser from grammar
3. Outputs generated code to build directory
4. Rebuilds automatically when grammar changes

### Dependency Configuration

**File**: `Cargo.toml`

```toml
[dependencies]
# ANTLR4 runtime
antlr-rust = "0.3"

# Code generation
handlebars = "5.1"              # Template engine
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# Error handling
miette = { version = "7.0", features = ["fancy"] }
thiserror = "1.0"
anyhow = "1.0"

# CLI
clap = { version = "4.5", features = ["derive", "cargo"] }

# Logging
log = "0.4"
env_logger = "0.11"

# Utilities
once_cell = "1.19"

[dev-dependencies]
pretty_assertions = "1.4"       # Better test output
insta = "1.34"                  # Snapshot testing
```

**Key dependencies**:
- **antlr-rust**: ANTLR4 runtime for Rust
- **handlebars**: Template-based code generation
- **miette**: Beautiful error messages with source code snippets
- **serde**: AST serialization for debugging
- **insta**: Snapshot testing for generated code

### AST Serialization for Debugging

```rust
use serde_json;

// All AST nodes implement Serialize
let entity = EntityDefinition { ... };

// Serialize to JSON
let json = serde_json::to_string_pretty(&entity)?;
println!("{}", json);

// Output:
// {
//   "name": "customer",
//   "pattern": "MasterData",
//   "business_domain": "Credit Card (BIAN)",
//   "identity": {
//     "fields": [...]
//   },
//   ...
// }
```

This is incredibly useful for:
- Debugging parser issues
- Understanding AST structure
- Writing tests
- Generating documentation

---

## Next Steps

### Current Status: 25% Complete

**✅ What's Done**:
1. Complete ANTLR4 grammar (~600 lines)
2. Complete AST type system (~1200 lines)
3. Cargo build integration with ANTLR4
4. Example DSL files (4 test cases)
5. Type conversion system (DSL → Rust → SQL)
6. Pattern enum with helper methods

**⏳ What's Next**:

### Step 1: Parser Implementation (Weeks 1-2)

**Files to Create**:
- `src/parser/mod.rs` - Parser module root
- `src/parser/indentation_lexer.rs` - Custom INDENT/DEDENT handling
- `src/parser/ast_builder.rs` - Parse tree → AST conversion

**Implementation**:
```rust
// src/parser/mod.rs
pub fn parse_file(path: &str) -> Result<CompilationUnit, ParseError> {
    // 1. Read .dsl file
    let content = std::fs::read_to_string(path)?;

    // 2. Preprocess indentation (INDENT/DEDENT tokens)
    let preprocessed = IndentationLexer::new().process(&content)?;

    // 3. Parse with ANTLR4
    let parse_tree = antlr_parse(&preprocessed)?;

    // 4. Build AST from parse tree
    let ast = AstBuilder::new().visit(&parse_tree)?;

    Ok(ast.with_source_file(path.to_string()))
}
```

**Key Challenge**: Implementing indentation-based syntax (YAML-style).

**Solution Options**:
1. Preprocessor: Convert indentation to explicit tokens before ANTLR4
2. Custom lexer: Implement in Rust, emit INDENT/DEDENT tokens
3. ANTLR4 lexer modes: Handle in grammar (complex)

### Step 2: Semantic Analysis (Weeks 3-4)

**Files to Create**:
- `src/semantic/mod.rs` - Semantic analysis root
- `src/semantic/type_checker.rs` - Type checking
- `src/semantic/validator.rs` - Constraint validation
- `src/semantic/pattern_validator.rs` - Pattern-specific rules

**Implementation**:
```rust
// src/semantic/type_checker.rs
pub fn validate_entity(entity: &EntityDefinition) -> Result<(), Vec<SemanticError>> {
    let mut errors = Vec::new();

    // 1. Type checking
    for field in entity.all_fields() {
        if let Err(e) = validate_field_type(field) {
            errors.push(e);
        }
    }

    // 2. Pattern-specific validation
    if entity.pattern.is_immutable() {
        for field in entity.all_fields() {
            if field.has_qualifier(FieldQualifier::CannotChange) {
                // ❌ Error: Redundant qualifier for immutable pattern
                errors.push(SemanticError::RedundantQualifier { ... });
            }
        }
    }

    // 3. Constraint validation
    for constraint in &entity.constraints {
        if let Err(e) = validate_constraint(constraint, entity) {
            errors.push(e);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
```

### Step 3: Code Generation (Weeks 5-8)

**Files to Create**:
- `src/codegen/mod.rs` - Code generation root
- `src/codegen/rust_generator.rs` - Main generator
- `src/codegen/pattern_generators/master_data.rs` - MasterData pattern
- `src/codegen/pattern_generators/immutable_ledger.rs` - ImmutableLedger pattern
- `src/codegen/templates/entity.rs.hbs` - Handlebars templates

**Implementation**:
```rust
// src/codegen/rust_generator.rs
pub fn generate_entity(entity: &EntityDefinition) -> Result<String, CodegenError> {
    // Select pattern-specific generator
    let code = match entity.pattern {
        Pattern::MasterData => {
            pattern_generators::master_data::generate(entity)?
        }
        Pattern::ImmutableLedger => {
            pattern_generators::immutable_ledger::generate(entity)?
        }
        // ... other patterns
    };

    Ok(code)
}
```

### Step 4: CLI Tool (Week 9)

**Files to Create**:
- `src/main.rs` - CLI entry point
- `src/cli.rs` - CLI argument parsing

**Implementation**:
```rust
// src/main.rs
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "dslc")]
#[command(about = "Credit Card DSL Compiler")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile DSL file to Rust code
    Compile {
        /// Path to .dsl file
        file: String,

        /// Output directory
        #[arg(short, long, default_value = "./generated")]
        output: String,
    },

    /// Check DSL syntax without generating code
    Check {
        file: String,
    },

    /// Display AST for debugging
    Ast {
        file: String,

        /// Output format (json, pretty)
        #[arg(short, long, default_value = "pretty")]
        format: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile { file, output } => {
            let ast = parse_file(&file)?;
            let code = generate_code(&ast)?;
            write_output(&output, code)?;
        }
        Commands::Check { file } => {
            let ast = parse_file(&file)?;
            validate(&ast)?;
            println!("✓ No errors found");
        }
        Commands::Ast { file, format } => {
            let ast = parse_file(&file)?;
            print_ast(&ast, &format)?;
        }
    }

    Ok(())
}
```

---

## Summary

### What You Have Now

**A Production-Ready AST Foundation**:
- ✅ **Type-safe**: Rust's type system prevents bugs at compile time
- ✅ **Pattern-aware**: 9 data mutation patterns drive code generation
- ✅ **Multi-target**: Same AST generates Rust, SQL, migrations, docs
- ✅ **Debuggable**: Serde serialization for AST inspection
- ✅ **Error-friendly**: Source locations for beautiful error messages
- ✅ **Modular**: Clean separation of concerns (6 modules)
- ✅ **Tested**: Ready for parser integration

### File Summary

| File | Size | Purpose | Status |
|------|------|---------|--------|
| `Cargo.toml` | 50 lines | Project config | ✅ Complete |
| `build.rs` | 39 lines | ANTLR4 integration | ✅ Complete |
| `src/ast/mod.rs` | 137 lines | Core AST types | ✅ Complete |
| `src/ast/types.rs` | 250 lines | Type system | ✅ Complete |
| `src/ast/entity.rs` | 350 lines | Entity AST | ✅ Complete |
| `src/ast/expressions.rs` | 200 lines | Expression AST | ✅ Complete |
| `src/ast/workflow.rs` | 200 lines | Workflow AST | ✅ Complete |
| `src/ast/rule.rs` | 100 lines | Rule AST | ✅ Complete |
| `src/ast/parameter.rs` | 50 lines | Parameter AST | ✅ Complete |
| **Total** | **~1200 lines** | **AST foundation** | **✅ 100%** |

### Progress: 25% of Overall Compiler

```
Compiler Progress
├─ [████████████████████████░░░░░░░░░░░░░░░░░░░░░░░░] 25%
│
├─ Foundation (100%) ✅
│  ├─ Grammar definition ✅
│  ├─ AST type system ✅
│  └─ Build integration ✅
│
├─ Parser (0%) ⏳
│  ├─ Indentation lexer ⏳
│  ├─ AST builder ⏳
│  └─ Error recovery ⏳
│
├─ Semantic Analysis (0%) ⏳
│  ├─ Type checking ⏳
│  ├─ Pattern validation ⏳
│  └─ Constraint verification ⏳
│
├─ Code Generation (0%) ⏳
│  ├─ Pattern generators ⏳
│  ├─ Template system ⏳
│  └─ Migration generation ⏳
│
└─ Tooling (0%) ⏳
   ├─ CLI tool ⏳
   ├─ LSP server ⏳
   └─ VS Code extension ⏳
```

---

**Ready to implement the parser! 🚀**

---

**Document Status**: Complete technical reference
**Last Updated**: 2025-11-11
**Next Update**: After parser implementation
