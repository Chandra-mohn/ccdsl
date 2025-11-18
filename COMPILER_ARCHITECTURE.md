# Compiler Architecture

**Language**: Financial Services DSL (FS-DSL)
**Parser Generator**: ANTLR4
**Compiler Implementation**: Rust
**Target Languages**: Python, Java, Rust

---

## Overview

The FS-DSL compiler uses a **single unified ANTLR4 grammar** that handles both Core DSL and Domain DSL constructs. The modular architecture is enforced through semantic analysis and module resolution, not at the parsing level.

**Architecture**: Multi-phase compilation pipeline
- **Phase 1**: Lexing and Parsing (ANTLR4)
- **Phase 2**: Module Resolution (Custom Rust)
- **Phase 3**: Semantic Analysis (Custom Rust)
- **Phase 4**: Code Generation (Custom Rust)

---

## Compilation Pipeline

```
┌─────────────────────────────────────────────────────────────┐
│ Phase 1: Lexing & Parsing (ANTLR4)                          │
├─────────────────────────────────────────────────────────────┤
│ Input: .dsl files (core/*.dsl, domains/*.dsl)               │
│ Process: ANTLR4 Lexer → Token Stream → Parser → Parse Tree  │
│ Output: Individual AST per file                             │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 2: Module Resolution (Rust)                           │
├─────────────────────────────────────────────────────────────┤
│ Input: Collection of ASTs                                   │
│ Process:                                                     │
│   1. Extract module declarations                            │
│   2. Build dependency graph                                 │
│   3. Resolve version constraints                            │
│   4. Detect circular dependencies                           │
│   5. Topological sort for compilation order                 │
│ Output: Ordered list of modules ready for semantic analysis │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 3: Semantic Analysis (Rust)                           │
├─────────────────────────────────────────────────────────────┤
│ Input: Ordered modules with ASTs                            │
│ Process:                                                     │
│   1. Build symbol table (core modules first)                │
│   2. Validate imports (check symbols exist)                 │
│   3. Type checking across modules                           │
│   4. Constraint validation                                  │
│   5. Cross-module reference resolution                      │
│ Output: Validated and linked program AST                    │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ Phase 4: Code Generation (Rust)                             │
├─────────────────────────────────────────────────────────────┤
│ Input: Linked program AST                                   │
│ Process:                                                     │
│   1. Generate runtime library (target language)             │
│   2. Generate core constructs                               │
│   3. Generate domain-specific code                          │
│   4. Link all generated code                                │
│ Output: Executable code (Python/Java/Rust)                  │
└─────────────────────────────────────────────────────────────┘
```

---

## Phase 1: Lexing & Parsing (ANTLR4)

### Grammar Structure

**File**: `DSL.g4` (Single unified grammar)

```antlr
grammar DSL;

// ============================================
// ROOT
// ============================================

dslFile
    : moduleDeclaration? importDeclaration* definition* EOF
    ;

definition
    : entityDefinition
    | workflowDefinition
    | ruleDefinition
    | parameterDefinition
    | referenceDataDefinition
    | errorStrategyDefinition
    | configurationBlock
    | actionDefinition
    ;

// ============================================
// MODULE SYSTEM
// ============================================

moduleDeclaration
    : MODULE COLON moduleIdentifier
      VERSION COLON semanticVersion
      STABILITY COLON stabilityLevel
      (DESCRIPTION COLON STRING)?
      (AUTHOR COLON STRING)?
      (LICENSE COLON STRING)?
      bianDomainsDeclaration?
      exportsDeclaration?
    ;

moduleIdentifier
    : moduleType DOT IDENTIFIER
    ;

moduleType
    : CORE | DOMAIN
    ;

semanticVersion
    : INT DOT INT DOT INT
    ;

stabilityLevel
    : STABLE | BETA | ALPHA
    ;

bianDomainsDeclaration
    : BIAN_DOMAINS COLON (DASH STRING)+
    ;

exportsDeclaration
    : EXPORTS COLON exportGroup+
    ;

exportGroup
    : exportCategory COLON (DASH IDENTIFIER)+
    ;

exportCategory
    : PATTERNS | TYPES | CONSTRAINTS | OPERATORS
    | ENTITIES | WORKFLOWS | RULES | PARAMETERS
    ;

importDeclaration
    : IMPORTS COLON importStatement+
    ;

importStatement
    : DASH moduleIdentifier versionConstraint? importSelector? importAlias?
    ;

versionConstraint
    : comparisonOperator semanticVersion                          # SimpleConstraint
    | comparisonOperator semanticVersion COMMA LT semanticVersion # RangeConstraint
    | CARET semanticVersion                                       # CompatibleConstraint
    | TILDE semanticVersion                                       # ApproximateConstraint
    ;

comparisonOperator
    : GTE | GT | LTE | LT | EQ
    ;

importSelector
    : COLON LBRACE importItem (COMMA importItem)* RBRACE
    ;

importItem
    : importCategory COLON LBRACKET IDENTIFIER (COMMA IDENTIFIER)* RBRACKET
    ;

importCategory
    : PATTERNS | TYPES | OPERATORS | CONSTRUCTS
    ;

importAlias
    : AS IDENTIFIER
    ;

// ============================================
// CORE DSL CONSTRUCTS
// ============================================

entityDefinition
    : DEFINE ENTITY COLON IDENTIFIER
      PATTERN COLON patternName
      (BUSINESS_DOMAIN COLON STRING)?
      (BIAN_SERVICE_DOMAIN COLON STRING)?
      entityBody
    ;

patternName
    : MASTER_DATA
    | IMMUTABLE_LEDGER
    | VERSIONED_CONFIGURATION
    | OPERATIONAL_PARAMETERS
    | EVENT_LOG
    | STATE_MACHINE
    | TEMPORAL_DATA
    | REFERENCE_DATA
    | BUSINESS_LOGIC
    ;

// ... (rest of core grammar rules for workflows, rules, etc.)

// ============================================
// LEXER RULES
// ============================================

// Keywords
MODULE          : 'module';
VERSION         : 'version';
STABILITY       : 'stability';
IMPORTS         : 'imports';
EXPORTS         : 'exports';
CORE            : 'core';
DOMAIN          : 'domain';
STABLE          : 'stable';
BETA            : 'beta';
ALPHA           : 'alpha';
DEFINE          : 'define';
ENTITY          : 'entity';
WORKFLOW        : 'workflow';
PATTERN         : 'pattern';
BIAN_DOMAINS    : 'bian_domains';
AS              : 'as';

// Pattern Names
MASTER_DATA              : 'master_data';
IMMUTABLE_LEDGER         : 'immutable_ledger';
VERSIONED_CONFIGURATION  : 'versioned_configuration';
OPERATIONAL_PARAMETERS   : 'operational_parameters';
EVENT_LOG                : 'event_log';
STATE_MACHINE            : 'state_machine';
TEMPORAL_DATA            : 'temporal_data';
REFERENCE_DATA           : 'reference_data';
BUSINESS_LOGIC           : 'business_logic';

// Symbols
COLON      : ':';
DOT        : '.';
COMMA      : ',';
DASH       : '-';
PIPE       : '|';
ARROW      : '→';
CARET      : '^';
TILDE      : '~';
LBRACE     : '{';
RBRACE     : '}';
LBRACKET   : '[';
RBRACKET   : ']';
GT         : '>';
LT         : '<';
GTE        : '>=';
LTE        : '<=';
EQ         : '=';

// Literals
IDENTIFIER  : [a-z][a-z0-9_]* ;
INT         : [0-9]+ ;
STRING      : '"' (~["\r\n])* '"' ;

// Whitespace
WS          : [ \t\r\n]+ -> skip ;
COMMENT     : '//' ~[\r\n]* -> skip ;
```

### ANTLR4 Tool Chain

**Generate Parser** (Java target for compiler written in Rust):
```bash
antlr4 -Dlanguage=Java -visitor -no-listener DSL.g4
```

**Generate Parser** (Alternative: Rust target using antlr4rust):
```bash
# Using antlr4rust crate
antlr4 -Dlanguage=Rust -visitor DSL.g4
```

### Parse Tree Generation

**Input**: `domains/credit_card.dsl`
```
module: domain.credit_card
version: 1.0.0
stability: stable

imports:
  - core.entity >= 2.0.0
  - core.workflow >= 2.0.0

define entity: card_account
  pattern: master_data
  business_domain: "Credit Card"
```

**ANTLR4 Processing**:
```java
// Java code (called from Rust via JNI or subprocess)
DSLLexer lexer = new DSLLexer(CharStreams.fromFileName("domains/credit_card.dsl"));
CommonTokenStream tokens = new CommonTokenStream(lexer);
DSLParser parser = new DSLParser(tokens);

// Parse and get parse tree
DslFileContext tree = parser.dslFile();

// Use visitor to build AST
ASTBuilder builder = new ASTBuilder();
ASTNode ast = builder.visit(tree);
```

**Parse Tree Structure**:
```
dslFile
├── moduleDeclaration
│   ├── moduleIdentifier: domain.credit_card
│   ├── semanticVersion: 1.0.0
│   └── stabilityLevel: stable
├── importDeclaration
│   ├── importStatement: core.entity >= 2.0.0
│   └── importStatement: core.workflow >= 2.0.0
└── definition
    └── entityDefinition
        ├── identifier: card_account
        ├── patternName: master_data
        └── businessDomain: "Credit Card"
```

---

## Phase 2: Module Resolution

### Module Registry

**Rust Implementation**:

```rust
use std::collections::HashMap;
use semver::{Version, VersionReq};

#[derive(Debug, Clone)]
pub struct ModuleId {
    pub module_type: ModuleType,
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum ModuleType {
    Core,
    Domain,
}

#[derive(Debug)]
pub struct Module {
    pub id: ModuleId,
    pub version: Version,
    pub stability: Stability,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub ast: ASTNode,
}

#[derive(Debug)]
pub struct Import {
    pub module_id: ModuleId,
    pub version_constraint: VersionReq,
    pub selector: Option<ImportSelector>,
    pub alias: Option<String>,
}

#[derive(Debug)]
pub struct Export {
    pub category: ExportCategory,
    pub items: Vec<String>,
}

#[derive(Debug)]
pub enum ExportCategory {
    Patterns,
    Types,
    Constraints,
    Operators,
    Entities,
    Workflows,
    Rules,
    Parameters,
}

pub struct ModuleRegistry {
    modules: HashMap<ModuleId, Module>,
}

impl ModuleRegistry {
    pub fn new() -> Self {
        ModuleRegistry {
            modules: HashMap::new(),
        }
    }

    pub fn register(&mut self, module: Module) -> Result<(), CompilerError> {
        // Check for duplicate module IDs
        if self.modules.contains_key(&module.id) {
            return Err(CompilerError::DuplicateModule(module.id.clone()));
        }

        self.modules.insert(module.id.clone(), module);
        Ok(())
    }

    pub fn get(&self, id: &ModuleId) -> Option<&Module> {
        self.modules.get(id)
    }

    pub fn all_modules(&self) -> Vec<&Module> {
        self.modules.values().collect()
    }
}
```

### Dependency Graph

```rust
use std::collections::{HashMap, HashSet, VecDeque};

pub struct DependencyGraph {
    adjacency: HashMap<ModuleId, Vec<ModuleId>>,
    in_degree: HashMap<ModuleId, usize>,
}

impl DependencyGraph {
    pub fn new() -> Self {
        DependencyGraph {
            adjacency: HashMap::new(),
            in_degree: HashMap::new(),
        }
    }

    pub fn add_node(&mut self, id: ModuleId) {
        self.adjacency.entry(id.clone()).or_insert_with(Vec::new);
        self.in_degree.entry(id).or_insert(0);
    }

    pub fn add_edge(&mut self, from: ModuleId, to: ModuleId) {
        self.adjacency.get_mut(&from).unwrap().push(to.clone());
        *self.in_degree.get_mut(&to).unwrap() += 1;
    }

    pub fn topological_sort(&self) -> Result<Vec<ModuleId>, CompilerError> {
        let mut sorted = Vec::new();
        let mut in_degree = self.in_degree.clone();
        let mut queue = VecDeque::new();

        // Find all nodes with in-degree 0
        for (node, &degree) in &in_degree {
            if degree == 0 {
                queue.push_back(node.clone());
            }
        }

        while let Some(node) = queue.pop_front() {
            sorted.push(node.clone());

            // Reduce in-degree for neighbors
            if let Some(neighbors) = self.adjacency.get(&node) {
                for neighbor in neighbors {
                    let degree = in_degree.get_mut(neighbor).unwrap();
                    *degree -= 1;
                    if *degree == 0 {
                        queue.push_back(neighbor.clone());
                    }
                }
            }
        }

        // Check for cycles
        if sorted.len() != self.adjacency.len() {
            return Err(CompilerError::CircularDependency);
        }

        Ok(sorted)
    }
}
```

### Module Resolver

```rust
pub struct ModuleResolver {
    registry: ModuleRegistry,
}

impl ModuleResolver {
    pub fn new(registry: ModuleRegistry) -> Self {
        ModuleResolver { registry }
    }

    pub fn resolve(&self) -> Result<Vec<ModuleId>, CompilerError> {
        // 1. Build dependency graph
        let mut graph = DependencyGraph::new();

        for module in self.registry.all_modules() {
            graph.add_node(module.id.clone());

            for import in &module.imports {
                graph.add_edge(module.id.clone(), import.module_id.clone());
            }
        }

        // 2. Validate version constraints
        self.validate_versions()?;

        // 3. Topological sort for compilation order
        graph.topological_sort()
    }

    fn validate_versions(&self) -> Result<(), CompilerError> {
        for module in self.registry.all_modules() {
            for import in &module.imports {
                let imported_module = self.registry
                    .get(&import.module_id)
                    .ok_or_else(|| CompilerError::ModuleNotFound(import.module_id.clone()))?;

                if !import.version_constraint.matches(&imported_module.version) {
                    return Err(CompilerError::VersionMismatch {
                        module: module.id.clone(),
                        import: import.module_id.clone(),
                        required: import.version_constraint.clone(),
                        found: imported_module.version.clone(),
                    });
                }
            }
        }

        Ok(())
    }
}
```

---

## Phase 3: Semantic Analysis

### Symbol Table

```rust
use std::collections::HashMap;

pub struct SymbolTable {
    scopes: Vec<Scope>,
    current_module: Option<ModuleId>,
}

pub struct Scope {
    symbols: HashMap<String, Symbol>,
}

pub struct Symbol {
    pub name: String,
    pub symbol_type: SymbolType,
    pub defined_in: ModuleId,
    pub visibility: Visibility,
}

pub enum SymbolType {
    Pattern(PatternInfo),
    Type(TypeInfo),
    Entity(EntityInfo),
    Workflow(WorkflowInfo),
    Rule(RuleInfo),
}

pub enum Visibility {
    Public,      // Exported by module
    Private,     // Internal to module
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope::new()],  // Global scope
            current_module: None,
        }
    }

    pub fn enter_module(&mut self, module_id: ModuleId) {
        self.current_module = Some(module_id);
        self.scopes.push(Scope::new());
    }

    pub fn exit_module(&mut self) {
        self.scopes.pop();
        self.current_module = None;
    }

    pub fn define(&mut self, name: String, symbol: Symbol) -> Result<(), CompilerError> {
        let scope = self.scopes.last_mut().unwrap();

        if scope.symbols.contains_key(&name) {
            return Err(CompilerError::SymbolAlreadyDefined(name));
        }

        scope.symbols.insert(name, symbol);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn lookup_in_module(&self, name: &str, module_id: &ModuleId) -> Option<&Symbol> {
        for scope in &self.scopes {
            if let Some(symbol) = scope.symbols.get(name) {
                if &symbol.defined_in == module_id && matches!(symbol.visibility, Visibility::Public) {
                    return Some(symbol);
                }
            }
        }
        None
    }
}
```

### Semantic Analyzer

```rust
pub struct SemanticAnalyzer {
    registry: ModuleRegistry,
    symbol_table: SymbolTable,
    type_checker: TypeChecker,
}

impl SemanticAnalyzer {
    pub fn new(registry: ModuleRegistry) -> Self {
        SemanticAnalyzer {
            registry,
            symbol_table: SymbolTable::new(),
            type_checker: TypeChecker::new(),
        }
    }

    pub fn analyze(&mut self, compilation_order: Vec<ModuleId>) -> Result<LinkedProgram, CompilerError> {
        // Process modules in dependency order
        for module_id in compilation_order {
            self.analyze_module(&module_id)?;
        }

        // Build linked program
        self.link_program()
    }

    fn analyze_module(&mut self, module_id: &ModuleId) -> Result<(), CompilerError> {
        let module = self.registry.get(module_id).unwrap();

        self.symbol_table.enter_module(module_id.clone());

        // 1. Register exports in symbol table
        self.register_exports(module)?;

        // 2. Validate imports
        self.validate_imports(module)?;

        // 3. Analyze definitions
        self.analyze_definitions(module)?;

        // 4. Type checking
        self.type_check_module(module)?;

        self.symbol_table.exit_module();

        Ok(())
    }

    fn register_exports(&mut self, module: &Module) -> Result<(), CompilerError> {
        for export in &module.exports {
            for item in &export.items {
                let symbol = Symbol {
                    name: item.clone(),
                    symbol_type: self.infer_symbol_type(&export.category, item)?,
                    defined_in: module.id.clone(),
                    visibility: Visibility::Public,
                };

                self.symbol_table.define(item.clone(), symbol)?;
            }
        }

        Ok(())
    }

    fn validate_imports(&self, module: &Module) -> Result<(), CompilerError> {
        for import in &module.imports {
            let imported_module = self.registry.get(&import.module_id)
                .ok_or_else(|| CompilerError::ModuleNotFound(import.module_id.clone()))?;

            // Validate imported symbols exist
            if let Some(selector) = &import.selector {
                for item in &selector.items {
                    if self.symbol_table.lookup_in_module(item, &import.module_id).is_none() {
                        return Err(CompilerError::SymbolNotExported {
                            symbol: item.clone(),
                            module: import.module_id.clone(),
                        });
                    }
                }
            }
        }

        Ok(())
    }

    fn analyze_definitions(&mut self, module: &Module) -> Result<(), CompilerError> {
        // Visit AST and validate each definition
        let visitor = DefinitionVisitor::new(&self.symbol_table, &self.type_checker);
        visitor.visit(&module.ast)?;

        Ok(())
    }

    fn type_check_module(&self, module: &Module) -> Result<(), CompilerError> {
        self.type_checker.check_module(module, &self.symbol_table)?;
        Ok(())
    }

    fn link_program(&self) -> Result<LinkedProgram, CompilerError> {
        // Create linked program with all modules and resolved symbols
        Ok(LinkedProgram {
            modules: self.registry.all_modules().into_iter().cloned().collect(),
            symbol_table: self.symbol_table.clone(),
        })
    }
}
```

### Type Checker

```rust
pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker
    }

    pub fn check_module(&self, module: &Module, symbol_table: &SymbolTable) -> Result<(), CompilerError> {
        // Type check all definitions in the module
        let visitor = TypeCheckVisitor::new(symbol_table);
        visitor.visit(&module.ast)?;

        Ok(())
    }

    pub fn check_expression(&self, expr: &Expression, symbol_table: &SymbolTable) -> Result<Type, CompilerError> {
        match expr {
            Expression::Identifier(name) => {
                let symbol = symbol_table.lookup(name)
                    .ok_or_else(|| CompilerError::UndefinedSymbol(name.clone()))?;

                Ok(self.get_symbol_type(symbol))
            }
            Expression::BinaryOp { left, op, right } => {
                let left_type = self.check_expression(left, symbol_table)?;
                let right_type = self.check_expression(right, symbol_table)?;

                self.check_binary_op(left_type, op, right_type)
            }
            Expression::FunctionCall { name, args } => {
                let symbol = symbol_table.lookup(name)
                    .ok_or_else(|| CompilerError::UndefinedSymbol(name.clone()))?;

                self.check_function_call(symbol, args, symbol_table)
            }
            // ... other expression types
        }
    }

    fn check_binary_op(&self, left: Type, op: &BinaryOperator, right: Type) -> Result<Type, CompilerError> {
        // Type checking rules for binary operations
        match (left, op, right) {
            (Type::Number, BinaryOperator::Add, Type::Number) => Ok(Type::Number),
            (Type::Number, BinaryOperator::Subtract, Type::Number) => Ok(Type::Number),
            (Type::Text, BinaryOperator::Concat, Type::Text) => Ok(Type::Text),
            // ... more type rules

            (left, op, right) => Err(CompilerError::TypeMismatch {
                expected: format!("{:?} {} {:?}", left, op, right),
                found: "incompatible types".to_string(),
            }),
        }
    }
}
```

---

## Phase 4: Code Generation

### Code Generator Architecture

```rust
pub trait CodeGenerator {
    fn generate(&self, program: &LinkedProgram) -> Result<GeneratedCode, CompilerError>;
}

pub struct PythonCodeGenerator;
pub struct JavaCodeGenerator;
pub struct RustCodeGenerator;

pub struct GeneratedCode {
    pub files: Vec<GeneratedFile>,
}

pub struct GeneratedFile {
    pub path: String,
    pub content: String,
}

impl PythonCodeGenerator {
    pub fn new() -> Self {
        PythonCodeGenerator
    }
}

impl CodeGenerator for PythonCodeGenerator {
    fn generate(&self, program: &LinkedProgram) -> Result<GeneratedCode, CompilerError> {
        let mut files = Vec::new();

        // 1. Generate runtime library
        files.push(self.generate_runtime()?);

        // 2. Generate core modules
        for module in &program.modules {
            if matches!(module.id.module_type, ModuleType::Core) {
                files.push(self.generate_core_module(module, &program.symbol_table)?);
            }
        }

        // 3. Generate domain modules
        for module in &program.modules {
            if matches!(module.id.module_type, ModuleType::Domain) {
                files.push(self.generate_domain_module(module, &program.symbol_table)?);
            }
        }

        Ok(GeneratedCode { files })
    }
}

impl PythonCodeGenerator {
    fn generate_runtime(&self) -> Result<GeneratedFile, CompilerError> {
        let content = r#"
# FS-DSL Runtime Library for Python
from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional
from datetime import datetime
from decimal import Decimal

class Pattern:
    """Base class for all data patterns"""
    pass

class MasterData(Pattern):
    """Mutable, audited records with version tracking"""
    def __init__(self):
        self.version = 1
        self.updated_at = datetime.now()
        self.updated_by = None

class ImmutableLedger(Pattern):
    """Append-only records with reversal support"""
    def __init__(self):
        self.created_at = datetime.now()
        self.is_reversed = False
        self.reversal_id = None

# ... more runtime classes
"#;

        Ok(GeneratedFile {
            path: "fsdsl/runtime.py".to_string(),
            content: content.to_string(),
        })
    }

    fn generate_core_module(&self, module: &Module, symbol_table: &SymbolTable) -> Result<GeneratedFile, CompilerError> {
        // Generate Python code for core module
        let mut code = String::new();

        code.push_str(&format!("# Core Module: {}\n", module.id.name));
        code.push_str(&format!("# Version: {}\n\n", module.version));

        code.push_str("from fsdsl.runtime import *\n\n");

        // Generate exports
        for export in &module.exports {
            match export.category {
                ExportCategory::Patterns => {
                    for pattern in &export.items {
                        code.push_str(&self.generate_pattern_class(pattern)?);
                    }
                }
                // ... other export categories
                _ => {}
            }
        }

        Ok(GeneratedFile {
            path: format!("fsdsl/core/{}.py", module.id.name),
            content: code,
        })
    }

    fn generate_domain_module(&self, module: &Module, symbol_table: &SymbolTable) -> Result<GeneratedFile, CompilerError> {
        let mut code = String::new();

        code.push_str(&format!("# Domain Module: {}\n", module.id.name));
        code.push_str(&format!("# Version: {}\n\n", module.version));

        // Import core modules
        for import in &module.imports {
            code.push_str(&format!("from fsdsl.core.{} import *\n", import.module_id.name));
        }

        code.push_str("\n");

        // Generate domain entities, workflows, rules
        let visitor = PythonCodeGenVisitor::new(symbol_table);
        let domain_code = visitor.visit(&module.ast)?;

        code.push_str(&domain_code);

        Ok(GeneratedFile {
            path: format!("fsdsl/domains/{}.py", module.id.name),
            content: code,
        })
    }

    fn generate_pattern_class(&self, pattern: &str) -> Result<String, CompilerError> {
        // Generate Python class for pattern
        Ok(format!(
            r#"
class {}:
    """Pattern: {}"""
    def __init__(self):
        pass
"#,
            pattern, pattern
        ))
    }
}
```

---

## Build System Integration

### Project Structure

```
fsdsl-compiler/
├── Cargo.toml                  # Rust project
├── build.rs                    # Build script
├── grammar/
│   └── DSL.g4                  # ANTLR4 grammar
├── src/
│   ├── main.rs                 # Compiler entry point
│   ├── parser/
│   │   ├── mod.rs
│   │   ├── antlr_bridge.rs     # JNI bridge to ANTLR
│   │   └── ast.rs              # AST definitions
│   ├── module/
│   │   ├── mod.rs
│   │   ├── registry.rs         # Module registry
│   │   ├── resolver.rs         # Dependency resolver
│   │   └── graph.rs            # Dependency graph
│   ├── semantic/
│   │   ├── mod.rs
│   │   ├── analyzer.rs         # Semantic analyzer
│   │   ├── symbol_table.rs     # Symbol table
│   │   └── type_checker.rs     # Type checker
│   └── codegen/
│       ├── mod.rs
│       ├── python.rs           # Python code generator
│       ├── java.rs             # Java code generator
│       └── rust.rs             # Rust code generator
└── tests/
    ├── parser_tests.rs
    ├── module_tests.rs
    ├── semantic_tests.rs
    └── codegen_tests.rs
```

### Build Script

**build.rs**:
```rust
use std::process::Command;

fn main() {
    // Generate ANTLR parser during build
    println!("cargo:rerun-if-changed=grammar/DSL.g4");

    let status = Command::new("antlr4")
        .args(&[
            "-Dlanguage=Java",
            "-visitor",
            "-no-listener",
            "-o",
            "target/generated-sources/antlr4",
            "grammar/DSL.g4",
        ])
        .status()
        .expect("Failed to generate ANTLR parser");

    if !status.success() {
        panic!("ANTLR code generation failed");
    }

    // Compile Java parser
    let status = Command::new("javac")
        .args(&[
            "-d",
            "target/classes",
            "target/generated-sources/antlr4/*.java",
        ])
        .status()
        .expect("Failed to compile Java parser");

    if !status.success() {
        panic!("Java compilation failed");
    }
}
```

### Cargo.toml

```toml
[package]
name = "fsdsl-compiler"
version = "1.0.0"
edition = "2021"

[dependencies]
# Module resolution
semver = "1.0"
petgraph = "0.6"  # For dependency graph

# JNI bridge to ANTLR Java parser
jni = "0.21"

# Code generation
tera = "1.19"  # Template engine

# CLI
clap = { version = "4.0", features = ["derive"] }

# Error handling
anyhow = "1.0"
thiserror = "1.0"

[build-dependencies]
# None needed if ANTLR is installed globally
```

---

## Compiler CLI

### Command Line Interface

```rust
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "fsdslc")]
#[command(about = "Financial Services DSL Compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile DSL files to target language
    Compile {
        /// Input DSL files or directories
        #[arg(short, long, value_name = "FILES")]
        input: Vec<String>,

        /// Output directory
        #[arg(short, long, value_name = "DIR")]
        output: String,

        /// Target language
        #[arg(short, long, value_enum)]
        target: TargetLanguage,

        /// Optimization level
        #[arg(short = 'O', long, default_value = "2")]
        opt_level: u8,
    },

    /// Check DSL files for errors without generating code
    Check {
        /// Input DSL files or directories
        #[arg(short, long, value_name = "FILES")]
        input: Vec<String>,
    },

    /// Show module dependencies
    Graph {
        /// Input DSL files or directories
        #[arg(short, long, value_name = "FILES")]
        input: Vec<String>,

        /// Output format
        #[arg(short, long, value_enum, default_value = "dot")]
        format: GraphFormat,
    },
}

#[derive(clap::ValueEnum, Clone)]
enum TargetLanguage {
    Python,
    Java,
    Rust,
}

#[derive(clap::ValueEnum, Clone)]
enum GraphFormat {
    Dot,
    Json,
    Text,
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile { input, output, target, opt_level } => {
            compile_command(input, output, target, opt_level);
        }
        Commands::Check { input } => {
            check_command(input);
        }
        Commands::Graph { input, format } => {
            graph_command(input, format);
        }
    }
}

fn compile_command(input: Vec<String>, output: String, target: TargetLanguage, opt_level: u8) {
    println!("Compiling DSL files...");
    println!("  Input: {:?}", input);
    println!("  Output: {}", output);
    println!("  Target: {:?}", target);
    println!("  Optimization: -O{}", opt_level);

    // 1. Parse all input files
    let asts = parse_files(&input).expect("Parsing failed");

    // 2. Build module registry
    let registry = build_registry(asts).expect("Module registration failed");

    // 3. Resolve dependencies
    let resolver = ModuleResolver::new(registry.clone());
    let compilation_order = resolver.resolve().expect("Dependency resolution failed");

    // 4. Semantic analysis
    let mut analyzer = SemanticAnalyzer::new(registry);
    let linked_program = analyzer.analyze(compilation_order).expect("Semantic analysis failed");

    // 5. Code generation
    let generator: Box<dyn CodeGenerator> = match target {
        TargetLanguage::Python => Box::new(PythonCodeGenerator::new()),
        TargetLanguage::Java => Box::new(JavaCodeGenerator::new()),
        TargetLanguage::Rust => Box::new(RustCodeGenerator::new()),
    };

    let generated_code = generator.generate(&linked_program).expect("Code generation failed");

    // 6. Write output files
    write_generated_code(&output, generated_code).expect("Failed to write output");

    println!("Compilation successful!");
}
```

### Usage Examples

```bash
# Compile to Python
fsdslc compile -i core/ domains/ -o build/python -t python

# Compile to Java with optimization
fsdslc compile -i core/ domains/ -o build/java -t java -O 3

# Check for errors without generating code
fsdslc check -i domains/credit_card.dsl

# Generate dependency graph
fsdslc graph -i core/ domains/ -f dot > deps.dot
dot -Tpng deps.dot -o deps.png
```

---

## Testing Strategy

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_module_declaration() {
        let input = r#"
module: core.entity
version: 2.0.0
stability: stable
"#;

        let ast = parse_string(input).expect("Parse failed");

        assert!(matches!(ast, ASTNode::ModuleDeclaration { .. }));
    }

    #[test]
    fn test_version_constraint_matching() {
        let constraint = VersionReq::parse(">=2.0.0").unwrap();
        let version = Version::parse("2.1.0").unwrap();

        assert!(constraint.matches(&version));
    }

    #[test]
    fn test_circular_dependency_detection() {
        let mut graph = DependencyGraph::new();

        let a = ModuleId { module_type: ModuleType::Domain, name: "a".to_string() };
        let b = ModuleId { module_type: ModuleType::Domain, name: "b".to_string() };

        graph.add_node(a.clone());
        graph.add_node(b.clone());
        graph.add_edge(a.clone(), b.clone());
        graph.add_edge(b.clone(), a.clone());  // Circular!

        assert!(graph.topological_sort().is_err());
    }
}
```

### Integration Tests

```rust
#[test]
fn test_compile_credit_card_domain() {
    // Full end-to-end compilation test
    let input_files = vec![
        "core/entity.dsl",
        "core/workflow.dsl",
        "domains/credit_card.dsl",
    ];

    let asts = parse_files(&input_files).expect("Parsing failed");
    let registry = build_registry(asts).expect("Registry failed");
    let resolver = ModuleResolver::new(registry.clone());
    let order = resolver.resolve().expect("Resolution failed");

    let mut analyzer = SemanticAnalyzer::new(registry);
    let program = analyzer.analyze(order).expect("Analysis failed");

    let generator = PythonCodeGenerator::new();
    let code = generator.generate(&program).expect("Generation failed");

    assert!(code.files.len() > 0);
}
```

---

## Performance Considerations

### Caching

**Parse Tree Caching**:
```rust
use std::collections::HashMap;
use std::time::SystemTime;

pub struct ParseCache {
    cache: HashMap<String, (SystemTime, ASTNode)>,
}

impl ParseCache {
    pub fn get_or_parse(&mut self, file_path: &str) -> Result<ASTNode, CompilerError> {
        let metadata = std::fs::metadata(file_path)?;
        let modified = metadata.modified()?;

        if let Some((cached_time, cached_ast)) = self.cache.get(file_path) {
            if *cached_time == modified {
                return Ok(cached_ast.clone());
            }
        }

        // Parse and cache
        let ast = parse_file(file_path)?;
        self.cache.insert(file_path.to_string(), (modified, ast.clone()));

        Ok(ast)
    }
}
```

### Incremental Compilation

**Change Detection**:
```rust
pub struct IncrementalCompiler {
    previous_compilation: Option<CompilationResult>,
}

impl IncrementalCompiler {
    pub fn compile(&mut self, files: &[String]) -> Result<GeneratedCode, CompilerError> {
        // Detect changed files
        let changed = self.detect_changes(files)?;

        if changed.is_empty() && self.previous_compilation.is_some() {
            // No changes, return cached result
            return Ok(self.previous_compilation.as_ref().unwrap().code.clone());
        }

        // Recompile only affected modules
        let affected = self.compute_affected_modules(&changed)?;

        // ... recompile affected modules
    }
}
```

---

## Error Handling

### Compiler Errors

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Parse error at {line}:{column}: {message}")]
    ParseError {
        line: usize,
        column: usize,
        message: String,
    },

    #[error("Module not found: {0:?}")]
    ModuleNotFound(ModuleId),

    #[error("Duplicate module: {0:?}")]
    DuplicateModule(ModuleId),

    #[error("Circular dependency detected")]
    CircularDependency,

    #[error("Version mismatch: {module:?} requires {import:?} {required}, but found {found}")]
    VersionMismatch {
        module: ModuleId,
        import: ModuleId,
        required: VersionReq,
        found: Version,
    },

    #[error("Symbol not exported: {symbol} from module {module:?}")]
    SymbolNotExported {
        symbol: String,
        module: ModuleId,
    },

    #[error("Undefined symbol: {0}")]
    UndefinedSymbol(String),

    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
    },

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}
```

### Error Reporting

```rust
pub struct ErrorReporter {
    errors: Vec<CompilerError>,
    warnings: Vec<String>,
}

impl ErrorReporter {
    pub fn report(&self) {
        for error in &self.errors {
            eprintln!("Error: {}", error);
        }

        for warning in &self.warnings {
            eprintln!("Warning: {}", warning);
        }

        if !self.errors.is_empty() {
            eprintln!("\n{} error(s), {} warning(s)", self.errors.len(), self.warnings.len());
        }
    }
}
```

---

## See Also

- **DSL_GRAMMAR.ebnf**: Complete EBNF grammar specification
- **MODULE_SYSTEM.md**: Module system specification
- **core/CORE_DSL_SPECIFICATION.md**: Core DSL constructs
- **domains/**: Domain DSL specifications
