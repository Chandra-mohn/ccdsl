# Architecture Decisions and Discussion Log

**Project**: Credit Card DSL Compiler
**Date**: 2025-11-11
**Status**: Foundation Complete, Visual Editor Strategy Defined

---

## Table of Contents

1. [Overview](#overview)
2. [Initial Toolchain Decision](#initial-toolchain-decision)
3. [MPS Visual Editor Challenge](#mps-visual-editor-challenge)
4. [Hybrid Architecture Solution](#hybrid-architecture-solution)
5. [Implementation Roadmap](#implementation-roadmap)
6. [Technology Stack Decisions](#technology-stack-decisions)
7. [Key Trade-offs and Rationale](#key-trade-offs-and-rationale)

---

## Overview

This document captures the architectural discussions and decisions for the Credit Card DSL compiler project. The primary challenge was choosing between a pure text-based approach (ANTLR4) versus a projectional editing approach (JetBrains MPS), with a final decision to pursue a **hybrid architecture** that combines the strengths of both.

### Project Goals

1. **Business-Friendly DSL**: YAML-inspired syntax that non-programmers can read and edit
2. **Pattern-Based Code Generation**: Generate different Rust code based on data mutation patterns
3. **Production Quality**: Type-safe, validated code generation with comprehensive error messages
4. **Developer Experience**: Great IDE support, visual tools where helpful
5. **Version Control Friendly**: Plain text files that work well with git

---

## Initial Toolchain Decision

### The Question

Should we use **ANTLR4** (traditional parser generator) or **JetBrains MPS** (language workbench with projectional editing)?

### ANTLR4 Approach Analysis

**Strengths**:
- ✅ Perfect for text-based YAML-inspired DSL
- ✅ Version control friendly (plain .dsl text files)
- ✅ Mature, battle-tested technology (used by Presto, Hive, etc.)
- ✅ Full control over code generation pipeline
- ✅ Users can edit with any text editor
- ✅ Lightweight tooling (no heavy IDE required)
- ✅ Large community and extensive documentation
- ✅ Multi-language targets (Rust, Java, Python, JavaScript, etc.)

**Weaknesses**:
- ❌ No built-in visual editing capabilities
- ❌ Complex constructs (decision tables, state machines) harder to visualize
- ❌ Manual LSP server implementation required
- ❌ Custom indentation handling needed (YAML-style)

**Architecture**:
```
.dsl files → ANTLR4 Lexer → ANTLR4 Parser → Parse Tree
           → AST Builder → Type-safe AST → Semantic Analysis
           → Pattern-Based Code Generator → Rust Code
```

### JetBrains MPS Approach Analysis

**Strengths**:
- ✅ Built-in projectional editing (edit AST directly)
- ✅ Custom visual editors for specific constructs
- ✅ Decision tables, diagrams, specialized UIs out-of-box
- ✅ Type-safe editing with immediate validation
- ✅ Integrated language workbench (grammar + editor + generator)
- ✅ Multiple notations for same concepts

**Weaknesses**:
- ❌ **Projectional editing conflicts with text-based goal**
- ❌ Users MUST use MPS IDE (can't use vim, VS Code, etc.)
- ❌ Version control complex (models stored as XML fragments)
- ❌ Heavy tooling requirement (JVM + MPS)
- ❌ Steeper learning curve for contributors
- ❌ Less control over compilation pipeline
- ❌ Business users can't edit with familiar text editors

**Critical Conflict**:
> MPS's projectional editing fundamentally conflicts with our goal of a **business-friendly text-based DSL**. Business users want to edit plain text files with familiar editors, not learn a specialized IDE to edit AST projections.

### Initial Decision: ANTLR4 + tree-sitter

**Rationale**:
1. **Text-first approach** aligns with business user requirements
2. **Version control** is critical for enterprise adoption
3. **Any editor works** - no forced tooling
4. **Full compilation control** for pattern-based code generation
5. **tree-sitter** can provide excellent IDE support later

**Architecture**:
```
Phase 1: ANTLR4 Compiler (grammar → parser → AST → codegen)
Phase 2: tree-sitter grammar (fast incremental parsing)
Phase 3: LSP server (IDE features)
Phase 4: VS Code extension (syntax highlighting, autocomplete)
```

---

## MPS Visual Editor Challenge

### The Valid Concern

After the initial ANTLR4 decision, a critical challenge was raised:

> **"One significant advantage of using MPS is we can create editors to suit specific use cases, such as complex if/else matrix, a data modeling editor, etc."**

This is a **100% valid and important point**. Visual editors provide enormous value for:

1. **Decision Tables**: Complex if/else logic is much clearer as a matrix/table
2. **Entity-Relationship Diagrams**: Data modeling is easier to understand visually
3. **State Machines**: State diagrams are clearer than textual state definitions
4. **Workflows**: Process flows benefit from visual BPMN-like diagrams

### Examples of Where Visual Editing Helps

#### Example 1: Decision Table for Late Fee Calculation

**DSL Text** (harder to understand):
```
rule: calculate_late_fee
  calculate:
    base_fee = when account_balance >= $5000: $40.00
               when account_balance >= $1000: $30.00
               otherwise: $25.00

    late_fee = when days_past_due > 30: base_fee
               when days_past_due > 15: base_fee * 0.75
               otherwise: $0.00
```

**Visual Decision Table** (instantly clear):
```
┌──────────────────┬──────────────┬─────────┐
│ Account Balance  │ Days Past Due│ Late Fee│
├──────────────────┼──────────────┼─────────┤
│ >= $5000         │ > 30         │ $40.00  │
│ >= $5000         │ > 15         │ $30.00  │
│ >= $5000         │ else         │ $0.00   │
├──────────────────┼──────────────┼─────────┤
│ >= $1000         │ > 30         │ $30.00  │
│ >= $1000         │ > 15         │ $22.50  │
│ >= $1000         │ else         │ $0.00   │
├──────────────────┼──────────────┼─────────┤
│ else             │ > 30         │ $25.00  │
│ else             │ > 15         │ $18.75  │
│ else             │ else         │ $0.00   │
└──────────────────┴──────────────┴─────────┘
```

#### Example 2: Entity Relationship Diagram

**DSL Text**:
```
define entity: customer
  identity: customer_id
  profile:
    first_name: text
    last_name: text

define entity: account
  identity: account_id
  belongs_to: customer
  has_many: transactions

define entity: transaction
  identity: transaction_id
  belongs_to: account
```

**Visual ER Diagram**:
```
┌────────────┐
│  Customer  │
│────────────│
│ customer_id│
│ first_name │
│ last_name  │
└──────┬─────┘
       │ 1
       │
       │ *
┌──────┴─────┐
│  Account   │
│────────────│
│ account_id │
│ customer_id│
└──────┬─────┘
       │ 1
       │
       │ *
┌──────┴─────────┐
│  Transaction   │
│────────────────│
│ transaction_id │
│ account_id     │
│ amount         │
│ timestamp      │
└────────────────┘
```

#### Example 3: State Machine Diagram

**DSL Text**:
```
states:
  - pending
  - active
  - suspended
  - closed

transitions:
  pending → active: on activation
  active → suspended: on suspension
  active → closed: on closure
  suspended → active: on reactivation
  suspended → closed: on closure
```

**Visual State Diagram**:
```
    ┌─────────┐
    │ pending │
    └────┬────┘
         │ activate
         ▼
    ┌─────────┐ suspend  ┌───────────┐
    │ active  │─────────▶│ suspended │
    └────┬────┘          └─────┬─────┘
         │                     │
         │ close               │ reactivate
         │                     │
         ▼                     ▼
    ┌─────────┐◀───────────────┘
    │ closed  │
    └─────────┘
         ▲
         │ close
         │
    ┌────┴────┐
    │suspended│
    └─────────┘
```

### Why This Matters

- **Cognitive Load**: Visual representations reduce mental overhead for complex logic
- **Error Detection**: Easier to spot logic errors in visual format
- **Communication**: Business stakeholders understand diagrams better than code
- **Onboarding**: New team members grasp system faster with visual tools

---

## Hybrid Architecture Solution

### The Answer: Best of Both Worlds

Instead of choosing ANTLR4 **OR** MPS, we should combine their strengths:

```
┌─────────────────────────────────────────┐
│  Visual Editors (Web-Based)             │
│  ├─ Decision Table Editor                │
│  ├─ ER Diagram Editor                    │
│  ├─ State Diagram Editor                 │
│  └─ Workflow Designer                    │
└──────────────┬──────────────────────────┘
               │ Generates/Syncs
               ▼
┌─────────────────────────────────────────┐
│  .dsl Text Files (Version Control)       │  ← Git-friendly
│  - Can edit directly OR via visual UI    │
│  - Plain text for simple cases           │
│  - Generated from visual for complex     │
└──────────────┬──────────────────────────┘
               │ Compiles
               ▼
┌─────────────────────────────────────────┐
│  ANTLR4 Compiler Pipeline                │
│  └─ Parser → AST → Semantic → Codegen    │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│  Production Rust Code                    │
└─────────────────────────────────────────┘
```

### Key Principles

1. **Text Files as Source of Truth**: .dsl files are version controlled
2. **Visual Editors are Optional**: Users choose text OR visual editing
3. **Bidirectional Sync**: Visual ↔ Text conversion both ways
4. **Targeted Visual Tools**: Only for constructs that benefit (not everything)
5. **Compilation Independence**: Visual editors are separate from compiler

### Comparison: Pure MPS vs Hybrid Approach

| Aspect | Pure MPS | Hybrid Approach |
|--------|----------|-----------------|
| **Visual Editors** | ✅ Excellent | ✅ Excellent (for specific constructs) |
| **Text Editing** | ❌ Projectional only | ✅ Full text editing freedom |
| **Version Control** | ⚠️ Complex (XML models) | ✅ Plain text .dsl files |
| **Learning Curve** | ❌ High (MPS-specific) | ✅ Text + optional visual |
| **Tooling Weight** | ❌ Heavy (JVM + MPS IDE) | ✅ Lightweight (web browser) |
| **Compilation Control** | ⚠️ MPS generators | ✅ Full ANTLR4 control |
| **Editor Flexibility** | ❌ MPS IDE only | ✅ Any editor + visual tools |
| **Business User Friendly** | ⚠️ Must learn MPS | ✅ Familiar text OR visual |
| **Git Diffs** | ❌ XML fragments | ✅ Readable text diffs |
| **CI/CD Integration** | ⚠️ Complex | ✅ Simple (text files) |

### Advantages Over Pure ANTLR4

1. **Visual editing for complex logic** - Decision tables, diagrams where they help
2. **Better communication** - Stakeholders understand visual models
3. **Faster onboarding** - Visual tools reduce learning curve
4. **Error prevention** - Visual editors can prevent invalid states
5. **Multiple representations** - Same entity as text OR diagram

### Advantages Over Pure MPS

1. **Text editing freedom** - Use any editor (vim, VS Code, IntelliJ, etc.)
2. **Version control friendly** - Plain text diffs, no XML fragments
3. **Lightweight tooling** - No forced MPS installation
4. **Compilation control** - Full control over code generation
5. **Incremental adoption** - Start with text, add visual tools later
6. **Web-based editors** - No installation, works anywhere

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-12) **← CURRENT PHASE**

**Status**: 25% Complete (Grammar ✅, AST ✅, Build System ✅)

**Focus**: Prove ANTLR4 compiler works end-to-end

**Tasks**:
- ✅ Complete ANTLR4 grammar (~600 lines)
- ✅ Complete AST type system (6 modules, ~1200 lines)
- ✅ Cargo project setup with ANTLR4 integration
- ✅ Example DSL files (4 test cases)
- ⏳ Parser implementation (ANTLR4 → AST)
- ⏳ Custom indentation lexer (INDENT/DEDENT tokens)
- ⏳ Semantic analysis (type checking, validation)
- ⏳ Pattern-based code generation (start with master_data)
- ⏳ Basic CLI tool (compile, check, ast commands)

**Goal**: Working compiler that transforms .dsl → Rust code

**Success Criteria**:
- Can parse all example .dsl files without errors
- Generates valid Rust code for master_data pattern
- Error messages are clear and helpful
- CLI tool is usable for basic workflows

### Phase 2: Developer Experience (Weeks 13-20)

**Focus**: Make text-based editing professional and productive

**Tasks**:
- LSP server implementation
  - Syntax validation
  - Autocomplete for keywords
  - Go-to-definition for entities
  - Hover documentation
  - Error diagnostics

- tree-sitter grammar
  - Fast incremental parsing
  - Syntax highlighting
  - Code folding
  - Selection ranges

- VS Code extension
  - Language support
  - Snippets for common patterns
  - Pattern selection wizard
  - DSL file templates

- Error message improvements
  - Beautiful formatting (miette)
  - Helpful suggestions
  - Error recovery strategies
  - Warning system

**Goal**: Professional IDE experience for text editing

**Success Criteria**:
- VS Code provides autocomplete, validation, navigation
- Error messages are beautiful and helpful
- Syntax highlighting works perfectly
- Developer productivity matches typed languages

### Phase 3: Visual Editors (Weeks 21-32)

**Focus**: Add visual editing for constructs that benefit most

#### 3.1 Decision Table Editor (Weeks 21-24)

**Technology Stack**:
- Frontend: React + TypeScript
- Grid: ag-grid or react-table
- Parser: ANTLR4 JavaScript target
- Export: DSL text generation

**Features**:
- Excel-like grid editing interface
- Row/column insertion and deletion
- Formula validation
- Condition expression builder
- Export to DSL text
- Import from DSL text (parse and render)
- Highlighting for errors/warnings

**Example UI**:
```
┌────────────────────────────────────────────────┐
│  Decision Table: calculate_late_fee            │
├────────────────────────────────────────────────┤
│  Conditions          │ Actions                 │
├──────────┬───────────┼─────────────────────────┤
│ Balance  │ Days Past │ Late Fee                │
│          │ Due       │                         │
├──────────┼───────────┼─────────────────────────┤
│ >= 5000  │ > 30      │ $40.00                  │
│ >= 5000  │ > 15      │ $30.00                  │
│ >= 5000  │ else      │ $0.00                   │
│ >= 1000  │ > 30      │ $30.00                  │
│ >= 1000  │ > 15      │ $22.50                  │
│ >= 1000  │ else      │ $0.00                   │
│ else     │ > 30      │ $25.00                  │
│ else     │ > 15      │ $18.75                  │
│ else     │ else      │ $0.00                   │
└──────────┴───────────┴─────────────────────────┘
[+ Add Row] [+ Add Condition] [Export to DSL] [Import]
```

#### 3.2 ER Diagram Editor (Weeks 25-27)

**Technology Stack**:
- Frontend: React + TypeScript
- Diagrams: react-flow or reactflow
- Layout: dagre (auto-layout)
- Styling: Tailwind CSS

**Features**:
- Drag-and-drop entity boxes
- Visual relationship drawing
- Cardinality editing (1:1, 1:*, *:*)
- Field type selection
- Constraint visualization
- Auto-layout algorithm
- Export to entity DSL
- Import from entity DSL

**Example UI**:
```
┌────────────────────────────────────────────────┐
│  Entity Relationship Designer                  │
├────────────────────────────────────────────────┤
│  [Tools: Entity] [Relationship] [Layout]       │
├────────────────────────────────────────────────┤
│                                                 │
│   ┌──────────────┐                             │
│   │  Customer    │                             │
│   │──────────────│                             │
│   │ customer_id  │                             │
│   │ first_name   │                             │
│   │ last_name    │                             │
│   └──────┬───────┘                             │
│          │ 1:*                                 │
│          ↓                                     │
│   ┌──────────────┐                             │
│   │  Account     │                             │
│   │──────────────│                             │
│   │ account_id   │                             │
│   │ balance      │                             │
│   └──────┬───────┘                             │
│          │ 1:*                                 │
│          ↓                                     │
│   ┌──────────────┐                             │
│   │ Transaction  │                             │
│   │──────────────│                             │
│   │ trans_id     │                             │
│   │ amount       │                             │
│   └──────────────┘                             │
│                                                 │
└────────────────────────────────────────────────┘
[Export to DSL] [Import from DSL] [Auto-Layout]
```

#### 3.3 State Diagram Editor (Weeks 28-30)

**Technology Stack**:
- Frontend: React + TypeScript
- Diagrams: react-flow
- Layout: elk.js (hierarchical layout)

**Features**:
- Visual state creation
- Transition drawing with conditions
- Event labeling
- Initial/final state marking
- Validation (no orphan states)
- Export to state machine DSL
- Import from state machine DSL

**Example UI**:
```
┌────────────────────────────────────────────────┐
│  State Machine Designer: Account               │
├────────────────────────────────────────────────┤
│  [States] [Transitions] [Events]               │
├────────────────────────────────────────────────┤
│                                                 │
│        ┌─────────┐                             │
│        │ pending │ ⦿ (initial)                 │
│        └────┬────┘                             │
│             │ activate                         │
│             ↓                                  │
│        ┌─────────┐                             │
│        │ active  │                             │
│        └────┬────┘                             │
│         ╱       ╲                              │
│    suspend    close                            │
│      ╱           ╲                             │
│     ↓             ↓                            │
│ ┌──────────┐  ┌────────┐                      │
│ │suspended │  │ closed │ ⊗ (final)            │
│ └────┬─────┘  └────────┘                      │
│      │ reactivate                              │
│      └─────────→ active                        │
│                                                 │
└────────────────────────────────────────────────┘
[Export to DSL] [Import from DSL] [Validate]
```

#### 3.4 Integration and Polish (Weeks 31-32)

- Web app hosting and deployment
- Authentication and project management
- Real-time collaboration (optional)
- Undo/redo for all editors
- Keyboard shortcuts
- Accessibility compliance (WCAG)
- Mobile-responsive design
- Documentation and tutorials

**Goal**: Production-ready visual editors for complex constructs

**Success Criteria**:
- Decision tables are easier to edit visually than text
- ER diagrams are generated from entities automatically
- State machines are clearer in visual form
- All editors support bidirectional sync with DSL text
- Visual editors feel professional and polished

### Phase 4: Full Visual Suite (Weeks 33+)

**Additional Visual Tools**:

1. **Workflow Designer** (BPMN-style)
   - Visual process flow design
   - Step configuration
   - Conditional routing visualization
   - Error handling paths

2. **Rule Visualization**
   - Visual rule dependency graphs
   - Calculation flow diagrams
   - Expression builder UI

3. **Data Flow Diagrams**
   - Visualize data movement between entities
   - Transaction flow visualization
   - Event propagation diagrams

4. **Advanced Features**:
   - Real-time collaboration (multiple users)
   - Version control integration (git diffs for visual changes)
   - Diff visualization for DSL changes
   - Import from Excel/CSV to decision tables
   - Export diagrams to PNG/SVG
   - AI-assisted pattern suggestion

**Long-term Vision**:
- Complete visual workbench for credit card domain modeling
- Seamless integration between text and visual editing
- Professional tooling on par with commercial DSL tools
- Open source community around visual editors

---

## Technology Stack Decisions

### Core Compiler (Phase 1-2)

```yaml
compiler:
  language: Rust
  parser_generator: ANTLR4 4.13.1
  dependencies:
    - antlr-rust: "ANTLR4 Rust runtime"
    - handlebars: "Template-based code generation"
    - miette: "Beautiful error messages"
    - clap: "CLI argument parsing"
    - serde: "AST serialization"
    - insta: "Snapshot testing"

editor_support:
  incremental_parser: tree-sitter
  protocol: Language Server Protocol (LSP)
  ide_integration: VS Code extension

build_system:
  build_tool: Cargo
  antlr_integration: build.rs
  test_framework: cargo test
```

### Visual Editors (Phase 3-4)

```yaml
frontend:
  framework: React 18 + TypeScript
  styling: Tailwind CSS
  state_management: Zustand or Redux Toolkit

decision_table_editor:
  grid_library: ag-grid or react-table
  parser: ANTLR4 JavaScript target
  validation: Zod schema validation

er_diagram_editor:
  diagram_library: react-flow
  layout_engine: dagre
  export: SVG, PNG, DSL text

state_diagram_editor:
  diagram_library: react-flow
  layout_engine: elk.js
  validation: State machine verification

workflow_designer:
  diagram_library: react-flow
  notation: BPMN-inspired
  export: DSL text, BPMN XML (optional)

backend:
  runtime: Node.js or Deno
  api: REST or GraphQL
  validation: Use Rust compiler as validation service
  storage: File system or Git backend

deployment:
  hosting: Vercel, Netlify, or self-hosted
  cdn: Cloudflare
  authentication: Auth0 or similar (optional)
```

---

## Key Trade-offs and Rationale

### Trade-off 1: Text Files vs Binary Models

**Decision**: Text files (.dsl) as source of truth

**Rationale**:
- ✅ Version control works perfectly (git diff, blame, merge)
- ✅ Any text editor works (no forced tooling)
- ✅ CI/CD integration is simple
- ✅ Business users can edit with familiar tools
- ⚠️ Visual editors require parsing/generation step
- ⚠️ Potential sync issues between visual and text

**Why This Wins**: Git-friendly version control is non-negotiable for enterprise adoption. Binary models would make code review and collaboration much harder.

### Trade-off 2: Pure ANTLR4 vs Pure MPS

**Decision**: Hybrid (ANTLR4 compiler + web-based visual editors)

**Rationale**:
- ✅ Get ANTLR4's text-based benefits
- ✅ Get MPS-like visual editing benefits
- ✅ Users choose text OR visual based on task
- ✅ No forced tooling (MPS IDE not required)
- ⚠️ More implementation work (compiler + editors)
- ⚠️ Need to maintain bidirectional sync

**Why This Wins**: Best of both worlds. Users get choice, and we don't sacrifice text-based simplicity for visual capabilities.

### Trade-off 3: Web-Based vs Desktop Visual Editors

**Decision**: Web-based editors (React + browser)

**Rationale**:
- ✅ No installation required
- ✅ Cross-platform by default
- ✅ Easier updates and deployment
- ✅ Can integrate with cloud services
- ⚠️ Requires internet for hosted version
- ⚠️ Performance may be slightly worse than native

**Why This Wins**: Lower barrier to entry. Users can try visual editors immediately without installing anything. Can still provide offline-capable PWA version later.

### Trade-off 4: All-Visual vs Targeted Visual

**Decision**: Targeted visual (only for complex constructs)

**Rationale**:
- ✅ Focus effort where visual editing provides most value
- ✅ Simple constructs stay simple (text)
- ✅ Don't force visual editing where it's not helpful
- ✅ Incremental adoption (add editors as needed)
- ⚠️ Users need to switch between text and visual

**Why This Wins**: Visual editing has cognitive overhead. Use it only where the benefit justifies the complexity. Simple field definitions are fine as text.

### Trade-off 5: Immediate Visual Editors vs Later

**Decision**: Compiler first (Phase 1-2), visual editors later (Phase 3-4)

**Rationale**:
- ✅ Prove the DSL works before building editors
- ✅ Understand what needs visual editing through usage
- ✅ Get working code generation sooner
- ✅ Validate text-based workflow is acceptable
- ⚠️ Early adopters won't have visual tools

**Why This Wins**: Building visual editors before proving the compiler works is risky. Get the foundation right, then add visual enhancements based on real usage patterns.

### Trade-off 6: LSP Server vs Tree-sitter

**Decision**: Both (tree-sitter for syntax, LSP for semantic features)

**Rationale**:
- ✅ tree-sitter: Fast incremental parsing for syntax highlighting
- ✅ LSP: Rich semantic features (autocomplete, validation, etc.)
- ✅ Different strengths complement each other
- ⚠️ Two parsers to maintain
- ⚠️ Need to keep them in sync with grammar

**Why This Wins**: tree-sitter is vastly superior for syntax highlighting and incremental parsing. LSP provides semantic features that tree-sitter can't. Using both gives best editor experience.

---

## Summary

### Final Architecture Decision

**Hybrid ANTLR4 + Web-Based Visual Editors**

This architecture:
1. ✅ Preserves text-based simplicity and version control friendliness
2. ✅ Adds MPS-like visual editing where it provides most value
3. ✅ Gives users choice (text OR visual editing)
4. ✅ Keeps tooling lightweight (no forced MPS installation)
5. ✅ Allows incremental adoption (start with compiler, add editors)
6. ✅ Maintains full control over code generation pipeline

### Why This Is The Right Decision

**For Business Users**:
- Can edit simple DSL with any text editor
- Can use visual tools for complex logic (decision tables)
- No forced tooling or heavy IDE installation
- Familiar text files work with existing workflows

**For Developers**:
- Full control over compilation and code generation
- Text files work perfectly with git, CI/CD, code review
- Professional IDE support via LSP and tree-sitter
- Can contribute without learning MPS

**For the Project**:
- Pragmatic phased approach (working compiler first)
- Incremental value delivery (compiler → LSP → visual editors)
- Technology de-risking (prove ANTLR4 works before visual)
- Community-friendly (open source web editors, no proprietary MPS)

### Next Steps

1. **Complete Phase 1** (ANTLR4 compiler foundation) - 75% remaining
2. **Validate with real use cases** - Ensure text-based DSL works
3. **Implement Phase 2** (LSP + tree-sitter for great DX)
4. **Build Phase 3** (visual editors for high-value constructs)

The foundation is 25% complete. The path forward is clear.

---

**Document Status**: Living document, updated as implementation progresses
**Last Updated**: 2025-11-11
**Next Review**: After Phase 1 completion
