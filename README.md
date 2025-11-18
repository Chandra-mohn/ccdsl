# Financial Services Domain-Specific Language (FS-DSL)

**Architecture**: Modular (Core + Domain DSLs)
**Status**: Production-Ready Specification

---

## Overview

A **strongly opinionated** modular DSL platform for financial services. The platform separates technical infrastructure (Core DSL) from business domain knowledge (Domain DSLs), enabling reusable patterns across multiple financial domains.

**Architecture**:
```
Core DSL (Technical Foundation)
├── Entities, Workflows, Rules, Decision Tables
├── Pipeline Operators, Error Handling
└── Type System, Constraints

Domain DSLs (Business Knowledge)
├── Credit Card Domain
├── Lending Domain
├── Payments Domain
└── Billing Domain (and more...)
```

**Key Benefits**:
- ✅ **Reusable Core**: Technical constructs shared across all domains
- ✅ **Domain-Focused**: Each domain DSL contains only business knowledge
- ✅ **Independent Evolution**: Core and domains version separately
- ✅ **Extensibility**: Add new domains without changing core
- ✅ **Type Safe**: Strong typing with compile-time validation
- ✅ **Multi-Target**: Generates Python, Java, and Rust code

---

## Architecture

### Core DSL (Foundation Layer)

**Purpose**: Provides domain-agnostic technical constructs

**Modules**:
- `core.entity` - Entity definitions with 9 data mutation patterns
- `core.workflow` - Workflow orchestration and pipeline operators
- `core.rules` - Business rules and decision tables
- `core.parameter` - Runtime configuration (PCF)
- `core.reference` - Reference data and lookup tables
- `core.api` - API definitions (planned)
- `core.ui` - UI definitions (planned)

**See**: [core/CORE_DSL_SPECIFICATION.md](core/CORE_DSL_SPECIFICATION.md)

---

### Domain DSLs (Business Layer)

**Purpose**: Business domain knowledge built on core constructs

**Available Domains**:
- **Credit Card** - Card accounts, payments, fees, fraud detection
- **Lending** - Loan origination, credit assessment, servicing (planned)
- **Payments** - Payment processing, settlement (planned)
- **Billing** - Invoice generation, payment collection (planned)

**Example - Credit Card Domain**:
```
module: domain.credit_card
version: 1.0.0

imports:
  - core.entity >= 2.0.0
  - core.workflow >= 2.0.0
  - core.rules >= 2.0.0

define entity: card_account
  pattern: master_data  // from core.entity
  business_domain: "Credit Card"

  // credit card specific fields and rules
```

**See**: [domains/CREDIT_CARD_DOMAIN.md](domains/CREDIT_CARD_DOMAIN.md)

---

## Documentation

### Core Specifications

1. **[core/CORE_DSL_SPECIFICATION.md](core/CORE_DSL_SPECIFICATION.md)**
   - Complete core technical constructs
   - 9 data mutation patterns
   - Pipeline operators
   - Decision tables
   - Type system and constraints

2. **[MODULE_SYSTEM.md](MODULE_SYSTEM.md)**
   - Module declaration syntax
   - Import/export system
   - Versioning and dependencies
   - Module registry

3. **[DSL_GRAMMAR.ebnf](DSL_GRAMMAR.ebnf)**
   - Formal EBNF grammar
   - Module system syntax
   - Complete language specification

---

### Domain Specifications

4. **[domains/CREDIT_CARD_DOMAIN.md](domains/CREDIT_CARD_DOMAIN.md)**
   - Credit card entities, workflows, rules
   - BIAN service domain mapping
   - Late fees, fraud detection, approvals
   - Complete domain examples

---

### Reference Documentation

5. **[DSL_SAMPLES.md](DSL_SAMPLES.md)**
   - 30+ complete working examples
   - All patterns demonstrated
   - Workflow and rule examples

6. **[DSL_DECISION_TABLES.md](DSL_DECISION_TABLES.md)**
   - Decision table specification
   - All condition and action types
   - Integration patterns

7. **[ARCHITECTURE_DOM_ANALYSIS.md](ARCHITECTURE_DOM_ANALYSIS.md)**
   - DOM-based execution model
   - Architecture rationale

---

## Quick Start

### 1. Learn Core Concepts (10 minutes)

**Module Structure**:
```
// Core module provides technical constructs
module: core.entity
version: 2.0.0

exports:
  patterns: [master_data, immutable_ledger, ...]

// Domain module uses core constructs
module: domain.credit_card
version: 1.0.0

imports:
  - core.entity >= 2.0.0

define entity: customer
  pattern: master_data  // from core
```

**9 Data Mutation Patterns**:
1. **master_data** - Mutable, audited (customers, accounts)
2. **immutable_ledger** - Append-only (transactions, payments)
3. **versioned_configuration** - Point-in-time (fee schedules, rates)
4. **operational_parameters** - Hot-reloadable (feature flags, limits)
5. **event_log** - Immutable events (audit logs)
6. **state_machine** - Valid transitions (order status, workflows)
7. **temporal_data** - Bi-temporal (price history, rate changes)
8. **reference_data** - Read-only cached (country codes, currencies)
9. **business_logic** - Compiled rules (calculations, validations)

---

### 2. Understand Pipeline Syntax (5 minutes)

**Data Transformation**:
```
define workflow: process_payments
  step: handle
    actions:
      - payments
          | filter: status = "pending"
          | map: validate_payment
          | foreach: process_transaction
          | sum: total_amount
```

**Pipeline Operators**:
- Filtering: `filter`, `filter_not`
- Transformation: `map`, `map_values`
- Iteration: `foreach`, `foreach_parallel`
- Aggregation: `count`, `sum`, `avg`, `min`, `max`
- Joining: `join`, `left_join`
- Grouping: `group_by`, `sort_by`

---

### 3. Explore Decision Tables (5 minutes)

**Business Rules**:
```
define rules: late_fee_calculator
  pattern: business_logic

  decision_table: calculate_fee
    given:
      - account_type: text
      - days_late: number

    decide:
      | account_type | days_late | → late_fee   |
      |--------------|-----------|--------------|
      | premier      | 1-7       | 15.00 USD    |
      | premier      | 8-30      | 25.00 USD    |
      | standard     | 1-7       | 35.00 USD    |
      | standard     | 8-30      | 45.00 USD    |
      | *            | *         | 50.00 USD    |

    return:
      - late_fee: money
```

**Condition Types**: Exact match, ranges, expressions, functions, IN/NOT IN, wildcards
**Action Types**: Literal values, expressions, function calls, executable actions

---

## Design Principles

### 1. Modular Architecture
- Core DSL: Purely technical, domain-agnostic
- Domain DSLs: Pure business knowledge
- Clean separation of concerns

### 2. Strongly Opinionated
- Enforces financial services best practices
- Pattern-driven development
- One way to do things (no style debates)

### 3. Business-Friendly
- Reads like documentation
- Natural language keywords
- Visual decision tables

### 4. Type Safe
- Strong typing throughout
- Compile-time validation
- Zero runtime type errors

### 5. Multi-Domain Support
- BIAN service domain alignment (322+ domains)
- Reusable core across domains
- Domain-specific validation

---

## BIAN Integration

The DSL aligns with Banking Industry Architecture Network (BIAN) standards, supporting 322+ service domains:

**Domain Mapping**:
```
module: domain.credit_card

bian_domains:
  - "Customer Agreement (11)"
  - "Account Management (17)"
  - "Payment Execution (141)"
  - "Fee & Commission Management"
  - "Credit Risk Operations"
  - "Fraud Detection"
```

---

## Technology Stack

**Compiler**: Rust
- Parser and code generator implemented in Rust
- High-performance compilation
- Memory safety and reliability

**Code Generation Targets** (priority order):
1. **Python** - Primary target for rapid deployment and ML integration
2. **Java** - Enterprise integration and legacy system compatibility
3. **Rust** - High-performance scenarios and system-level applications

**Execution Model**: DOM-based
- Transactional integrity
- Automatic audit trails
- Execution isolation

---

## Module Development

### Creating a New Domain DSL

1. **Declare Module**:
```
module: domain.my_domain
version: 1.0.0
stability: beta

imports:
  - core.entity >= 2.0.0
  - core.workflow >= 2.0.0
  - core.rules >= 2.0.0
```

2. **Define Domain Entities**:
```
define entity: domain_specific_entity
  pattern: master_data  // from core
  business_domain: "My Domain"

  // domain-specific fields
```

3. **Add Domain Workflows**:
```
define workflow: domain_workflow
  // use core pipeline operators
  actions:
    - data | filter: condition | map: transform
```

4. **Specify Domain Rules**:
```
define rules: domain_rules
  decision_table: domain_logic
    // use core decision table syntax
```

---

## File Organization

```
fs-dsl/
├── README.md                           # This file
│
├── Core DSL (Foundation)
│   ├── core/
│   │   └── CORE_DSL_SPECIFICATION.md   # Core constructs
│   └── MODULE_SYSTEM.md                # Module system spec
│
├── Domain DSLs (Business)
│   └── domains/
│       └── CREDIT_CARD_DOMAIN.md       # Credit card domain
│
├── Grammar & References
│   ├── DSL_GRAMMAR.ebnf                # Complete formal grammar
│   ├── DSL_SAMPLES.md                  # Working examples
│   ├── DSL_DECISION_TABLES.md          # Decision tables spec
│   └── ARCHITECTURE_DOM_ANALYSIS.md    # Execution model
```

---

## Next Steps

### For Business Users
1. Review [domains/CREDIT_CARD_DOMAIN.md](domains/CREDIT_CARD_DOMAIN.md)
2. Study decision table examples
3. Assess readability vs current code

### For Language Users
1. Read [core/CORE_DSL_SPECIFICATION.md](core/CORE_DSL_SPECIFICATION.md)
2. Study [DSL_SAMPLES.md](DSL_SAMPLES.md) examples
3. Try adapting examples to your domain

### For Domain Experts
1. Review [MODULE_SYSTEM.md](MODULE_SYSTEM.md)
2. Study [domains/CREDIT_CARD_DOMAIN.md](domains/CREDIT_CARD_DOMAIN.md) structure
3. Design your domain DSL

### For Compiler Implementers
1. Study [DSL_GRAMMAR.ebnf](DSL_GRAMMAR.ebnf)
2. Review [MODULE_SYSTEM.md](MODULE_SYSTEM.md) for dependency resolution
3. Plan module compilation strategy

---

## Contributing

### Core DSL
- Technical constructs only
- Must be domain-agnostic
- Requires backward compatibility plan

### Domain DSLs
- Business knowledge only
- Must import from core
- Can be domain-specific

---

**Total Documentation**: ~27,000 words
**Core Modules**: 7 (5 available, 2 planned)
**Domain Modules**: 1 complete (credit card), 3 planned
**Supported BIAN Domains**: 322+
**Target Languages**: Python, Java, Rust
**Grammar Coverage**: 100% of all features
