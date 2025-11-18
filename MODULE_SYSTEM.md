# Module System Specification

**Purpose**: Define how Core DSL and Domain DSLs interact through imports and dependencies
**Status**: Production Specification

---

## Overview

The module system enables:
- **Separation of Concerns**: Technical infrastructure (Core) vs business knowledge (Domains)
- **Reusability**: Core constructs shared across all domains
- **Versioning**: Independent evolution of core and domain modules
- **Extensibility**: Third parties can create domain modules

---

## Module Declaration

Every DSL module must declare its identity:

```
module: <module_type>.<module_name>
version: <semantic_version>
stability: stable | beta | alpha

description: "<module_purpose>"
author: "<author_or_organization>"
license: "<license_type>"
```

**Module Types**:
- `core.*` - Technical infrastructure modules (foundation layer)
- `domain.*` - Business domain modules (business layer)

**Examples**:
```
module: core.entity
version: 2.0.0
stability: stable

module: domain.credit_card
version: 1.0.0
stability: stable

module: domain.lending
version: 1.0.0
stability: beta
```

---

## Semantic Versioning

All modules follow semantic versioning (MAJOR.MINOR.PATCH):

- **MAJOR**: Breaking changes (incompatible API changes)
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

**Version Constraints**:
- `>= 2.0.0` - Minimum version 2.0.0
- `>= 2.0.0, < 3.0.0` - Version 2.x only
- `= 2.1.0` - Exact version (not recommended)
- `^2.1.0` - Compatible with 2.1.0 (>= 2.1.0, < 3.0.0)
- `~2.1.0` - Approximately 2.1.0 (>= 2.1.0, < 2.2.0)

---

## Import System

### Core Module Imports

Domain DSLs import core modules they depend on:

```
module: domain.credit_card
version: 1.0.0

imports:
  - core.entity >= 2.0.0
  - core.workflow >= 2.0.0
  - core.rules >= 2.0.0
  - core.parameter >= 2.0.0
  - core.reference >= 2.0.0
```

### Selective Imports

Import specific constructs from core modules:

```
imports:
  - core.entity:
      patterns: [master_data, immutable_ledger, versioned_configuration]
      types: [text, number, money, date]

  - core.workflow:
      operators: [filter, map, foreach, sum]

  - core.rules:
      constructs: [decision_table]
```

### Namespace Imports

Import with namespace prefix:

```
imports:
  - core.entity as entity
  - core.workflow as wf

// Usage:
define entity: customer
  pattern: entity.master_data

define workflow: process_payments
  step: process
    actions:
      - payments | wf.filter: status = "pending"
```

---

## Export System

### Core Module Exports

Core modules explicitly declare what they export:

```
module: core.entity
version: 2.0.0

exports:
  patterns:
    - master_data
    - immutable_ledger
    - versioned_configuration
    - operational_parameters
    - event_log
    - state_machine
    - temporal_data
    - reference_data
    - business_logic

  types:
    - text
    - number
    - money
    - date
    - datetime
    - boolean
    - percentage
    - duration
    - email
    - phone

  constraints:
    - required
    - unique
    - min
    - max
    - pattern
    - values
```

### Domain Module Exports

Domain modules can export reusable domain constructs:

```
module: domain.credit_card
version: 1.0.0

exports:
  entities:
    - customer
    - card_account
    - transaction

  workflows:
    - process_payment
    - assess_late_fees

  rules:
    - late_fee_calculator
    - credit_approval

  parameters:
    - grace_period_days
    - fraud_threshold_amount
```

---

## Dependency Resolution

### Dependency Graph

The compiler builds a dependency graph:

```
domain.credit_card (1.0.0)
├── core.entity (>= 2.0.0) → resolves to 2.1.0
├── core.workflow (>= 2.0.0) → resolves to 2.1.0
├── core.rules (>= 2.0.0) → resolves to 2.1.0
├── core.parameter (>= 2.0.0) → resolves to 2.0.0
└── core.reference (>= 2.0.0) → resolves to 2.0.0
```

### Version Conflict Resolution

**Conflict Example**:
```
domain.credit_card requires core.entity >= 2.0.0
domain.lending requires core.entity >= 2.1.0

Resolution: Use core.entity 2.1.0 (satisfies both)
```

**Incompatible Versions**:
```
domain.credit_card requires core.entity >= 2.0.0, < 3.0.0
domain.lending requires core.entity >= 3.0.0

Error: Cannot resolve core.entity version
```

---

## Module Registry

### Local Registry

Modules are organized in the file system:

```
ccdsl/
├── core/
│   ├── CORE_DSL_SPECIFICATION.md
│   ├── entity.dsl
│   ├── workflow.dsl
│   ├── rules.dsl
│   ├── parameter.dsl
│   └── reference.dsl
│
├── domains/
│   ├── CREDIT_CARD_DOMAIN.md
│   ├── credit_card.dsl
│   ├── lending.dsl
│   ├── payments.dsl
│   └── billing.dsl
│
└── MODULE_REGISTRY.json
```

### Module Registry File

`MODULE_REGISTRY.json`:
```json
{
  "modules": {
    "core.entity": {
      "version": "2.1.0",
      "path": "core/entity.dsl",
      "spec": "core/CORE_DSL_SPECIFICATION.md",
      "stability": "stable"
    },
    "core.workflow": {
      "version": "2.1.0",
      "path": "core/workflow.dsl",
      "spec": "core/CORE_DSL_SPECIFICATION.md",
      "stability": "stable"
    },
    "domain.credit_card": {
      "version": "1.0.0",
      "path": "domains/credit_card.dsl",
      "spec": "domains/CREDIT_CARD_DOMAIN.md",
      "stability": "stable",
      "dependencies": {
        "core.entity": ">=2.0.0",
        "core.workflow": ">=2.0.0",
        "core.rules": ">=2.0.0"
      }
    }
  }
}
```

---

## Compilation Process

### Step 1: Module Discovery

1. Read module declarations from source files
2. Build module dependency graph
3. Detect circular dependencies
4. Validate version constraints

### Step 2: Dependency Resolution

1. Resolve all module versions
2. Check for version conflicts
3. Load modules in dependency order
4. Validate imports match exports

### Step 3: Module Compilation

1. Compile core modules first (in dependency order)
2. Compile domain modules (with core symbols available)
3. Link domain constructs to core constructs
4. Generate target code (Python, Java, Rust)

### Step 4: Validation

1. Type checking across module boundaries
2. Verify all imports are satisfied
3. Check for undefined references
4. Validate business constraints

---

## Module Interoperability

### Cross-Domain References

Domain modules can reference other domain modules:

```
module: domain.banking
version: 1.0.0

imports:
  - core.entity >= 2.0.0
  - domain.credit_card >= 1.0.0

define entity: bank_account
  pattern: master_data

  has_many:
    - credit_card.card_account  // Reference to credit card domain
```

### Shared Core Extensions

Multiple domains can extend the same core module:

```
// Credit Card domain extends core.entity
module: domain.credit_card
exports:
  custom_patterns:
    - reward_points_ledger

// Lending domain also extends core.entity
module: domain.lending
exports:
  custom_patterns:
    - loan_amortization_schedule
```

---

## Backward Compatibility

### Breaking Changes

**MAJOR version changes** that break compatibility:
- Removing exported constructs
- Changing syntax of existing constructs
- Removing or renaming types
- Changing constraint semantics

**Example**:
```
core.entity 2.x.x → 3.0.0

Breaking changes:
- Removed pattern: legacy_data
- Renamed type: money → currency_amount
- Changed constraint: min/max now inclusive (was exclusive)
```

### Deprecation Process

1. **Mark as deprecated** in MINOR version
2. **Provide migration path** in documentation
3. **Remove in next MAJOR** version

**Example**:
```
// core.entity 2.5.0
pattern: legacy_data
  deprecated: yes
  deprecated_since: 2.5.0
  removed_in: 3.0.0
  migration: "Use 'temporal_data' pattern instead"
```

---

## Module Development Guidelines

### Core Module Guidelines

1. **Stability**: Core modules must be highly stable
2. **Generality**: No domain-specific knowledge
3. **Extensibility**: Provide extension points for domains
4. **Documentation**: Comprehensive docs for all exports
5. **Testing**: Extensive test coverage

### Domain Module Guidelines

1. **Focus**: Single business domain only
2. **Dependencies**: Minimize cross-domain dependencies
3. **BIAN Alignment**: Map to appropriate BIAN service domains
4. **Examples**: Provide comprehensive domain examples
5. **Validation**: Domain-specific validation rules

---

## Future Extensions

### Package Manager

Future tool for module distribution:

```bash
# Install a domain module
dsl install domain.lending

# Update core modules
dsl update core.*

# Publish a domain module
dsl publish domain.my_custom_domain
```

### Module Marketplace

Central registry for community-contributed domain modules:
- Versioned module packages
- Dependency management
- Security scanning
- License validation

---

## See Also

- **core/CORE_DSL_SPECIFICATION.md**: Core module specifications
- **domains/**: Domain module specifications
- **DSL_GRAMMAR.ebnf**: Grammar with module support
