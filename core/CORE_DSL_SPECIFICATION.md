# Core DSL Specification

**Layer**: Foundation / Technical Infrastructure
**Purpose**: Provides reusable technical constructs for all financial domain DSLs
**Status**: Production Specification
**Version**: 3.0.0

---

## Table of Contents

### I. OVERVIEW & INTRODUCTION
1. [Executive Summary](#executive-summary)
2. [Design Philosophy](#design-philosophy)
3. [Module System](#module-system)

### II. LANGUAGE REFERENCE
4. [Naming Conventions](#naming-conventions)
5. [Type System](#type-system)
6. [Syntax Rules](#syntax-rules)
7. [Keywords Reference](#keywords-reference)

### III. CORE CONCEPTS
8. [Data Mutation Patterns](#data-mutation-patterns)
9. [Execution Context (DOM)](#execution-context-dom)
10. [Infrastructure Abstraction](#infrastructure-abstraction)

### IV. LANGUAGE CONSTRUCTS
11. [Entity Definitions](#entity-definitions)
12. [Workflow Definitions](#workflow-definitions)
13. [Rule Definitions](#rule-definitions)
14. [Parameter Definitions (PCF)](#parameter-definitions-pcf)
15. [Reference Data](#reference-data)

### V. ADVANCED FEATURES
16. [Pattern Composition](#pattern-composition)
17. [State Machines](#state-machines)
18. [Temporal Data](#temporal-data)

### VI. APPENDICES
19. [Complete Keyword List](#complete-keyword-list)
20. [Compiler Directives](#compiler-directives)

---

# I. OVERVIEW & INTRODUCTION

## Executive Summary

The Core DSL is a **business-friendly, domain-specific language** for defining financial services processing logic with:

- **Pattern-based data mutation** (hide implementation complexity)
- **Rule-based business logic** (compiled to high-performance code)
- **Workflow orchestration** (process automation)
- **Type safety** (compile-time validation)
- **Audit-ready** (automatic tracking and compliance)
- **Modular architecture** (domain-agnostic foundation)

### Key Innovations

1. **Pattern-Driven Development**: Declare pattern, compiler generates operations
2. **Business Readability**: Natural language syntax, readable by non-programmers
3. **Complete Consistency**: One naming rule (snake_case) for everything
4. **Transparent DOM**: Execution context hidden from DSL, exposed in implementation
5. **Pipeline Syntax (v3.0)**: Map-filter-reduce operators for data transformations
6. **Implicit Infrastructure (v3.0)**: Storage and error handling abstracted automatically

### What Gets Generated

From DSL specifications, the compiler generates:

- **Database schemas** (with history tables, constraints, indexes)
- **Rust code** (business rules, validations, workflows)
- **REST APIs** (CRUD operations, workflow triggers)
- **Audit infrastructure** (change tracking, event logs)
- **Documentation** (OpenAPI specs, entity-relationship diagrams)

---

## Design Philosophy

### Core Principles

**1. Business Intent Over Implementation**
```
DSL describes WHAT, compiler handles HOW
User writes: "calculate fee"
Compiler generates: SQL, Rust, API endpoints, audit logs
```

**2. Consistency Over Distinction**
```
One rule: snake_case for all user-defined identifiers
No special cases, no exceptions
```

**3. Explicitness Over Cleverness**
```
Pattern declarations make behavior obvious
No magic, no hidden complexity
```

**4. Safety Over Speed**
```
Compile-time type checking
Constraint validation
Immutability enforcement
```

**5. Standards Over Invention**
```
Industry terminology alignment
Best practices built-in
Regulatory compliance
```

### Design Decisions

| Decision | Rationale |
|----------|-----------|
| **snake_case everywhere** | Complete consistency, Python/YAML style |
| **yes/no for booleans** | More natural than true/false |
| **amount currency format** | Readable, unambiguous (e.g., 100.00 USD) |
| **Pattern-first** | Implementation hidden from users |
| **Indentation-based** | Clean, familiar (YAML/Python) |
| **Lowercase keywords** | Language constructs distinct from user identifiers |
| **DOM transparent** | Business logic clean, execution robust |

---

## Module System

### Module Declaration

Every Core DSL module must declare its identity and version:

```
module: core.entity
version: 2.0.0
stability: stable
```

### Versioning

Core modules follow semantic versioning (MAJOR.MINOR.PATCH):
- **MAJOR**: Breaking changes to syntax or semantics
- **MINOR**: New features, backward compatible
- **PATCH**: Bug fixes, clarifications

### Core Modules

1. **core.entity** - Entity definitions with mutation patterns
2. **core.workflow** - Workflow orchestration and process flows
3. **core.rules** - Business logic and decision structures
4. **core.parameter** - Runtime configuration parameters (PCF)
5. **core.reference** - Reference data and lookup tables

### Future Modules

6. **core.api** - API definitions (REST, GraphQL)
7. **core.ui** - User interface components

### Usage by Domain DSLs

Domain DSLs import core modules:

```
module: domain.payments
version: 1.0.0

imports:
  - core.entity >= 2.0.0
  - core.workflow >= 2.0.0
  - core.rules >= 2.0.0

// Use core constructs with domain-specific knowledge
define entity: payment_account
  pattern: master_data  // from core.entity
  // domain-specific fields
```

---

# II. LANGUAGE REFERENCE

## Naming Conventions

### THE ONE RULE

```
┌────────────────────────────────────────────┐
│  ALL user-defined identifiers: snake_case │
│  NO exceptions. NO special cases.         │
└────────────────────────────────────────────┘
```

### Examples

```
# Entities
customer                    # ✓
payment_account             # ✓
fee_schedule                # ✓

# Fields
customer_id                 # ✓
payment_due_date            # ✓
on_time_percentage          # ✓

# Enum Values
premier                     # ✓
late_fee                    # ✓
active                      # ✓

# Workflows
payment_processing          # ✓
fee_assessment              # ✓

# Rules
calculate_fee_amount        # ✓
evaluate_waiver             # ✓

# Parameters
grace_period_days           # ✓
auto_waiver_limit           # ✓
```

### What's Wrong

```
Customer                    # ✗ PascalCase
paymentAccount              # ✗ camelCase
LATE_FEE                    # ✗ SCREAMING_SNAKE_CASE
payment-processing          # ✗ kebab-case
```

---

## Type System

### Built-in Types (lowercase)

| Type | Description | Example Values |
|------|-------------|----------------|
| `text` | String values | "John Doe", "ABC123" |
| `number` | Integer or decimal | 42, 3.14, -100 |
| `money` | Money amounts | 25.00 USD, 1000.50 EUR |
| `date` | Calendar dates | 2024-01-15 |
| `timestamp` | Date + time | 2024-01-15T10:30:00Z |
| `boolean` | yes/no values | yes, no |
| `email` | Email addresses | user@example.com |
| `phone` | Phone numbers | +1-555-0123 |
| `duration` | Time periods | 24 hours, 30 days |
| `time` | Time of day | 14:30:00 |
| `percentage` | Percentage values | 95, 12.5 |

### Custom Types

- **Entity references**: Use entity name (snake_case)
  ```
  belongs_to: customer        # References customer entity
  uses: product_config        # References product_config entity
  ```

- **Enum values**: Use snake_case literals
  ```
  customer_segment: text, values: premier | preferred | standard
  account_status: text, values: active | suspended | closed
  ```

### Type Qualifiers

```
field_name: type, qualifier1, qualifier2, ...
```

**Available Qualifiers**:

| Qualifier | Meaning | Example |
|-----------|---------|---------|
| `unique` | Must be unique | customer_id: text, unique |
| `required` | Cannot be null | email: email, required |
| `optional` | Can be null (default) | phone: phone, optional |
| `cannot_change` | Immutable after creation | ssn: text, cannot_change |
| `encrypted` | Stored encrypted | password: text, encrypted |
| `default X` | Default value | status: text, default active |
| `between X and Y` | Range constraint | age: number, between 18 and 120 |
| `values: A \| B` | Enum constraint | tier: text, values: gold \| silver |

### Type Inference

The compiler infers specific types from context:

```
balance: money                    # Money type
days: number                      # Could be integer
percentage: number, between 0 and 100  # Decimal with constraint
due_date: date                    # Date type
created_at: timestamp, default now     # Timestamp with function
```

---

## Syntax Rules

### Indentation (Strict)

- **2 spaces per level**, strictly enforced
- NO tabs
- NO mixing spaces and tabs

```
define entity: customer         # 0 spaces
  pattern: master_data          # 2 spaces
  identity:                     # 2 spaces
    customer_id: text           # 4 spaces
  profile:                      # 2 spaces
    name: text                  # 4 spaces
```

### Comments

```
# Single-line comment
field_name: text  # Inline comment

#==============================================================================
# Major Section Header
#==============================================================================

#------------------------------------------------------------------------------
# Minor Section Header
#------------------------------------------------------------------------------
```

### String Literals

```
description: "This is a text value"      # ✓ Double quotes for strings
message: "Fee assessed"                  # ✓

# No quotes for identifiers
belongs_to: customer                      # ✓ Identifier, no quotes
belongs_to: "customer"                    # ✗ Wrong
```

### Blank Lines

- One blank line between major declarations (entities, workflows)
- NO blank lines within definitions
- NO trailing whitespace

```
define entity: customer
  pattern: master_data
  identity:
    customer_id: text
                                # ← Blank line between entities
define entity: account
  pattern: master_data
```

### Line Length

- Maximum 100 characters per line
- Break long lines at logical boundaries

```
# ✓ Correct
must:
  - credit_score >= 300 and credit_score <= 850
  - customer_segment is one of premier | preferred | standard

# ✗ Wrong - too long
must:
  - credit_score >= 300 and credit_score <= 850 and customer_segment is one of premier | preferred | standard and account_status == active
```

---

## Keywords Reference

### Declaration Keywords

```
define          # Start entity, workflow, rule, or parameter declaration
entity          # Entity declaration
workflow        # Workflow declaration
rules           # Rules declaration
parameters      # Parameters declaration
reference       # Reference data declaration
```

### Entity Keywords

```
pattern         # Pattern declaration
description     # Human-readable description
identity        # Identity fields section
references      # Foreign key relationships
relationships   # One-to-many relationships
must            # Constraints section
cannot          # Prohibitions section
operations      # Allowed operations (for patterns)
```

### Relationship Keywords

```
belongs_to      # Many-to-one relationship (foreign key)
has_many        # One-to-many relationship
uses            # Configuration reference
```

### Workflow Keywords

```
triggered_by    # Workflow triggers
inputs          # Workflow inputs
outputs         # Workflow outputs
step            # Workflow step
actions         # Step actions
next            # Next step logic
goto            # Unconditional transition
when            # Conditional logic
otherwise       # Default condition
return          # Return values
```

### Rule Keywords

```
rule            # Rule declaration
given           # Rule inputs
calculate       # Calculation logic
evaluate        # Evaluation logic
when            # Conditional
then            # Consequent
otherwise       # Default case
return          # Rule outputs
examples        # Test cases
```

### Parameter Keywords

```
parameter       # Parameter declaration
type            # Parameter type
current_value   # Current value
default_value   # Default value
range           # Value range
can_schedule    # Can be scheduled
change_frequency # Change frequency
```

### Constraint Keywords

```
must            # Required constraint
cannot          # Prohibition
between         # Range constraint
values          # Enum constraint
is              # Identity check
and             # Logical AND
or              # Logical OR
not             # Logical NOT
```

### Special Values

```
yes             # Boolean true
no              # Boolean false
null            # Null/None value
now             # Current timestamp function
today           # Current date function
```

---

# III. CORE CONCEPTS

## Data Mutation Patterns

### Overview

**Patterns hide implementation complexity** - declare the pattern, compiler generates everything.

### The 9 Patterns

#### 1. master_data (Slowly Changing Dimension)

**Use Case**: Customer profiles, account details, entity master data

**Characteristics**:
- Mutable with full history tracking (SCD Type 2)
- Soft delete only (never hard delete)
- Audit trail for all changes

**Auto-Generated**:
- entity table
- entity_history table (SCD Type 2)
- add(), update(), soft_delete(), retrieve(), get_history()
- Audit triggers

**Example**:
```
define entity: customer
  pattern: master_data

  identity:
    customer_id: text, unique

  profile:
    name: text
    email: email
```

**Generated Tables**:
```sql
CREATE TABLE customer (
  customer_id VARCHAR(100) PRIMARY KEY,
  name VARCHAR(255),
  email VARCHAR(255),
  valid_from TIMESTAMP NOT NULL,
  valid_to TIMESTAMP,
  is_current BOOLEAN DEFAULT TRUE,
  deleted_at TIMESTAMP
);

CREATE TABLE customer_history (
  history_id SERIAL PRIMARY KEY,
  customer_id VARCHAR(100),
  name VARCHAR(255),
  email VARCHAR(255),
  valid_from TIMESTAMP,
  valid_to TIMESTAMP,
  change_type VARCHAR(20),
  changed_by VARCHAR(100),
  changed_at TIMESTAMP
);
```

---

#### 2. immutable_ledger (Append-Only Financial Records)

**Use Case**: Transactions, fees, payments - immutable financial data

**Characteristics**:
- Append-only (no updates or deletes)
- Immutability enforced at database level
- Compensating transactions for corrections (reversals)

**Auto-Generated**:
- entity table (with immutability constraints)
- add(), retrieve() ONLY (no update/delete)
- Reversal support

**Example**:
```
define entity: transaction
  pattern: immutable_ledger

  identity:
    transaction_id: text, unique

  transaction_details:
    amount: money, cannot_change
    transaction_date: date, cannot_change
```

**Generated Constraints**:
```sql
CREATE TABLE transaction (
  transaction_id VARCHAR(100) PRIMARY KEY,
  amount DECIMAL(15,2) NOT NULL,
  transaction_date DATE NOT NULL,
  created_at TIMESTAMP DEFAULT NOW(),
  -- Triggers prevent UPDATE/DELETE
);
```

---

#### 3. versioned_configuration (Product Configurations)

**Use Case**: Product definitions, fee schedules, pricing structures

**Characteristics**:
- Immutable versions
- Effective dating support
- Version history

**Auto-Generated**:
- entity table with version_key
- get_version(), get_active(), get_history()
- Version migration support

**Example**:
```
define entity: product_config
  pattern: versioned_configuration

  identity:
    product_code: text, unique
    product_version: number

  effective_dating:
    effective_from: date
```

---

#### 4. operational_parameters (Hot-Reloadable PCF)

**Use Case**: Grace periods, thresholds, limits - operational tuning

**Characteristics**:
- Hot-reloadable (no code deployment)
- Effective dating support
- Monthly change frequency typical
- Timeline history

**Auto-Generated**:
- parameter table
- set_parameter(), get_current(), set_effective()
- Hot reload API
- Change audit trail

**Example**:
```
define parameters: fee_settings
  pattern: operational_parameters

  parameter: grace_period_days
    type: number
    current_value: 15
    can_schedule: yes
```

---

#### 5. event_log (Append-Only Event Stream)

**Use Case**: Audit events, calculation events, system events

**Characteristics**:
- Append-only
- Time-based retention
- High-performance logging

**Auto-Generated**:
- event table
- append(), retrieve(), stream(), count()
- Retention policies

**Example**:
```
define entity: calculation_event
  pattern: event_log

  event_details:
    event_timestamp: timestamp, default now
    event_type: text
    event_data: text
```

---

#### 6. state_machine (Workflow State Tracking)

**Use Case**: Status tracking, application workflow, process states

**Characteristics**:
- Valid state transitions only
- State change tracking
- Transition actions

**Auto-Generated**:
- state table
- state transition log
- transition_to(), get_current_state(), get_history()
- Validation for invalid transitions

**Example**:
```
define entity: process_state
  pattern: state_machine

  current_state: text, values: pending | approved | rejected

  initial_state: pending

  transitions:
    from pending: approved, rejected
    from approved: none
    from rejected: none
```

---

#### 7. temporal_data (Effective-Dated Values)

**Use Case**: Interest rates, limits - values that change over time

**Characteristics**:
- Point-in-time queries
- Future effective dates
- Timeline tracking

**Auto-Generated**:
- temporal table
- set_effective(), get_at_date(), get_current(), get_timeline()

**Example**:
```
define entity: interest_rate
  pattern: temporal_data

  for_account: account
  rate_percentage: percentage
  effective_from: date
  effective_until: date, optional
```

---

#### 8. reference_data (Lookup Tables)

**Use Case**: Fee types, customer segments, category codes

**Characteristics**:
- Shared reference data
- Referential integrity
- Deprecation (not deletion)

**Auto-Generated**:
- reference table
- register(), update(), deprecate(), retrieve()
- Usage tracking

**Example**:
```
define reference_data: fee_types
  pattern: reference_data

  entries:
    - code: "late_fee", name: "Late Payment Fee"
    - code: "annual_fee", name: "Annual Membership Fee"
```

---

#### 9. business_logic (Compiled Rules - Code Generation)

**Use Case**: Fee calculation, eligibility rules, validation logic

**Characteristics**:
- Compiled to Rust code (not runtime interpretation)
- Zero-cost abstractions
- Compile-time type safety
- Code deployment required for changes

**Auto-Generated**:
- Rust functions
- Test case validation
- Type-checked at compile time

**Example**:
```
define rules: fee_calculation
  pattern: business_logic

  rule: calculate_fee_amount
    given:
      - balance: money

    calculate:
      fee = when balance >= 5000.00 USD: 40.00 USD
            when balance >= 1000.00 USD: 35.00 USD
            otherwise: 25.00 USD

    return:
      - fee_amount: money
```

**Generated Rust**:
```rust
pub fn calculate_fee_amount(
  balance: Money
) -> Money {
  if balance >= Money::new(5000, 0, "USD") {
    Money::new(40, 0, "USD")
  } else if balance >= Money::new(1000, 0, "USD") {
    Money::new(35, 0, "USD")
  } else {
    Money::new(25, 0, "USD")
  }
}
```

---

## Execution Context (DOM)

### Architecture Overview

All workflow/rule executions operate on a **DOM-like execution context**:

```
┌─────────────────────────────────────┐
│     Execution Context (DOM)         │
├─────────────────────────────────────┤
│  - Data storage (entities, values)  │
│  - Mutation log (audit trail)       │
│  - Transaction state                │
│  - Cache (performance)              │
└─────────────────────────────────────┘
         ↑                    ↓
    get(path)            put(path, value)
         ↑                    ↓
┌─────────────────────────────────────┐
│     Workflow / Rule Code            │
└─────────────────────────────────────┘
```

### Key Principles

1. **Created at execution start**: Each workflow gets fresh DOM
2. **All data access via DOM**: No global state, no side effects
3. **Isolated executions**: Thread-safe, no interference
4. **Transactional**: Commit all changes or rollback
5. **Transparent in DSL**: Implementation detail, hidden from users

### Benefits

**Transactional Integrity**:
```rust
let mut context = ExecutionContext::new();
context.put("account", account);

workflow.execute(&mut context)?;

context.commit()?;  // All changes or none
```

**Automatic Audit**:
```
Every get/put logged automatically:
- context.get("account.balance") → logged
- context.put("account.balance", 1500) → logged with old/new values
```

**Testability**:
```rust
let test_context = MockExecutionContext::new();
test_context.put("account", test_account);

let result = workflow.execute(&mut test_context)?;

assert_eq!(test_context.get("fee_amount"), expected_fee);
```

### DSL Transparency

**You write** (DSL):
```
step: calculate_fee
  actions:
    - load account
    - calculate fee using rule
    - update account.balance
```

**Compiler generates** (with DOM):
```rust
let account = context.get::<Account>("account")?;
let fee = calculate_fee(&context)?;
let mut account = context.get_mut("account")?;
account.balance -= fee;
context.put("account", account)?;
```

**You don't see DOM in DSL** - it's an implementation detail that provides robust execution.

---

## Infrastructure Abstraction

### Design Philosophy (v3.0)

**Business users focus on WHAT, not HOW**:
- DSL describes data transformations and business logic
- Compiler handles storage, transactions, error handling, resource management
- Infrastructure concerns configured globally, not per-workflow

### Automatic Infrastructure Management

**Storage Abstraction**:
- Entities accessed by name (e.g., `customers`, `accounts`)
- Storage implementation transparent (files, database, API, cache)
- Compiler selects optimal storage based on entity pattern:
  - `master_data` → Relational database (ACID transactions)
  - `immutable_ledger` → Append-only log (audit trail)
  - `operational_parameters` → Key-value store (fast lookup)
  - `reference_data` → In-memory cache (read-heavy)
  - `event_log` → Message queue (streaming)

**Resource Management**:
- File handles, database connections, network sockets managed automatically
- RAII pattern (Rust) ensures cleanup even on errors
- Connection pooling, caching, and optimization transparent

**Transaction Boundaries**:
- Workflow steps execute within transactions automatically
- Commit on success, rollback on error
- Nested transactions supported for complex workflows

**Error Handling**:
- Infrastructure errors (file not found, network timeout) handled automatically
- Default strategies configurable globally
- Business-specific error handling via explicit overrides (optional)

### Error Handling Configuration

**Global Configuration** (dsl.config):
```
configure error_handling:

  validation_errors:
    default_strategy: skip_and_log
    log_level: warning
    continue_processing: true
    alert_threshold: 100

  infrastructure_errors:
    default_strategy: retry_then_fail
    max_retries: 3
    backoff_strategy: exponential
    base_delay_ms: 1000
    alert_on_failure: operations_team
    circuit_breaker: true

  business_logic_errors:
    default_strategy: rollback_and_alert
    alert_recipients: business_analysts
    create_incident: true

  system_errors:
    default_strategy: rollback_and_fail
    log_level: critical
    alert_recipients: on_call_engineer
    capture_stack_trace: true
```

**Default Error Strategies**:

| Error Type | Default Strategy | Behavior |
|------------|------------------|----------|
| Infrastructure | `retry_then_fail` | 3 retries with exponential backoff, then fail + alert ops |
| Validation | `skip_and_log` | Skip invalid record, log warning, continue processing |
| Business Logic | `rollback_and_alert` | Rollback transaction, alert business analysts |
| System | `rollback_and_fail` | Rollback, log critical error, fail workflow |

---

# IV. LANGUAGE CONSTRUCTS

## Entity Definitions

### Structure

```
define entity: entity_name
  pattern: pattern_name
  description: "Entity purpose"

  identity:
    primary_key_field: type, unique, required

  references:
    belongs_to: other_entity
    uses: configuration_entity

  field_group_name:
    field1: type, qualifiers
    field2: type, qualifiers

  relationships:
    has_many: related_entities

  must:
    - constraint 1
    - constraint 2
```

### Section Ordering (Required)

1. `pattern:` (REQUIRED - must be first)
2. `description:` (optional)
3. `identity:` (for entities with primary key)
4. `references:` (foreign keys, configuration references)
5. Custom field groups (any name, multiple allowed)
6. `relationships:` (one-to-many relationships)
7. `must:` (constraints)
8. `operations:` (for specific patterns)

### Complete Example

```
define entity: account
  pattern: master_data
  description: "Customer financial account"

  identity:
    account_id: text, unique, required

  references:
    belongs_to: customer
    uses: product_config

  account_details:
    account_number: text, unique, cannot_change
    opened_date: date, cannot_change
    account_status: text, values: active | suspended | closed

  financial_balances:
    current_balance: money, required
    credit_limit: money, required
    available_credit: money, required

  relationships:
    has_many: transactions

  must:
    - credit_limit >= 0.00 USD
    - available_credit <= credit_limit
    - account_number is unique
```

---

## Workflow Definitions

### Structure

```
define workflow: workflow_name
  description: "Workflow purpose"

  triggered_by:
    - trigger 1
    - trigger 2

  inputs:
    - input1: type
    - input2: type

  outputs:
    - output1: type
    - output2: type

  step: step_name_1
    description: "Step purpose"

    actions:
      - action 1
      - action 2

    next:
      when condition: goto step_name_2
      otherwise: goto step_name_3

  step: step_name_2
    actions:
      - action

    return:
      - field1: value1
      - field2: value2
```

### Pipeline Actions (v3.0)

**Pipeline Operators**:

| Operator | Purpose | Example |
|----------|---------|---------|
| `filter: condition` | Keep matching records | `filter: balance > 1000.00 USD` |
| `map: transformation` | Transform each record | `map: calculate_interest` |
| `foreach: action` | Execute action on each | `foreach: post_charge` |
| `reduce: aggregation` | Combine into single value | `reduce: sum` |
| `join: (alias => entity where condition)` | Navigate relationships | `join: (acc => accounts where id = acc.id)` |
| `group_by: field` | Group records | `group_by: customer_id` |
| `order_by: field [asc\|desc]` | Sort records | `order_by: date desc` |
| `take: n` | Limit results | `take: 100` |
| `count: variable` | Count items | `count: total_records` |
| `sum: variable` | Sum numeric field | `sum: total_amount` |
| `avg: variable` | Average of field | `avg: avg_balance` |
| `on_validation_error: strategy` | Override error handling | `on_validation_error: escalate` |

**Example**:
```
actions:
  - customers
      | filter: validate_entity
      | map: calculate_interest
      | foreach: post_interest_charge
      | sum: total_interest

  - log_event("Processing complete", amount: total_interest)
```

### Complete Example

```
define workflow: payment_processing
  description: "Process customer payments"

  triggered_by:
    - payment received from customer
    - daily batch at 03:00 UTC

  inputs:
    - account: account
    - payment_amount: money
    - processing_date: date

  outputs:
    - payment_status: text
    - fee_triggered: boolean

  step: validate_payment
    description: "Validate payment details"

    actions:
      - load parameters from system
      - validate payment_amount > 0.00 USD
      - check account status is active

    next:
      when account.status == active: goto process_payment
      otherwise: goto reject_payment

  step: process_payment
    actions:
      - update account.balance
      - create transaction record

    return:
      - payment_status: completed
      - fee_triggered: no

  step: reject_payment
    return:
      - payment_status: rejected
      - fee_triggered: no
```

---

## Rule Definitions

### Procedural Rule Structure

```
define rules: rule_group_name
  pattern: business_logic
  description: "Rules purpose"

  rule: rule_name
    description: "Rule purpose"

    given:
      - input1: type
      - input2: type

    calculate:
      calculation logic

    return:
      - output1: type
      - output2: type

    examples:
      - given: input values
        then: expected outputs
```

### Decision Table Structure (v3.1)

```
define rules: rule_group_name
  pattern: business_logic
  description: "Rules purpose"

  decision_table: table_name
    description: "Decision logic purpose"

    given:
      - input1: type
      - input2: type

    decide:
      | input1     | input2  | → output1 | output2 |
      |------------|---------|-----------|---------|
      | value1     | value2  | result1   | result2 |
      | condition3 | value4  | result3   | result4 |
      | *          | *       | default1  | default2|

    return:
      - output1: type
      - output2: type
```

### Complete Example

```
define rules: fee_calculation
  pattern: business_logic
  description: "Calculate fees with regulatory compliance"

  rule: calculate_fee_amount
    description: "Tiered fee calculation"

    given:
      - account_balance: money
      - is_repeat_violation: boolean
      - maximum_fee: money

    calculate:
      base_fee =
        when account_balance >= 5000.00 USD: 40.00 USD
        when account_balance >= 1000.00 USD: 35.00 USD
        otherwise: 25.00 USD

      calculated_fee =
        when is_repeat_violation: base_fee
        otherwise: min(base_fee, 30.00 USD)

      final_fee = min(calculated_fee, maximum_fee)

    return:
      - fee_amount: money

    examples:
      - given:
          account_balance: 2500.00 USD
          is_repeat_violation: no
          maximum_fee: 30.00 USD
        then:
          fee_amount: 30.00 USD

      - given:
          account_balance: 6000.00 USD
          is_repeat_violation: yes
          maximum_fee: 40.00 USD
        then:
          fee_amount: 40.00 USD
```

---

## Parameter Definitions (PCF)

### Structure

```
define parameters: parameter_group_name
  pattern: operational_parameters
  environment: PRODUCTION | STAGING | DEVELOPMENT
  description: "Parameters purpose"

  parameter_category:

    parameter: parameter_name
      description: "Parameter purpose"
      type: parameter_type
      current_value: value
      default_value: value
      range: min to max
      can_schedule: yes | no
      change_frequency: frequency
```

### Complete Example

```
define parameters: fee_settings
  pattern: operational_parameters
  environment: PRODUCTION
  description: "Fee calculation parameters"

  grace_period_settings:

    parameter: grace_period_days
      description: "Days after due date before fee applies"
      type: number
      current_value: 15
      default_value: 15
      range: 10 to 30
      can_schedule: yes
      change_frequency: monthly

    parameter: weekend_extension
      description: "Extend grace period if ends on weekend"
      type: boolean
      current_value: yes
      default_value: yes
      can_schedule: no

  fee_limits:

    parameter: auto_waiver_limit
      description: "Fees below this auto-approved if eligible"
      type: money
      current_value: 25.00 USD
      default_value: 25.00 USD
      range: 0.00 USD to 100.00 USD
      can_schedule: yes
```

---

## Reference Data

### Structure

```
define reference: reference_name
  pattern: reference_data

  values:
    enum_value_1:
      name: "Display Name"
      description: "Description"

    enum_value_2:
      name: "Display Name"
      description: "Description"
```

### Example

```
define reference: customer_segment
  pattern: reference_data

  values:
    premier:
      name: "Premier"
      description: "High-value customers with premium benefits"

    preferred:
      name: "Preferred"
      description: "Above-average customers with enhanced benefits"

    standard:
      name: "Standard"
      description: "Standard customers with basic benefits"
```

---

# V. ADVANCED FEATURES

## Pattern Composition

Some entities need **multiple patterns**:

```
define entity: product_config
  pattern: versioned_configuration
  pattern: temporal_data

  # Versioned AND effective-dated
```

**Field-level pattern override**:

```
define entity: account
  pattern: master_data  # Default for entity

  account_details:
    account_number: text, cannot_change  # Immutable override

  state_tracking:
    status: process_state  # State machine pattern
```

---

## State Machines

### Complete Structure

```
define entity: state_entity_name
  pattern: state_machine

  tracks_state_for: entity_name
  current_state: text, values: state1 | state2 | state3
  state_entered_date: date

  initial_state: state1

  transitions:
    from state1: state2
    from state2: state1, state3
    from state3: none

  on_enter_state:
    when state2:
      - action 1
      - action 2

    when state3:
      - action 3
```

### Example

```
define entity: process_state
  pattern: state_machine

  tracks_state_for: account
  current_state: text, values: pending | approved | rejected
  state_entered_date: date

  initial_state: pending

  transitions:
    from pending: approved, rejected
    from approved: none
    from rejected: none

  on_enter_state:
    when approved:
      - send notification with type approval

    when rejected:
      - send notification with type rejection
```

---

## Temporal Data

### Effective-Dated Values

```
define entity: interest_rate
  pattern: temporal_data

  for_account: account
  rate_percentage: percentage
  effective_from: date
  effective_until: date, optional
```

**Auto-generated operations**:
- `set_effective(value, date)`
- `get_at_date(date)`
- `get_current()`
- `get_timeline()`

### Usage in Workflows

```
step: apply_rate
  actions:
    - set interest rate to 12.5 effective 2024-02-01
    - get current interest rate for account
    - get interest rate at date 2024-01-15
```

---

# VI. APPENDICES

## Complete Keyword List

### Declaration Keywords
```
define, entity, workflow, rules, parameters, reference
```

### Entity Keywords
```
pattern, description
identity, references, relationships
must, cannot, operations
belongs_to, has_many, uses
```

### Workflow Keywords
```
triggered_by, inputs, outputs
step, actions, next, goto, when, otherwise, return
```

### Rule Keywords
```
rule, given, calculate, evaluate, when, then, otherwise, return, examples
decision_table, decide
```

### Parameter Keywords
```
parameter, type, current_value, default_value, range, can_schedule, change_frequency
```

### Constraint Keywords
```
must, cannot, between, values, is, and, or, not
```

### Built-in Types
```
text, number, money, date, timestamp, boolean, email, phone, duration, time, percentage
```

### Qualifiers
```
unique, required, optional, cannot_change, encrypted, default
```

### Special Values
```
yes, no, null, now, today
```

---

## Compiler Directives

### Validation

```bash
dsl-compiler validate file.dsl
```

### Code Generation

```bash
dsl-compiler generate --target rust --output ./generated file.dsl
```

### Schema Generation

```bash
dsl-compiler schema --database postgres --output ./schema file.dsl
```

### API Generation

```bash
dsl-compiler api --format openapi --output ./api file.dsl
```

### Verbose Mode

```bash
dsl-compiler compile --verbose file.dsl
dsl-compiler run --verbose file.dsl
```

---

**End of Core DSL Specification**

This specification provides the domain-agnostic technical foundation for all financial services DSLs. Domain-specific knowledge should be implemented in Domain DSL modules that import and extend these core constructs.
