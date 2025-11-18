# Decision Tables - Quick Reference (v3.1)

**Feature**: Matrix-based decision/conditional logic
**Status**: Fully Specified, Ready for Implementation
**Target**: v3.1 (after v3.0 pipeline syntax)

---

## Quick Example

### Your Original Request

```
| if      | status  | Action  |
|---------|---------|---------|
| success | 200     | Proceed |
| failure | 400     | Retry   |
| pending | 102     | Wait    |
```

### DSL Syntax

```
define rules: http_response_handler
  pattern: business_logic

  decision_table: determine_action
    given:
      - result: text
      - status: number

    decide:
      | result  | status | → action  |
      |---------|--------|-----------|
      | success | 200    | Proceed   |
      | failure | 400    | Retry     |
      | pending | 102    | Wait      |
      | *       | *      | Error     |

    return:
      - action: text
```

---

## Complete Feature Set ✅

### Conditions

| Feature | Example | Supported |
|---------|---------|-----------|
| **Exact match** | `200`, `"success"` | ✅ |
| **Ranges** | `1-7`, `> $10,000`, `<= 850` | ✅ |
| **Complex expressions** | `(credit_score >= 700) AND (dti < 0.3)` | ✅ |
| **Function calls** | `is_vip_customer(id)` | ✅ |
| **IN/NOT IN** | `IN (active, pending)` | ✅ |
| **Wildcards** | `*` (matches any) | ✅ |

### Actions

| Feature | Example | Supported |
|---------|---------|-----------|
| **Return values** | `$15`, `"Approve"` | ✅ |
| **Expressions** | `balance * 0.01` | ✅ |
| **Function calls** | `calculate_limit(customer)` | ✅ |
| **Executable actions** | `send_email(customer)` | ✅ |
| **Multiple actions** | 3+ action columns | ✅ |
| **Mixed return/execute** | Some return, some execute | ✅ |

---

## Credit Card Examples

### 1. Late Fee Calculation

```
decision_table: calculate_late_fee
  given:
    - account_type: text
    - days_late: number

  decide:
    | account_type | days_late | → late_fee |
    |--------------|-----------|------------|
    | premier      | 1-7       | $15        |
    | premier      | 8-30      | $25        |
    | standard     | 1-7       | $35        |
    | standard     | 8-30      | $45        |
    | *            | *         | $50        |

  return:
    - late_fee: money
```

### 2. Credit Approval (Multi-Condition)

```
decision_table: approve_credit
  given:
    - credit_score: number
    - debt_ratio: percentage
    - account_age: number

  decide:
    | credit_score | debt_ratio | account_age | → decision | credit_limit |
    |--------------|------------|-------------|------------|--------------|
    | >= 750       | < 0.3      | *           | Approve    | $30,000      |
    | >= 700       | < 0.35     | > 2         | Approve    | $20,000      |
    | >= 650       | < 0.4      | > 2         | Review     | $15,000      |
    | *            | *          | *           | Deny       | $0           |

  return:
    - decision: text
    - credit_limit: money
```

### 3. Transaction Routing (Functions + Expressions)

```
decision_table: route_transaction
  given:
    - amount: money
    - risk_score: number
    - transaction: transaction

  decide:
    | condition                                    | → processor | execute                  |
    |----------------------------------------------|-------------|--------------------------|
    | amount > $10,000 AND risk_score > 70         | manual      | alert_fraud(transaction) |
    | amount > $10,000                             | review      | flag_review(transaction) |
    | risk_score > 50                              | standard    | -                        |
    | *                                            | fast        | -                        |

  return:
    - processor: text

  execute: execute
```

---

## Integration with Pipelines

### Lookup Pattern

```
workflow: process_payments
  step: handle_responses
    actions:
      - payment_responses
          | map: determine_action(result, status)  # Decision table
          | filter: action = "Proceed"
          | foreach: complete_payment
```

### Group and Route

```
workflow: route_transactions
  step: process_by_routing
    actions:
      - transactions
          | map: route_transaction(amount, risk_score, self)
          | group_by: processor
          | foreach: send_to_processor(processor, transactions)
```

---

## Benefits

### vs Nested If-Else

**Before** (hard to read, easy to miss cases):
```
if credit_score >= 750 and dti < 0.3:
    decision = "Approve"
elif credit_score >= 700 and dti < 0.35 and age > 2:
    decision = "Approve"
elif ...
    decision = "Review"
else:
    decision = "Deny"
```

**After** (all cases visible, business-friendly):
```
| credit_score | dti    | age | → decision |
|--------------|--------|-----|------------|
| >= 750       | < 0.3  | *   | Approve    |
| >= 700       | < 0.35 | > 2 | Approve    |
| >= 650       | < 0.4  | > 2 | Review     |
| *            | *      | *   | Deny       |
```

### Industry Alignment

- ✅ **DMN** (Decision Model and Notation) - OMG standard
- ✅ **Drools** (Red Hat) - Enterprise rules engine
- ✅ **Excel decision tables** - Business analyst familiar

### Validation

- ✅ **Exhaustiveness checking**: Warns if cases not covered
- ✅ **Overlap detection**: Errors on conflicting rules
- ✅ **Type checking**: Validates condition/action types
- ✅ **Test generation**: Each row = test case

---

## Implementation Plan

### Phase 1: Basic Tables (Week 1)
- Exact matching, simple ranges
- Single return value
- Parser and AST

### Phase 2: Advanced Conditions (Week 2)
- Complex expressions
- Function calls
- IN/NOT IN operators

### Phase 3: Advanced Actions (Week 3)
- Multiple return values
- Executable actions
- Multi-action per row

### Phase 4: Optimization (Week 4)
- Decision tree compilation
- Validation (exhaustiveness, overlap)
- Performance optimization

**Total**: 4 weeks after v3.0 completion

---

## Documentation

**Complete Specification**: DSL_DECISION_TABLES_SPECIFICATION.md (40+ pages)

**Covers**:
- All condition types with examples
- All action types with examples
- Credit card domain examples
- Integration patterns
- Grammar specification
- Code generation strategy
- Validation rules
- Performance optimization

---

## Status

✅ **Fully Specified**
✅ **All Features Documented**
✅ **Credit Card Examples Complete**
✅ **Integration with v3.0 Defined**
✅ **Ready for Implementation**

**Next Step**: Implement after v3.0 pipeline syntax is complete
