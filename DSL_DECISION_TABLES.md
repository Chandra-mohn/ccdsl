# Decision Tables Specification

**Feature**: Matrix-based decision/conditional logic
**Status**: Complete Specification

---

## Executive Summary

**Decision tables** provide a tabular, visual way to express complex conditional logic with multiple conditions and actions. They are industry-proven (DMN, Drools) and ideal for credit card business rules.

**Key Capabilities**:
- ✅ Exact matching and range conditions
- ✅ Complex expressions (AND, OR, NOT)
- ✅ Function calls in conditions
- ✅ Return values (lookup tables)
- ✅ Executable actions (workflow integration)
- ✅ Multiple action columns
- ✅ First-match semantics with optional priorities
- ✅ Exhaustiveness checking and overlap detection

---

## Table of Contents

1. [Basic Syntax](#basic-syntax)
2. [Condition Types](#condition-types)
3. [Action Types](#action-types)
4. [Complete Examples](#complete-examples)
5. [Integration with Workflows](#integration-with-workflows)
6. [Grammar Specification](#grammar-specification)
7. [Code Generation](#code-generation)
8. [Validation Rules](#validation-rules)

---

## Basic Syntax

### Minimal Decision Table

```
define rules: response_handler
  pattern: business_logic

  decision_table: determine_action
    given:
      - status: number

    decide:
      | status | → action  |
      |--------|-----------|
      | 200    | Proceed   |
      | 400    | Retry     |
      | 500    | Error     |

    return:
      - action: text
```

### Full-Featured Decision Table

```
define rules: credit_approval
  pattern: business_logic
  description: "Complex credit approval with multiple conditions and actions"

  decision_table: approve_credit_application
    given:
      - credit_score: number
      - debt_ratio: percentage
      - account_age: number
      - annual_income: money
      - customer: customer  # Entity reference

    decide:
      | priority | credit_score | debt_ratio | account_age | annual_income | → decision | credit_limit      | review_required | notify_manager |
      |----------|--------------|------------|-------------|---------------|------------|-------------------|-----------------|----------------|
      | 1        | >= 800       | < 0.2      | > 5         | > $100,000    | Approve    | $50,000           | no              | no             |
      | 2        | >= 750       | < 0.3      | > 3         | > $75,000     | Approve    | $30,000           | no              | no             |
      | 3        | >= 700       | < 0.35     | > 2         | > $50,000     | Approve    | $20,000           | yes             | no             |
      | 4        | >= 650       | < 0.4      | > 2         | > $40,000     | Review     | calculate_limit() | yes             | yes            |
      | 5        | >= 600       | < 0.35     | > 5         | *             | Review     | $10,000           | yes             | yes            |
      | 6        | *            | *          | *           | *             | Deny       | $0                | no              | yes            |

    return:
      - decision: text
      - credit_limit: money
      - review_required: boolean
      - notify_manager: boolean
```

**Key Elements**:
- `priority`: Optional explicit ordering (lower number = higher priority)
- Multiple condition columns: All must match (AND logic)
- `→` arrow: Separates conditions from actions
- Multiple action columns: All are evaluated/executed
- Wildcard `*`: Matches any value (default case)

---

## Condition Types

### 1. Exact Match

**Syntax**: Literal value

```
| status | → action  |
|--------|-----------|
| 200    | Success   |
| 404    | NotFound  |
| 500    | Error     |
```

**Semantics**: `status == 200`

---

### 2. Range Conditions

**Numeric Ranges**:
```
| credit_score | → rating    |
|--------------|-------------|
| 800-850      | Excellent   |
| 750-799      | Very Good   |
| 700-749      | Good        |
| 650-699      | Fair        |
| 300-649      | Poor        |
```

**Money Ranges**:
```
| balance           | → fee_tier |
|-------------------|------------|
| $0-$1,000         | Tier1      |
| $1,001-$10,000    | Tier2      |
| > $10,000         | Tier3      |
```

**Date Ranges**:
```
| payment_date      | → status   |
|-------------------|------------|
| <= due_date       | OnTime     |
| due_date + 1-7    | Grace      |
| > due_date + 7    | Late       |
```

**Semantics**: `credit_score >= 800 AND credit_score <= 850`

---

### 3. Comparison Operators

**Supported Operators**:

| Operator | Meaning | Example |
|----------|---------|---------|
| `>` | Greater than | `> 700` |
| `>=` | Greater or equal | `>= 750` |
| `<` | Less than | `< 0.3` |
| `<=` | Less or equal | `<= $10,000` |
| `=` | Equal (explicit) | `= "active"` |
| `!=` | Not equal | `!= "suspended"` |

**Example**:
```
| credit_score | debt_ratio | → decision |
|--------------|------------|------------|
| >= 700       | < 0.3      | Approve    |
| >= 650       | < 0.4      | Review     |
| < 650        | *          | Deny       |
```

---

### 4. Complex Expressions

**AND Logic** (implicit - all conditions in row must match):
```
| credit_score | debt_ratio | account_age | → decision |
|--------------|------------|-------------|------------|
| >= 700       | < 0.3      | > 2         | Approve    |
```
**Semantics**: `(credit_score >= 700) AND (debt_ratio < 0.3) AND (account_age > 2)`

**OR Logic** (explicit - use parentheses):
```
| condition                                      | → decision |
|------------------------------------------------|------------|
| (credit_score >= 800) OR (debt_ratio < 0.2)   | Approve    |
| (account_age > 10) OR (annual_income > 200k)  | Review     |
```

**NOT Logic**:
```
| account_status      | → action   |
|---------------------|------------|
| NOT "suspended"     | Process    |
| NOT IN (closed, inactive) | Review |
```

**Nested Expressions**:
```
| condition                                                           | → decision |
|---------------------------------------------------------------------|------------|
| (credit_score >= 700 AND debt_ratio < 0.3) OR account_age > 10    | Approve    |
| (credit_score < 600) AND NOT (account_age > 5)                     | Deny       |
```

---

### 5. Function Calls in Conditions

**Built-in Functions**:
```
| condition                           | → action   |
|-------------------------------------|------------|
| is_vip_customer(customer_id)        | FastTrack  |
| has_outstanding_balance(account_id) | Collect    |
| days_since(last_payment) > 30       | Late       |
```

**Custom Validation Functions**:
```
| condition                                  | → decision |
|--------------------------------------------|------------|
| validate_credit_history(customer) = true   | Approve    |
| check_fraud_score(transaction) < 50        | Process    |
| verify_identity(customer, document) = true | Proceed    |
```

**Combining Functions and Operators**:
```
| condition                                              | → action   |
|--------------------------------------------------------|------------|
| calculate_dti(income, debts) < 0.4 AND credit_score >= 700 | Approve |
| get_account_age(account) > 2 OR is_referral(customer)      | Review  |
```

---

### 6. IN/NOT IN Operators

**Enum Matching**:
```
| account_status              | → action   |
|-----------------------------|------------|
| IN (active, pending)        | Process    |
| IN (suspended, frozen)      | Block      |
| NOT IN (closed, terminated) | Continue   |
```

**Set Matching**:
```
| merchant_category                    | → risk_level |
|--------------------------------------|--------------|
| IN (gambling, adult, crypto)         | High         |
| NOT IN (grocery, gas, pharmacy)      | Review       |
```

---

### 7. Wildcard (Default Case)

**Syntax**: `*` matches any value

```
| credit_score | → decision |
|--------------|------------|
| >= 700       | Approve    |
| >= 600       | Review     |
| *            | Deny       |  # Catches all other values
```

**Best Practice**: Always include a wildcard row as the last row to ensure exhaustive coverage.

---

## Action Types

### 1. Return Values (Lookup Tables)

**Single Return Value**:
```
decision_table: get_interest_rate
  given:
    - credit_score: number

  decide:
    | credit_score | → interest_rate |
    |--------------|-----------------|
    | >= 800       | 3.5%            |
    | >= 750       | 4.5%            |
    | >= 700       | 5.5%            |
    | >= 650       | 7.5%            |
    | *            | 12.0%           |

  return:
    - interest_rate: percentage
```

**Usage in Workflow**:
```
rate = get_interest_rate(customer.credit_score)
```

---

### 2. Multiple Return Values

**Multiple Columns**:
```
decision_table: calculate_fee_and_grace
  given:
    - account_type: text
    - days_late: number

  decide:
    | account_type | days_late | → late_fee | grace_period | waiver_eligible |
    |--------------|-----------|------------|--------------|-----------------|
    | premier      | 1-7       | $15        | 3 days       | yes             |
    | premier      | 8-30      | $25        | 0 days       | no              |
    | standard     | 1-7       | $35        | 1 day        | no              |
    | standard     | 8-30      | $45        | 0 days       | no              |

  return:
    - late_fee: money
    - grace_period: duration
    - waiver_eligible: boolean
```

**Usage in Workflow**:
```
(fee, grace, eligible) = calculate_fee_and_grace(account.type, days_late)
```

---

### 3. Executable Actions (Single)

**Single Action Execution**:
```
decision_table: handle_payment_response
  given:
    - result: text
    - status: number
    - payment: payment

  decide:
    | result  | status | → execute                         |
    |---------|--------|-----------------------------------|
    | success | 200    | complete_payment(payment)         |
    | failure | 400    | schedule_retry(payment, 5min)     |
    | failure | 500    | schedule_retry(payment, 30min)    |
    | pending | 102    | mark_as_pending(payment)          |
    | error   | *      | alert_operations(payment, status) |

  execute: yes  # Actions are callable, not return values
```

**Usage in Workflow**:
```
handle_payment_response(result, status, payment)
# Action is executed directly based on matching row
```

---

### 4. Multiple Actions (Sequential Execution)

**Multiple Action Columns**:
```
decision_table: process_high_value_transaction
  given:
    - amount: money
    - risk_score: number
    - transaction: transaction

  decide:
    | amount      | risk_score | → execute_1                    | execute_2                    | execute_3                |
    |-------------|------------|--------------------------------|------------------------------|--------------------------|
    | > $10,000   | > 70       | hold_transaction(transaction)  | alert_fraud_team(transaction) | notify_customer(transaction) |
    | > $10,000   | 30-70      | flag_for_review(transaction)   | log_event("high_value")      | -                        |
    | > $10,000   | < 30       | process_transaction(transaction) | log_event("processed")       | -                        |
    | $1,000-$10,000 | > 50    | flag_for_review(transaction)   | -                            | -                        |
    | < $1,000    | *          | process_transaction(transaction) | -                            | -                        |

  execute: yes
```

**Notes**:
- Actions execute left-to-right in order
- `-` means "no action" for that column
- All non-empty actions in the matched row are executed

---

### 5. Computed Actions

**Using Expressions in Actions**:
```
decision_table: calculate_dynamic_fee
  given:
    - balance: money
    - days_late: number

  decide:
    | balance      | days_late | → fee_amount                    |
    |--------------|-----------|----------------------------------|
    | > $10,000    | 1-7       | balance * 0.01                  |
    | > $10,000    | 8-30      | $15 + (days_late - 7) * $2      |
    | <= $10,000   | 1-7       | $35                             |
    | <= $10,000   | 8-30      | $35 + (days_late - 7) * $3      |

  return:
    - fee_amount: money
```

**Using Function Calls in Actions**:
```
decision_table: assign_credit_limit
  given:
    - credit_score: number
    - annual_income: money
    - customer: customer

  decide:
    | credit_score | annual_income | → credit_limit                        |
    |--------------|---------------|---------------------------------------|
    | >= 800       | *             | calculate_premium_limit(customer)     |
    | >= 700       | > $100,000    | min(annual_income * 0.3, $50,000)    |
    | >= 700       | <= $100,000   | annual_income * 0.2                  |
    | >= 650       | *             | calculate_standard_limit(customer)    |
    | *            | *             | $5,000                               |

  return:
    - credit_limit: money
```

---

### 6. Mixed: Returns + Actions

**Hybrid Approach**:
```
decision_table: approve_and_notify
  given:
    - credit_score: number
    - application: application

  decide:
    | credit_score | → decision | credit_limit | execute_notification          |
    |--------------|------------|--------------|-------------------------------|
    | >= 750       | Approve    | $30,000      | send_approval_email(application) |
    | >= 650       | Review     | $15,000      | assign_to_reviewer(application) |
    | < 650        | Deny       | $0           | send_denial_email(application) |

  return:
    - decision: text
    - credit_limit: money

  execute: notify  # Only 'execute_notification' column is executable
```

**Notes**:
- Some columns return values, others execute actions
- Use `execute: <column_name>` to specify which columns are executable
- Return columns are specified in `return:` block

---

## Complete Examples

### Example 1: Late Fee Calculation (Complex Conditions)

```
define rules: late_fee_assessment
  pattern: business_logic
  description: "Calculate late fee based on account type, balance, payment history"

  decision_table: assess_late_fee
    given:
      - account_type: text
      - current_balance: money
      - days_late: number
      - payment_history_score: number
      - account: account

    decide:
      | account_type | current_balance | days_late | payment_history_score | → late_fee | waiver_eligible | grace_days | notification_action              |
      |--------------|-----------------|-----------|----------------------|------------|-----------------|------------|----------------------------------|
      | premier      | > $10,000       | 1-7       | >= 90                | $10        | yes             | 3          | send_courtesy_reminder(account)  |
      | premier      | > $10,000       | 1-7       | < 90                 | $15        | yes             | 2          | send_standard_notice(account)    |
      | premier      | > $10,000       | 8-30      | *                    | $25        | no              | 0          | send_urgent_notice(account)      |
      | premier      | <= $10,000      | 1-7       | >= 80                | $20        | yes             | 2          | send_courtesy_reminder(account)  |
      | premier      | <= $10,000      | 8-30      | *                    | $35        | no              | 0          | send_urgent_notice(account)      |
      | standard     | *               | 1-7       | >= 85                | $30        | no              | 1          | send_standard_notice(account)    |
      | standard     | *               | 8-30      | >= 70                | $40        | no              | 0          | send_urgent_notice(account)      |
      | standard     | *               | > 30      | *                    | $50        | no              | 0          | escalate_to_collections(account) |
      | *            | *               | *         | *                    | $35        | no              | 0          | send_standard_notice(account)    |

    return:
      - late_fee: money
      - waiver_eligible: boolean
      - grace_days: number

    execute: notification_action
```

---

### Example 2: Transaction Routing (Functions + Expressions)

```
define rules: transaction_routing
  pattern: business_logic
  description: "Route transactions based on amount, risk, merchant category"

  decision_table: route_transaction
    given:
      - amount: money
      - merchant_category: text
      - risk_score: number
      - customer: customer
      - transaction: transaction

    decide:
      | condition                                                                      | → processor | fraud_check | priority | execute_action                           |
      |--------------------------------------------------------------------------------|-------------|-------------|----------|------------------------------------------|
      | amount > $50,000 OR merchant_category IN (gambling, crypto, adult)            | manual      | required    | urgent   | hold_and_alert(transaction, "high_risk") |
      | amount > $10,000 AND risk_score > 70                                          | review      | required    | high     | flag_for_review(transaction)             |
      | amount > $10,000 AND is_new_customer(customer)                                | review      | required    | medium   | verify_customer(customer, transaction)   |
      | merchant_category IN (high_risk_list) AND NOT is_vip_customer(customer)       | review      | required    | medium   | enhanced_verification(transaction)       |
      | amount > $5,000 AND risk_score > 50                                           | standard    | optional    | normal   | log_event("flagged_transaction")         |
      | amount <= $5,000 AND risk_score < 30                                          | fast        | skip        | low      | process_immediately(transaction)         |
      | *                                                                              | standard    | optional    | normal   | standard_processing(transaction)         |

    return:
      - processor: text
      - fraud_check: text
      - priority: text

    execute: execute_action
```

---

### Example 3: Credit Approval (Multi-Action)

```
define rules: credit_approval_workflow
  pattern: business_logic
  description: "Approve credit applications with multi-step actions"

  decision_table: approve_application
    given:
      - credit_score: number
      - debt_to_income: percentage
      - account_age: number
      - annual_income: money
      - application: application
      - customer: customer

    decide:
      | credit_score | debt_to_income | account_age | annual_income | → decision | credit_limit              | action_1                        | action_2                          | action_3                        |
      |--------------|----------------|-------------|---------------|------------|---------------------------|----------------------------------|-----------------------------------|----------------------------------|
      | >= 800       | < 0.2          | > 5         | > $150,000    | Approve    | $75,000                   | approve_application(application) | send_approval_email(customer)     | setup_account(customer, $75k)    |
      | >= 750       | < 0.3          | > 3         | > $100,000    | Approve    | $50,000                   | approve_application(application) | send_approval_email(customer)     | setup_account(customer, $50k)    |
      | >= 700       | < 0.35         | > 2         | > $75,000     | Approve    | $30,000                   | approve_application(application) | send_approval_email(customer)     | setup_account(customer, $30k)    |
      | >= 650       | < 0.4          | > 2         | > $50,000     | Review     | calculate_limit(customer) | assign_to_underwriter(application)| send_review_email(customer)       | -                                |
      | >= 600       | < 0.35         | > 5         | *             | Review     | $10,000                   | assign_to_senior_underwriter(application) | request_additional_docs(customer) | -                |
      | < 600        | *              | *           | *             | Deny       | $0                        | deny_application(application)    | send_denial_email(customer)       | log_denial_reason(application)   |
      | *            | > 0.5          | *           | *             | Deny       | $0                        | deny_application(application)    | send_denial_email(customer)       | suggest_debt_counseling(customer)|

    return:
      - decision: text
      - credit_limit: money

    execute: multi  # All action_* columns are executable
```

---

## Integration with Workflows

### Pattern 1: Lookup in Pipeline

```
define workflow: calculate_customer_rates
  step: determine_rates
    actions:
      - customers
          | filter: is_active
          | map: get_interest_rate(credit_score)  # Decision table lookup
          | foreach: update_customer_rate
```

---

### Pattern 2: Conditional Routing

```
define workflow: process_applications
  step: route_by_decision
    actions:
      - applications
          | map: approve_application(credit_score, dti, age, income, self, customer)
          | group_by: decision
          | foreach: process_by_decision(decision, applications)
```

---

### Pattern 3: Direct Action Execution

```
define workflow: handle_transactions
  step: route_and_process
    actions:
      - transactions
          | foreach: route_transaction(amount, merchant_category, risk_score, customer, self)
          # Decision table executes routing action directly
```

---

### Pattern 4: Multi-Step with Decision Tables

```
define workflow: credit_application_workflow
  step: gather_data
    actions:
      - load application data
      - calculate credit_score, debt_to_income
    next: goto evaluate

  step: evaluate
    actions:
      - (decision, limit) = approve_application(
          credit_score, dti, account_age, income, application, customer
        )
      - update application with decision and limit
    next:
      when decision = "Approve": goto finalize_approval
      when decision = "Review": goto manual_review
      when decision = "Deny": goto finalize_denial

  step: finalize_approval
    actions:
      - complete approval workflow
    return: approved

  step: manual_review
    actions:
      - assign to underwriter
    return: pending_review

  step: finalize_denial
    actions:
      - complete denial workflow
    return: denied
```

---

## Grammar Specification

### EBNF Grammar

```ebnf
(* Decision Table Definition *)
decision_table_definition ::=
    "decision_table", ":", identifier,
    [ "description", ":", string_literal ],
    "given", ":", { input_parameter },
    "decide", ":", decision_matrix,
    [ return_specification | execute_specification | hybrid_specification ] ;

(* Input Parameters *)
input_parameter ::=
    "-", identifier, ":", type_spec ;

(* Decision Matrix *)
decision_matrix ::=
    table_header,
    { table_row } ;

table_header ::=
    "|", [ "priority", "|" ], { condition_column_header, "|" }, "→", { action_column_header, "|" } ;

condition_column_header ::=
    identifier ;

action_column_header ::=
    identifier ;

table_row ::=
    "|", [ integer_literal, "|" ], { condition_cell, "|" }, "→", { action_cell, "|" } ;

(* Condition Cells *)
condition_cell ::=
    exact_match
  | range_condition
  | comparison_condition
  | complex_expression
  | function_call
  | in_condition
  | wildcard ;

exact_match ::=
    literal ;

range_condition ::=
    ( number_range | money_range | date_range ) ;

number_range ::=
    numeric_literal, "-", numeric_literal ;

comparison_condition ::=
    comparison_operator, expression ;

comparison_operator ::=
    ">" | ">=" | "<" | "<=" | "=" | "!=" ;

complex_expression ::=
    boolean_expression_with_parens ;

boolean_expression_with_parens ::=
    "(", boolean_expression, ")",
    [ ( "AND" | "OR" ), boolean_expression_with_parens ] ;

function_call ::=
    identifier, "(", [ argument_list ], ")" ;

in_condition ::=
    [ "NOT" ], "IN", "(", value_list, ")" ;

value_list ::=
    literal, { ",", literal } ;

wildcard ::=
    "*" ;

(* Action Cells *)
action_cell ::=
    literal
  | expression
  | function_call
  | no_action ;

no_action ::=
    "-" ;

(* Return/Execute Specifications *)
return_specification ::=
    "return", ":", { return_parameter } ;

return_parameter ::=
    "-", identifier, ":", type_spec ;

execute_specification ::=
    "execute", ":", ( "yes" | identifier | "multi" ) ;

hybrid_specification ::=
    return_specification,
    execute_specification ;
```

---

## Code Generation

### Strategy: Compile to Match Statements

**DSL**:
```
decision_table: get_fee
  given:
    - account_type: text
    - days_late: number

  decide:
    | account_type | days_late | → fee    |
    |--------------|-----------|----------|
    | premier      | 1-7       | $15      |
    | premier      | 8-30      | $25      |
    | standard     | 1-7       | $35      |
    | *            | *         | $50      |

  return:
    - fee: money
```

**Generated Rust**:
```rust
pub fn get_fee(account_type: &str, days_late: i32) -> Money {
    match (account_type, days_late) {
        ("premier", 1..=7) => Money::from_dollars(15),
        ("premier", 8..=30) => Money::from_dollars(25),
        ("standard", 1..=7) => Money::from_dollars(35),
        (_, _) => Money::from_dollars(50),
    }
}
```

---

### Complex Conditions: Compile to If-Else

**DSL**:
```
decision_table: route_transaction
  given:
    - amount: money
    - risk_score: number

  decide:
    | condition                                | → processor |
    |------------------------------------------|-------------|
    | amount > $10,000 AND risk_score > 70     | manual      |
    | amount > $10,000                         | review      |
    | *                                        | standard    |

  return:
    - processor: text
```

**Generated Rust**:
```rust
pub fn route_transaction(amount: Money, risk_score: i32) -> String {
    if amount > Money::from_dollars(10000) && risk_score > 70 {
        "manual".to_string()
    } else if amount > Money::from_dollars(10000) {
        "review".to_string()
    } else {
        "standard".to_string()
    }
}
```

---

### Executable Actions: Generate Function Calls

**DSL**:
```
decision_table: handle_response
  given:
    - status: number
    - payment: payment

  decide:
    | status | → execute                     |
    |--------|-------------------------------|
    | 200    | complete_payment(payment)     |
    | 400    | schedule_retry(payment, 5min) |

  execute: yes
```

**Generated Rust**:
```rust
pub fn handle_response(status: i32, payment: &Payment, ctx: &WorkflowContext) {
    match status {
        200 => complete_payment(payment, ctx),
        400 => schedule_retry(payment, Duration::from_secs(300), ctx),
        _ => {}
    }
}
```

---

### Multiple Actions: Sequential Execution

**DSL**:
```
decision_table: process_application
  given:
    - credit_score: number
    - application: application

  decide:
    | credit_score | → action_1            | action_2                  |
    |--------------|----------------------|---------------------------|
    | >= 700       | approve(application) | send_email(application)   |
    | < 700        | deny(application)    | log_denial(application)   |

  execute: multi
```

**Generated Rust**:
```rust
pub fn process_application(credit_score: i32, application: &Application, ctx: &WorkflowContext) {
    if credit_score >= 700 {
        approve(application, ctx);
        send_email(application, ctx);
    } else {
        deny(application, ctx);
        log_denial(application, ctx);
    }
}
```

---

## Validation Rules

### Compile-Time Validation

**1. Exhaustiveness Checking**

Warn if no wildcard row exists and conditions don't cover all cases:

```
WARNING: Decision table 'get_fee' may not be exhaustive
  Missing cases for: account_type NOT IN (premier, standard)

  Suggestion: Add wildcard row:
  | *  | *  | $50 |
```

---

**2. Overlap Detection**

Error if multiple rows can match the same input:

```
ERROR: Decision table 'calculate_fee' has overlapping rules
  Row 2: | premier | 1-7  | $15 |
  Row 3: | premier | 1-10 | $20 |

  Conflict: days_late = 5 matches both rows

  Fix: Add explicit priority or make conditions mutually exclusive
```

---

**3. Type Checking**

Error if condition types don't match input types:

```
ERROR: Type mismatch in decision table 'approve_credit'
  Column 'credit_score' expects: number
  Row 3 has: "high" (text)

  Fix: Use numeric value or change input type
```

---

**4. Action Validation**

Error if action references unknown functions:

```
ERROR: Unknown function in decision table 'handle_payment'
  Action: complete_payment(payment)

  Function 'complete_payment' is not defined

  Available actions: [process_payment, cancel_payment, retry_payment]
```

---

### Runtime Validation

**1. Default Case Fallback**

Always execute wildcard row if no other rows match:

```rust
// Generated code always includes fallback
match (conditions) {
    ... // specific cases
    _ => default_action()  // wildcard row
}
```

---

**2. Priority Enforcement**

If explicit priorities exist, enforce ordering:

```rust
// Rows evaluated in priority order (low to high)
let rows_by_priority = sort_rows_by_priority(decision_table);
for row in rows_by_priority {
    if row.matches(inputs) {
        return row.action();
    }
}
```

---

**3. Null Handling**

Warn if optional inputs might be null:

```
WARNING: Decision table 'calculate_limit' may receive null
  Input 'annual_income' is optional (nullable)

  Suggestion: Add explicit null handling:
  | * | null | $5,000 |
```

---

## Performance Optimization

### 1. Decision Tree Compilation

For large tables (>20 rows), compile to decision tree:

```
If credit_score >= 750:
    If debt_ratio < 0.3: → Approve
    Else: → Review
Else:
    If credit_score >= 650:
        If account_age > 2: → Review
        Else: → Deny
    Else: → Deny
```

**Benefit**: O(log n) instead of O(n) for row matching

---

### 2. Range Index Building

For range conditions, build interval tree:

```rust
// Build interval tree at compile time
let range_index = IntervalTree::from_ranges([
    (300..649, "Poor"),
    (650..699, "Fair"),
    (700..749, "Good"),
    (750..799, "Very Good"),
    (800..850, "Excellent"),
]);

// O(log n) lookup at runtime
let rating = range_index.lookup(credit_score);
```

---

### 3. Exact Match Hash Table

For exact match tables, use hash map:

```rust
// Compile to hash map
let status_actions = HashMap::from([
    (200, "Proceed"),
    (400, "Retry"),
    (500, "Error"),
]);

// O(1) lookup
let action = status_actions.get(&status).unwrap_or(&"Unknown");
```

---

## Migration from Nested If-Else

### Before: Nested Conditionals

```
rule: calculate_late_fee_old_style
  given:
    - account_type: text
    - days_late: number

  calculate:
    if account_type = "premier":
      if days_late <= 7:
        late_fee = $15
      else if days_late <= 30:
        late_fee = $25
      else:
        late_fee = $35
    else if account_type = "standard":
      if days_late <= 7:
        late_fee = $35
      else:
        late_fee = $45
    else:
      late_fee = $50

  return:
    - late_fee: money
```

**Problems**:
- Hard to visualize all cases
- Easy to miss cases
- Difficult to validate exhaustiveness
- Poor readability for business users

---

### After: Decision Table

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
    | premier      | > 30      | $35        |
    | standard     | 1-7       | $35        |
    | standard     | > 7       | $45        |
    | *            | *         | $50        |

  return:
    - late_fee: money
```

**Benefits**:
- ✅ All cases visible at once
- ✅ Easy to spot gaps or overlaps
- ✅ Business users can validate logic
- ✅ Automated exhaustiveness checking
- ✅ 10x more readable

---

## Best Practices

### 1. Always Include Default Case

```
| *  | *  | default_value |
```

Ensures no runtime errors from unmatched cases.

---

### 2. Order Rows by Priority

Place most specific conditions first:

```
| premier | > $10,000 | 1-7  | $10 |  # Most specific
| premier | *         | 1-7  | $15 |  # Less specific
| *       | *         | *    | $35 |  # Default
```

---

### 3. Use Explicit Priorities for Complex Logic

When row ordering isn't sufficient:

```
| priority | condition | → action |
|----------|-----------|----------|
| 1        | special   | A        |
| 2        | normal    | B        |
```

---

### 4. Keep Tables Focused

One table = one decision. Don't mix unrelated logic:

❌ **Bad**: One table for fee calculation AND approval logic
✅ **Good**: Separate tables: `calculate_fee`, `approve_application`

---

### 5. Document Complex Conditions

```
decision_table: complex_routing
  description: "Routes transactions based on amount, risk, and customer tier"

  # NOTE: High-risk merchants always go to manual review
  # VIP customers get fast-track even with higher amounts

  decide:
    ...
```

---

### 6. Test Each Row

Each row is a test case:

```
test "late_fee_calculator":
  # Row 1
  assert calculate_late_fee("premier", 5) = $15

  # Row 2
  assert calculate_late_fee("premier", 15) = $25

  # Row 3
  assert calculate_late_fee("standard", 5) = $35

  # Default case
  assert calculate_late_fee("unknown", 100) = $50
```

---

## Summary

### Feature Matrix

| Feature | Supported | Example |
|---------|-----------|---------|
| Exact matching | ✅ | `200`, `"success"` |
| Range conditions | ✅ | `1-7`, `> $10,000`, `<= 850` |
| Complex expressions | ✅ | `(A AND B) OR C` |
| Function calls (conditions) | ✅ | `is_vip(customer)` |
| Function calls (actions) | ✅ | `complete_payment(payment)` |
| IN/NOT IN operators | ✅ | `IN (active, pending)` |
| Wildcards | ✅ | `*` |
| Return values | ✅ | Single or multiple |
| Executable actions | ✅ | Single or multiple |
| Mixed return/execute | ✅ | Hybrid tables |
| Priority ordering | ✅ | Explicit `priority` column |
| Exhaustiveness checking | ✅ | Compile-time validation |
| Overlap detection | ✅ | Compile-time warning |

---

### Implementation Roadmap

**Phase 1** (Week 1): Basic tables
- Exact matching
- Simple ranges
- Single return value
- Parser and AST

**Phase 2** (Week 2): Advanced conditions
- Complex expressions
- Function calls in conditions
- IN/NOT IN operators

**Phase 3** (Week 3): Advanced actions
- Multiple return values
- Executable actions
- Multiple actions per row

**Phase 4** (Week 4): Optimization & Validation
- Decision tree compilation
- Exhaustiveness checking
- Overlap detection
- Performance optimization

---

**Status**: ✅ **Complete Specification - Ready for Implementation**
**Target Version**: v3.1
**Estimated Effort**: 4 weeks
**Dependencies**: v3.0 pipeline syntax (foundation)

---

**Related Documents**:
- **DSL_LANGUAGE_SPECIFICATION.md**: Core language reference
- **DSL_EVOLUTION_V3_PIPELINE_SYNTAX.md**: v3.0 pipeline design
- **DSL_GRAMMAR_V3.ebnf**: Formal grammar (to be updated with decision tables)
