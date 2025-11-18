# DOM-Like Execution Context - Impact Analysis

**Date**: 2025-11-10
**Status**: Analysis Only (Awaiting Approval for DSL Changes)

---

## Proposed Architecture

### Core Concept: Execution Context (DOM)

All workflow/rule executions operate on a **DOM-like data object**:

1. **Context Creation**: DOM created at workflow/rule start
2. **Data Access**: All data retrieved via `get(path)` from DOM
3. **Data Mutation**: All changes via `put(path, value)` to DOM
4. **Isolation**: Each execution has its own DOM instance
5. **No Direct Parameters**: Functions don't pass data directly, only through DOM

**Similar Patterns**:
- Browser Document Object Model (DOM)
- React Virtual DOM / Fiber
- Workflow engines (Temporal, Cadence) execution context
- Blackboard pattern (AI systems)
- HTTP Request context (Express, Flask)
- Database transaction context

---

## Benefits of DOM Architecture

### 1. Transactional Integrity
```
context = create_execution_context()
try:
  workflow.execute(context)
  context.commit()  # All changes or none
except:
  context.rollback()  # Undo all changes
```

### 2. Automatic Audit Trail
```
Every get/put operation logged:
  - context.get("account.balance") → logged with timestamp
  - context.put("account.balance", 1500) → logged with old/new values

Result: Complete audit trail with zero developer effort
```

### 3. Execution Isolation
```
workflow_1_context = ExecutionContext()
workflow_2_context = ExecutionContext()

# No interference between executions
# Each has its own data snapshot
# Thread-safe by design
```

### 4. Enhanced Testability
```
# Easy to mock and inject test data
test_context = MockExecutionContext({
  "account": test_account,
  "schedule": test_schedule
})

result = workflow.execute(test_context)
assert test_context.get("fee_amount") == expected_fee
```

### 5. Debugging & Observability
```
# Inspect context at any point
breakpoint_snapshot = context.snapshot()

# Time-travel debugging
context.restore_to(earlier_snapshot)

# Full execution trace
context.get_mutation_log()
```

### 6. Validation & Type Safety
```
context.put("account.balance", "invalid")  # Type error caught
context.put("account.credit_limit", -100)  # Constraint violation caught
context.commit()  # Validates all constraints before committing
```

### 7. Performance Optimization
```
# Lazy loading
account = context.get("account")  # Loaded on first access
account = context.get("account")  # Cached, no DB query

# Batch operations
context.put_batch([
  ("account.balance", 1500),
  ("account.days_past_due", 0),
  ("account.last_payment_date", today)
])  # Single DB update
```

---

## DSL Impact Analysis

### Impact Level: **MINIMAL** ✅

**Recommendation**: Keep DOM as **implementation detail**, mostly invisible in DSL.

---

## Current DSL Constructs - Mapping to DOM

### 1. Workflow Inputs/Outputs

**Current DSL**:
```
define workflow: payment_processing
  inputs:
    - account: account
    - payment_amount: money
    - processing_date: date

  outputs:
    - payment_status: text
    - late_fee_triggered: boolean
```

**DOM Implementation** (transparent to DSL):
```rust
// Generated code:
fn payment_processing(inputs: WorkflowInputs) -> WorkflowOutputs {
  // Create execution context
  let mut context = ExecutionContext::new();

  // Populate from inputs
  context.put("account", inputs.account);
  context.put("payment_amount", inputs.payment_amount);
  context.put("processing_date", inputs.processing_date);

  // Execute workflow steps...

  // Extract outputs
  WorkflowOutputs {
    payment_status: context.get("payment_status"),
    late_fee_triggered: context.get("late_fee_triggered")
  }
}
```

**DSL CHANGE NEEDED**: ❌ NO - Current syntax maps perfectly

---

### 2. Rule Definitions

**Current DSL**:
```
rule: calculate_fee_amount
  given:
    - account: account
    - schedule: late_fee_schedule
    - parameters: operational_parameters

  calculate:
    base_fee = when balance >= $5000: tier_3_amount

  return:
    - fee_amount: money
    - fee_reason: text
```

**DOM Implementation**:
```rust
// Generated code:
fn calculate_fee_amount(context: &mut ExecutionContext) -> RuleResult {
  // Read from context (generated from 'given')
  let account = context.get::<Account>("account");
  let schedule = context.get::<LateFeeSchedule>("schedule");
  let parameters = context.get::<Parameters>("parameters");

  // Business logic
  let base_fee = if account.balance >= Money::usd(5000) {
    schedule.tier_3_amount
  } else { ... };

  // Write to context (generated from 'return')
  context.put("fee_amount", calculated_fee);
  context.put("fee_reason", reason_string);

  RuleResult::Success
}
```

**DSL CHANGE NEEDED**: ❌ NO - Compiler translates given/return to DOM ops

---

### 3. Data Access in Actions

**Current DSL**:
```
step: calculate_fee
  actions:
    - load product from account.card_product
    - load schedule from product.late_fee_schedule
    - calculate fee using calculate_fee_amount rule
    - update account.total_fees_ytd
```

**DOM Implementation**:
```rust
// Generated code:
fn step_calculate_fee(context: &mut ExecutionContext) {
  // load product from account.card_product
  let account = context.get::<Account>("account");
  let product = context.get::<CardProduct>(&account.card_product_id);
  context.put("product", product);

  // load schedule from product.late_fee_schedule
  let schedule = context.get::<LateFeeSchedule>(&product.late_fee_schedule_id);
  context.put("schedule", schedule);

  // calculate fee using rule
  calculate_fee_amount(context);

  // update account.total_fees_ytd
  let fee_amount = context.get::<Money>("fee_amount");
  let mut account = context.get_mut::<Account>("account");
  account.total_fees_ytd += fee_amount;
  context.put("account", account);
}
```

**DSL CHANGE NEEDED**: ❌ NO - Compiler generates DOM operations

---

### 4. Field Access

**Current DSL**:
```
when account.current_balance >= $5000:
when customer.customer_segment == premier:
increment customer.fee_waivers_used_ytd by 1
```

**DOM Implementation**:
```rust
// when account.current_balance >= $5000:
let account = context.get::<Account>("account");
if account.current_balance >= Money::usd(5000) { ... }

// when customer.customer_segment == premier:
let customer = context.get::<Customer>("customer");
if customer.customer_segment == CustomerSegment::Premier { ... }

// increment customer.fee_waivers_used_ytd by 1
let mut customer = context.get_mut::<Customer>("customer");
customer.fee_waivers_used_ytd += 1;
context.put("customer", customer);
```

**DSL CHANGE NEEDED**: ❌ NO - Natural syntax, DOM is transparent

---

## Optional DSL Enhancements (Future)

These features COULD be added to expose DOM capabilities when needed:

### Enhancement 1: Explicit Transaction Control

**Use Case**: Critical operations needing explicit commit/rollback

**Proposed DSL**:
```
step: critical_account_update
  transaction: required
  isolation_level: serializable

  actions:
    - update account.balance
    - update account.credit_limit
    - create audit_log_entry

  on_error:
    - rollback transaction
    - notify admin

  on_success:
    - commit transaction
```

**Benefit**: Explicit control over transaction boundaries

---

### Enhancement 2: Context Variables (Temporary State)

**Use Case**: Store intermediate calculations in workflow state

**Proposed DSL**:
```
step: evaluate_eligibility
  actions:
    - context.is_eligible = evaluate_waiver_eligibility()
    - context.auto_approve = check_auto_approval()
    - context.calculation_timestamp = now

  next:
    when context.is_eligible and context.auto_approve:
      goto apply_auto_waiver
    otherwise:
      goto manual_review
```

**Benefit**: Explicit temporary variables vs entity fields

---

### Enhancement 3: Nested Contexts (Sub-workflows)

**Use Case**: Process multiple items with isolated contexts

**Proposed DSL**:
```
step: process_all_accounts
  actions:
    - for each account in delinquent_accounts:
        with isolated_context:
          - context.account = account
          - run fee_assessment_workflow
          - collect results
        merge results to parent_context
```

**Benefit**: Parallel processing with isolated state

---

### Enhancement 4: Context Snapshots (Debugging)

**Use Case**: Debugging and error recovery

**Proposed DSL**:
```
step: complex_calculation
  actions:
    - snapshot context as before_calculation

    - perform risky_operation

    - on error:
        - log snapshot before_calculation
        - restore context from before_calculation
        - try alternative_approach
```

**Benefit**: Time-travel debugging, error recovery

---

### Enhancement 5: Context Inspection (Observability)

**Use Case**: Monitoring and debugging

**Proposed DSL**:
```
step: fee_assessment
  observe:
    - log context.account.customer_id
    - log context.fee_amount
    - track context.mutation_count

  actions:
    - calculate and assess fee

  validate:
    - assert context.fee_amount > $0
    - assert context.fee_amount <= context.account.balance
```

**Benefit**: Built-in observability and assertions

---

## Recommended Approach

### Phase 1: Transparent DOM (Current) ✅

**No DSL changes**:
- DOM is pure implementation detail
- Current DSL syntax unchanged
- Compiler generates all DOM operations
- Business users unaware of DOM

**Advantages**:
- Zero learning curve
- Clean, simple DSL
- Full DOM benefits in implementation

### Phase 2: Optional DOM Features (Future) 🔮

**Add DOM-aware features only when needed**:
- `context.*` for temporary variables
- Transaction control keywords
- Nested context support

**Advantages**:
- Advanced users can leverage DOM explicitly
- Basic users continue with simple syntax
- Gradual learning curve

---

## Code Generation Examples

### Example 1: Simple Rule

**DSL**:
```
rule: calculate_grace_period_end
  given:
    - payment_due_date: date
    - grace_period_days: number

  calculate:
    grace_end = payment_due_date + grace_period_days

  return:
    - grace_period_end: date
```

**Generated Rust Code with DOM**:
```rust
pub fn calculate_grace_period_end(
  context: &mut ExecutionContext
) -> Result<(), RuleError> {
  // Read inputs from context
  let payment_due_date = context.get::<Date>("payment_due_date")?;
  let grace_period_days = context.get::<i32>("grace_period_days")?;

  // Business logic
  let grace_end = payment_due_date.add_days(grace_period_days);

  // Write outputs to context
  context.put("grace_period_end", grace_end)?;

  Ok(())
}
```

### Example 2: Workflow Step

**DSL**:
```
step: assess_fee
  actions:
    - load schedule from product.late_fee_schedule
    - calculate fee using calculate_fee_amount rule
    - create fee_transaction
    - update account.total_fees_ytd

  next: goto send_notification
```

**Generated Rust Code with DOM**:
```rust
pub fn step_assess_fee(
  context: &mut ExecutionContext
) -> Result<NextStep, WorkflowError> {
  // Load schedule
  let product = context.get::<CardProduct>("product")?;
  let schedule_id = product.late_fee_schedule_id;
  let schedule = db::load::<LateFeeSchedule>(schedule_id)?;
  context.put("schedule", schedule)?;

  // Calculate fee
  calculate_fee_amount(context)?;

  // Create transaction
  let fee_amount = context.get::<Money>("fee_amount")?;
  let account_id = context.get::<Account>("account")?.account_id;
  let transaction = FeeTransaction::new(account_id, fee_amount);
  db::insert(&transaction)?;
  context.put("fee_transaction", transaction)?;

  // Update account
  let mut account = context.get_mut::<Account>("account")?;
  account.total_fees_ytd += fee_amount;
  db::update(&account)?;
  context.put("account", account)?;

  Ok(NextStep::Goto("send_notification"))
}
```

---

## DOM Implementation Architecture

### Core DOM Structure

```rust
pub struct ExecutionContext {
  // Data storage
  data: HashMap<String, Box<dyn Any>>,

  // Audit trail
  mutations: Vec<Mutation>,

  // Transaction state
  transaction: Option<Transaction>,

  // Metadata
  execution_id: Uuid,
  started_at: Timestamp,

  // Caching
  cache: LruCache<String, Box<dyn Any>>,
}

impl ExecutionContext {
  pub fn get<T: 'static>(&self, path: &str) -> Result<&T, ContextError> {
    // Check cache first
    if let Some(cached) = self.cache.get(path) {
      return Ok(cached.downcast_ref::<T>().unwrap());
    }

    // Load from data
    let value = self.data.get(path)
      .ok_or(ContextError::NotFound(path.to_string()))?;

    // Type check
    value.downcast_ref::<T>()
      .ok_or(ContextError::TypeMismatch)
  }

  pub fn put<T: 'static>(&mut self, path: &str, value: T) -> Result<(), ContextError> {
    // Record mutation for audit
    self.mutations.push(Mutation {
      path: path.to_string(),
      old_value: self.data.get(path).cloned(),
      new_value: Box::new(value.clone()),
      timestamp: Timestamp::now(),
    });

    // Store value
    self.data.insert(path.to_string(), Box::new(value));

    // Invalidate cache
    self.cache.remove(path);

    Ok(())
  }

  pub fn commit(&mut self) -> Result<(), ContextError> {
    // Validate all constraints
    self.validate_constraints()?;

    // Persist all changes to database
    for (path, value) in &self.data {
      db::persist(path, value)?;
    }

    // Mark transaction as committed
    self.transaction = None;

    Ok(())
  }

  pub fn rollback(&mut self) {
    // Discard all changes
    self.data.clear();
    self.mutations.clear();
    self.transaction = None;
  }
}
```

---

## Summary & Recommendation

### Current State: ✅ EXCELLENT COMPATIBILITY

The DOM architecture fits **perfectly** with the current DSL:
- No DSL syntax changes needed
- DOM is transparent implementation detail
- All benefits of DOM (transactions, audit, isolation) achieved
- Business users see clean, simple DSL

### Recommendation: **NO DSL CHANGES NOW**

**Rationale**:
1. Current DSL already expresses intent clearly
2. DOM translation is straightforward for compiler
3. Adding DOM-specific syntax would complicate DSL unnecessarily
4. "WHAT not HOW" principle preserved

### Future Enhancements: 🔮 OPTIONAL

When advanced use cases emerge:
- Add `context.*` syntax for temporary variables
- Add transaction control keywords
- Add nested context support

But these are **enhancements**, not requirements.

### Decision Point: 🤔

**Should we add ANY DOM-visible features to DSL now?**

My recommendation: **NO** - Keep it simple. The DOM architecture is brilliant for implementation, but DSL should stay focused on business logic, not execution mechanics.

**Awaiting your approval** before making any DSL changes.
