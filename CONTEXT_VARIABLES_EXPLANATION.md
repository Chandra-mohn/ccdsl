# Context Variables - Explanation

**Status**: Optional future enhancement (NOT in current DSL)
**Decision**: Keep DSL as-is, DOM transparent

---

## What Are Context Variables?

**Context variables** are temporary workflow state that exists only during execution, not persisted to database.

### The Problem They Solve

**Scenario**: You need to store intermediate calculation results to use in multiple places within a workflow.

**Current Approach** (DSL as-is):
```
step: evaluate_waiver
  actions:
    - evaluate waiver using evaluate_waiver_eligibility rule
      # This puts results in: waiver.eligible, waiver.auto_approve

  next:
    when waiver.eligible and waiver.auto_approve:
      goto apply_auto_waiver
    when waiver.eligible:
      goto assess_with_waiver_option
    otherwise:
      goto assess_fee
```

**Problem**: Where does `waiver` object live? Options:

1. **Create temporary entity** (heavyweight):
   ```
   define entity: waiver_evaluation_result
     pattern: ... what pattern? Not master_data, not ledger...
     eligible: boolean
     auto_approve: boolean
   ```
   This creates a database table for temporary data! Overkill.

2. **Use workflow inputs/outputs** (pollutes interface):
   ```
   define workflow: fee_assessment
     inputs:
       - account: account
       - fee_type: text

     outputs:
       - fee_transaction: fee_transaction
       - waiver_applied: boolean
       - temp_waiver_eligible: boolean  # ← Shouldn't be output!
       - temp_waiver_auto_approve: boolean  # ← Internal detail!
   ```
   Temporary variables leak into public interface.

3. **Hardcode in generated code** (loses DSL expressiveness):
   ```
   # DSL says nothing about where waiver result is stored
   # Compiler hardcodes it as local variable
   # Hard to reason about, debug, or modify
   ```

### The `context.*` Solution (Optional Future Feature)

**Explicit temporary variables in workflow scope**:

```
step: evaluate_waiver
  actions:
    - evaluate waiver using evaluate_waiver_eligibility rule
    - context.is_eligible = waiver.eligible
    - context.auto_approve = waiver.auto_approve
    - context.evaluation_timestamp = now

  next:
    when context.is_eligible and context.auto_approve:
      goto apply_auto_waiver
    when context.is_eligible:
      goto assess_with_waiver_option
    otherwise:
      goto assess_fee
```

**Benefits**:
- **Explicit**: Clear that these are temporary workflow variables
- **Not persisted**: `context.*` lives only in execution DOM, not database
- **Scoped**: Visible only within this workflow execution
- **Type-safe**: Compiler can validate types
- **Debuggable**: Can inspect context.* in debugger

**Implementation** (in DOM):
```rust
// These live in ExecutionContext, not database
context.put("context.is_eligible", true);
context.put("context.auto_approve", false);
context.put("context.evaluation_timestamp", Timestamp::now());

// Not persisted when workflow completes
context.commit();  // Only entities persisted, not context.* variables
```

---

## Current DSL Works Fine Without This

**Why you don't need `context.*` now:**

### Approach 1: Use Return Values Directly

```
step: evaluate_waiver
  actions:
    - evaluate waiver using evaluate_waiver_eligibility rule

  next:
    # Rule returns: waiver.eligible, waiver.auto_approve
    # These are implicitly in context, can be referenced
    when waiver.eligible and waiver.auto_approve:
      goto apply_auto_waiver
```

**Compiler generates**:
```rust
// Rule puts results in context
context.put("waiver.eligible", true);
context.put("waiver.auto_approve", false);

// Conditional reads from context
let eligible = context.get::<bool>("waiver.eligible");
let auto_approve = context.get::<bool>("waiver.auto_approve");
if eligible && auto_approve { ... }
```

**Works perfectly!** No `context.*` syntax needed.

### Approach 2: Implicit Intermediate Values

```
step: calculate_and_apply_fee
  actions:
    - calculate fee using calculate_fee_amount rule
      # Implicitly creates: fee_amount, fee_reason, cap_applied

    - create fee_transaction with:
        amount: fee_amount  # ← References rule output
        reason: fee_reason

    - update account.total_fees_ytd += fee_amount
```

**Compiler knows** `fee_amount` came from the rule, stores it in context automatically.

---

## When Would `context.*` Be Useful?

### Use Case 1: Complex Multi-Step Calculations

**Without `context.*`**:
```
step: complex_eligibility
  actions:
    - calculate tenure_score from customer.customer_since
    - calculate payment_score from customer.on_time_percentage
    - calculate balance_score from account.current_balance
    - calculate weighted_score from tenure_score, payment_score, balance_score

  next:
    when weighted_score > 80:  # Where is weighted_score stored?
      goto approve
```

**Problem**: Where does `weighted_score` live? Rule outputs pollute namespace.

**With `context.*` (hypothetical)**:
```
step: complex_eligibility
  actions:
    - context.tenure_score = calculate_tenure_score(customer.customer_since)
    - context.payment_score = calculate_payment_score(customer.on_time_percentage)
    - context.balance_score = calculate_balance_score(account.current_balance)
    - context.weighted_score = (
        context.tenure_score * 0.3 +
        context.payment_score * 0.5 +
        context.balance_score * 0.2
      )

  next:
    when context.weighted_score > 80:
      goto approve
```

**Clearer**: Explicit temporary calculation state.

### Use Case 2: Loop State

**Without `context.*`**:
```
step: process_multiple_fees
  actions:
    - for each fee_type in [late_fee, over_limit_fee, returned_payment_fee]:
        # How do we track total_fees_assessed across iterations?
        - assess fee_type
```

**With `context.*` (hypothetical)**:
```
step: process_multiple_fees
  actions:
    - context.total_fees_assessed = $0.00
    - context.fees_assessed_count = 0

    - for each fee_type in [late_fee, over_limit_fee, returned_payment_fee]:
        - assess fee_type
        - context.total_fees_assessed += assessed_fee_amount
        - context.fees_assessed_count += 1

  next:
    when context.total_fees_assessed > $100:
      goto notify_customer_of_high_fees
```

**Clearer**: Explicit accumulator variables.

### Use Case 3: Debugging Snapshots

**With `context.*` (hypothetical)**:
```
step: risky_calculation
  actions:
    - context.before_balance = account.current_balance
    - context.before_limit = account.credit_limit

    - perform complex_balance_adjustment

    - context.after_balance = account.current_balance
    - context.after_limit = account.credit_limit

    - log "Balance changed from {context.before_balance} to {context.after_balance}"
```

**Useful for**: Debugging, audit trails, monitoring.

---

## Current DSL Handles This Implicitly

**You DON'T need `context.*` because the compiler can:**

1. **Infer temporary state** from rule outputs
2. **Automatically scope variables** to workflow execution
3. **Generate context operations** transparently

**Example - Current DSL**:
```
rule: calculate_weighted_score
  given:
    - customer: customer
    - account: account

  calculate:
    tenure_score = calculate_tenure(customer.customer_since)
    payment_score = calculate_payment(customer.on_time_percentage)
    balance_score = calculate_balance(account.current_balance)

    weighted_score = (
      tenure_score * 0.3 +
      payment_score * 0.5 +
      balance_score * 0.2
    )

  return:
    - weighted_score: number
```

**Then in workflow**:
```
step: evaluate
  actions:
    - calculate weighted_score using calculate_weighted_score rule

  next:
    when weighted_score > 80:  # Compiler knows this came from rule
      goto approve
```

**Works perfectly!** The compiler:
- Knows `weighted_score` comes from the rule
- Stores it in `context.get("weighted_score")`
- Scopes it to this workflow execution
- Doesn't persist it to database

---

## Why `context.*` Would Be "Nice to Have"

### Benefit 1: Explicitness

**Implicit** (current):
```
- calculate fee using rule
- when fee_amount > $50:  # Where did fee_amount come from? Rule return.
```

**Explicit** (with `context.*`):
```
- calculate fee using rule
- context.calculated_fee = fee_amount  # Explicitly stored
- when context.calculated_fee > $50:  # Clear it's from context
```

### Benefit 2: Namespace Clarity

**Without `context.*`**:
```
# Are these entity fields or temporary variables?
when fee_amount > threshold:
when customer_eligible == yes:
when calculation_result > 100:
```

**With `context.*`**:
```
# Clear distinction:
when account.current_balance > $1000:  # Entity field
when context.calculated_fee > $50:     # Temporary variable
```

### Benefit 3: Advanced Control

```
step: complex_workflow
  actions:
    - context.checkpoint_1 = snapshot current_state
    - perform risky_operation
    - context.checkpoint_2 = snapshot current_state

    - when operation_failed:
        restore from context.checkpoint_1

    - context.cleanup()  # Explicit cleanup
```

---

## Recommendation: Wait and See

**Current approach (DSL as-is)**:
- Compiler handles temporary state implicitly
- DOM stores all intermediate values
- No special syntax needed
- Works great for 90% of cases

**If we find we need `context.*` later**:
- Add it as **optional enhancement**
- Backward compatible (existing DSL still works)
- Use only when needed for clarity

**Example of backward compatible addition**:
```
# Old DSL (still works)
step: calculate
  actions:
    - calculate fee using rule
  next:
    when fee_amount > $50: goto manual_review

# New DSL (optional explicit context)
step: calculate
  actions:
    - calculate fee using rule
    - context.fee = fee_amount  # Explicitly store for later
  next:
    when context.fee > $50: goto manual_review
```

Both work, choice is stylistic.

---

## Summary

**What `context.*` means**:
- Explicit temporary variables in workflow scope
- Lives only in execution DOM, not persisted
- Clear distinction from entity fields

**Do you need it now?**:
- **NO** - Current DSL handles this implicitly
- Compiler stores all intermediate values in DOM automatically
- No special syntax needed

**When might you want it?**:
- Complex multi-step calculations with many intermediate values
- Loop accumulators
- Debugging snapshots
- Namespace clarity

**Decision**: ✅ Keep DSL as-is, add `context.*` only if real use cases emerge.

---

## Comparison Table

| Aspect | Current DSL (Implicit) | With `context.*` (Optional) |
|--------|------------------------|----------------------------|
| **Simplicity** | ✅ Simple, clean | ⚠️ Slightly more verbose |
| **Explicitness** | ⚠️ Implicit temporary state | ✅ Explicit temporary state |
| **Learning Curve** | ✅ Easy, no new concepts | ⚠️ New concept to learn |
| **Namespace** | ⚠️ All values look similar | ✅ Clear entity vs temp distinction |
| **90% use cases** | ✅ Works great | ✅ Works great |
| **Complex cases** | ⚠️ Can be unclear | ✅ More explicit control |
| **Backward compatible** | N/A | ✅ Can add later without breaking existing DSL |

**Conclusion**: Current approach is excellent. Add `context.*` only if we find real need for it.
