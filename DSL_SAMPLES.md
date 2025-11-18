# Credit Card DSL - Sample Code

**Version**: 2.0
**Last Updated**: 2025-11-11
**Companion Document**: DSL_LANGUAGE_SPECIFICATION.md

---

## Table of Contents

### Quick Navigation
- [I. BASIC EXAMPLES](#i-basic-examples)
  - [Simple Entity](#simple-entity)
  - [Basic Workflow](#basic-workflow)
  - [Simple Rule](#simple-rule)
  - [Parameter Definition](#parameter-definition)

- [II. COMPLETE DOMAIN EXAMPLE - LATE FEE CALCULATION](#ii-complete-domain-example---late-fee-calculation)
  - [Overview](#overview)
  - [Master Data Entities](#master-data-entities)
  - [Versioned Configuration](#versioned-configuration)
  - [Operational Parameters (PCF)](#operational-parameters-pcf)
  - [Immutable Ledger](#immutable-ledger)
  - [State Machine](#state-machine)
  - [Event Log](#event-log)
  - [Business Logic Rules](#business-logic-rules)
  - [Complete Workflow](#complete-workflow)

- [III. PATTERN SHOWCASE](#iii-pattern-showcase)
  - [Master Data Pattern](#master-data-pattern)
  - [Immutable Ledger Pattern](#immutable-ledger-pattern)
  - [Versioned Configuration Pattern](#versioned-configuration-pattern)
  - [Operational Parameters Pattern](#operational-parameters-pattern)
  - [Event Log Pattern](#event-log-pattern)
  - [State Machine Pattern](#state-machine-pattern)
  - [Temporal Data Pattern](#temporal-data-pattern)
  - [Reference Data Pattern](#reference-data-pattern)
  - [Business Logic Pattern](#business-logic-pattern)

- [IV. WORKFLOW EXAMPLES](#iv-workflow-examples)
  - [Linear Workflow](#linear-workflow)
  - [Conditional Workflow](#conditional-workflow)
  - [Parallel Execution](#parallel-execution)
  - [Error Handling](#error-handling)

- [V. RULE EXAMPLES](#v-rule-examples)
  - [Calculation Rules](#calculation-rules)
  - [Validation Rules](#validation-rules)
  - [Eligibility Rules](#eligibility-rules)
  - [Complex Decision Trees](#complex-decision-trees)

- [VI. ADVANCED EXAMPLES](#vi-advanced-examples)
  - [Pattern Composition](#pattern-composition)
  - [BIAN Integration](#bian-integration)
  - [Temporal Queries](#temporal-queries)
  - [Regulatory Compliance](#regulatory-compliance)

---

## I. BASIC EXAMPLES

### Simple Entity

**Purpose**: Basic customer entity with master_data pattern

```
define entity: customer
  pattern: master_data
  business_domain: "Credit Card (BIAN)"

  identity:
    customer_id: text, unique, required

  profile:
    first_name: text, required
    last_name: text, required
    customer_segment: text, values: premier | preferred | standard

  metrics:
    credit_score: number, between 300 and 850

  must:
    - customer_segment is one of premier | preferred | standard
    - credit_score >= 300 and credit_score <= 850
```

**Auto-Generated**:
- `customer` table with all fields
- `customer_history` table for audit trail
- CRUD operations: `add_customer()`, `update_customer()`, `get_customer()`
- Automatic constraint validation on all operations

---

### Basic Workflow

**Purpose**: Simple linear workflow

```
define workflow: customer_onboarding
  inputs:
    - customer_data: customer

  outputs:
    - onboarding_complete: boolean
    - customer_id: text

  step: validate_customer
    actions:
      - validate customer_data using customer_validation_rules

    next:
      when validation_passed: goto create_account
      otherwise: goto reject_application

  step: create_account
    actions:
      - create customer record
      - generate customer_id

    next: goto complete_onboarding

  step: reject_application
    actions:
      - log rejection_reason
      - notify customer service

    next: goto end_workflow

  step: complete_onboarding
    actions:
      - set onboarding_complete to yes
      - send welcome_email

    next: goto end_workflow

  step: end_workflow
    actions:
      - return results
```

---

### Simple Rule

**Purpose**: Basic calculation rule

```
define rules: interest_calculation

  rule: calculate_daily_interest
    given:
      - balance: money
      - annual_rate: percentage

    calculate:
      daily_rate = annual_rate / 365
      interest = balance * daily_rate

    return:
      - daily_interest: money
```

---

### Parameter Definition

**Purpose**: Operational parameter (PCF)

```
define parameter: grace_period_days
  pattern: operational_parameters
  business_domain: "Credit Card (BIAN)"

  type: number
  default: 21
  constraints:
    - value >= 0
    - value <= 90

  hot_reload: yes
  validation: must be between 0 and 90 days
```

---

## II. COMPLETE DOMAIN EXAMPLE - LATE FEE CALCULATION

### Overview

**Domain**: Late Fee Assessment and Management
**Business Context**: Credit card late payment fee calculation with regulatory compliance
**Patterns Used**: 7 of 9 patterns (excludes only temporal_data and reference_data)

**BIAN Service Domains Referenced**:
- Customer Agreement (11)
- Account Management (17)
- Payment Execution (141)
- Fee Processing (custom)

---

### Master Data Entities

#### Customer Entity

```
define entity: customer
  pattern: master_data
  business_domain: "Credit Card (BIAN) - Customer Agreement (11)"

  identity:
    customer_id: text, unique, required
    external_customer_id: text, unique

  profile:
    first_name: text, required
    last_name: text, required
    email: email, required
    phone: phone
    customer_segment: text, values: premier | preferred | standard
    customer_since: date, required, cannot_change

  account_status:
    account_status: text, values: active | suspended | closed | delinquent
    status_reason: text
    status_changed_date: date

  credit_profile:
    credit_score: number, between 300 and 850
    credit_limit: money, required
    available_credit: money
    utilization_ratio: percentage

  payment_history:
    on_time_percentage: number, between 0 and 100
    consecutive_on_time_payments: number, default 0
    last_payment_date: date
    last_payment_amount: money

  fee_tracking:
    total_fees_ytd: money, default $0.00
    fee_waivers_granted_ytd: number, default 0
    fee_waivers_used_ytd: number, default 0

  relationships:
    - has many accounts
    - has many fee_transactions

  must:
    - customer_segment is one of premier | preferred | standard
    - account_status is one of active | suspended | closed | delinquent
    - credit_score >= 300 and credit_score <= 850
    - credit_limit > $0.00
    - available_credit <= credit_limit
    - on_time_percentage >= 0 and on_time_percentage <= 100
    - total_fees_ytd >= $0.00
    - fee_waivers_used_ytd <= fee_waivers_granted_ytd
```

#### Account Entity

```
define entity: account
  pattern: master_data
  business_domain: "Credit Card (BIAN) - Account Management (17)"

  identity:
    account_id: text, unique, required
    account_number: text, unique, required, cannot_change

  ownership:
    customer_id: text, required
    card_product_id: text, required

  balances:
    current_balance: money, default $0.00
    minimum_payment_due: money, default $0.00
    total_fees_assessed: money, default $0.00

  payment_tracking:
    payment_due_date: date, required
    days_past_due: number, default 0
    last_payment_date: date
    last_payment_amount: money
    grace_period_end: date

  fee_assessment:
    last_fee_assessed_date: date
    late_fee_count_ytd: number, default 0
    fee_waiver_eligible: boolean, default no

  relationships:
    - belongs_to customer
    - belongs_to card_product
    - has many fee_transactions
    - has many payment_transactions

  must:
    - current_balance >= $0.00
    - minimum_payment_due >= $0.00
    - days_past_due >= 0
    - late_fee_count_ytd >= 0
    - payment_due_date must be valid date
```

#### Card Product Entity

```
define entity: card_product
  pattern: master_data
  business_domain: "Credit Card (BIAN) - Product Directory (18)"

  identity:
    card_product_id: text, unique, required
    product_name: text, required
    product_code: text, unique, required

  product_type:
    product_category: text, values: premium | rewards | standard | secured
    target_segment: text, values: premier | preferred | standard

  fee_configuration:
    late_fee_schedule_id: text, required
    waiver_policy_id: text, required

  product_status:
    is_active: boolean, default yes
    effective_date: date, required
    discontinuation_date: date

  relationships:
    - references late_fee_schedule
    - references waiver_policy
    - has many accounts

  must:
    - product_category is one of premium | rewards | standard | secured
    - target_segment is one of premier | preferred | standard
    - effective_date <= discontinuation_date when discontinuation_date is not null
```

---

### Versioned Configuration

#### Late Fee Schedule

```
define entity: late_fee_schedule
  pattern: versioned_configuration
  business_domain: "Credit Card (BIAN) - Fee Configuration"

  identity:
    schedule_id: text, unique, required
    schedule_name: text, required

  versioning:
    version: number, required, auto_increment
    effective_date: date, required
    expiration_date: date
    is_active: boolean, default yes

  fee_tiers:
    tier_1_threshold: money, required
    tier_1_amount: money, required

    tier_2_threshold: money, required
    tier_2_amount: money, required

    tier_3_threshold: money, required
    tier_3_amount: money, required

  fee_caps:
    maximum_fee: money, required
    minimum_fee: money, required

  regulatory:
    regulatory_cap: money, required
    cap_basis: text, values: federal | state | product

  must:
    - tier_1_threshold < tier_2_threshold
    - tier_2_threshold < tier_3_threshold
    - tier_1_amount < tier_2_amount
    - tier_2_amount < tier_3_amount
    - minimum_fee <= maximum_fee
    - maximum_fee <= regulatory_cap
    - effective_date <= expiration_date when expiration_date is not null
```

#### Fee Waiver Policy

```
define entity: waiver_policy
  pattern: versioned_configuration
  business_domain: "Credit Card (BIAN) - Policy Management"

  identity:
    policy_id: text, unique, required
    policy_name: text, required

  versioning:
    version: number, required, auto_increment
    effective_date: date, required
    expiration_date: date
    is_active: boolean, default yes

  eligibility:
    min_customer_tenure_months: number, required
    min_on_time_percentage: number, required
    min_credit_score: number
    max_waivers_per_year: number, required

  auto_approval:
    auto_approve_enabled: boolean, default no
    auto_approve_max_waivers: number, default 1
    auto_approve_tenure_required: number, default 12

  manual_review:
    manual_review_threshold: money
    requires_manager_approval: boolean, default no

  must:
    - min_on_time_percentage >= 0 and min_on_time_percentage <= 100
    - min_credit_score >= 300 and min_credit_score <= 850
    - max_waivers_per_year >= 1
    - auto_approve_max_waivers <= max_waivers_per_year
```

---

### Operational Parameters (PCF)

```
define parameter: grace_period_days
  pattern: operational_parameters
  business_domain: "Credit Card (BIAN) - Payment Processing"

  type: number
  default: 21
  constraints:
    - value >= 0
    - value <= 90

  hot_reload: yes
  validation: must be between 0 and 90 days
  documentation: "Number of days after due date before late fee is assessed"


define parameter: fee_assessment_batch_size
  pattern: operational_parameters
  business_domain: "Credit Card (BIAN) - Batch Processing"

  type: number
  default: 1000
  constraints:
    - value >= 100
    - value <= 10000

  hot_reload: yes
  validation: must be between 100 and 10000 accounts
  documentation: "Number of accounts to process in each batch run"


define parameter: auto_waiver_limit
  pattern: operational_parameters
  business_domain: "Credit Card (BIAN) - Fee Waiver Management"

  type: money
  default: $25.00
  constraints:
    - value >= $0.00
    - value <= $100.00

  hot_reload: yes
  validation: must be between $0 and $100
  documentation: "Maximum fee amount eligible for automatic waiver approval"
```

---

### Immutable Ledger

```
define entity: fee_transaction
  pattern: immutable_ledger
  business_domain: "Credit Card (BIAN) - Payment Execution (141)"

  identity:
    transaction_id: text, unique, required, cannot_change
    account_id: text, required, cannot_change

  transaction_details:
    transaction_type: text, values: late_fee | returned_payment_fee | over_limit_fee, cannot_change
    transaction_date: timestamp, required, cannot_change
    posting_date: date, required, cannot_change

  financial:
    fee_amount: money, required, cannot_change
    balance_before: money, required, cannot_change
    balance_after: money, required, cannot_change

  assessment_context:
    days_past_due: number, cannot_change
    schedule_version: number, cannot_change
    regulatory_cap_applied: boolean, default no, cannot_change

  waiver_tracking:
    waiver_applied: boolean, default no, cannot_change
    waiver_amount: money, default $0.00, cannot_change
    waiver_reason: text, cannot_change
    waiver_approved_by: text, cannot_change

  reversal:
    is_reversed: boolean, default no
    reversal_transaction_id: text
    reversal_date: timestamp
    reversal_reason: text

  relationships:
    - belongs_to account, cannot_change
    - belongs_to customer, cannot_change

  must:
    - transaction_type is one of late_fee | returned_payment_fee | over_limit_fee
    - fee_amount >= $0.00
    - waiver_amount >= $0.00
    - waiver_amount <= fee_amount
    - balance_after == balance_before + fee_amount when not is_reversed
```

---

### State Machine

```
define entity: fee_assessment_state
  pattern: state_machine
  business_domain: "Credit Card (BIAN) - Fee Processing Workflow"

  identity:
    assessment_id: text, unique, required
    account_id: text, required

  current_state: text, values: pending | evaluating | approved | waived | disputed | reversed

  initial_state: pending

  transitions:
    from pending:
      - to evaluating, when assessment_initiated

    from evaluating:
      - to approved, when fee_calculated_and_within_limits
      - to waived, when waiver_approved
      - to pending, when requires_retry

    from approved:
      - to reversed, when reversal_requested
      - to disputed, when customer_disputes

    from waived:
      - to reversed, when waiver_revoked

    from disputed:
      - to approved, when dispute_rejected
      - to reversed, when dispute_approved
      - to waived, when dispute_results_in_waiver

  state_data:
    entered_state_at: timestamp
    state_reason: text
    processed_by: text

  must:
    - current_state is one of pending | evaluating | approved | waived | disputed | reversed
```

---

### Event Log

```
define entity: fee_assessment_event
  pattern: event_log
  business_domain: "Credit Card (BIAN) - Audit Trail"

  identity:
    event_id: text, unique, required, cannot_change
    assessment_id: text, required, cannot_change

  event_metadata:
    event_type: text, required, cannot_change
    event_timestamp: timestamp, required, cannot_change
    event_source: text, required, cannot_change

  event_details:
    old_state: text, cannot_change
    new_state: text, cannot_change
    action_taken: text, cannot_change
    action_by: text, cannot_change

  context_data:
    customer_id: text, cannot_change
    account_id: text, cannot_change
    fee_amount: money, cannot_change
    waiver_applied: boolean, cannot_change

  audit:
    ip_address: text, cannot_change
    user_agent: text, cannot_change
    correlation_id: text, cannot_change

  must:
    - event_timestamp must be valid timestamp
    - event_type must not be empty
```

---

### Business Logic Rules

#### Fee Calculation Rule

```
define rules: late_fee_calculation

  rule: calculate_fee_amount
    given:
      - account: account
      - schedule: late_fee_schedule
      - parameters: operational_parameters

    calculate:
      base_fee = when account.current_balance >= schedule.tier_3_threshold:
                   schedule.tier_3_amount
                 when account.current_balance >= schedule.tier_2_threshold:
                   schedule.tier_2_amount
                 when account.current_balance >= schedule.tier_1_threshold:
                   schedule.tier_1_amount
                 otherwise:
                   schedule.minimum_fee

      capped_fee = minimum of base_fee and schedule.maximum_fee
      regulatory_capped_fee = minimum of capped_fee and schedule.regulatory_cap

      final_fee = regulatory_capped_fee

    return:
      - fee_amount: money
      - cap_applied: boolean
      - fee_reason: text
```

#### Waiver Eligibility Rule

```
define rules: fee_waiver_evaluation

  rule: evaluate_waiver_eligibility
    given:
      - customer: customer
      - account: account
      - policy: waiver_policy
      - fee_amount: money

    calculate:
      customer_tenure_months = months_between(customer.customer_since, today)

      is_eligible =
        customer_tenure_months >= policy.min_customer_tenure_months and
        customer.on_time_percentage >= policy.min_on_time_percentage and
        customer.fee_waivers_used_ytd < policy.max_waivers_per_year

      auto_approve =
        is_eligible and
        policy.auto_approve_enabled and
        customer_tenure_months >= policy.auto_approve_tenure_required and
        customer.fee_waivers_used_ytd < policy.auto_approve_max_waivers and
        fee_amount <= auto_waiver_limit

      requires_manual_review =
        is_eligible and
        not auto_approve and
        fee_amount > policy.manual_review_threshold

    return:
      - eligible: boolean
      - auto_approve: boolean
      - requires_manual_review: boolean
      - eligibility_reason: text
```

#### Grace Period Rule

```
define rules: grace_period_calculation

  rule: calculate_grace_period_end
    given:
      - payment_due_date: date
      - grace_period_days: number

    calculate:
      grace_end = payment_due_date + grace_period_days

    return:
      - grace_period_end: date


  rule: should_assess_fee
    given:
      - account: account
      - current_date: date
      - grace_period_end: date

    when:
      current_date > grace_period_end and
      account.minimum_payment_due > $0.00 and
      account.days_past_due > 0

    then:
      return assess_fee: yes

    otherwise:
      return assess_fee: no
```

---

### Complete Workflow

```
define workflow: late_fee_assessment
  inputs:
    - account: account
    - customer: customer
    - processing_date: date

  outputs:
    - fee_assessed: boolean
    - fee_amount: money
    - waiver_applied: boolean
    - assessment_state: text

  step: initialize_assessment
    actions:
      - load parameters from system
      - load product from account.card_product
      - load schedule from product.late_fee_schedule
      - load waiver_policy from product.waiver_policy
      - create fee_assessment_state with initial_state pending

    next: goto check_grace_period

  step: check_grace_period
    actions:
      - calculate grace_period_end using grace_period_calculation rule
      - calculate should_assess using grace_period_calculation rule

    next:
      when should_assess == yes: goto evaluate_waiver
      otherwise: goto complete_without_fee

  step: evaluate_waiver
    actions:
      - transition fee_assessment_state to evaluating
      - evaluate waiver using fee_waiver_evaluation rule
      - log fee_assessment_event with event_type waiver_evaluation

    next:
      when waiver.eligible and waiver.auto_approve: goto apply_auto_waiver
      when waiver.eligible and waiver.requires_manual_review: goto queue_manual_review
      otherwise: goto calculate_and_assess_fee

  step: apply_auto_waiver
    actions:
      - transition fee_assessment_state to waived
      - increment customer.fee_waivers_used_ytd by 1
      - log fee_assessment_event with event_type auto_waiver_applied
      - set waiver_applied to yes

    next: goto complete_without_fee

  step: queue_manual_review
    actions:
      - create manual_review_task
      - notify fee_review_team
      - log fee_assessment_event with event_type manual_review_queued

    next: goto await_manual_decision

  step: await_manual_decision
    wait_for: manual_review_completion

    next:
      when manual_decision == approve_waiver: goto apply_manual_waiver
      when manual_decision == deny_waiver: goto calculate_and_assess_fee
      otherwise: goto error_state

  step: apply_manual_waiver
    actions:
      - transition fee_assessment_state to waived
      - increment customer.fee_waivers_used_ytd by 1
      - log fee_assessment_event with event_type manual_waiver_applied
      - set waiver_applied to yes

    next: goto complete_without_fee

  step: calculate_and_assess_fee
    actions:
      - calculate fee using late_fee_calculation rule
      - validate fee_amount >= schedule.minimum_fee
      - validate fee_amount <= schedule.regulatory_cap

    next: goto post_fee_transaction

  step: post_fee_transaction
    actions:
      - create fee_transaction with:
          transaction_type: late_fee
          fee_amount: calculated_fee
          balance_before: account.current_balance
          balance_after: account.current_balance + calculated_fee
          days_past_due: account.days_past_due
          schedule_version: schedule.version
          waiver_applied: no

      - update account.current_balance += calculated_fee
      - update account.total_fees_assessed += calculated_fee
      - update account.last_fee_assessed_date = processing_date
      - increment account.late_fee_count_ytd by 1
      - increment customer.total_fees_ytd by calculated_fee

      - transition fee_assessment_state to approved
      - log fee_assessment_event with event_type fee_assessed

    next: goto send_notification

  step: send_notification
    actions:
      - send fee_assessment_notice to customer
      - create customer_communication_record
      - log fee_assessment_event with event_type notification_sent

    next: goto complete_assessment

  step: complete_without_fee
    actions:
      - set fee_assessed to no
      - set fee_amount to $0.00
      - log fee_assessment_event with event_type assessment_completed_no_fee

    next: goto complete_assessment

  step: complete_assessment
    actions:
      - return results with:
          fee_assessed: fee_assessed
          fee_amount: fee_amount
          waiver_applied: waiver_applied
          assessment_state: fee_assessment_state.current_state

  step: error_state
    actions:
      - log error_event
      - notify support_team
      - rollback all changes

    next: goto end_workflow
```

---

## III. PATTERN SHOWCASE

### Master Data Pattern

**Use Case**: Customer profiles, account details
**Characteristics**: Mutable, audited, versioned

```
define entity: customer_profile
  pattern: master_data

  identity:
    profile_id: text, unique, required

  personal:
    first_name: text, required
    last_name: text, required
    date_of_birth: date, required, cannot_change

  contact:
    email: email, required
    phone: phone
    preferred_contact_method: text, values: email | phone | sms

  must:
    - email must be valid email format
    - preferred_contact_method is one of email | phone | sms
```

**Auto-Generated**:
```sql
-- Main table
CREATE TABLE customer_profile (
  profile_id TEXT PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  ...
  version INT,
  created_at TIMESTAMP,
  updated_at TIMESTAMP
);

-- History table
CREATE TABLE customer_profile_history (
  history_id SERIAL PRIMARY KEY,
  profile_id TEXT,
  ...
  valid_from TIMESTAMP,
  valid_to TIMESTAMP
);
```

---

### Immutable Ledger Pattern

**Use Case**: Financial transactions, audit records
**Characteristics**: Append-only, cannot modify, reversal-based corrections

```
define entity: payment_transaction
  pattern: immutable_ledger

  identity:
    transaction_id: text, unique, required, cannot_change

  transaction:
    transaction_type: text, values: payment | refund | adjustment, cannot_change
    transaction_date: timestamp, required, cannot_change
    amount: money, required, cannot_change

  reversal:
    is_reversed: boolean, default no
    reversal_transaction_id: text
    reversal_reason: text

  must:
    - amount > $0.00
    - transaction_type is one of payment | refund | adjustment
```

**Auto-Generated Operations**:
```rust
// Append only - no update()
fn add_payment_transaction(tx: PaymentTransaction) -> Result<(), Error>;
fn get_payment_transaction(id: &str) -> Result<PaymentTransaction, Error>;
fn reverse_payment_transaction(id: &str, reason: &str) -> Result<(), Error>;
```

---

### Versioned Configuration Pattern

**Use Case**: Fee schedules, policy configurations
**Characteristics**: Point-in-time queries, version tracking

```
define entity: pricing_schedule
  pattern: versioned_configuration

  identity:
    schedule_id: text, unique, required

  versioning:
    version: number, required, auto_increment
    effective_date: date, required
    expiration_date: date
    is_active: boolean, default yes

  pricing:
    base_price: money, required
    discount_percentage: percentage

  must:
    - effective_date <= expiration_date when expiration_date is not null
    - discount_percentage >= 0 and discount_percentage <= 100
```

**Auto-Generated Queries**:
```rust
// Point-in-time query
fn get_pricing_at_date(schedule_id: &str, date: Date) -> Result<PricingSchedule, Error>;

// Version history
fn get_version_history(schedule_id: &str) -> Vec<PricingSchedule>;
```

---

### Operational Parameters Pattern

**Use Case**: Runtime configuration, hot-reloadable settings
**Characteristics**: PCF (Product Configuration Fields), no deployment

```
define parameter: daily_transaction_limit
  pattern: operational_parameters

  type: money
  default: $5000.00
  constraints:
    - value >= $100.00
    - value <= $50000.00

  hot_reload: yes
  validation: must be between $100 and $50000
  documentation: "Maximum daily transaction limit per card"
```

**Runtime Behavior**:
```rust
// Hot reload without restart
let limit = parameters.get::<Money>("daily_transaction_limit");
// Updated value available immediately
```

---

### Event Log Pattern

**Use Case**: Audit trails, system events
**Characteristics**: Append-only, immutable, timestamped

```
define entity: system_event
  pattern: event_log

  identity:
    event_id: text, unique, required, cannot_change

  event:
    event_type: text, required, cannot_change
    event_timestamp: timestamp, required, cannot_change
    event_source: text, required, cannot_change

  payload:
    event_data: text, cannot_change
    user_id: text, cannot_change

  must:
    - event_type must not be empty
```

---

### State Machine Pattern

**Use Case**: Order processing, approval workflows
**Characteristics**: Valid transitions, state validation

```
define entity: order_state
  pattern: state_machine

  identity:
    order_id: text, unique, required

  current_state: text, values: draft | submitted | approved | rejected | completed

  initial_state: draft

  transitions:
    from draft:
      - to submitted, when order_validated

    from submitted:
      - to approved, when approval_granted
      - to rejected, when approval_denied

    from approved:
      - to completed, when order_fulfilled

  state_data:
    entered_state_at: timestamp
    state_reason: text
```

---

### Temporal Data Pattern

**Use Case**: Prices effective over time
**Characteristics**: Bi-temporal, effective/valid time tracking

```
define entity: product_price
  pattern: temporal_data

  identity:
    price_id: text, unique, required

  temporal:
    effective_from: date, required
    effective_to: date
    valid_from: timestamp, required
    valid_to: timestamp

  pricing:
    product_id: text, required
    price: money, required

  must:
    - effective_from <= effective_to when effective_to is not null
```

**Temporal Queries**:
```rust
// Price as of specific date
fn get_price_effective_at(product_id: &str, date: Date) -> Money;

// Price known at specific time
fn get_price_valid_at(product_id: &str, timestamp: Timestamp) -> Money;
```

---

### Reference Data Pattern

**Use Case**: Country codes, currency lists
**Characteristics**: Read-only, rarely changes, cached

```
define entity: country_code
  pattern: reference_data

  identity:
    country_code: text, unique, required, cannot_change

  details:
    country_name: text, required, cannot_change
    iso_alpha_2: text, unique, required, cannot_change
    iso_alpha_3: text, unique, required, cannot_change

  must:
    - iso_alpha_2 must be exactly 2 characters
    - iso_alpha_3 must be exactly 3 characters
```

**Caching Behavior**:
```rust
// Automatically cached, rare invalidation
static COUNTRY_CODES: LazyCache<Vec<CountryCode>> = cache_reference_data();
```

---

### Business Logic Pattern

**Use Case**: Calculation rules, decision logic
**Characteristics**: Compiled to code, type-safe

```
define rules: discount_calculation

  rule: calculate_tier_discount
    given:
      - customer_segment: text
      - purchase_amount: money

    calculate:
      discount_rate =
        when customer_segment == premier and purchase_amount >= $1000: 0.15
        when customer_segment == premier: 0.10
        when customer_segment == preferred and purchase_amount >= $500: 0.08
        when customer_segment == preferred: 0.05
        otherwise: 0.00

      discount_amount = purchase_amount * discount_rate

    return:
      - discount: money
      - rate_applied: percentage
```

**Compiled Output**:
```rust
pub fn calculate_tier_discount(
  customer_segment: &str,
  purchase_amount: Money
) -> (Money, f64) {
  let discount_rate = match (customer_segment, purchase_amount) {
    ("premier", amt) if amt >= Money::usd(1000) => 0.15,
    ("premier", _) => 0.10,
    // ...
  };
  (purchase_amount * discount_rate, discount_rate)
}
```

---

## IV. WORKFLOW EXAMPLES

### Linear Workflow

**Purpose**: Simple sequential processing

```
define workflow: account_closure
  inputs:
    - account_id: text
    - closure_reason: text

  outputs:
    - closed_successfully: boolean

  step: validate_account
    actions:
      - load account from database
      - validate account exists
      - validate account.current_balance == $0.00

    next: goto process_closure

  step: process_closure
    actions:
      - update account.account_status = closed
      - update account.closure_date = today
      - create closure_event

    next: goto notify_customer

  step: notify_customer
    actions:
      - send closure_confirmation email
      - create notification_record

    next: goto complete_closure

  step: complete_closure
    actions:
      - return closed_successfully: yes
```

---

### Conditional Workflow

**Purpose**: Branching logic based on conditions

```
define workflow: credit_limit_increase
  inputs:
    - customer: customer
    - requested_increase: money

  outputs:
    - approved: boolean
    - new_limit: money

  step: evaluate_eligibility
    actions:
      - calculate customer_score using credit_evaluation_rule

    next:
      when customer_score >= 750 and requested_increase <= $5000:
        goto auto_approve
      when customer_score >= 700:
        goto manual_review
      otherwise:
        goto auto_deny

  step: auto_approve
    actions:
      - update customer.credit_limit += requested_increase
      - send approval_notification

    next: goto complete

  step: manual_review
    actions:
      - create review_task
      - notify credit_team

    next: goto await_decision

  step: auto_deny
    actions:
      - send denial_notification with reason

    next: goto complete

  step: await_decision
    wait_for: review_completion

    next:
      when decision == approve: goto auto_approve
      otherwise: goto auto_deny

  step: complete
    actions:
      - return results
```

---

### Parallel Execution

**Purpose**: Process multiple items concurrently

```
define workflow: batch_fee_assessment
  inputs:
    - account_list: list of account

  outputs:
    - total_fees_assessed: money
    - accounts_processed: number

  step: initialize_batch
    actions:
      - set total_fees = $0.00
      - set processed_count = 0

    next: goto process_accounts

  step: process_accounts
    actions:
      - for each account in account_list:
          parallel:
            - run late_fee_assessment workflow
            - collect result
            - accumulate total_fees += result.fee_amount
            - increment processed_count

    next: goto complete_batch

  step: complete_batch
    actions:
      - log batch_completion_event
      - return total_fees_assessed: total_fees
      - return accounts_processed: processed_count
```

---

### Error Handling

**Purpose**: Graceful failure management

```
define workflow: payment_processing
  inputs:
    - payment: payment_transaction

  outputs:
    - success: boolean
    - error_message: text

  step: validate_payment
    actions:
      - validate payment.amount > $0.00
      - validate payment.account exists

    next:
      when validation_passed: goto process_payment
      otherwise: goto handle_validation_error

  step: process_payment
    actions:
      - begin transaction
      - update account.current_balance -= payment.amount
      - create payment_record
      - commit transaction

    on_error: goto handle_processing_error

    next: goto send_confirmation

  step: send_confirmation
    actions:
      - send payment_confirmation

    on_error: goto log_notification_error

    next: goto complete_success

  step: handle_validation_error
    actions:
      - log validation_error
      - set error_message = "Invalid payment data"
      - set success = no

    next: goto complete_failure

  step: handle_processing_error
    actions:
      - rollback transaction
      - log processing_error
      - set error_message = "Payment processing failed"
      - set success = no

    next: goto complete_failure

  step: log_notification_error
    actions:
      - log notification_error
      - queue retry_notification

    next: goto complete_success

  step: complete_success
    actions:
      - return success: yes

  step: complete_failure
    actions:
      - return success: no
      - return error_message: error_message
```

---

## V. RULE EXAMPLES

### Calculation Rules

```
define rules: interest_calculations

  rule: calculate_monthly_interest
    given:
      - principal: money
      - annual_rate: percentage
      - days_in_period: number

    calculate:
      daily_rate = annual_rate / 365
      interest = principal * daily_rate * days_in_period

    return:
      - monthly_interest: money


  rule: calculate_minimum_payment
    given:
      - current_balance: money
      - minimum_percentage: percentage
      - minimum_floor: money

    calculate:
      percentage_based = current_balance * minimum_percentage
      calculated_minimum = maximum of percentage_based and minimum_floor

    return:
      - minimum_payment: money
```

---

### Validation Rules

```
define rules: account_validation

  rule: validate_account_creation
    given:
      - customer: customer
      - requested_limit: money

    when:
      customer.credit_score >= 650 and
      customer.account_status == active and
      requested_limit >= $500 and
      requested_limit <= $50000

    then:
      return valid: yes

    otherwise:
      return valid: no
      return reason: "Account creation validation failed"


  rule: validate_payment_amount
    given:
      - payment_amount: money
      - current_balance: money
      - minimum_payment: money

    when:
      payment_amount >= minimum_payment and
      payment_amount <= current_balance + $100

    then:
      return valid: yes

    otherwise:
      return valid: no
      return reason: "Payment amount out of acceptable range"
```

---

### Eligibility Rules

```
define rules: promotion_eligibility

  rule: evaluate_balance_transfer_offer
    given:
      - customer: customer
      - account: account

    calculate:
      tenure_months = months_between(customer.customer_since, today)

      is_eligible =
        customer.credit_score >= 700 and
        tenure_months >= 6 and
        customer.on_time_percentage >= 95 and
        account.current_balance < account.credit_limit * 0.5

      offer_rate =
        when customer.customer_segment == premier: 0.0
        when customer.credit_score >= 750: 0.029
        otherwise: 0.039

    return:
      - eligible: boolean
      - promotional_rate: percentage
      - offer_duration_months: number
```

---

### Complex Decision Trees

```
define rules: credit_decision_engine

  rule: evaluate_credit_application
    given:
      - applicant: customer
      - requested_limit: money

    calculate:
      # Tier 1 evaluation
      tier_1_pass = applicant.credit_score >= 750

      # Tier 2 evaluation
      tenure_score = when months_between(applicant.customer_since, today) >= 24: 100
                     when months_between(applicant.customer_since, today) >= 12: 75
                     otherwise: 50

      payment_score = when applicant.on_time_percentage >= 98: 100
                      when applicant.on_time_percentage >= 95: 75
                      when applicant.on_time_percentage >= 90: 50
                      otherwise: 0

      balance_score = when applicant.utilization_ratio <= 0.3: 100
                      when applicant.utilization_ratio <= 0.5: 75
                      when applicant.utilization_ratio <= 0.7: 50
                      otherwise: 25

      composite_score = (tenure_score + payment_score + balance_score) / 3

      # Final decision
      decision =
        when tier_1_pass and requested_limit <= $25000: approved
        when tier_1_pass and requested_limit <= $50000 and composite_score >= 80: approved
        when applicant.credit_score >= 700 and composite_score >= 85: approved
        when applicant.credit_score >= 650 and composite_score >= 90 and requested_limit <= $10000: approved
        otherwise: denied

      approved_limit =
        when decision == approved and tier_1_pass: requested_limit
        when decision == approved and composite_score >= 90: requested_limit * 0.9
        when decision == approved: requested_limit * 0.75
        otherwise: $0.00

    return:
      - decision: text
      - approved_limit: money
      - composite_score: number
      - decision_reason: text
```

---

## VI. ADVANCED EXAMPLES

### Pattern Composition

**Purpose**: Combining multiple patterns for complex entities

```
define entity: subscription_contract
  pattern: master_data + versioned_configuration + state_machine

  # Master data aspect
  identity:
    contract_id: text, unique, required
    customer_id: text, required

  # Versioned configuration aspect
  versioning:
    version: number, required, auto_increment
    effective_date: date, required
    expiration_date: date

  terms:
    monthly_fee: money, required
    billing_cycle: number, required
    auto_renew: boolean, default yes

  # State machine aspect
  current_state: text, values: draft | active | suspended | cancelled | expired

  initial_state: draft

  transitions:
    from draft:
      - to active, when contract_signed

    from active:
      - to suspended, when payment_failed
      - to cancelled, when cancellation_requested
      - to expired, when expiration_date_reached

    from suspended:
      - to active, when payment_received
      - to cancelled, when suspension_period_exceeded

  # Combined constraints
  must:
    - effective_date <= expiration_date when expiration_date is not null
    - monthly_fee > $0.00
    - billing_cycle >= 1 and billing_cycle <= 365
    - current_state is one of draft | active | suspended | cancelled | expired
```

**Auto-Generated**:
- Master data CRUD operations
- Version history tracking
- State transition validation
- Combined audit trail

---

### BIAN Integration

**Purpose**: Full BIAN service domain alignment

```
# BIAN Service Domain: Customer Agreement (11)
define entity: customer_agreement
  pattern: master_data
  business_domain: "Credit Card (BIAN) - Customer Agreement (11)"
  bian_mapping:
    service_domain: "Customer Agreement"
    service_domain_id: 11
    asset_type: "Customer Agreement"
    generic_artifact: "Agreement"

  identity:
    agreement_id: text, unique, required
    customer_reference: text, required

  agreement_terms:
    agreement_type: text, values: card_member_agreement | terms_of_service
    agreement_version: text, required
    effective_date: date, required
    expiration_date: date

  legal:
    jurisdiction: text, required
    governing_law: text, required
    signatures_required: number, default 1
    is_fully_executed: boolean, default no

  regulatory:
    truth_in_lending_disclosure: boolean, required
    privacy_notice_provided: boolean, required
    cardholder_rights_provided: boolean, required

  must:
    - agreement_type is one of card_member_agreement | terms_of_service
    - signatures_required >= 1
    - effective_date <= expiration_date when expiration_date is not null


# BIAN Service Domain: Payment Execution (141)
define entity: payment_order
  pattern: immutable_ledger
  business_domain: "Credit Card (BIAN) - Payment Execution (141)"
  bian_mapping:
    service_domain: "Payment Execution"
    service_domain_id: 141
    asset_type: "Payment Order"
    generic_artifact: "Transaction"

  identity:
    payment_order_id: text, unique, required, cannot_change
    account_id: text, required, cannot_change

  order_details:
    payment_type: text, values: minimum_payment | full_balance | custom_amount, cannot_change
    payment_amount: money, required, cannot_change
    payment_date: date, required, cannot_change

  execution:
    execution_status: text, values: pending | processing | completed | failed
    completion_timestamp: timestamp
    confirmation_number: text

  must:
    - payment_type is one of minimum_payment | full_balance | custom_amount
    - payment_amount > $0.00
    - execution_status is one of pending | processing | completed | failed
```

---

### Temporal Queries

**Purpose**: Point-in-time and bi-temporal queries

```
define entity: exchange_rate
  pattern: temporal_data

  identity:
    rate_id: text, unique, required

  temporal:
    effective_from: date, required
    effective_to: date
    valid_from: timestamp, required
    valid_to: timestamp

  rate_details:
    from_currency: text, required
    to_currency: text, required
    exchange_rate: number, required

  must:
    - exchange_rate > 0
    - effective_from <= effective_to when effective_to is not null


# Usage in workflows
define workflow: currency_conversion
  inputs:
    - amount: money
    - from_currency: text
    - to_currency: text
    - conversion_date: date

  step: lookup_rate
    actions:
      # Point-in-time query
      - load rate using temporal query:
          effective_at: conversion_date
          where from_currency == from_currency
          and to_currency == to_currency

      - calculate converted_amount = amount * rate.exchange_rate

    next: goto complete_conversion


# Bi-temporal query example
define rules: historical_rate_analysis

  rule: get_rate_as_known_at
    given:
      - from_currency: text
      - to_currency: text
      - effective_date: date
      - knowledge_timestamp: timestamp

    calculate:
      # What rate was effective on effective_date,
      # as known at knowledge_timestamp
      rate = query exchange_rate where:
        from_currency == from_currency and
        to_currency == to_currency and
        effective_from <= effective_date and
        (effective_to is null or effective_to >= effective_date) and
        valid_from <= knowledge_timestamp and
        (valid_to is null or valid_to >= knowledge_timestamp)

    return:
      - historical_rate: number
```

---

### Regulatory Compliance

**Purpose**: Built-in compliance and audit requirements

```
define entity: regulatory_fee_disclosure
  pattern: immutable_ledger + event_log
  business_domain: "Credit Card (BIAN) - Regulatory Compliance"

  identity:
    disclosure_id: text, unique, required, cannot_change

  disclosure_details:
    customer_id: text, required, cannot_change
    disclosure_type: text, values: truth_in_lending | fee_schedule | cardholder_agreement, cannot_change
    disclosure_date: timestamp, required, cannot_change

  content:
    disclosure_version: text, required, cannot_change
    disclosure_content: text, required, cannot_change
    disclosure_language: text, default "en-US", cannot_change

  delivery:
    delivery_method: text, values: email | postal_mail | online_portal, cannot_change
    delivery_status: text, values: sent | delivered | failed
    delivery_timestamp: timestamp
    delivery_confirmation: text

  acknowledgment:
    acknowledged: boolean, default no
    acknowledgment_timestamp: timestamp
    acknowledgment_ip: text
    acknowledgment_method: text

  regulatory:
    regulation_reference: text, cannot_change
    retention_period_years: number, default 7, cannot_change
    destruction_eligible_date: date

  must:
    - disclosure_type is one of truth_in_lending | fee_schedule | cardholder_agreement
    - delivery_method is one of email | postal_mail | online_portal
    - retention_period_years >= 7
    - destruction_eligible_date == disclosure_date + retention_period_years when not null


# Compliance workflow
define workflow: fee_disclosure_compliance
  inputs:
    - customer: customer
    - fee_transaction: fee_transaction

  outputs:
    - disclosure_sent: boolean
    - compliant: boolean

  step: check_disclosure_requirement
    actions:
      - load last_disclosure for customer
      - calculate days_since_last = days_between(last_disclosure.disclosure_date, today)

    next:
      when days_since_last > 365 or last_disclosure is null:
        goto prepare_disclosure
      otherwise:
        goto complete_compliant

  step: prepare_disclosure
    actions:
      - generate fee_schedule_disclosure
      - create regulatory_fee_disclosure record
      - log disclosure_preparation_event

    next: goto send_disclosure

  step: send_disclosure
    actions:
      - send disclosure via customer.preferred_contact_method
      - update delivery_status = sent
      - create delivery_audit_record

    next: goto await_delivery_confirmation

  step: await_delivery_confirmation
    wait_for: delivery_confirmation
    timeout: 7 days

    next:
      when confirmed: goto log_compliance_success
      otherwise: goto retry_delivery

  step: retry_delivery
    actions:
      - send disclosure via alternate method
      - log retry_attempt

    next: goto await_delivery_confirmation

  step: log_compliance_success
    actions:
      - update disclosure.delivery_status = delivered
      - create compliance_event
      - set compliant = yes

    next: goto complete_disclosure

  step: complete_compliant
    actions:
      - set disclosure_sent = no
      - set compliant = yes

    next: goto complete_disclosure

  step: complete_disclosure
    actions:
      - return results
```

---

## Summary

This document provides comprehensive working examples of the Credit Card DSL, organized from basic concepts to advanced real-world applications.

**Navigation**:
- Use table of contents for quick access to specific topics
- See DSL_LANGUAGE_SPECIFICATION.md for complete language reference
- Examples progress from simple to complex for learning path

**Key Domains Covered**:
- Late Fee Assessment (complete workflow)
- All 9 data mutation patterns
- Workflow patterns (linear, conditional, parallel, error handling)
- Rule types (calculation, validation, eligibility, decision trees)
- Advanced features (pattern composition, BIAN integration, temporal queries, compliance)

**Next Steps**:
1. Review basic examples for syntax familiarity
2. Study late fee calculation for complete domain understanding
3. Explore pattern showcase for specific pattern details
4. Reference advanced examples for production scenarios

---

**Document Version**: 2.0
**Total Examples**: 30+ complete working examples
**Patterns Covered**: 9/9 data mutation patterns + code generation
**Companion**: DSL_LANGUAGE_SPECIFICATION.md
