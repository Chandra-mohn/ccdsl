# Credit Card Domain DSL

**Domain**: Credit Card / Payment Card Services
**Layer**: Business / Domain Knowledge
**Status**: Production Specification

---

## Overview

The Credit Card Domain DSL extends the Core DSL with credit card-specific business knowledge, entities, workflows, and rules. It leverages the technical infrastructure from Core DSL while adding domain expertise.

**Module Declaration**:
```
module: domain.credit_card
version: 1.0.0
stability: stable

imports:
  - core.entity >= 2.0.0
  - core.workflow >= 2.0.0
  - core.rules >= 2.0.0
  - core.parameter >= 2.0.0
  - core.reference >= 2.0.0

bian_domains:
  - "Customer Agreement (11)"
  - "Account Management (17)"
  - "Card Financial Settlement (91)"
  - "Card Case (134)"
  - "Card Collections (135)"
  - "Payment Execution (141)"
```

---

## Domain Entities

### Customer Management

```
define entity: customer
  pattern: master_data
  business_domain: "Credit Card"
  bian_service_domain: "Customer Agreement (11)"

  identity:
    customer_id: text, unique, required

  profile:
    full_name: text, required
    date_of_birth: date, required
    email: email, required
    phone: phone, required
    customer_segment: text, values: premier | preferred | standard

  credit:
    credit_score: number, min: 300, max: 850
    annual_income: money
    debt_to_income_ratio: percentage

  status:
    account_status: text, values: active | suspended | closed
    status_reason: text
    last_updated: datetime

  belongs_to:
    - primary_address
    - billing_address

  must:
    - customer_segment is one of premier | preferred | standard
    - credit_score >= 300 and credit_score <= 850
    - account_status is one of active | suspended | closed
```

---

### Card Account

```
define entity: card_account
  pattern: master_data
  business_domain: "Credit Card"
  bian_service_domain: "Account Management (17)"

  identity:
    account_id: text, unique, required
    card_number: text, unique, required, pattern: "^[0-9]{16}$"

  account_details:
    account_type: text, values: premier | standard
    credit_limit: money, required
    available_credit: money, required
    current_balance: money
    minimum_payment_due: money
    payment_due_date: date

  card_details:
    card_status: text, values: active | blocked | expired | closed
    expiry_date: date, required
    cvv: text, pattern: "^[0-9]{3}$"
    card_holder_name: text, required

  interest:
    apr: percentage, min: 0, max: 0.35
    grace_period_days: number, default: 25

  belongs_to:
    - customer
    - card_product

  has_many:
    - transactions
    - fee_transactions
    - interest_charges

  must:
    - available_credit <= credit_limit
    - current_balance >= 0
    - account_type is one of premier | standard
    - card_status is one of active | blocked | expired | closed
```

---

### Card Product

```
define entity: card_product
  pattern: versioned_configuration
  business_domain: "Credit Card"

  identity:
    product_id: text, unique, required
    product_code: text, unique, required

  product_details:
    product_name: text, required
    product_tier: text, values: premium | gold | standard
    annual_fee: money

  credit_terms:
    min_credit_limit: money
    max_credit_limit: money
    default_apr: percentage
    cash_advance_apr: percentage
    balance_transfer_apr: percentage

  fees:
    late_fee_schedule: reference(late_fee_schedule)
    foreign_transaction_fee: percentage
    cash_advance_fee: percentage
    balance_transfer_fee: percentage

  rewards:
    rewards_program: text
    cashback_rate: percentage
    points_per_dollar: number

  effective:
    effective_from: date, required
    effective_to: date

  must:
    - product_tier is one of premium | gold | standard
    - min_credit_limit < max_credit_limit
    - default_apr > 0 and default_apr <= 0.35
```

---

### Transaction

```
define entity: transaction
  pattern: immutable_ledger
  business_domain: "Credit Card"
  bian_service_domain: "Payment Execution (141)"

  identity:
    transaction_id: text, unique, required

  transaction_details:
    transaction_date: datetime, required
    transaction_type: text, values: purchase | payment | refund | reversal
    merchant_name: text
    merchant_category: text
    transaction_amount: money, required
    transaction_currency: text, required

  authorization:
    authorization_code: text
    authorization_date: datetime
    authorization_status: text, values: approved | declined | pending

  processing:
    posting_date: datetime
    settlement_date: datetime
    processing_status: text, values: pending | posted | reversed

  belongs_to:
    - card_account

  must:
    - transaction_type is one of purchase | payment | refund | reversal
    - transaction_amount != 0
    - authorization_status is one of approved | declined | pending
    - processing_status is one of pending | posted | reversed
```

---

### Fee Transaction

```
define entity: fee_transaction
  pattern: immutable_ledger
  business_domain: "Credit Card"

  identity:
    fee_id: text, unique, required

  fee_details:
    fee_type: text, values: late_fee | annual_fee | foreign_transaction | cash_advance | overlimit
    fee_amount: money, required
    fee_date: datetime, required
    waived: boolean, default: no
    waiver_reason: text

  assessment:
    assessed_by_rule: text
    assessment_date: datetime

  belongs_to:
    - card_account

  must:
    - fee_type is one of late_fee | annual_fee | foreign_transaction | cash_advance | overlimit
    - fee_amount > 0
```

---

### Interest Charge

```
define entity: interest_charge
  pattern: immutable_ledger
  business_domain: "Credit Card"
  bian_service_domain: "Card Financial Settlement (91)"

  identity:
    interest_id: text, unique, required

  charge_details:
    interest_type: text, values: purchase | cash_advance | balance_transfer
    interest_amount: money, required
    interest_rate: percentage, required
    balance_subject_to_interest: money, required
    charge_date: datetime, required
    statement_period: text

  calculation:
    daily_periodic_rate: percentage
    days_in_period: number
    average_daily_balance: money

  belongs_to:
    - card_account

  must:
    - interest_type is one of purchase | cash_advance | balance_transfer
    - interest_amount >= 0
    - interest_rate >= 0
```

---

### Fee Calculation Event

```
define entity: fee_calculation_event
  pattern: event_log
  business_domain: "Credit Card"

  identity:
    event_id: text, unique, required

  event_details:
    event_timestamp: datetime, default: now
    event_type: text, values: fee_assessed | fee_waived | fee_reversed
    account_id: text, required
    fee_type: text, required
    fee_amount: money, required

  calculation_context:
    rule_applied: text
    parameters_used: text
    account_balance: money
    days_past_due: number
    customer_segment: text

  outcome:
    fee_assessed: boolean
    waiver_applied: boolean
    waiver_reason: text

  must:
    - event_type is one of fee_assessed | fee_waived | fee_reversed
```

---

### Delinquency State

```
define entity: delinquency_state
  pattern: state_machine
  business_domain: "Credit Card"
  bian_service_domain: "Card Collections (135)"

  identity:
    state_id: text, unique, required

  state_tracking:
    current_state: text, values: current | delinquent_15 | delinquent_30 | delinquent_60 | delinquent_90 | charged_off
    state_entered_date: date, required
    days_past_due: number

  belongs_to:
    - card_account

  initial_state: current

  transitions:
    from current: delinquent_15
    from delinquent_15: current, delinquent_30
    from delinquent_30: current, delinquent_60
    from delinquent_60: current, delinquent_90
    from delinquent_90: current, charged_off
    from charged_off: none

  on_enter_state:
    when delinquent_15:
      - send notification with type warning
      - log_event("Account 15 days past due")

    when delinquent_30:
      - send notification with type escalation
      - report to credit_bureaus
      - log_event("Account 30 days past due")

    when delinquent_60:
      - send notification with type final_warning
      - assign to collections_queue
      - log_event("Account 60 days past due")

    when delinquent_90:
      - send notification with type collections
      - escalate to external_collections
      - log_event("Account 90 days past due")

    when charged_off:
      - send notification with type account_closed
      - report charge_off to credit_bureaus
      - close_account()
      - log_event("Account charged off")

    when current:
      - send notification with type account_current
      - log_event("Account returned to current status")

  must:
    - current_state is one of current | delinquent_15 | delinquent_30 | delinquent_60 | delinquent_90 | charged_off
```

---

### Late Fee Schedule

```
define entity: late_fee_schedule
  pattern: versioned_configuration
  business_domain: "Credit Card"

  identity:
    schedule_id: text, unique, required
    version: number, required

  schedule_details:
    schedule_name: text, required
    customer_segment: text, values: premier | preferred | standard

  fee_tiers:
    tier_1_days: number
    tier_1_fee: money
    tier_2_days: number
    tier_2_fee: money
    tier_3_days: number
    tier_3_fee: money

  caps:
    maximum_fee: money
    minimum_balance_for_fee: money

  effective:
    effective_from: date, required
    effective_to: date

  must:
    - tier_1_days < tier_2_days
    - tier_2_days < tier_3_days
    - tier_1_fee <= tier_2_fee
    - tier_2_fee <= tier_3_fee
```

---

### Dispute Case

```
define entity: dispute_case
  pattern: master_data
  business_domain: "Credit Card"
  bian_service_domain: "Card Case (134)"

  identity:
    case_id: text, unique, required

  case_details:
    case_type: text, values: unauthorized_charge | billing_error | goods_not_received | quality_dispute
    case_status: text, values: open | investigating | resolved | closed
    dispute_amount: money, required
    filed_date: datetime, required
    resolution_date: datetime

  investigation:
    assigned_to: text
    investigation_notes: text
    supporting_documents: text
    merchant_response: text

  resolution:
    resolution_type: text, values: customer_favor | merchant_favor | partial_credit | withdrawn
    credit_issued: money
    resolution_notes: text

  belongs_to:
    - card_account
    - transaction

  must:
    - case_type is one of unauthorized_charge | billing_error | goods_not_received | quality_dispute
    - case_status is one of open | investigating | resolved | closed
    - dispute_amount > 0
```

---

## Domain Workflows

### Process Monthly Statement

```
define workflow: process_monthly_statement
  triggered_by: scheduled batch job (monthly, day: 1)
  business_domain: "Credit Card"
  bian_service_domain: "Card Financial Settlement (91)"

  step: calculate_interest
    actions:
      - card_accounts
          | filter: account_status = "active"
          | filter: current_balance > 0
          | map: calculate_monthly_interest(account_id, current_balance, apr)
          | foreach: post_interest_charge

  step: assess_fees
    actions:
      - card_accounts
          | filter: account_status = "active"
          | filter: payment_due_date < today() - grace_period_days
          | map: calculate_late_fee(account_id, account_type, days_late)
          | filter: late_fee > 0
          | foreach: post_fee

  step: generate_statements
    actions:
      - card_accounts
          | filter: account_status = "active"
          | map: generate_statement(account_id, statement_date: today())
          | foreach: send_statement

    return:
      - statements_generated: number
```

---

### Process Card Payment

```
define workflow: process_card_payment
  triggered_by: api call
  business_domain: "Credit Card"
  bian_service_domain: "Payment Execution (141)"

  inputs:
    - account_id: text
    - payment_amount: money
    - payment_method: text

  step: validate_payment
    actions:
      - validate_account(account_id)
      - validate_payment_amount(payment_amount)
      - validate_payment_method(payment_method)

    next:
      - when: validation_failed
        goto: handle_error
      - otherwise:
        goto: process_payment

  step: process_payment
    actions:
      - create_payment_transaction(
          account_id: account_id,
          amount: payment_amount,
          method: payment_method,
          status: "pending"
        )
      - update_account_balance(account_id, payment_amount)
      - update_available_credit(account_id, payment_amount)

    next:
      - when: processing_successful
        goto: complete
      - otherwise:
        goto: handle_error

  step: complete
    actions:
      - update_payment_status(payment_id, "posted")
      - log_event("Payment processed successfully")

    return:
      - payment_id: text
      - status: text

  step: handle_error
    actions:
      - log_error(error_message)
      - rollback_transaction()

    return:
      - status: text
      - error_message: text
```

---

### Fee Assessment Workflow

```
define workflow: fee_assessment
  triggered_by: scheduled batch job (daily)
  business_domain: "Credit Card"
  bian_service_domain: "Card Financial Settlement (91)"

  step: identify_late_accounts
    description: "Find accounts past grace period with unpaid balances"

    actions:
      - card_accounts
          | filter: account_status = "active"
          | filter: payment_due_date < today() - grace_period_days
          | filter: minimum_payment_due > 0
          | map: calculate_days_late(payment_due_date)
          | filter: days_late > 0

    next:
      - when: late_accounts found
        goto: calculate_fees
      - otherwise:
        goto: complete_no_fees

  step: calculate_fees
    description: "Calculate late fees using business rules"

    actions:
      - late_accounts
          | join: (acc => late_fee_schedules where customer_segment = acc.customer_segment)
          | map: calculate_late_fee_amount(
              account_type: account_type,
              days_late: days_late,
              schedule: late_fee_schedule
            )
          | map: evaluate_waiver_eligibility(
              account_id: account_id,
              fee_amount: calculated_fee
            )

    next: goto assess_or_waive

  step: assess_or_waive
    description: "Post fees or apply waivers based on eligibility"

    actions:
      - calculated_fees
          | filter: waiver_eligible = no
          | foreach: post_fee_transaction(
              account_id: account_id,
              fee_type: "late_fee",
              fee_amount: fee_amount
            )

      - calculated_fees
          | filter: waiver_eligible = yes
          | foreach: log_waiver_event(
              account_id: account_id,
              fee_amount: fee_amount,
              waiver_reason: waiver_reason
            )

    next: goto send_notifications

  step: send_notifications
    description: "Notify customers of assessed fees"

    actions:
      - assessed_fees
          | foreach: send_notification(
              customer_id: customer_id,
              notification_type: "late_fee_assessed",
              fee_amount: fee_amount
            )

    return:
      - fees_assessed: number
      - fees_waived: number
      - total_fee_amount: money

  step: complete_no_fees
    return:
      - fees_assessed: 0
      - fees_waived: 0
      - total_fee_amount: 0.00 USD
```

---

### Dispute Management Workflow

```
define workflow: dispute_management
  triggered_by: api call
  business_domain: "Credit Card"
  bian_service_domain: "Card Case (134)"

  inputs:
    - account_id: text
    - transaction_id: text
    - dispute_type: text
    - dispute_amount: money
    - customer_statement: text

  step: create_case
    description: "Create dispute case and issue provisional credit"

    actions:
      - create_dispute_case(
          account_id: account_id,
          transaction_id: transaction_id,
          case_type: dispute_type,
          dispute_amount: dispute_amount,
          case_status: "open"
        )
      - issue_provisional_credit(account_id, dispute_amount)
      - log_event("Dispute case created", case_id: case_id)

    next: goto investigate

  step: investigate
    description: "Investigate dispute with merchant and customer"

    actions:
      - assign_investigator(case_id)
      - request_merchant_response(transaction_id, dispute_type)
      - collect_supporting_documents(case_id)
      - update_case_status(case_id, "investigating")

    next:
      - when: merchant_response = "refund_issued"
        goto: resolve_customer_favor
      - when: merchant_response = "dispute_invalid"
        goto: review_evidence
      - when: investigation_timeout
        goto: resolve_customer_favor
      - otherwise:
        goto: review_evidence

  step: review_evidence
    description: "Review evidence and make determination"

    actions:
      - evaluate_dispute_evidence(
          case_id: case_id,
          merchant_response: merchant_response,
          customer_statement: customer_statement,
          supporting_documents: supporting_documents
        )

    next:
      - when: evidence_favors_customer
        goto: resolve_customer_favor
      - when: evidence_favors_merchant
        goto: resolve_merchant_favor
      - otherwise:
        goto: resolve_partial_credit

  step: resolve_customer_favor
    actions:
      - finalize_provisional_credit(case_id)
      - update_case_status(case_id, "resolved")
      - send_notification(customer_id, "dispute_resolved_favor")
      - log_event("Dispute resolved in customer favor")

    return:
      - case_id: text
      - resolution: "customer_favor"
      - credit_amount: money

  step: resolve_merchant_favor
    actions:
      - reverse_provisional_credit(case_id, dispute_amount)
      - update_case_status(case_id, "resolved")
      - send_notification(customer_id, "dispute_resolved_merchant")
      - log_event("Dispute resolved in merchant favor")

    return:
      - case_id: text
      - resolution: "merchant_favor"
      - credit_amount: 0.00 USD

  step: resolve_partial_credit
    actions:
      - calculate_partial_credit(dispute_amount)
      - adjust_provisional_credit(case_id, partial_credit_amount)
      - update_case_status(case_id, "resolved")
      - send_notification(customer_id, "dispute_partial_credit")
      - log_event("Dispute resolved with partial credit")

    return:
      - case_id: text
      - resolution: "partial_credit"
      - credit_amount: money
```

---

### Delinquency Management Workflow

```
define workflow: delinquency_management
  triggered_by: scheduled batch job (daily)
  business_domain: "Credit Card"
  bian_service_domain: "Card Collections (135)"

  step: evaluate_delinquency_status
    description: "Update delinquency state for all accounts"

    actions:
      - card_accounts
          | filter: account_status = "active"
          | map: calculate_days_past_due(payment_due_date)
          | foreach: update_delinquency_state(account_id, days_past_due)

    next: goto process_state_transitions

  step: process_state_transitions
    description: "Handle state transitions and trigger actions"

    actions:
      - delinquency_states
          | filter: state_changed = yes
          | foreach: execute_state_actions(
              account_id: account_id,
              new_state: current_state,
              days_past_due: days_past_due
            )

    next: goto generate_reports

  step: generate_reports
    description: "Generate delinquency reports for collections team"

    actions:
      - delinquency_states
          | filter: current_state != "current"
          | group_by: current_state
          | map: generate_collections_report(state: current_state, accounts: accounts)
          | foreach: send_to_collections_team

    return:
      - total_delinquent_accounts: number
      - total_delinquent_balance: money
      - accounts_by_state: object
```

---

## Domain Rules

### Late Fee Calculation

```
define rules: late_fee_calculator
  pattern: business_logic
  business_domain: "Credit Card"

  decision_table: calculate_late_fee
    given:
      - account_type: text
      - days_late: number

    decide:
      | account_type | days_late | → late_fee   |
      |--------------|-----------|--------------|
      | premier      | 1-7       | 15.00 USD    |
      | premier      | 8-30      | 25.00 USD    |
      | premier      | > 30      | 35.00 USD    |
      | standard     | 1-7       | 35.00 USD    |
      | standard     | 8-30      | 45.00 USD    |
      | standard     | > 30      | 50.00 USD    |
      | *            | *         | 50.00 USD    |

    return:
      - late_fee: money
```

---

### Fee Waiver Eligibility

```
define rules: fee_waiver_rules
  pattern: business_logic
  business_domain: "Credit Card"

  decision_table: evaluate_waiver_eligibility
    given:
      - customer_segment: text
      - payment_history_score: number
      - fee_amount: money
      - previous_waivers_count: number

    decide:
      | customer_segment | payment_history_score | fee_amount  | previous_waivers_count | → waiver_eligible | waiver_reason           |
      |------------------|-----------------------|-------------|------------------------|-------------------|-------------------------|
      | premier          | >= 95                 | *           | < 2                    | yes               | premier_auto_waiver     |
      | premier          | >= 85                 | <= 25 USD   | < 3                    | yes               | premier_small_fee       |
      | preferred        | >= 95                 | <= 25 USD   | < 2                    | yes               | preferred_auto_waiver   |
      | standard         | >= 98                 | <= 15 USD   | < 1                    | yes               | standard_exceptional    |
      | *                | *                     | *           | *                      | no                | not_eligible            |

    return:
      - waiver_eligible: boolean
      - waiver_reason: text
```

---

### Credit Approval Rules

```
define rules: credit_approval
  pattern: business_logic
  business_domain: "Credit Card"

  decision_table: approve_credit_application
    given:
      - credit_score: number
      - debt_to_income_ratio: percentage
      - account_age_years: number

    decide:
      | credit_score | debt_to_income_ratio | account_age_years | → decision | credit_limit |
      |--------------|----------------------|-------------------|------------|--------------|
      | >= 750       | < 0.30               | *                 | approve    | 30000 USD    |
      | >= 700       | < 0.35               | >= 2              | approve    | 20000 USD    |
      | >= 650       | < 0.40               | >= 2              | review     | 15000 USD    |
      | >= 600       | < 0.45               | >= 3              | review     | 10000 USD    |
      | *            | *                    | *                 | deny       | 0 USD        |

    return:
      - decision: text
      - credit_limit: money
```

---

### Interest Calculation

```
define rules: interest_calculator
  pattern: business_logic
  business_domain: "Credit Card"
  bian_service_domain: "Card Financial Settlement (91)"

  rule: calculate_monthly_interest
    description: "Calculate interest charges for billing cycle"

    given:
      - average_daily_balance: money
      - apr: percentage
      - days_in_cycle: number

    calculate:
      daily_periodic_rate = apr / 365

      interest_amount =
        average_daily_balance * daily_periodic_rate * days_in_cycle

      minimum_interest =
        when interest_amount > 0 and interest_amount < 1.00 USD: 1.00 USD
        otherwise: interest_amount

    return:
      - interest_amount: money
      - daily_periodic_rate: percentage

    examples:
      - given:
          average_daily_balance: 1000.00 USD
          apr: 18.99
          days_in_cycle: 30
        then:
          interest_amount: 15.62 USD

      - given:
          average_daily_balance: 500.00 USD
          apr: 24.99
          days_in_cycle: 30
        then:
          interest_amount: 10.28 USD
```

---

### Fraud Detection Rules

```
define rules: fraud_detection
  pattern: business_logic
  business_domain: "Credit Card"

  decision_table: detect_suspicious_transaction
    given:
      - transaction_amount: money
      - merchant_category: text
      - time_since_last_transaction: duration
      - distance_from_last_transaction: number

    decide:
      | transaction_amount | merchant_category | time_since_last_transaction | distance_from_last_transaction | → risk_level | action        |
      |--------------------|-------------------|-----------------------------|---------------------------------|--------------|---------------|
      | > 5000 USD         | *                 | < 1 hour                    | *                               | high         | block_card    |
      | > 2000 USD         | high_risk         | *                           | *                               | high         | require_auth  |
      | *                  | *                 | < 30 minutes                | > 100 miles                     | high         | require_auth  |
      | > 1000 USD         | *                 | < 2 hours                   | > 50 miles                      | medium       | notify_user   |
      | *                  | *                 | *                           | *                               | low          | approve       |

    return:
      - risk_level: text
      - action: text
```

---

## Domain Parameters

```
define parameters: credit_card_parameters
  pattern: operational_parameters
  business_domain: "Credit Card"

  parameter: grace_period_days
    type: number
    default: 25
    description: "Number of days before late fees apply"
    valid_range: 15 to 30
    reload: hot

  parameter: max_daily_transaction_limit
    type: money
    default: 10000 USD
    description: "Maximum transaction amount per day"
    reload: hot

  parameter: fraud_threshold_amount
    type: money
    default: 5000 USD
    description: "Amount above which transactions require additional verification"
    reload: hot

  parameter: auto_waiver_threshold
    type: money
    default: 5 USD
    description: "Late fees below this amount are automatically waived"
    reload: warm

  parameter: minimum_payment_percentage
    type: percentage
    default: 2.0
    description: "Minimum payment as percentage of statement balance"
    valid_range: 1.0 to 5.0
    reload: warm

  parameter: cash_advance_limit_percentage
    type: percentage
    default: 30.0
    description: "Cash advance limit as percentage of credit limit"
    valid_range: 10.0 to 50.0
    reload: warm
```

---

## Domain Reference Data

```
define reference_data: merchant_categories
  pattern: reference_data
  business_domain: "Credit Card"

  entries:
    - code: "5411", name: "Grocery Stores", risk_level: "low"
    - code: "5812", name: "Restaurants", risk_level: "low"
    - code: "5999", name: "Miscellaneous Retail", risk_level: "medium"
    - code: "6211", name: "Securities Brokers", risk_level: "high"
    - code: "7995", name: "Gambling", risk_level: "high"
    - code: "4829", name: "Money Transfer", risk_level: "high"
    - code: "5912", name: "Drug Stores", risk_level: "low"
    - code: "5541", name: "Service Stations", risk_level: "low"

define reference_data: dispute_categories
  pattern: reference_data
  business_domain: "Credit Card"

  entries:
    - code: "unauthorized", name: "Unauthorized Charge", resolution_days: 30
    - code: "billing_error", name: "Billing Error", resolution_days: 45
    - code: "goods_not_received", name: "Goods Not Received", resolution_days: 60
    - code: "quality_dispute", name: "Quality Dispute", resolution_days: 60
    - code: "duplicate_charge", name: "Duplicate Charge", resolution_days: 30
    - code: "cancelled_recurring", name: "Cancelled Recurring Charge", resolution_days: 45

define reference_data: delinquency_actions
  pattern: reference_data
  business_domain: "Credit Card"

  entries:
    - state: "delinquent_15", action: "send_reminder", priority: "low"
    - state: "delinquent_30", action: "send_warning", priority: "medium"
    - state: "delinquent_60", action: "assign_collections", priority: "high"
    - state: "delinquent_90", action: "external_collections", priority: "critical"
    - state: "charged_off", action: "close_account", priority: "critical"
```

---

## BIAN Service Domain Mapping

The Credit Card Domain DSL aligns with the following BIAN service domains:

### Core Service Domains

| BIAN Service Domain | Domain Code | DSL Usage | Purpose |
|---------------------|-------------|-----------|---------|
| **Customer Agreement** | 11 | Customer entity, profile management | Customer profiles and agreements |
| **Account Management** | 17 | Card account entity | Card account lifecycle management |
| **Card Financial Settlement** | 91 | Payment workflows, interest calculation | Transaction processing and settlement |
| **Card Case** | 134 | Dispute management workflow | Dispute investigation and resolution |
| **Card Collections** | 135 | Delinquency state machine | Collections activities and recovery |
| **Payment Execution** | 141 | Transaction entity, payment workflows | Payment authorization and processing |

### Supporting Service Domains

| BIAN Service Domain | Domain Code | DSL Usage |
|---------------------|-------------|-----------|
| **Fee & Commission Management** | 71 | Fee assessment workflow, fee rules |
| **Credit Risk Operations** | 75 | Credit approval rules, risk evaluation |
| **Fraud Detection** | 78 | Fraud detection rules, transaction monitoring |
| **Product Directory** | 97 | Card product configuration |
| **Customer Event History** | 121 | Event log pattern for audit trail |

---

## BIAN Terminology Mapping

### BIAN to DSL Concept Mapping

| BIAN Concept | DSL Construct | Example |
|--------------|---------------|---------|
| **Service Domain** | business_domain | "Credit Card", "Card Collections" |
| **Control Record** | Entity (master_data pattern) | customer, card_account |
| **Behavior Qualifier** | Workflow step | calculate_interest, assess_fees |
| **Service Operation** | Workflow action | post_fee, send_notification |
| **Functional Pattern** | Pattern declaration | master_data, immutable_ledger |
| **Asset Type** | Entity type | transaction, fee_transaction |
| **Feature** | Entity field group | account_details, interest |
| **Retrieve** | Workflow read action | load account, get parameters |
| **Update** | Workflow write action | update balance, post transaction |

### BIAN Service Operation Mapping

| BIAN Operation | DSL Equivalent | Usage |
|----------------|----------------|-------|
| **Initiate** | Workflow triggered_by | Start new workflow execution |
| **Update** | Workflow action (update) | Modify existing entity state |
| **Execute** | Workflow step | Perform business operation |
| **Request** | Workflow input | Receive external request |
| **Retrieve** | Workflow action (load) | Query entity data |
| **Notify** | Workflow action (send) | External notification |
| **Control** | State machine transition | State change management |

---

## Extension Points

Domain DSLs can extend core capabilities:

- **Custom Patterns**: Domain-specific mutation patterns
- **Custom Operators**: Domain-specific pipeline operators
- **Custom Validations**: Domain-specific constraint rules
- **Custom Actions**: Domain-specific workflow actions

---

## See Also

- **core/CORE_DSL_SPECIFICATION.md**: Foundation technical constructs
- **DSL_SAMPLES.md**: Complete credit card examples
- **DSL_DECISION_TABLES.md**: Decision table detailed specification
- **BIAN_INTEGRATION.md**: Complete BIAN alignment documentation
