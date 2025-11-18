# Simple parameter example for testing parser

define parameter: grace_period_days
  pattern: operational_parameters
  business_domain: "Credit Card (BIAN)"

  type: number
  default: 21

  constraints:
    - value >= 0
    - value <= 90

  hot_reload: yes
  validation: "must be between 0 and 90 days"
  documentation: "Number of days after due date before late fee is assessed"
