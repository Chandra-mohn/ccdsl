# Simple rule example for testing parser

define rules: fee_calculation

  rule: calculate_late_fee
    given:
      - account_balance: money
      - days_past_due: number

    calculate:
      base_fee = when account_balance >= $5000: $40.00
                 when account_balance >= $1000: $30.00
                 otherwise: $25.00

      late_fee = when days_past_due > 30: base_fee
                 when days_past_due > 15: base_fee * 0.75
                 otherwise: $0.00

    return:
      - fee_amount: money
      - fee_reason: text
