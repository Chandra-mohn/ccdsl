# Simple workflow example for testing parser

define workflow: payment_processing
  inputs:
    - account: account
    - payment_amount: money

  outputs:
    - success: boolean
    - transaction_id: text

  step: validate_payment
    actions:
      - load account from database
      - set valid to payment_amount > $0.00

    next:
      when valid: goto process_payment
      otherwise: goto reject_payment

  step: process_payment
    actions:
      - update account.current_balance -= payment_amount
      - create payment_transaction
      - set success to yes

    next: goto complete

  step: reject_payment
    actions:
      - log "Payment validation failed"
      - set success to no

    next: goto complete

  step: complete
    actions:
      - send confirmation
