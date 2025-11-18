# Simple entity example for testing parser

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
