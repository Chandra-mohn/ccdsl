/*
 * Credit Card Domain-Specific Language (DSL) Grammar
 * ANTLR4 Grammar Definition
 *
 * Version: 2.0
 * Date: 2025-11-11
 * Target: Rust code generation via antlr4rust
 *
 * Key Features:
 * - Indentation-based syntax (YAML-inspired)
 * - Business-friendly keywords
 * - Pattern-driven entity definitions
 * - Workflow and rule definitions
 * - Comprehensive type system
 */

grammar CreditCardDSL;

// ============================================================================
// PARSER RULES
// ============================================================================

// Top-level compilation unit
compilation_unit
    : definition* EOF
    ;

// Definition types
definition
    : entity_definition
    | workflow_definition
    | rules_definition
    | parameter_definition
    ;

// ============================================================================
// ENTITY DEFINITIONS
// ============================================================================

entity_definition
    : DEFINE ENTITY COLON IDENTIFIER NEWLINE
      INDENT
      entity_body
      DEDENT
    ;

entity_body
    : entity_clause+
    ;

entity_clause
    : pattern_clause
    | business_domain_clause
    | bian_mapping_clause
    | identity_clause
    | field_group_clause
    | relationships_clause
    | constraints_clause
    | state_machine_clause
    | versioning_clause
    | temporal_clause
    ;

// Pattern declaration
pattern_clause
    : PATTERN COLON pattern_name NEWLINE
    ;

pattern_name
    : MASTER_DATA
    | IMMUTABLE_LEDGER
    | VERSIONED_CONFIGURATION
    | OPERATIONAL_PARAMETERS
    | EVENT_LOG
    | STATE_MACHINE
    | TEMPORAL_DATA
    | REFERENCE_DATA
    | pattern_composition
    ;

pattern_composition
    : pattern_name PLUS pattern_name (PLUS pattern_name)*
    ;

// Business domain
business_domain_clause
    : BUSINESS_DOMAIN COLON STRING_LITERAL NEWLINE
    ;

// BIAN mapping
bian_mapping_clause
    : BIAN_MAPPING COLON NEWLINE
      INDENT
      bian_mapping_field+
      DEDENT
    ;

bian_mapping_field
    : IDENTIFIER COLON (STRING_LITERAL | NUMBER_LITERAL) NEWLINE
    ;

// Identity fields
identity_clause
    : IDENTITY COLON NEWLINE
      INDENT
      field_definition+
      DEDENT
    ;

// Field groups (profile, metrics, etc.)
field_group_clause
    : IDENTIFIER COLON NEWLINE
      INDENT
      field_definition+
      DEDENT
    ;

// Field definition
field_definition
    : IDENTIFIER COLON field_type field_qualifiers? NEWLINE
    ;

field_type
    : TEXT
    | NUMBER
    | MONEY
    | DATE
    | TIMESTAMP
    | BOOLEAN
    | EMAIL
    | PHONE
    | PERCENTAGE
    | DURATION
    | TIME
    | IDENTIFIER  // Reference to another entity
    ;

field_qualifiers
    : COMMA field_qualifier (COMMA field_qualifier)*
    ;

field_qualifier
    : UNIQUE
    | REQUIRED
    | CANNOT_CHANGE
    | AUTO_INCREMENT
    | default_qualifier
    | between_qualifier
    | values_qualifier
    ;

default_qualifier
    : DEFAULT literal_value
    ;

between_qualifier
    : BETWEEN literal_value AND literal_value
    ;

values_qualifier
    : VALUES COLON value_list
    ;

value_list
    : IDENTIFIER (PIPE IDENTIFIER)*
    ;

// Relationships
relationships_clause
    : RELATIONSHIPS COLON NEWLINE
      INDENT
      relationship_definition+
      DEDENT
    ;

relationship_definition
    : HYPHEN relationship_type IDENTIFIER (COMMA relationship_qualifier)? NEWLINE
    ;

relationship_type
    : BELONGS_TO
    | HAS_MANY
    | HAS_ONE
    | REFERENCES
    | USES
    ;

relationship_qualifier
    : CANNOT_CHANGE
    | REQUIRED
    ;

// Constraints (must block)
constraints_clause
    : MUST COLON NEWLINE
      INDENT
      constraint_definition+
      DEDENT
    ;

constraint_definition
    : HYPHEN constraint_expression NEWLINE
    ;

constraint_expression
    : comparison_expression
    | logical_expression
    | membership_expression
    | STRING_LITERAL  // Free-form constraint description
    ;

// State machine (for state_machine pattern)
state_machine_clause
    : CURRENT_STATE COLON field_type COMMA VALUES COLON value_list NEWLINE
      INITIAL_STATE COLON IDENTIFIER NEWLINE
      TRANSITIONS COLON NEWLINE
      INDENT
      transition_definition+
      DEDENT
    ;

transition_definition
    : FROM IDENTIFIER COLON NEWLINE
      INDENT
      transition_target+
      DEDENT
    ;

transition_target
    : HYPHEN TO IDENTIFIER (COMMA WHEN IDENTIFIER)? NEWLINE
    ;

// Versioning (for versioned_configuration pattern)
versioning_clause
    : VERSIONING COLON NEWLINE
      INDENT
      versioning_field+
      DEDENT
    ;

versioning_field
    : IDENTIFIER COLON field_type field_qualifiers? NEWLINE
    ;

// Temporal (for temporal_data pattern)
temporal_clause
    : TEMPORAL COLON NEWLINE
      INDENT
      temporal_field+
      DEDENT
    ;

temporal_field
    : IDENTIFIER COLON field_type field_qualifiers? NEWLINE
    ;

// ============================================================================
// WORKFLOW DEFINITIONS
// ============================================================================

workflow_definition
    : DEFINE WORKFLOW COLON IDENTIFIER NEWLINE
      INDENT
      workflow_body
      DEDENT
    ;

workflow_body
    : workflow_clause+
    ;

workflow_clause
    : inputs_clause
    | outputs_clause
    | step_definition
    ;

// Workflow inputs
inputs_clause
    : INPUTS COLON NEWLINE
      INDENT
      parameter_item+
      DEDENT
    ;

// Workflow outputs
outputs_clause
    : OUTPUTS COLON NEWLINE
      INDENT
      parameter_item+
      DEDENT
    ;

parameter_item
    : HYPHEN IDENTIFIER COLON field_type NEWLINE
    ;

// Workflow step
step_definition
    : STEP COLON IDENTIFIER NEWLINE
      INDENT
      step_body
      DEDENT
    ;

step_body
    : step_clause+
    ;

step_clause
    : actions_clause
    | wait_for_clause
    | on_error_clause
    | next_clause
    ;

// Actions
actions_clause
    : ACTIONS COLON NEWLINE
      INDENT
      action_statement+
      DEDENT
    ;

action_statement
    : HYPHEN action_expression NEWLINE
      (INDENT action_statement+ DEDENT)?  // Nested actions (for loops, conditionals)
    ;

action_expression
    : load_action
    | create_action
    | update_action
    | delete_action
    | calculate_action
    | transition_action
    | log_action
    | send_action
    | set_action
    | increment_action
    | for_each_action
    | parallel_action
    | when_action
    | STRING_LITERAL  // Free-form action description
    ;

load_action
    : LOAD IDENTIFIER FROM expression
    ;

create_action
    : CREATE IDENTIFIER (WITH COLON)?
    ;

update_action
    : UPDATE field_reference (ASSIGN_OP | PLUS_ASSIGN | MINUS_ASSIGN) expression
    ;

delete_action
    : DELETE IDENTIFIER
    ;

calculate_action
    : CALCULATE IDENTIFIER USING IDENTIFIER RULE
    ;

transition_action
    : TRANSITION IDENTIFIER TO IDENTIFIER
    ;

log_action
    : LOG STRING_LITERAL (WITH expression)?
    ;

send_action
    : SEND IDENTIFIER (TO IDENTIFIER)?
    ;

set_action
    : SET IDENTIFIER TO expression
    ;

increment_action
    : INCREMENT field_reference BY expression
    ;

for_each_action
    : FOR EACH IDENTIFIER IN expression COLON
    ;

parallel_action
    : PARALLEL COLON
    ;

when_action
    : WHEN expression COLON
    ;

// Wait for
wait_for_clause
    : WAIT_FOR COLON IDENTIFIER NEWLINE
      (TIMEOUT COLON duration_literal NEWLINE)?
    ;

// Error handling
on_error_clause
    : ON_ERROR COLON GOTO IDENTIFIER NEWLINE
    ;

// Next step routing
next_clause
    : NEXT COLON next_target NEWLINE
    ;

next_target
    : GOTO IDENTIFIER
    | conditional_next
    ;

conditional_next
    : NEWLINE
      INDENT
      when_branch+
      (otherwise_branch)?
      DEDENT
    ;

when_branch
    : WHEN expression COLON GOTO IDENTIFIER NEWLINE
    ;

otherwise_branch
    : OTHERWISE COLON GOTO IDENTIFIER NEWLINE
    ;

// ============================================================================
// RULE DEFINITIONS
// ============================================================================

rules_definition
    : DEFINE RULES COLON IDENTIFIER NEWLINE
      INDENT
      rule_definition+
      DEDENT
    ;

rule_definition
    : RULE COLON IDENTIFIER NEWLINE
      INDENT
      rule_body
      DEDENT
    ;

rule_body
    : rule_clause+
    ;

rule_clause
    : given_clause
    | when_clause
    | then_clause
    | calculate_clause
    | return_clause
    | otherwise_clause
    ;

// Given (inputs)
given_clause
    : GIVEN COLON NEWLINE
      INDENT
      parameter_item+
      DEDENT
    ;

// When (condition)
when_clause
    : WHEN COLON NEWLINE
      INDENT
      expression NEWLINE
      DEDENT
    ;

// Then (consequence)
then_clause
    : THEN COLON NEWLINE
      INDENT
      action_statement+
      DEDENT
    ;

// Calculate (computation logic)
calculate_clause
    : CALCULATE COLON NEWLINE
      INDENT
      calculation_statement+
      DEDENT
    ;

calculation_statement
    : IDENTIFIER ASSIGN_OP calculation_expression NEWLINE
    ;

calculation_expression
    : when_expression
    | arithmetic_expression
    | function_call
    | literal_value
    ;

when_expression
    : WHEN expression COLON calculation_expression NEWLINE
      (INDENT when_branch_calc+ DEDENT)?
      (WHEN expression COLON calculation_expression NEWLINE)*
      (OTHERWISE COLON calculation_expression)?
    ;

when_branch_calc
    : WHEN expression COLON calculation_expression NEWLINE
    ;

// Return (outputs)
return_clause
    : RETURN COLON NEWLINE
      INDENT
      return_item+
      DEDENT
    ;

return_item
    : HYPHEN IDENTIFIER COLON field_type NEWLINE
    ;

// Otherwise (else clause)
otherwise_clause
    : OTHERWISE COLON NEWLINE
      INDENT
      action_statement+
      DEDENT
    ;

// ============================================================================
// PARAMETER DEFINITIONS (PCF)
// ============================================================================

parameter_definition
    : DEFINE PARAMETER COLON IDENTIFIER NEWLINE
      INDENT
      parameter_body
      DEDENT
    ;

parameter_body
    : parameter_clause+
    ;

parameter_clause
    : pattern_clause
    | business_domain_clause
    | type_clause
    | default_clause
    | constraints_param_clause
    | hot_reload_clause
    | validation_clause
    | documentation_clause
    ;

type_clause
    : TYPE COLON field_type NEWLINE
    ;

default_clause
    : DEFAULT COLON literal_value NEWLINE
    ;

constraints_param_clause
    : CONSTRAINTS COLON NEWLINE
      INDENT
      constraint_definition+
      DEDENT
    ;

hot_reload_clause
    : HOT_RELOAD COLON boolean_literal NEWLINE
    ;

validation_clause
    : VALIDATION COLON STRING_LITERAL NEWLINE
    ;

documentation_clause
    : DOCUMENTATION COLON STRING_LITERAL NEWLINE
    ;

// ============================================================================
// EXPRESSIONS
// ============================================================================

expression
    : logical_expression
    ;

logical_expression
    : comparison_expression (logical_op comparison_expression)*
    ;

logical_op
    : AND
    | OR
    ;

comparison_expression
    : arithmetic_expression (comparison_op arithmetic_expression)?
    ;

comparison_op
    : EQUALS
    | NOT_EQUALS
    | LESS_THAN
    | LESS_THAN_EQUALS
    | GREATER_THAN
    | GREATER_THAN_EQUALS
    ;

arithmetic_expression
    : multiplicative_expression ((PLUS | MINUS) multiplicative_expression)*
    ;

multiplicative_expression
    : unary_expression ((MULTIPLY | DIVIDE) unary_expression)*
    ;

unary_expression
    : NOT? primary_expression
    ;

primary_expression
    : literal_value
    | field_reference
    | function_call
    | LPAREN expression RPAREN
    ;

field_reference
    : IDENTIFIER (DOT IDENTIFIER)*
    ;

function_call
    : IDENTIFIER LPAREN (expression (COMMA expression)*)? RPAREN
    ;

membership_expression
    : field_reference IS ONE OF value_list
    | field_reference IS NOT ONE OF value_list
    ;

// ============================================================================
// LITERALS
// ============================================================================

literal_value
    : STRING_LITERAL
    | NUMBER_LITERAL
    | money_literal
    | boolean_literal
    | date_literal
    | duration_literal
    | NULL
    ;

money_literal
    : DOLLAR NUMBER_LITERAL
    ;

boolean_literal
    : YES
    | NO
    ;

date_literal
    : TODAY
    | NOW
    ;

duration_literal
    : NUMBER_LITERAL (DAYS | HOURS | MINUTES | SECONDS | MONTHS | YEARS)
    ;

// ============================================================================
// LEXER RULES
// ============================================================================

// Keywords - Definition
DEFINE              : 'define';
ENTITY              : 'entity';
WORKFLOW            : 'workflow';
RULE                : 'rule';
RULES               : 'rules';
PARAMETER           : 'parameter';

// Keywords - Entity
PATTERN             : 'pattern';
BUSINESS_DOMAIN     : 'business_domain';
BIAN_MAPPING        : 'bian_mapping';
IDENTITY            : 'identity';
RELATIONSHIPS       : 'relationships';
MUST                : 'must';
CURRENT_STATE       : 'current_state';
INITIAL_STATE       : 'initial_state';
TRANSITIONS         : 'transitions';
VERSIONING          : 'versioning';
TEMPORAL            : 'temporal';

// Keywords - Patterns
MASTER_DATA         : 'master_data';
IMMUTABLE_LEDGER    : 'immutable_ledger';
VERSIONED_CONFIGURATION : 'versioned_configuration';
OPERATIONAL_PARAMETERS : 'operational_parameters';
EVENT_LOG           : 'event_log';
STATE_MACHINE       : 'state_machine';
TEMPORAL_DATA       : 'temporal_data';
REFERENCE_DATA      : 'reference_data';

// Keywords - Types
TEXT                : 'text';
NUMBER              : 'number';
MONEY               : 'money';
DATE                : 'date';
TIMESTAMP           : 'timestamp';
BOOLEAN             : 'boolean';
EMAIL               : 'email';
PHONE               : 'phone';
PERCENTAGE          : 'percentage';
DURATION            : 'duration';
TIME                : 'time';

// Keywords - Field Qualifiers
UNIQUE              : 'unique';
REQUIRED            : 'required';
CANNOT_CHANGE       : 'cannot_change';
AUTO_INCREMENT      : 'auto_increment';
DEFAULT             : 'default';
BETWEEN             : 'between';
VALUES              : 'values';

// Keywords - Relationships
BELONGS_TO          : 'belongs_to';
HAS_MANY            : 'has_many';
HAS_ONE             : 'has_one';
REFERENCES          : 'references';
USES                : 'uses';

// Keywords - Workflow
INPUTS              : 'inputs';
OUTPUTS             : 'outputs';
STEP                : 'step';
ACTIONS             : 'actions';
NEXT                : 'next';
GOTO                : 'goto';
WHEN                : 'when';
OTHERWISE           : 'otherwise';
WAIT_FOR            : 'wait_for';
TIMEOUT             : 'timeout';
ON_ERROR            : 'on_error';

// Keywords - Actions
LOAD                : 'load';
CREATE              : 'create';
UPDATE              : 'update';
DELETE              : 'delete';
CALCULATE           : 'calculate';
TRANSITION          : 'transition';
LOG                 : 'log';
SEND                : 'send';
SET                 : 'set';
INCREMENT           : 'increment';
FOR                 : 'for';
EACH                : 'each';
IN                  : 'in';
PARALLEL            : 'parallel';

// Keywords - Rules
GIVEN               : 'given';
THEN                : 'then';
RETURN              : 'return';

// Keywords - State Machine
FROM                : 'from';
TO                  : 'to';

// Keywords - Parameter
TYPE                : 'type';
CONSTRAINTS         : 'constraints';
HOT_RELOAD          : 'hot_reload';
VALIDATION          : 'validation';
DOCUMENTATION       : 'documentation';

// Keywords - Operators
AND                 : 'and';
OR                  : 'or';
NOT                 : 'not';
IS                  : 'is';
ONE                 : 'one';
OF                  : 'of';

// Keywords - Special Values
YES                 : 'yes';
NO                  : 'no';
NULL                : 'null';
TODAY               : 'today';
NOW                 : 'now';

// Keywords - Prepositions
FROM_KEYWORD        : 'from';
WITH                : 'with';
BY                  : 'by';
USING               : 'using';

// Time Units
DAYS                : 'days';
HOURS               : 'hours';
MINUTES             : 'minutes';
SECONDS             : 'seconds';
MONTHS              : 'months';
YEARS               : 'years';

// Operators
COLON               : ':';
COMMA               : ',';
DOT                 : '.';
PIPE                : '|';
HYPHEN              : '-';
PLUS                : '+';
MINUS               : '-';
MULTIPLY            : '*';
DIVIDE              : '/';
ASSIGN_OP           : '=';
PLUS_ASSIGN         : '+=';
MINUS_ASSIGN        : '-=';
EQUALS              : '==';
NOT_EQUALS          : '!=';
LESS_THAN           : '<';
LESS_THAN_EQUALS    : '<=';
GREATER_THAN        : '>';
GREATER_THAN_EQUALS : '>=';
LPAREN              : '(';
RPAREN              : ')';
DOLLAR              : '$';

// Literals
STRING_LITERAL
    : '"' (~["\r\n] | '\\' .)* '"'
    ;

NUMBER_LITERAL
    : [0-9]+ ('.' [0-9]+)?
    ;

IDENTIFIER
    : [a-z] [a-z0-9_]*
    ;

// Whitespace and Indentation
NEWLINE
    : '\r'? '\n' SPACES?
    ;

fragment SPACES
    : [ \t]*
    ;

WS
    : [ \t]+ -> skip
    ;

// Comments
COMMENT
    : '#' ~[\r\n]* -> skip
    ;

// Indentation tokens (to be handled by custom lexer)
INDENT
    : '<INDENT>'  // Placeholder - will be emitted by custom lexer
    ;

DEDENT
    : '<DEDENT>'  // Placeholder - will be emitted by custom lexer
    ;
