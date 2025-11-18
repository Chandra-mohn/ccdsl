// Rule definition AST nodes

use super::{SourceLocation, expressions::*, types::*, workflow::*};
use serde::{Deserialize, Serialize};

/// Rules collection
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RulesDefinition {
    pub name: String,
    pub rules: Vec<Rule>,
    pub location: SourceLocation,
}

/// Individual rule
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Rule {
    pub name: String,
    pub given: Vec<Parameter>,
    pub when_condition: Option<Expression>,
    pub then_actions: Vec<Action>,
    pub calculate: Vec<Calculation>,
    pub returns: Vec<ReturnValue>,
    pub otherwise_actions: Vec<Action>,
    pub location: SourceLocation,
}

/// Calculation statement
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Calculation {
    pub variable: String,
    pub expression: CalculationExpression,
}

/// Calculation expression (can include when/otherwise)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CalculationExpression {
    Simple(Expression),
    Conditional(ConditionalCalculation),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConditionalCalculation {
    pub branches: Vec<ConditionalBranch>,
    pub otherwise: Option<Box<CalculationExpression>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConditionalBranch {
    pub condition: Expression,
    pub result: Box<CalculationExpression>,
}

/// Return value specification
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReturnValue {
    pub name: String,
    pub return_type: FieldType,
}
