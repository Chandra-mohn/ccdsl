// Expression AST nodes

use super::types::*;
use super::SourceLocation;
use serde::{Deserialize, Serialize};

/// Generic expression
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Literal(Value),
    FieldReference(FieldReference),
    FunctionCall(FunctionCall),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Logical(LogicalExpression),
    Comparison(ComparisonExpression),
    Membership(MembershipExpression),
}

/// Field reference (e.g., account.current_balance)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldReference {
    pub path: Vec<String>,  // ["account", "current_balance"]
    pub location: SourceLocation,
}

impl FieldReference {
    pub fn new(path: Vec<String>, location: SourceLocation) -> Self {
        Self { path, location }
    }

    pub fn simple(name: String, location: SourceLocation) -> Self {
        Self {
            path: vec![name],
            location,
        }
    }

    pub fn to_string(&self) -> String {
        self.path.join(".")
    }
}

/// Function call
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionCall {
    pub function_name: String,
    pub arguments: Vec<Expression>,
    pub location: SourceLocation,
}

/// Binary expression (arithmetic)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOperator {
    Add,       // +
    Subtract,  // -
    Multiply,  // *
    Divide,    // /
}

impl BinaryOperator {
    pub fn to_str(&self) -> &'static str {
        match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
        }
    }
}

/// Unary expression
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOperator {
    Not,      // not
    Negate,   // -
}

/// Logical expression (and/or)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LogicalExpression {
    pub left: Box<Expression>,
    pub operator: LogicalOperator,
    pub right: Box<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LogicalOperator {
    And,
    Or,
}

impl LogicalOperator {
    pub fn to_str(&self) -> &'static str {
        match self {
            LogicalOperator::And => "and",
            LogicalOperator::Or => "or",
        }
    }
}

/// Comparison expression
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ComparisonExpression {
    pub left: Box<Expression>,
    pub operator: ComparisonOperator,
    pub right: Box<Expression>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ComparisonOperator {
    Equals,              // ==
    NotEquals,           // !=
    LessThan,            // <
    LessThanEquals,      // <=
    GreaterThan,         // >
    GreaterThanEquals,   // >=
}

impl ComparisonOperator {
    pub fn to_str(&self) -> &'static str {
        match self {
            ComparisonOperator::Equals => "==",
            ComparisonOperator::NotEquals => "!=",
            ComparisonOperator::LessThan => "<",
            ComparisonOperator::LessThanEquals => "<=",
            ComparisonOperator::GreaterThan => ">",
            ComparisonOperator::GreaterThanEquals => ">=",
        }
    }
}

/// Membership expression (is one of / is not one of)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MembershipExpression {
    pub field: FieldReference,
    pub negated: bool,  // "is not one of"
    pub values: Vec<String>,
    pub location: SourceLocation,
}
