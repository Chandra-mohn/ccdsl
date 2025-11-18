// Workflow definition AST nodes

use super::{SourceLocation, expressions::*, types::*};
use serde::{Deserialize, Serialize};

/// Workflow definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WorkflowDefinition {
    pub name: String,
    pub inputs: Vec<Parameter>,
    pub outputs: Vec<Parameter>,
    pub steps: Vec<Step>,
    pub location: SourceLocation,
}

/// Workflow parameter (input/output)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub param_type: FieldType,
    pub location: SourceLocation,
}

/// Workflow step
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Step {
    pub name: String,
    pub actions: Vec<Action>,
    pub wait_for: Option<WaitFor>,
    pub on_error: Option<String>,  // Goto step name
    pub next: NextTarget,
    pub location: SourceLocation,
}

/// Wait for clause
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WaitFor {
    pub event: String,
    pub timeout: Option<Duration>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Duration {
    pub value: f64,
    pub unit: super::types::TimeUnit,
}

/// Next step target
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum NextTarget {
    Goto(String),
    Conditional(Vec<ConditionalBranch>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConditionalBranch {
    pub condition: Option<Expression>,  // None = otherwise
    pub target: String,
}

/// Action types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Action {
    Load(LoadAction),
    Create(CreateAction),
    Update(UpdateAction),
    Delete(DeleteAction),
    Calculate(CalculateAction),
    Transition(TransitionAction),
    Log(LogAction),
    Send(SendAction),
    Set(SetAction),
    Increment(IncrementAction),
    ForEach(ForEachAction),
    Parallel(ParallelAction),
    When(WhenAction),
    FreeForm(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LoadAction {
    pub variable: String,
    pub source: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CreateAction {
    pub entity_type: String,
    pub fields: Vec<FieldAssignment>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldAssignment {
    pub field: String,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UpdateAction {
    pub field: FieldReference,
    pub operator: UpdateOperator,
    pub value: Expression,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UpdateOperator {
    Assign,      // =
    PlusAssign,  // +=
    MinusAssign, // -=
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DeleteAction {
    pub entity: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CalculateAction {
    pub variable: String,
    pub rule_name: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TransitionAction {
    pub entity: String,
    pub to_state: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LogAction {
    pub message: String,
    pub context: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SendAction {
    pub message_type: String,
    pub recipient: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SetAction {
    pub variable: String,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IncrementAction {
    pub field: FieldReference,
    pub amount: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ForEachAction {
    pub variable: String,
    pub collection: Expression,
    pub actions: Vec<Action>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParallelAction {
    pub actions: Vec<Action>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WhenAction {
    pub condition: Expression,
    pub actions: Vec<Action>,
}
