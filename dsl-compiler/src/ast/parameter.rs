// Parameter definition AST nodes (PCF - Product Configuration Fields)

use super::{Pattern, SourceLocation, types::*, entity::Constraint};
use serde::{Deserialize, Serialize};

/// Parameter definition (operational configuration)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParameterDefinition {
    pub name: String,
    pub pattern: Pattern,  // Should be OperationalParameters
    pub business_domain: Option<String>,
    pub param_type: FieldType,
    pub default_value: Option<Value>,
    pub constraints: Vec<Constraint>,
    pub hot_reload: bool,
    pub validation: Option<String>,
    pub documentation: Option<String>,
    pub location: SourceLocation,
}

impl ParameterDefinition {
    pub fn new(name: String, location: SourceLocation) -> Self {
        Self {
            name,
            pattern: Pattern::OperationalParameters,
            business_domain: None,
            param_type: FieldType::Text,
            default_value: None,
            constraints: Vec::new(),
            hot_reload: false,
            validation: None,
            documentation: None,
            location,
        }
    }
}
