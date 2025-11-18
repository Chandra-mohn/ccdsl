// Entity definition AST nodes

use super::{Pattern, SourceLocation};
use super::types::*;
use super::expressions::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Complete entity definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EntityDefinition {
    pub name: String,
    pub pattern: Pattern,
    pub business_domain: Option<String>,
    pub bian_mapping: Option<BianMapping>,
    pub identity: Option<IdentityClause>,
    pub field_groups: Vec<FieldGroup>,
    pub relationships: Vec<Relationship>,
    pub constraints: Vec<Constraint>,
    pub state_machine: Option<StateMachine>,
    pub versioning: Option<VersioningClause>,
    pub temporal: Option<TemporalClause>,
    pub location: SourceLocation,
}

impl EntityDefinition {
    pub fn new(name: String, pattern: Pattern, location: SourceLocation) -> Self {
        Self {
            name,
            pattern,
            business_domain: None,
            bian_mapping: None,
            identity: None,
            field_groups: Vec::new(),
            relationships: Vec::new(),
            constraints: Vec::new(),
            state_machine: None,
            versioning: None,
            temporal: None,
            location,
        }
    }

    /// Get all fields across all groups
    pub fn all_fields(&self) -> Vec<&Field> {
        let mut fields = Vec::new();

        if let Some(identity) = &self.identity {
            fields.extend(identity.fields.iter());
        }

        for group in &self.field_groups {
            fields.extend(group.fields.iter());
        }

        fields
    }

    /// Find field by name
    pub fn find_field(&self, name: &str) -> Option<&Field> {
        self.all_fields().into_iter().find(|f| f.name == name)
    }

    /// Check if entity has a specific field qualifier
    pub fn has_unique_fields(&self) -> bool {
        self.all_fields().iter().any(|f| f.has_qualifier(FieldQualifier::Unique))
    }
}

/// BIAN service domain mapping
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BianMapping {
    pub service_domain: String,
    pub service_domain_id: Option<u32>,
    pub asset_type: Option<String>,
    pub generic_artifact: Option<String>,
    pub extra_fields: HashMap<String, String>,
}

/// Identity clause (primary keys)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdentityClause {
    pub fields: Vec<Field>,
}

/// Field group (profile, metrics, etc.)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldGroup {
    pub group_name: String,
    pub fields: Vec<Field>,
}

/// Field definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Field {
    pub name: String,
    pub field_type: FieldType,
    pub qualifiers: Vec<FieldQualifier>,
    pub location: SourceLocation,
}

impl Field {
    pub fn new(name: String, field_type: FieldType, location: SourceLocation) -> Self {
        Self {
            name,
            field_type,
            qualifiers: Vec::new(),
            location,
        }
    }

    pub fn with_qualifiers(mut self, qualifiers: Vec<FieldQualifier>) -> Self {
        self.qualifiers = qualifiers;
        self
    }

    pub fn has_qualifier(&self, qualifier: FieldQualifier) -> bool {
        self.qualifiers.contains(&qualifier)
    }

    pub fn is_unique(&self) -> bool {
        self.has_qualifier(FieldQualifier::Unique)
    }

    pub fn is_required(&self) -> bool {
        self.has_qualifier(FieldQualifier::Required)
    }

    pub fn is_immutable(&self) -> bool {
        self.has_qualifier(FieldQualifier::CannotChange)
    }

    pub fn get_default(&self) -> Option<&Value> {
        for qualifier in &self.qualifiers {
            if let FieldQualifier::Default(value) = qualifier {
                return Some(value);
            }
        }
        None
    }
}

/// Field qualifiers
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum FieldQualifier {
    Unique,
    Required,
    CannotChange,
    AutoIncrement,
    Default(Value),
    Between { min: Value, max: Value },
    Values(Vec<String>),
}

/// Relationship types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Relationship {
    pub relationship_type: RelationshipType,
    pub target_entity: String,
    pub qualifiers: Vec<RelationshipQualifier>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RelationshipType {
    BelongsTo,
    HasMany,
    HasOne,
    References,
    Uses,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RelationshipQualifier {
    CannotChange,
    Required,
}

/// Constraint (must block)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Constraint {
    pub expression: ConstraintExpression,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ConstraintExpression {
    Comparison(ComparisonExpression),
    Logical(LogicalExpression),
    Membership(MembershipExpression),
    FreeForm(String),  // Human-readable constraint description
}

/// State machine definition (for state_machine pattern)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StateMachine {
    pub current_state_field: String,
    pub state_type: FieldType,
    pub states: Vec<String>,
    pub initial_state: String,
    pub transitions: Vec<StateTransition>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StateTransition {
    pub from_state: String,
    pub to_states: Vec<TransitionTarget>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TransitionTarget {
    pub to_state: String,
    pub condition: Option<String>,  // Optional "when" condition
}

/// Versioning clause (for versioned_configuration pattern)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VersioningClause {
    pub fields: Vec<Field>,
}

/// Temporal clause (for temporal_data pattern)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TemporalClause {
    pub fields: Vec<Field>,
}
