// Abstract Syntax Tree definitions for Credit Card DSL
//
// This module defines the AST structure that represents parsed DSL code.
// The AST is constructed from ANTLR4 parse tree and is the input to
// semantic analysis and code generation phases.

pub mod entity;
pub mod workflow;
pub mod rule;
pub mod parameter;
pub mod types;
pub mod expressions;

use serde::{Deserialize, Serialize};

/// Top-level compilation unit representing a complete DSL file
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CompilationUnit {
    pub definitions: Vec<Definition>,
    pub source_file: Option<String>,
}

impl CompilationUnit {
    pub fn new() -> Self {
        Self {
            definitions: Vec::new(),
            source_file: None,
        }
    }

    pub fn with_source_file(mut self, path: String) -> Self {
        self.source_file = Some(path);
        self
    }

    pub fn add_definition(&mut self, def: Definition) {
        self.definitions.push(def);
    }
}

/// Top-level definition types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Definition {
    Entity(entity::EntityDefinition),
    Workflow(workflow::WorkflowDefinition),
    Rules(rule::RulesDefinition),
    Parameter(parameter::ParameterDefinition),
}

/// Source location for error reporting
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceLocation {
    pub file: Option<String>,
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    pub fn new(line: usize, column: usize) -> Self {
        Self {
            file: None,
            line,
            column,
        }
    }

    pub fn with_file(mut self, file: String) -> Self {
        self.file = Some(file);
        self
    }
}

/// Data mutation patterns
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Pattern {
    MasterData,
    ImmutableLedger,
    VersionedConfiguration,
    OperationalParameters,
    EventLog,
    StateMachine,
    TemporalData,
    ReferenceData,
    Composition(Vec<Pattern>),  // For pattern composition
}

impl Pattern {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "master_data" => Some(Pattern::MasterData),
            "immutable_ledger" => Some(Pattern::ImmutableLedger),
            "versioned_configuration" => Some(Pattern::VersionedConfiguration),
            "operational_parameters" => Some(Pattern::OperationalParameters),
            "event_log" => Some(Pattern::EventLog),
            "state_machine" => Some(Pattern::StateMachine),
            "temporal_data" => Some(Pattern::TemporalData),
            "reference_data" => Some(Pattern::ReferenceData),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            Pattern::MasterData => "master_data",
            Pattern::ImmutableLedger => "immutable_ledger",
            Pattern::VersionedConfiguration => "versioned_configuration",
            Pattern::OperationalParameters => "operational_parameters",
            Pattern::EventLog => "event_log",
            Pattern::StateMachine => "state_machine",
            Pattern::TemporalData => "temporal_data",
            Pattern::ReferenceData => "reference_data",
            Pattern::Composition(_) => "composition",
        }
    }

    pub fn is_immutable(&self) -> bool {
        matches!(
            self,
            Pattern::ImmutableLedger | Pattern::EventLog | Pattern::ReferenceData
        )
    }

    pub fn supports_versioning(&self) -> bool {
        matches!(
            self,
            Pattern::VersionedConfiguration | Pattern::TemporalData
        )
    }

    pub fn supports_audit_trail(&self) -> bool {
        matches!(
            self,
            Pattern::MasterData | Pattern::ImmutableLedger | Pattern::EventLog
        )
    }
}
