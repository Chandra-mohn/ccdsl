// Type system for Credit Card DSL

use serde::{Deserialize, Serialize};

/// Field types in the DSL
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FieldType {
    Text,
    Number,
    Money,
    Date,
    Timestamp,
    Boolean,
    Email,
    Phone,
    Percentage,
    Duration,
    Time,
    Entity(String),  // Reference to another entity
}

impl FieldType {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "text" => Some(FieldType::Text),
            "number" => Some(FieldType::Number),
            "money" => Some(FieldType::Money),
            "date" => Some(FieldType::Date),
            "timestamp" => Some(FieldType::Timestamp),
            "boolean" => Some(FieldType::Boolean),
            "email" => Some(FieldType::Email),
            "phone" => Some(FieldType::Phone),
            "percentage" => Some(FieldType::Percentage),
            "duration" => Some(FieldType::Duration),
            "time" => Some(FieldType::Time),
            _ => Some(FieldType::Entity(s.to_string())),  // Assume entity reference
        }
    }

    pub fn to_str(&self) -> String {
        match self {
            FieldType::Text => "text".to_string(),
            FieldType::Number => "number".to_string(),
            FieldType::Money => "money".to_string(),
            FieldType::Date => "date".to_string(),
            FieldType::Timestamp => "timestamp".to_string(),
            FieldType::Boolean => "boolean".to_string(),
            FieldType::Email => "email".to_string(),
            FieldType::Phone => "phone".to_string(),
            FieldType::Percentage => "percentage".to_string(),
            FieldType::Duration => "duration".to_string(),
            FieldType::Time => "time".to_string(),
            FieldType::Entity(name) => name.clone(),
        }
    }

    pub fn is_primitive(&self) -> bool {
        !matches!(self, FieldType::Entity(_))
    }

    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            FieldType::Number | FieldType::Money | FieldType::Percentage
        )
    }

    pub fn is_temporal(&self) -> bool {
        matches!(
            self,
            FieldType::Date | FieldType::Timestamp | FieldType::Time | FieldType::Duration
        )
    }

    /// Get Rust type representation
    pub fn to_rust_type(&self) -> String {
        match self {
            FieldType::Text => "String".to_string(),
            FieldType::Number => "f64".to_string(),
            FieldType::Money => "Money".to_string(),  // Custom type
            FieldType::Date => "Date".to_string(),     // Custom type
            FieldType::Timestamp => "DateTime<Utc>".to_string(),
            FieldType::Boolean => "bool".to_string(),
            FieldType::Email => "Email".to_string(),  // Custom validated type
            FieldType::Phone => "Phone".to_string(),  // Custom validated type
            FieldType::Percentage => "f64".to_string(),
            FieldType::Duration => "Duration".to_string(),
            FieldType::Time => "Time".to_string(),
            FieldType::Entity(name) => to_pascal_case(name),
        }
    }

    /// Get SQL type representation
    pub fn to_sql_type(&self) -> String {
        match self {
            FieldType::Text => "TEXT".to_string(),
            FieldType::Number => "NUMERIC".to_string(),
            FieldType::Money => "NUMERIC(19, 4)".to_string(),
            FieldType::Date => "DATE".to_string(),
            FieldType::Timestamp => "TIMESTAMP".to_string(),
            FieldType::Boolean => "BOOLEAN".to_string(),
            FieldType::Email => "TEXT".to_string(),
            FieldType::Phone => "TEXT".to_string(),
            FieldType::Percentage => "NUMERIC(5, 4)".to_string(),
            FieldType::Duration => "INTERVAL".to_string(),
            FieldType::Time => "TIME".to_string(),
            FieldType::Entity(_) => "TEXT".to_string(),  // Foreign key
        }
    }
}

/// Literal values
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    String(String),
    Number(f64),
    Money(f64, String),  // amount, currency
    Boolean(bool),
    Date(String),  // ISO format string
    Null,
}

impl Value {
    pub fn get_type(&self) -> FieldType {
        match self {
            Value::String(_) => FieldType::Text,
            Value::Number(_) => FieldType::Number,
            Value::Money(_, _) => FieldType::Money,
            Value::Boolean(_) => FieldType::Boolean,
            Value::Date(_) => FieldType::Date,
            Value::Null => FieldType::Text,  // Null can be any type
        }
    }

    pub fn to_rust_literal(&self) -> String {
        match self {
            Value::String(s) => format!("\"{}\".to_string()", s),
            Value::Number(n) => n.to_string(),
            Value::Money(amount, currency) => {
                format!("Money::new({}, \"{}\")", amount, currency)
            }
            Value::Boolean(b) => b.to_string(),
            Value::Date(d) => format!("Date::parse(\"{}\")", d),
            Value::Null => "None".to_string(),
        }
    }
}

/// Time duration units
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TimeUnit {
    Days,
    Hours,
    Minutes,
    Seconds,
    Months,
    Years,
}

/// Helper function to convert snake_case to PascalCase
pub fn to_pascal_case(s: &str) -> String {
    s.split('_')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => {
                    first.to_uppercase().collect::<String>() + chars.as_str()
                }
            }
        })
        .collect()
}

/// Helper function to convert PascalCase to snake_case
pub fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    let mut prev_lower = false;

    for (i, ch) in s.chars().enumerate() {
        if ch.is_uppercase() {
            if i > 0 && prev_lower {
                result.push('_');
            }
            result.push(ch.to_lowercase().next().unwrap());
            prev_lower = false;
        } else {
            result.push(ch);
            prev_lower = ch.is_lowercase();
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_pascal_case() {
        assert_eq!(to_pascal_case("customer"), "Customer");
        assert_eq!(to_pascal_case("card_product"), "CardProduct");
        assert_eq!(to_pascal_case("late_fee_schedule"), "LateFeeSchedule");
    }

    #[test]
    fn test_to_snake_case() {
        assert_eq!(to_snake_case("Customer"), "customer");
        assert_eq!(to_snake_case("CardProduct"), "card_product");
        assert_eq!(to_snake_case("LateFeeSchedule"), "late_fee_schedule");
    }

    #[test]
    fn test_field_type_conversions() {
        assert_eq!(FieldType::Text.to_rust_type(), "String");
        assert_eq!(FieldType::Money.to_sql_type(), "NUMERIC(19, 4)");
        assert!(FieldType::Number.is_numeric());
        assert!(FieldType::Date.is_temporal());
    }
}
