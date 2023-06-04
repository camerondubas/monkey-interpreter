mod environment;

use std::fmt::Display;

pub use environment::Environment;

use crate::ast::{Expression, Statement};

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Function(Vec<Expression>, Box<Statement>, Environment),
    Error(String),
    Null,
}

impl Object {
    pub fn get_type(&self) -> &str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Return(_) => "RETURN",
            Object::Function(_, _, _) => "FUNCTION",
            Object::Error(_) => "ERROR",
            Object::Null => "NULL",
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Object::Error(_))
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Return(ret) => write!(f, "{}", ret),
            Object::Function(params, body, _) => {
                let params = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>();
                write!(f, "fn({}) {{\n  {} \n}}", params.join(", "), body)
            }
            Object::Error(message) => write!(f, "ERROR: {}", message),
            Object::Null => write!(f, "null"),
        }
    }
}
