mod environment;

use std::fmt::Display;

pub use environment::Environment;

use crate::ast::{Expression, Statement};

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Array(Vec<Object>),
    Return(Box<Object>),
    Function(Vec<Expression>, Box<Statement>, Environment),
    BuiltInFunction(fn(args: Vec<Object>) -> Object),
    Error(String),
    Null,
}

impl Object {
    pub fn get_type(&self) -> &str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::String(_) => "STRING",
            Object::Boolean(_) => "BOOLEAN",
            Object::Array(_) => "ARRAY",
            Object::Return(_) => "RETURN",
            Object::Function(_, _, _) => "FUNCTION",
            Object::BuiltInFunction(_) => "BUILTIN",
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
            Object::String(string) => write!(f, "\"{}\"", string),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Array(items) => {
                let items = items.iter().map(|i| i.to_string()).collect::<Vec<String>>();
                write!(f, "[{}]", items.join(", "))
            }
            Object::Return(ret) => write!(f, "{}", ret),
            Object::Function(params, body, _) => {
                let params = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>();
                write!(f, "fn({}) {{\n  {} \n}}", params.join(", "), body)
            }
            Object::BuiltInFunction(_) => write!(f, "builtin object"),
            Object::Error(message) => write!(f, "ERROR: {}", message),
            Object::Null => write!(f, "null"),
        }
    }
}
