pub mod constants;
mod environment;
use crate::ast::{Expression, Statement};
pub use environment::Environment;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use self::constants::{FALSE, TRUE};

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Array(Vec<Object>),
    Hash(HashMap<HashKey, Object>),
    Return(Box<Object>),
    Function(Vec<Expression>, Box<Statement>, Rc<RefCell<Environment>>),
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
            Object::Hash(_) => "HASH",
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

    pub fn is_truthy(&self) -> bool {
        match self.clone() {
            TRUE => true,
            FALSE => false,
            Object::Integer(_) => true,
            Object::Null => false,
            _ => true,
        }
    }
}

impl From<&HashKey> for Object {
    fn from(key: &HashKey) -> Self {
        match key {
            HashKey::String(string) => Object::String(string.clone()),
            HashKey::Integer(integer) => Object::Integer(*integer),
            HashKey::Boolean(boolean) => Object::Boolean(*boolean),
        }
    }
}

impl From<bool> for Object {
    fn from(boolean: bool) -> Self {
        if boolean {
            TRUE
        } else {
            FALSE
        }
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
            Object::Hash(items) => {
                let items = items
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>();
                write!(f, "{{{}}}", items.join(", "))
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

pub struct HashKeyError(pub Object);
impl Display for HashKeyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} cannot be used as hash key", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum HashKey {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl TryFrom<Object> for HashKey {
    type Error = HashKeyError;

    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        let key = match obj {
            Object::Integer(integer) => HashKey::Integer(integer),
            Object::Boolean(boolean) => HashKey::Boolean(boolean),
            Object::String(string) => HashKey::String(string),
            _ => return Err(HashKeyError(obj)),
        };

        Ok(key)
    }
}

impl Display for HashKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HashKey::Integer(integer) => write!(f, "{}", integer),
            HashKey::Boolean(boolean) => write!(f, "{}", boolean),
            HashKey::String(string) => write!(f, "\"{}\"", string),
        }
    }
}
