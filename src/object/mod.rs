mod environment;

use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

pub use environment::Environment;

use crate::ast::{Expression, Statement};

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

    pub fn from_hash_key(key: HashKey) -> Object {
        match key {
            HashKey::String(string) => Object::String(string),
            HashKey::Integer(integer) => Object::Integer(integer),
            HashKey::Boolean(boolean) => Object::Boolean(boolean),
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum HashKey {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl HashKey {
    pub fn from_obj(obj: Object) -> Result<HashKey, Object> {
        let key = match obj {
            Object::Integer(integer) => HashKey::Integer(integer),
            Object::Boolean(boolean) => HashKey::Boolean(boolean),
            Object::String(string) => HashKey::String(string),
            _ => return Err(unhashable_type_error(obj)),
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

fn unhashable_type_error(obj: Object) -> Object {
    Object::Error(format!("unusable as hash key: {}", obj.get_type()))
}
