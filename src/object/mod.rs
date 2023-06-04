use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer.value),
            Object::Boolean(boolean) => write!(f, "{}", boolean.value),
            Object::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Integer {
    pub value: i64,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Boolean {
    pub value: bool,
}
