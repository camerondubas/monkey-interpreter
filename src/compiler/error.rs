use crate::ast::{Expression, Statement};
use std::{fmt::Display, result::Result};

pub type CompilerResult = Result<(), CompilerError>;

pub enum CompilerError {
    UnhandledStatement(Statement),
    UnhandledExpression(Expression),
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::UnhandledStatement(statement) => {
                write!(f, "Unhandled statement: {:?}", statement)
            }
            CompilerError::UnhandledExpression(expression) => {
                write!(f, "Unhandled expression: {:?}", expression)
            }
        }
    }
}
