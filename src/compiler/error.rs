use crate::{
    ast::{Expression, Statement},
    lexer::Token,
};
use std::{fmt::Display, result::Result};

pub type CompilerResult = Result<(), CompilerError>;

pub enum CompilerError {
    UnhandledStatement(Statement),
    UnhandledExpression(Expression),
    UnknownInfixOperator(Token),
    UnknownPrefixOperator(Token),
    UndefinedVariable(String),
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
            CompilerError::UnknownInfixOperator(operator) => {
                write!(f, "Unknown infix operator: {:?} ", operator)
            }
            CompilerError::UnknownPrefixOperator(operator) => {
                write!(f, "Unknown prefix operator: {:?} ", operator)
            }
            CompilerError::UndefinedVariable(identifier) => {
                write!(f, "Undefined variable: {:?} ", identifier)
            }
        }
    }
}
