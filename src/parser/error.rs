use core::fmt;

use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError {
    ExpectedIdentifier(Token),
    ExpectedInteger(Token),
    ExpectedString(Token),
    ExpectedBoolean(Token),
    UnexpectedToken(Token, Token),
    MissingParsePrefixFunction(Token),
    IntegerParsingError(Token),
    UnknownError(Token),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::UnexpectedToken(expected, actual) => {
                write!(f, "Expected `{}`, got `{}`", expected, actual)
            }
            ParserError::ExpectedIdentifier(token) => {
                write!(f, "Expected an identifier, got `{}`", token)
            }
            ParserError::ExpectedInteger(token) => {
                write!(f, "Expected an integer, got `{}`", token)
            }
            ParserError::ExpectedString(token) => {
                write!(f, "Expected a string, got `{}`", token)
            }
            ParserError::ExpectedBoolean(token) => {
                write!(f, "Expected a boolean, got `{}`", token)
            }
            ParserError::MissingParsePrefixFunction(token) => {
                write!(f, "No prefix parse function for `{}`", token)
            }
            ParserError::IntegerParsingError(token) => {
                write!(f, "Could not parse `{}` as an integer", token)
            }
            ParserError::UnknownError(token) => {
                write!(f, "Unknown Error `{}`", token)
            }
        }
    }
}
