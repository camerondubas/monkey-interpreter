use std::{fmt::Display, str::FromStr};

use super::error::ReplError;

#[derive(Debug, PartialEq, Clone)]
pub enum ReplMode {
    Lexer,
    Parser,
    Ast,
    Eval,
    Compiler,
    VM,
}

impl Display for ReplMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mode_str = match self {
            ReplMode::Lexer => "lexer",
            ReplMode::Parser => "parser",
            ReplMode::Ast => "ast",
            ReplMode::Eval => "eval",
            ReplMode::Compiler => "compiler",
            ReplMode::VM => "vm",
        };

        write!(f, "{}", mode_str)
    }
}

impl FromStr for ReplMode {
    type Err = ReplError;

    fn from_str(mode_str: &str) -> Result<Self, Self::Err> {
        let mode = match mode_str {
            "lexer" => ReplMode::Lexer,
            "parser" => ReplMode::Parser,
            "ast" => ReplMode::Ast,
            "eval" => ReplMode::Eval,
            "compiler" => ReplMode::Compiler,
            "vm" => ReplMode::VM,
            _ => return Err(ReplError::UnknownMode(mode_str.to_string())),
        };

        Ok(mode)
    }
}
