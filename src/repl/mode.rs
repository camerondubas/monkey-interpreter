use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum ReplMode {
    Lexer,
    Parser,
    Ast,
    Eval,
    Compiler,
}

impl Display for ReplMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mode_str = match self {
            ReplMode::Lexer => "lexer",
            ReplMode::Parser => "parser",
            ReplMode::Ast => "ast",
            ReplMode::Eval => "eval",
            ReplMode::Compiler => "compiler",
        };

        write!(f, "{}", mode_str)
    }
}

impl ReplMode {
    pub fn from_str(mode_str: &str) -> Result<ReplMode, String> {
        let mode = match mode_str {
            "lexer" => ReplMode::Lexer,
            "parser" => ReplMode::Parser,
            "ast" => ReplMode::Ast,
            "eval" => ReplMode::Eval,
            "compiler" => ReplMode::Compiler,
            _ => return Err(format!("{:?} is not a valid mode", mode_str)),
        };

        Ok(mode)
    }
}
