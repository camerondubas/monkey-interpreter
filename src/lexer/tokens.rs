#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal(String),
    Eof,

    // Keywords
    Let,
    Function,
    True,
    False,
    If,
    Else,
    Return,

    // Identifiers + literals
    Indentifier(String),
    Integer(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,

    // Delimiters
    Comma,
    Semicolon,

    // Braces
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
}
