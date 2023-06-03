use std::fmt::Display;

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
    Identifier(String),
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

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token = match self {
            Token::Illegal(_) => "Illegal Token!",
            Token::Eof => "",
            Token::Let => "let",
            Token::Function => "fn",
            Token::True => "true",
            Token::False => "false",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::Identifier(value) => value,
            Token::Integer(value) => value,
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Bang => "!",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::Lt => "<",
            Token::Gt => ">",
            Token::Eq => "==",
            Token::NotEq => "!=",
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
        };

        write!(f, "{}", token)
    }
}
