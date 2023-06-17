use std::str;
mod tokens;

pub use tokens::Token;

const EOF: u8 = 0;

pub struct Lexer {
    input: Vec<u8>,
    position: usize,      // current position
    read_position: usize, // next char
    ch: u8,               // char under examination
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.as_bytes().to_vec(),
            position: 0,
            read_position: 0,
            ch: 0,
        };

        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input[self.read_position]
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            b'(' => Token::LeftParen,
            b')' => Token::RightParen,
            b'{' => Token::LeftBrace,
            b'}' => Token::RightBrace,
            b'[' => Token::LeftBracket,
            b']' => Token::RightBracket,
            b':' => Token::Colon,
            b';' => Token::Semicolon,
            b',' => Token::Comma,
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'!' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    Token::NotEq
                }
                _ => Token::Bang,
            },
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'<' => Token::Lt,
            b'>' => Token::Gt,
            b'=' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    Token::Eq
                }
                _ => Token::Assign,
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let identifier = &self.read_identifier();
                return Lexer::get_identifier_token(identifier);
            }
            b'0'..=b'9' => return Token::Integer(self.read_number()),
            b'"' => match self.read_string() {
                Ok(str) => Token::String(str),
                Err(str) => Token::Illegal(str),
            },
            EOF => Token::Eof,
            _ => Token::Illegal(String::from("Invalid token")),
        };
        self.read_char();

        token
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();
        }

        str::from_utf8(&self.input[position..self.position])
            .unwrap()
            .to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        str::from_utf8(&self.input[position..self.position])
            .unwrap()
            .to_string()
    }

    fn read_string(&mut self) -> Result<String, String> {
        self.read_char();
        let position = self.position;
        while self.ch != b'"' {
            if self.ch == EOF {
                return Err("Unterminated String".to_string());
            }

            self.read_char();
        }

        String::from_utf8(self.input[position..self.position].to_vec())
            .map_err(|err| err.to_string())
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }

    fn get_identifier_token(token: &str) -> Token {
        match token {
            "let" => Token::Let,
            "fn" => Token::Function,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Identifier(token.to_string()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, Token};

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;

        \"foobar\"
        \"foo bar\"

        [1,2];
        {\"foo\": \"bar\"}
        ";

        let tokens = vec![
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::Integer(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assign,
            Token::Integer(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LeftParen,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::RightParen,
            Token::LeftBrace,
            Token::Identifier(String::from("x")),
            Token::Plus,
            Token::Identifier(String::from("y")),
            Token::Semicolon,
            Token::RightBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assign,
            Token::Identifier(String::from("add")),
            Token::LeftParen,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::RightParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Integer(String::from("5")),
            Token::Semicolon,
            Token::Integer(String::from("5")),
            Token::Lt,
            Token::Integer(String::from("10")),
            Token::Gt,
            Token::Integer(String::from("5")),
            Token::Semicolon,
            Token::If,
            Token::LeftParen,
            Token::Integer(String::from("5")),
            Token::Lt,
            Token::Integer(String::from("10")),
            Token::RightParen,
            Token::LeftBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RightBrace,
            Token::Integer(String::from("10")),
            Token::Eq,
            Token::Integer(String::from("10")),
            Token::Semicolon,
            Token::Integer(String::from("10")),
            Token::NotEq,
            Token::Integer(String::from("9")),
            Token::Semicolon,
            Token::String(String::from("foobar")),
            Token::String(String::from("foo bar")),
            Token::LeftBracket,
            Token::Integer(String::from("1")),
            Token::Comma,
            Token::Integer(String::from("2")),
            Token::RightBracket,
            Token::Semicolon,
            Token::LeftBrace,
            Token::String(String::from("foo")),
            Token::Colon,
            Token::String(String::from("bar")),
            Token::RightBrace,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);
        for token in tokens {
            let _token = lexer.next_token();
            println!("token: {:?}", _token);

            assert_eq!(_token, token);
        }
    }

    #[test]
    fn test_illegal_tokens() {
        let input = "let five = \"something";

        let tokens = vec![
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::Illegal("Unterminated String".to_string()),
        ];

        let mut lexer = Lexer::new(input);
        for token in tokens {
            let _token = lexer.next_token();
            println!("token: {:?}", _token);

            assert_eq!(_token, token);
        }
    }
}
