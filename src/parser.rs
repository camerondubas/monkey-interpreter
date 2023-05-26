use crate::{
    ast::{self, Expression, Statement},
    lexer::{Lexer, Token},
};

#[derive(Debug, PartialEq)]
enum ParserError {
    ExpectedIdentifierToken(Token),
    UnexpectedToken(Token, Token),
    // UnknownError(String),
}

pub struct Parser {
    lexer: Lexer,

    current_token: Token,
    peek_token: Token,

    errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, token: Token, error: ParserError) -> bool {
        if self.peek_token.eq(&token) {
            self.next_token();
            true
        } else {
            self.errors.push(error);
            false
        }
    }

    // fn peek_error(&mut self, expectedToken: Token) {
    //     unimplemented!("Not implemented yet. Maybe not needed")
    // }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program::new();

        while self.current_token != Token::EOF {
            let statement = self.parse_statement();
            program.statements.push(statement);

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Statement {
        match self.current_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => Statement::ExpressionStatement(Expression::Identifier(
                "Not implemented yet".to_string(),
            )),
        }
    }

    fn parse_let_statement(&mut self) -> Statement {
        let name = match self.peek_token.clone() {
            Token::IDENT(ident) => {
                self.next_token();
                Some(ident)
            }
            _ => {
                self.errors.push(ParserError::ExpectedIdentifierToken(
                    self.peek_token.clone(),
                ));
                self.next_token();
                None
            }
        };

        self.expect_peek(
            Token::ASSIGN,
            ParserError::UnexpectedToken(Token::ASSIGN, self.peek_token.clone()),
        );

        let identifier = match self.peek_token.clone() {
            Token::INT(ident) => {
                self.next_token();
                Some(ident)
            }
            _ => {
                self.errors.push(ParserError::ExpectedIdentifierToken(
                    self.peek_token.clone(),
                ));
                None
            }
        };

        loop {
            // TODO: We skip everything until a semicolon, until we can parse expressions
            if self.current_token.eq(&Token::SEMICOLON) {
                break;
            }
            self.next_token();
        }

        if name.is_none() || identifier.is_none() {
            return Statement::LetStatement("".to_string(), Expression::Identifier("".to_string()));
        }

        Statement::LetStatement(name.unwrap(), Expression::Identifier(identifier.unwrap()))
    }

    fn parse_return_statement(&mut self) -> Statement {
        let token = self.current_token.clone();

        loop {
            println!("Looping: {:?}", self.current_token);
            // TODO: We skip everything until a semicolon, until we can parse expressions
            if self.current_token.eq(&Token::SEMICOLON) {
                break;
            }
            self.next_token();
        }

        Statement::ReturnStatement(token)
    }

    // fn parse_identifier(&mut self) -> Expression {
    //     match self.current_token.clone() {
    //         Token::IDENT(ident) => Expression::Identifier(ident),
    //         _ => panic!("Token is not an identifier"),
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Statement;

    #[test]
    fn test_let_statements() {
        let input = "let x = 5;
        let y = 10;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let expected_statements = vec![
            Statement::LetStatement("x".to_string(), Expression::Identifier("5".to_string())),
            Statement::LetStatement("y".to_string(), Expression::Identifier("10".to_string())),
        ];

        assert_eq!(program.statements, expected_statements);

        let expected_erors: Vec<ParserError> = vec![];
        assert_eq!(parser.errors, expected_erors);
    }

    #[test]
    fn test_let_parser_error_simple() {
        let input = "let x = 5;
        let 10 = 10;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program();

        let expected_erors = vec![ParserError::ExpectedIdentifierToken(Token::INT(
            "10".to_string(),
        ))];
        assert_eq!(parser.errors, expected_erors);
    }

    #[test]
    fn test_let_parser_error_multiple() {
        let input = "let x = 5;
        let 10 = 10;
        let a 11;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program();

        let expected_erors = vec![
            ParserError::ExpectedIdentifierToken(Token::INT("10".to_string())),
            ParserError::UnexpectedToken(Token::ASSIGN, Token::INT("11".to_string())),
        ];
        assert_eq!(parser.errors, expected_erors);
    }

    #[test]
    fn test_return_statements() {
        let input = "return 5;
        return 10;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let expected_statements = vec![
            Statement::ReturnStatement(Token::RETURN),
            Statement::ReturnStatement(Token::RETURN),
        ];
        assert_eq!(program.statements, expected_statements);

        let expected_erors: Vec<ParserError> = vec![];
        assert_eq!(parser.errors, expected_erors);
    }

    #[test]
    fn test_return_statements_error() {
        let input = "return =;
        return !=;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program();

        let expected_erors: Vec<ParserError> = vec![
            ParserError::ExpectedIdentifierToken(Token::ASSIGN),
            ParserError::ExpectedIdentifierToken(Token::NOT_EQ),
        ];

        assert_eq!(parser.errors, expected_erors);
    }
}
