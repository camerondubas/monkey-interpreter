use crate::{
    ast::{self, Expression, Statement},
    lexer::{Lexer, Token},
};

#[allow(dead_code)]
enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

#[derive(Debug, PartialEq)]
enum ParserError {
    ExpectedIdentifierToken(Token),
    UnexpectedToken(Token, Token),
    MissingParsePrefixFunction(Token),
    IntegerParsingError(Token),
    ExpectedIntegerToken(Token),
    // UnknownError(String),
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParserError>;
// type InfixParseFn = fn(&mut Parser, left_side_expression: Expression) -> Expression;

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

    fn prefix_parse_fns(token: &Token) -> Option<PrefixParseFn> {
        match token {
            Token::IDENT(_) => Some(Parser::parse_identifier),
            Token::INT(_) => Some(Parser::parse_integer),
            Token::BANG | Token::MINUS => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }

    // fn infix_parse_fns(token: &Token) -> Option<InfixParseFn> {
    //     None
    // }

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

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program::new();

        while self.current_token != Token::EOF {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => self.errors.push(error),
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.current_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let name = match self.peek_token.clone() {
            Token::IDENT(ident) => {
                self.next_token();
                ident
            }
            _ => {
                let err = Err(ParserError::ExpectedIdentifierToken(
                    self.peek_token.clone(),
                ));
                self.next_token();
                return err;
            }
        };

        self.expect_peek(
            Token::ASSIGN,
            ParserError::UnexpectedToken(Token::ASSIGN, self.peek_token.clone()),
        );

        let identifier = match self.peek_token.clone() {
            Token::INT(ident) => {
                self.next_token();
                ident
            }
            _ => {
                return Err(ParserError::ExpectedIdentifierToken(
                    self.peek_token.clone(),
                ))
            }
        };

        loop {
            // TODO: We skip everything until a semicolon, until we can parse expressions
            if self.current_token.eq(&Token::SEMICOLON) {
                break;
            }
            self.next_token();
        }

        Ok(Statement::LetStatement(
            name,
            Expression::Identifier(identifier),
        ))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.current_token.clone();

        loop {
            // TODO: We skip everything until a semicolon, until we can parse expressions
            if self.current_token.eq(&Token::SEMICOLON) {
                break;
            }
            self.next_token();
        }

        Ok(Statement::ReturnStatement(token))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::SEMICOLON {
            let _ = &self.next_token();
        }

        let statement = expression.map(|e| Statement::ExpressionStatement(e));

        statement
    }

    fn parse_expression(&mut self, _precedence: Precedence) -> Result<Expression, ParserError> {
        match Parser::prefix_parse_fns(&self.current_token) {
            Some(prefix) => prefix(self),
            None => Err(ParserError::MissingParsePrefixFunction(
                self.current_token.clone(),
            )),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        match &self.current_token {
            Token::IDENT(value) => Ok(Expression::Identifier(value.to_string())),
            _ => Err(ParserError::ExpectedIdentifierToken(
                self.current_token.clone(),
            )),
        }
    }

    fn parse_integer(&mut self) -> Result<Expression, ParserError> {
        let result = match &self.current_token {
            Token::INT(val) => val,
            _ => {
                return Err(ParserError::ExpectedIntegerToken(
                    self.current_token.clone(),
                ));
            }
        };

        let integer_value = match result.parse::<u64>() {
            Ok(val) => val,
            Err(_) => return Err(ParserError::IntegerParsingError(self.current_token.clone())),
        };

        Ok(Expression::IntegerLiteral(integer_value))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let operator = &self.current_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix);

        if right.is_err() {
            return right;
        }

        Ok(Expression::PrefixExpression(
            operator.clone(),
            Box::new(right.unwrap()),
        ))
    }
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
        expect_no_errors(&parser);

        let expected_statements = vec![
            Statement::LetStatement("x".to_string(), Expression::Identifier("5".to_string())),
            Statement::LetStatement("y".to_string(), Expression::Identifier("10".to_string())),
        ];

        assert_eq!(program.statements, expected_statements);
    }

    #[test]
    fn test_let_parser_error_simple() {
        let input = "let x = 5;
        let 10 = 10;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program();

        let expected_erors = vec![
            ParserError::ExpectedIdentifierToken(Token::INT("10".to_string())),
            ParserError::MissingParsePrefixFunction(Token::ASSIGN), // Temp Until Implemented
        ];
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
            ParserError::MissingParsePrefixFunction(Token::ASSIGN), // Temp Until Implemented
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

        expect_no_errors(&parser);

        let expected_statements = vec![
            Statement::ReturnStatement(Token::RETURN),
            Statement::ReturnStatement(Token::RETURN),
        ];
        assert_eq!(program.statements, expected_statements);
    }

    #[ignore]
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

    #[test]
    fn test_parse_identifier_simple() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.parse_program();

        expect_no_errors(&parser);

        let expected_statements = vec![Statement::ExpressionStatement(Expression::Identifier(
            "foobar".to_string(),
        ))];

        assert_eq!(program.statements, expected_statements);
    }

    #[test]
    fn test_parse_integer_simple() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.parse_program();

        expect_no_errors(&parser);

        let expected_statements = vec![Statement::ExpressionStatement(Expression::IntegerLiteral(
            5,
        ))];

        assert_eq!(program.statements, expected_statements);
    }

    #[test]
    fn test_parse_prefix_expression() {
        let inputs = vec![("!5", Token::BANG, 5), ("-15", Token::MINUS, 15)];

        for (input, operator, integer_value) in inputs {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            parser.parse_program();

            expect_no_errors(&parser);

            let expected_statements = vec![Statement::ExpressionStatement(
                Expression::PrefixExpression(
                    operator,
                    Box::new(Expression::IntegerLiteral(integer_value)),
                ),
            )];

            assert_eq!(program.statements, expected_statements);
        }
    }

    fn expect_no_errors(parser: &Parser) {
        assert_eq!(parser.errors, vec![], "Found Parser Errors");
    }
}
