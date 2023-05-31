use crate::{
    ast::{self, Expression, Statement},
    lexer::{Lexer, Token},
};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    ExpectedIdentifierToken(Token),
    UnexpectedToken(Token, Token),
    MissingParsePrefixFunction(Token),
    IntegerParsingError(Token),
    ExpectedIntegerToken(Token),
    // UnknownError(String),
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParserError>;
type InfixParseFn =
    fn(&mut Parser, left_side_expression: Expression) -> Result<Expression, ParserError>;

pub struct Parser {
    lexer: Lexer,

    current_token: Token,
    peek_token: Token,

    pub errors: Vec<ParserError>,
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
            Token::Indentifier(_) => Some(Parser::parse_identifier),
            Token::Integer(_) => Some(Parser::parse_integer),
            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }

    fn parse_infix_fns(token: &Token) -> Option<InfixParseFn> {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Gt
            | Token::Lt
            | Token::Eq
            | Token::NotEq => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }

    fn next_token(&mut self) -> () {
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

        while self.current_token != Token::Eof {
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
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let name = match self.peek_token.clone() {
            Token::Indentifier(ident) => {
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
            Token::Assign,
            ParserError::UnexpectedToken(Token::Assign, self.peek_token.clone()),
        );

        let identifier = match self.peek_token.clone() {
            Token::Integer(ident) => {
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
            if self.current_token.eq(&Token::Semicolon) {
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
            if self.current_token.eq(&Token::Semicolon) {
                break;
            }
            self.next_token();
        }

        Ok(Statement::ReturnStatement(token))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        expression.map(|e| Statement::ExpressionStatement(e))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let prefix_expression = match Parser::prefix_parse_fns(&self.current_token) {
            Some(prefix) => prefix,
            None => {
                return Err(ParserError::MissingParsePrefixFunction(
                    self.current_token.clone(),
                ))
            }
        };

        let mut left_expression = prefix_expression(self);

        if left_expression.is_err() {
            return left_expression;
        }

        while self.peek_token != Token::Semicolon && precedence < self.peek_precedence() {
            left_expression = match Parser::parse_infix_fns(&self.peek_token) {
                Some(infix) => {
                    self.next_token();
                    infix(self, left_expression.unwrap())
                }
                None => left_expression,
            }
        }

        left_expression
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        match &self.current_token {
            Token::Indentifier(value) => Ok(Expression::Identifier(value.to_string())),
            _ => Err(ParserError::ExpectedIdentifierToken(
                self.current_token.clone(),
            )),
        }
    }

    fn parse_integer(&mut self) -> Result<Expression, ParserError> {
        let result = match &self.current_token {
            Token::Integer(val) => val,
            _ => {
                return Err(ParserError::ExpectedIntegerToken(
                    self.current_token.clone(),
                ));
            }
        };

        result
            .parse::<u64>()
            .map(|value| Expression::IntegerLiteral(value))
            .map_err(|_| ParserError::IntegerParsingError(self.current_token.clone()))
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

    fn parse_infix_expression(
        &mut self,
        left_side_expression: Expression,
    ) -> Result<Expression, ParserError> {
        let left = left_side_expression;
        let operator = self.current_token.clone();
        let precedence = self.current_precedence();

        self.next_token();

        let right = self.parse_expression(precedence);

        if right.is_err() {
            return right;
        }

        Ok(Expression::InfixExpression(
            Box::new(left),
            operator,
            Box::new(right.unwrap()),
        ))
    }

    fn peek_precedence(&self) -> Precedence {
        self.get_operator_precedence(self.peek_token.clone())
    }

    fn current_precedence(&self) -> Precedence {
        self.get_operator_precedence(self.current_token.clone())
    }

    fn get_operator_precedence(&self, operator: Token) -> Precedence {
        match operator {
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Asterisk => Precedence::Product,
            Token::Slash => Precedence::Product,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Eq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            _ => Precedence::Lowest,
        }
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
            ParserError::ExpectedIdentifierToken(Token::Integer("10".to_string())),
            ParserError::MissingParsePrefixFunction(Token::Assign), // Temp Until Implemented
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
            ParserError::ExpectedIdentifierToken(Token::Integer("10".to_string())),
            ParserError::MissingParsePrefixFunction(Token::Assign), // Temp Until Implemented
            ParserError::UnexpectedToken(Token::Assign, Token::Integer("11".to_string())),
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
            Statement::ReturnStatement(Token::Return),
            Statement::ReturnStatement(Token::Return),
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
            ParserError::ExpectedIdentifierToken(Token::Assign),
            ParserError::ExpectedIdentifierToken(Token::NotEq),
        ];

        assert_eq!(parser.errors, expected_erors);
    }

    #[test]
    fn test_parse_identifier_simple() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

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

        expect_no_errors(&parser);

        let expected_statements = vec![Statement::ExpressionStatement(Expression::IntegerLiteral(
            5,
        ))];

        assert_eq!(program.statements, expected_statements);
    }

    #[test]
    fn test_parse_prefix_expression() {
        let inputs = vec![("!5", Token::Bang, 5), ("-15", Token::Minus, 15)];

        for (input, operator, value) in inputs {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            expect_no_errors(&parser);

            let expected_statements = vec![Statement::ExpressionStatement(
                Expression::PrefixExpression(operator, Box::new(Expression::IntegerLiteral(value))),
            )];

            assert_eq!(program.statements, expected_statements);
        }
    }

    #[test]
    fn test_parse_infix_expression() {
        let inputs = vec![
            ("5 + 5", 5, Token::Plus, 5),
            ("5 - 5", 5, Token::Minus, 5),
            ("5 * 5", 5, Token::Asterisk, 5),
            ("5 / 5", 5, Token::Slash, 5),
            ("5 > 5", 5, Token::Gt, 5),
            ("5 < 5", 5, Token::Lt, 5),
            ("5 == 5", 5, Token::Eq, 5),
            ("5 != 5", 5, Token::NotEq, 5),
        ];

        for (input, left, operator, right) in inputs {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            expect_no_errors(&parser);

            let expected_statements =
                vec![Statement::ExpressionStatement(Expression::InfixExpression(
                    Box::new(Expression::IntegerLiteral(left)),
                    operator,
                    Box::new(Expression::IntegerLiteral(right)),
                ))];

            assert_eq!(program.statements, expected_statements);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let inputs = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, output) in inputs {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            expect_no_errors(&parser);

            assert_eq!(program.to_string(), output);
        }
    }

    fn expect_no_errors(parser: &Parser) {
        assert_eq!(parser.errors, vec![], "Found Parser Errors");
    }
}
