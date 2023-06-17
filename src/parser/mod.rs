use crate::{
    ast::{Expression, Program, Statement},
    lexer::{Lexer, Token},
};

mod error;
mod tracer;

use error::ParserError;
use tracer::Tracer;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParserError>;
type InfixParseFn =
    fn(&mut Parser, left_side_expression: Expression) -> Result<Expression, ParserError>;

pub struct Parser {
    lexer: Lexer,
    tracer: Tracer,

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
            tracer: Tracer::new(),
            current_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    pub fn from_source(source: &str) -> Self {
        let lexer = Lexer::new(source);
        Parser::new(lexer)
    }

    pub fn trace(&mut self, enable: bool) {
        if enable {
            self.tracer.enable();
        } else {
            self.tracer.disable();
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

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
            Token::Identifier(ident) => {
                self.next_token();
                ident
            }
            _ => return Err(ParserError::ExpectedIdentifier(self.peek_token.clone())),
        };

        self.expect_peek(Token::Assign)?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let(name, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Return(value))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        self.tracer.trace("parse_expression");
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        self.tracer.untrace("parse_expression");
        expression.map(Statement::Expression)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let prefix_expression = Parser::parse_prefix_fns(&self.current_token).ok_or(
            ParserError::MissingParsePrefixFunction(self.current_token.clone()),
        )?;

        let mut left_expression = prefix_expression(self)?;

        while self.peek_token != Token::Semicolon && precedence < self.peek_precedence() {
            left_expression = match Parser::parse_infix_fns(&self.peek_token) {
                Some(infix) => {
                    self.next_token();
                    infix(self, left_expression)?
                }
                None => left_expression,
            }
        }

        Ok(left_expression)
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        match &self.current_token {
            Token::Identifier(value) => Ok(Expression::Identifier(value.to_string())),
            _ => Err(ParserError::ExpectedIdentifier(self.current_token.clone())),
        }
    }

    fn parse_integer(&mut self) -> Result<Expression, ParserError> {
        let result = match &self.current_token {
            Token::Integer(val) => val,
            _ => return Err(ParserError::ExpectedInteger(self.current_token.clone())),
        };

        result.parse::<i64>().map_or(
            Err(ParserError::IntegerParsingError(self.current_token.clone())),
            |value| Ok(Expression::IntegerLiteral(value)),
        )
    }

    fn parse_string(&mut self) -> Result<Expression, ParserError> {
        match self.current_token.clone() {
            Token::String(val) => Ok(Expression::StringLiteral(val)),
            err => Err(ParserError::ExpectedString(err)),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let operator = &self.current_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::PrefixExpression(
            operator.clone(),
            Box::new(right),
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

        let right = self.parse_expression(precedence)?;

        Ok(Expression::InfixExpression(
            Box::new(left),
            operator,
            Box::new(right),
        ))
    }

    fn parse_boolean(&mut self) -> Result<Expression, ParserError> {
        match &self.current_token {
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            _ => Err(ParserError::ExpectedBoolean(self.current_token.clone())),
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(Token::LeftParen)?;
        self.next_token();

        let condition = &self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RightParen)?;
        self.expect_peek(Token::LeftBrace)?;

        let consequence = self.parse_block_statement()?;
        let alternative = match self.peek_token {
            Token::Else => {
                self.next_token();
                self.expect_peek(Token::LeftBrace)?;
                Some(self.parse_block_statement()?)
            }
            _ => None,
        };

        Ok(Expression::If(
            Box::new(condition.clone()),
            consequence,
            alternative,
        ))
    }

    fn parse_block_statement(&mut self) -> Result<Box<Statement>, ParserError> {
        let mut statements = vec![];

        self.next_token();

        while self.current_token != Token::RightBrace && self.current_token != Token::Eof {
            statements.push(self.parse_statement()?.clone());
            self.next_token();
        }

        Ok(Box::new(Statement::Block(statements)))
    }

    fn parse_fn_literal(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(Token::LeftParen)?;

        let params = self.parse_fn_parameters()?;

        self.expect_peek(Token::LeftBrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::FunctionLiteral(params, body))
    }

    fn parse_fn_parameters(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut identifiers = vec![];

        if self.peek_token == Token::RightParen {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        identifiers.push(self.parse_identifier()?);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            identifiers.push(self.parse_identifier()?);
        }

        self.expect_peek(Token::RightParen)?;

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParserError> {
        let arguments = self.parse_expression_list(Token::RightParen)?;

        Ok(Expression::CallExpression(Box::new(function), arguments))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        self.expect_peek(Token::RightParen)?;

        expression
    }

    fn parse_expression_list(
        &mut self,
        closing_token: Token,
    ) -> Result<Vec<Expression>, ParserError> {
        let mut list = vec![];

        if self.peek_token == closing_token {
            self.next_token();
            return Ok(list);
        }

        self.next_token();

        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(closing_token)?;
        Ok(list)
    }

    fn parse_array(&mut self) -> Result<Expression, ParserError> {
        let items = self.parse_expression_list(Token::RightBracket)?;
        Ok(Expression::ArrayLiteral(items))
    }

    fn parse_hash(&mut self) -> Result<Expression, ParserError> {
        let mut pairs = vec![];

        if self.peek_token == Token::RightBrace {
            self.next_token();
            return Ok(Expression::HashLiteral(pairs));
        }

        pairs.push(self.parse_hash_pairs()?);

        while self.peek_token == Token::Comma {
            self.next_token();

            pairs.push(self.parse_hash_pairs()?);
        }

        self.expect_peek(Token::RightBrace)?;

        Ok(Expression::HashLiteral(pairs))
    }

    fn parse_hash_pairs(&mut self) -> Result<(Expression, Expression), ParserError> {
        self.next_token();

        let key = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::Colon)?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        Ok((key, value))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        self.next_token();

        let idx = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RightBracket)?;

        Ok(Expression::Index(Box::new(left), Box::new(idx)))
    }

    fn parse_illegal_token(&mut self) -> Result<Expression, ParserError> {
        Err(ParserError::UnknownError(self.current_token.clone()))
    }

    fn parse_prefix_fns(token: &Token) -> Option<PrefixParseFn> {
        match token {
            Token::Identifier(_) => Some(Parser::parse_identifier),
            Token::Integer(_) => Some(Parser::parse_integer),
            Token::String(_) => Some(Parser::parse_string),
            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            Token::True | Token::False => Some(Parser::parse_boolean),
            Token::LeftParen => Some(Parser::parse_grouped_expression),
            Token::If => Some(Parser::parse_if_expression),
            Token::Function => Some(Parser::parse_fn_literal),
            Token::LeftBrace => Some(Parser::parse_hash),
            Token::LeftBracket => Some(Parser::parse_array),
            Token::Illegal(_) => Some(Parser::parse_illegal_token),
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
            Token::LeftParen => Some(Parser::parse_call_expression),
            Token::LeftBracket => Some(Parser::parse_index_expression),
            _ => None,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, token: Token) -> Result<(), ParserError> {
        if self.peek_token.eq(&token) {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(token, self.peek_token.clone()))
        }
    }

    fn peek_precedence(&self) -> Precedence {
        self.get_operator_precedence(&self.peek_token)
    }

    fn current_precedence(&self) -> Precedence {
        self.get_operator_precedence(&self.current_token)
    }

    fn get_operator_precedence(&self, operator: &Token) -> Precedence {
        match operator {
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Asterisk => Precedence::Product,
            Token::Slash => Precedence::Product,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Eq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::LeftParen => Precedence::Call,
            Token::LeftBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::ast::Statement;

    #[test]
    fn test_let_statements() {
        let inputs = vec![
            (
                "let x = 5;",
                Statement::Let("x".to_string(), Expression::IntegerLiteral(5)),
            ),
            (
                "let y = true;",
                Statement::Let("y".to_string(), Expression::Boolean(true)),
            ),
            (
                "let foo = bar;",
                Statement::Let("foo".to_string(), Expression::Identifier("bar".to_string())),
            ),
        ];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_return_statements() {
        let inputs = vec![
            (
                "return 5;",
                Statement::Return(Expression::IntegerLiteral(5)),
            ),
            ("return true;", Statement::Return(Expression::Boolean(true))),
            (
                "return bar;",
                Statement::Return(Expression::Identifier("bar".to_string())),
            ),
        ];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_identifier() {
        let inputs = vec![
            (
                "foobar;",
                Statement::Expression(Expression::Identifier("foobar".to_string())),
            ),
            (
                "foo + bar;",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::Identifier("foo".to_string())),
                    Token::Plus,
                    Box::new(Expression::Identifier("bar".to_string())),
                )),
            ),
        ];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_boolean() {
        let inputs = vec![
            ("true;", Statement::Expression(Expression::Boolean(true))),
            ("false;", Statement::Expression(Expression::Boolean(false))),
        ];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_integer() {
        let inputs = vec![
            ("5;", Statement::Expression(Expression::IntegerLiteral(5))),
            (
                "10234;",
                Statement::Expression(Expression::IntegerLiteral(10234)),
            ),
        ];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_string_literal() {
        let inputs = vec![
            (
                "\"Hello World\"",
                Statement::Expression(Expression::StringLiteral("Hello World".to_string())),
            ),
            (
                "\"Helloworld\"",
                Statement::Expression(Expression::StringLiteral("Helloworld".to_string())),
            ),
            (
                "\"with1234numbers\"",
                Statement::Expression(Expression::StringLiteral("with1234numbers".to_string())),
            ),
            (
                "\"with->=_'_|+!#@symbols;\"",
                Statement::Expression(Expression::StringLiteral(
                    "with->=_'_|+!#@symbols;".to_string(),
                )),
            ),
        ];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_if_expression() {
        let inputs = vec![
            (
                "if (x < y) { x }",
                Statement::Expression(Expression::If(
                    Box::new(Expression::InfixExpression(
                        Box::new(Expression::Identifier("x".to_string())),
                        Token::Lt,
                        Box::new(Expression::Identifier("y".to_string())),
                    )),
                    Box::new(Statement::Block(vec![Statement::Expression(
                        Expression::Identifier("x".to_string()),
                    )])),
                    None,
                )),
            ),
            (
                "if (x < y) { x } else { y }",
                Statement::Expression(Expression::If(
                    Box::new(Expression::InfixExpression(
                        Box::new(Expression::Identifier("x".to_string())),
                        Token::Lt,
                        Box::new(Expression::Identifier("y".to_string())),
                    )),
                    Box::new(Statement::Block(vec![Statement::Expression(
                        Expression::Identifier("x".to_string()),
                    )])),
                    Some(Box::new(Statement::Block(vec![Statement::Expression(
                        Expression::Identifier("y".to_string()),
                    )]))),
                )),
            ),
        ];

        expect_inputs_to_match(inputs);
    }
    #[test]
    fn test_function_literal() {
        let inputs = vec![
            (
                "fn() {}",
                Statement::Expression(Expression::FunctionLiteral(
                    vec![],
                    Box::new(Statement::Block(vec![])),
                )),
            ),
            (
                "fn(x) { x }",
                Statement::Expression(Expression::FunctionLiteral(
                    vec![Expression::Identifier("x".to_string())],
                    Box::new(Statement::Block(vec![Statement::Expression(
                        Expression::Identifier("x".to_string()),
                    )])),
                )),
            ),
            (
                "fn(x, y) { x + y; }",
                Statement::Expression(Expression::FunctionLiteral(
                    vec![
                        Expression::Identifier("x".to_string()),
                        Expression::Identifier("y".to_string()),
                    ],
                    Box::new(Statement::Block(vec![Statement::Expression(
                        Expression::InfixExpression(
                            Box::new(Expression::Identifier("x".to_string())),
                            Token::Plus,
                            Box::new(Expression::Identifier("y".to_string())),
                        ),
                    )])),
                )),
            ),
        ];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_function_call() {
        let inputs = vec![(
            "add(a, 5 + 4);",
            Statement::Expression(Expression::CallExpression(
                Box::new(Expression::Identifier("add".to_string())),
                vec![
                    Expression::Identifier("a".to_string()),
                    Expression::InfixExpression(
                        Box::new(Expression::IntegerLiteral(5)),
                        Token::Plus,
                        Box::new(Expression::IntegerLiteral(4)),
                    ),
                ],
            )),
        )];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_array() {
        let inputs = vec![(
            "[a, 1, true, fn() { return false }]",
            Statement::Expression(Expression::ArrayLiteral(vec![
                Expression::Identifier("a".to_string()),
                Expression::IntegerLiteral(1),
                Expression::Boolean(true),
                Expression::FunctionLiteral(
                    vec![],
                    Box::new(Statement::Block(vec![Statement::Return(
                        Expression::Boolean(false),
                    )])),
                ),
            ])),
        )];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_index() {
        let inputs = vec![(
            "a[7]",
            Statement::Expression(Expression::Index(
                Box::new(Expression::Identifier("a".to_string())),
                Box::new(Expression::IntegerLiteral(7)),
            )),
        )];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_parse_hash_literal() {
        let inputs = vec![
            ("{}", Statement::Expression(Expression::HashLiteral(vec![]))),
            (
                "{ \"one\": 1, \"two\": 2, \"three\": 3 }",
                Statement::Expression(Expression::HashLiteral(vec![
                    (
                        Expression::StringLiteral("one".to_string()),
                        Expression::IntegerLiteral(1),
                    ),
                    (
                        Expression::StringLiteral("two".to_string()),
                        Expression::IntegerLiteral(2),
                    ),
                    (
                        Expression::StringLiteral("three".to_string()),
                        Expression::IntegerLiteral(3),
                    ),
                ])),
            ),
            (
                "{ true: 1, 5: 2, 5 + 5: 3 }",
                Statement::Expression(Expression::HashLiteral(vec![
                    (Expression::Boolean(true), Expression::IntegerLiteral(1)),
                    (Expression::IntegerLiteral(5), Expression::IntegerLiteral(2)),
                    (
                        Expression::InfixExpression(
                            Box::new(Expression::IntegerLiteral(5)),
                            Token::Plus,
                            Box::new(Expression::IntegerLiteral(5)),
                        ),
                        Expression::IntegerLiteral(3),
                    ),
                ])),
            ),
        ];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_prefix_expression() {
        let inputs = vec![
            (
                "!5;",
                Statement::Expression(Expression::PrefixExpression(
                    Token::Bang,
                    Box::new(Expression::IntegerLiteral(5)),
                )),
            ),
            (
                "-15;",
                Statement::Expression(Expression::PrefixExpression(
                    Token::Minus,
                    Box::new(Expression::IntegerLiteral(15)),
                )),
            ),
            (
                "!true;",
                Statement::Expression(Expression::PrefixExpression(
                    Token::Bang,
                    Box::new(Expression::Boolean(true)),
                )),
            ),
            (
                "!false;",
                Statement::Expression(Expression::PrefixExpression(
                    Token::Bang,
                    Box::new(Expression::Boolean(false)),
                )),
            ),
        ];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_infix_expression() {
        let inputs = vec![
            (
                "5 + 5;",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::IntegerLiteral(5)),
                    Token::Plus,
                    Box::new(Expression::IntegerLiteral(5)),
                )),
            ),
            (
                "5 - 5;",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::IntegerLiteral(5)),
                    Token::Minus,
                    Box::new(Expression::IntegerLiteral(5)),
                )),
            ),
            (
                "5 * 5;",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::IntegerLiteral(5)),
                    Token::Asterisk,
                    Box::new(Expression::IntegerLiteral(5)),
                )),
            ),
            (
                "5 / 5;",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::IntegerLiteral(5)),
                    Token::Slash,
                    Box::new(Expression::IntegerLiteral(5)),
                )),
            ),
            (
                "5 > 5;",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::IntegerLiteral(5)),
                    Token::Gt,
                    Box::new(Expression::IntegerLiteral(5)),
                )),
            ),
            (
                "5 < 5;",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::IntegerLiteral(5)),
                    Token::Lt,
                    Box::new(Expression::IntegerLiteral(5)),
                )),
            ),
            (
                "5 == 5;",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::IntegerLiteral(5)),
                    Token::Eq,
                    Box::new(Expression::IntegerLiteral(5)),
                )),
            ),
            (
                "5 != 5;",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::IntegerLiteral(5)),
                    Token::NotEq,
                    Box::new(Expression::IntegerLiteral(5)),
                )),
            ),
            (
                "true == true",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::Boolean(true)),
                    Token::Eq,
                    Box::new(Expression::Boolean(true)),
                )),
            ),
            (
                "true != false",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::Boolean(true)),
                    Token::NotEq,
                    Box::new(Expression::Boolean(false)),
                )),
            ),
            (
                "false == false",
                Statement::Expression(Expression::InfixExpression(
                    Box::new(Expression::Boolean(false)),
                    Token::Eq,
                    Box::new(Expression::Boolean(false)),
                )),
            ),
        ];

        expect_inputs_to_match(inputs);
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let inputs = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("true", "true"),
            ("false", "false"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
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
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, output) in inputs {
            let mut parser = Parser::from_source(input);
            let program = parser.parse_program();

            expect_no_errors(&parser);

            assert_eq!(program.to_string(), output);
        }
    }

    #[test]
    fn test_parser_errors() {
        let inputs = vec![
            (
                "let 10 = 9",
                vec![
                    ParserError::ExpectedIdentifier(Token::Integer("10".to_string())),
                    ParserError::MissingParsePrefixFunction(Token::Assign),
                ],
            ),
            (
                "let a 11;",
                vec![ParserError::UnexpectedToken(
                    Token::Assign,
                    Token::Integer("11".to_string()),
                )],
            ),
            (
                "fn(() { return 11;}",
                vec![
                    ParserError::ExpectedIdentifier(Token::LeftParen),
                    ParserError::MissingParsePrefixFunction(Token::RightParen),
                    ParserError::MissingParsePrefixFunction(Token::Return),
                    ParserError::MissingParsePrefixFunction(Token::RightBrace),
                ],
            ),
        ];

        for (input, expected_errors) in inputs {
            let mut parser = Parser::from_source(input);
            parser.parse_program();

            assert_eq!(parser.errors, expected_errors);
        }
    }

    fn expect_no_errors(parser: &Parser) {
        let expected: Vec<ParserError> = vec![];
        assert_eq!(expected, parser.errors, "Found Parser Errors");
    }

    fn expect_inputs_to_match(inputs: Vec<(&str, Statement)>) {
        for (input, output) in inputs {
            let mut parser = Parser::from_source(input);
            let program = parser.parse_program();

            expect_no_errors(&parser);

            assert_eq!(program.statements, vec![output]);
        }
    }
}
