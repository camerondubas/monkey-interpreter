use std::fmt;

use crate::lexer::Token;

#[derive(Debug, PartialEq, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|statement| format!("{}", statement))
                .collect::<Vec<String>>()
                .join(""),
        )
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
    Block(Vec<Statement>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(name, value) => {
                write!(f, "{} {} {} {};", Token::Let, name, Token::Assign, value)
            }
            Statement::Return(value) => write!(f, "{} {};", Token::Return, value),
            Statement::Expression(expression) => write!(f, "{}", expression),
            Statement::Block(statements) => {
                for statement in statements {
                    write!(f, "{}", statement)?;
                }

                Ok(())
            }
        }
    }
}

type Operator = Token;
type Condition = Box<Expression>;
type Consequence = Box<Statement>;
type Alternative = Option<Box<Statement>>;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    StringLiteral(String),
    ArrayLiteral(Vec<Expression>),
    HashLiteral(Vec<(Expression, Expression)>),
    PrefixExpression(Operator, Box<Expression>),
    InfixExpression(Box<Expression>, Operator, Box<Expression>),
    Boolean(bool),
    If(Condition, Consequence, Alternative),
    FunctionLiteral(Vec<Expression>, Box<Statement>),
    CallExpression(Box<Expression>, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(value) => write!(f, "{}", value),
            Expression::IntegerLiteral(value) => write!(f, "{}", value),
            Expression::StringLiteral(value) => write!(f, "{}", value),
            Expression::Boolean(value) => write!(f, "{}", value),
            Expression::Index(left, index) => write!(f, "({}[{}])", left, index),
            Expression::PrefixExpression(operator_token, right) => match operator_token {
                Token::Bang | Token::Minus => write!(f, "({}{})", operator_token, right),
                _ => panic!("Invalid Prefix Token: {:?}", operator_token),
            },
            Expression::InfixExpression(left, operator_token, right) => match operator_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Gt
                | Token::Lt
                | Token::Eq
                | Token::NotEq => write!(f, "({} {} {})", left, operator_token, right),
                _ => panic!("Invalid Infix Token: {:?}", operator_token),
            },
            Expression::If(condition, consequence, alternative) => {
                let if_str = format!("{} ({})", Token::If, condition);
                let consequence_str = format!("{{ {} }}", consequence);
                let alternative_str = match alternative {
                    Some(value) => format!("{} {{ {} }}", Token::Else, value),
                    None => String::new(),
                };

                let full = format!("{} {} {}", if_str, consequence_str, alternative_str);
                let _ = full.trim();

                write!(f, "{}", full)
            }
            Expression::FunctionLiteral(args, body) => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.to_string()).collect();
                write!(
                    f,
                    "{}({}) {{ {} }}",
                    Token::Function,
                    args_str.join(", "),
                    body
                )
            }
            Expression::CallExpression(function, args) => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.to_string()).collect();
                write!(f, "{}({})", function, args_str.join(", "))
            }
            Expression::ArrayLiteral(items) => {
                let args_str: Vec<String> = items.iter().map(|item| item.to_string()).collect();
                write!(f, "[{}]", args_str.join(", "))
            }
            Expression::HashLiteral(items) => {
                let args_str: Vec<String> = items
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect();
                write!(f, "{{{}}}", args_str.join(", "))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_program_stringify() {
        let mut program = Program::new();
        program.statements.push(Statement::Let(
            "myVar".to_string(),
            Expression::Identifier("5".to_string()),
        ));

        program
            .statements
            .push(Statement::Return(Expression::IntegerLiteral(4)));

        assert_eq!(program.to_string(), "let myVar = 5;return 4;");
    }

    #[test]
    fn test_if_strignify() {
        let expression = Expression::If(
            Box::new(Expression::Boolean(true)),
            Box::new(Statement::Let(
                "a".to_string(),
                Expression::IntegerLiteral(1),
            )),
            Some(Box::new(Statement::Let(
                "b".to_string(),
                Expression::IntegerLiteral(2),
            ))),
        );

        let output = "if (true) { let a = 1; } else { let b = 2; }";

        assert_eq!(expression.to_string(), output.to_string());
    }

    #[test]
    fn test_fn_literal_strignify() {
        let expression = Expression::FunctionLiteral(
            vec![
                Expression::Identifier("a".to_string()),
                Expression::Identifier("b".to_string()),
                Expression::Identifier("c".to_string()),
            ],
            Box::new(Statement::Block(vec![Statement::Return(
                Expression::Identifier("d".to_string()),
            )])),
        );

        let output = "fn(a, b, c) { return d; }";

        assert_eq!(expression.to_string(), output.to_string());
    }

    #[test]
    fn test_fn_call_strignify() {
        let expression = Expression::CallExpression(
            Box::new(Expression::Identifier("add".to_string())),
            vec![
                Expression::Identifier("a".to_string()),
                Expression::Identifier("b".to_string()),
                Expression::Identifier("c".to_string()),
            ],
        );

        let output = "add(a, b, c)";

        assert_eq!(expression.to_string(), output.to_string());
    }

    #[test]
    fn test_array_strignify() {
        let expression = Expression::ArrayLiteral(vec![
            Expression::Identifier("a".to_string()),
            Expression::IntegerLiteral(7),
            Expression::Boolean(true),
        ]);

        let output = "[a, 7, true]";

        assert_eq!(expression.to_string(), output.to_string());
    }

    #[test]
    fn test_index_strignify() {
        let expression = Expression::Index(
            Box::new(Expression::Identifier("a".to_string())),
            Box::new(Expression::IntegerLiteral(7)),
        );

        let output = "(a[7])";

        assert_eq!(expression.to_string(), output.to_string());
    }

    #[test]
    fn test_hash_stringify() {
        let expression = Expression::HashLiteral(vec![
            (
                Expression::StringLiteral("a".to_string()),
                Expression::IntegerLiteral(7),
            ),
            (
                Expression::StringLiteral("b".to_string()),
                Expression::Boolean(true),
            ),
        ]);

        let output = "{a: 7, b: true}";

        assert_eq!(expression.to_string(), output.to_string());
    }
}
