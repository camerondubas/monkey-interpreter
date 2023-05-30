use std::fmt;

use crate::lexer::Token;

#[derive(Debug, PartialEq)]
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
                .join("\n"),
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement(String, Expression),
    ReturnStatement(Token),
    ExpressionStatement(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::LetStatement(name, value) => write!(f, "let {} = {};", name, value),
            Statement::ReturnStatement(_) => write!(f, "{}", "return (todo);"),
            Statement::ExpressionStatement(expression) => write!(f, "{}", expression),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(u64),
    PrefixExpression(Token, Box<Expression>),
    InfixExpression(Box<Expression>, Token, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(value) => write!(f, "{}", value),
            Expression::IntegerLiteral(value) => write!(f, "{}", value),
            Expression::PrefixExpression(operator_token, right) => {
                let operator = match operator_token {
                    Token::Bang => "!",
                    Token::Minus => "-",
                    _ => panic!("Invalid Prefix Token: {:?}", operator_token),
                };

                write!(f, "({}{})", operator, right)
            }
            Expression::InfixExpression(left, operator_token, right) => {
                let operator = match operator_token {
                    Token::Plus => "+",
                    Token::Minus => "-",
                    Token::Asterisk => "*",
                    Token::Slash => "/",
                    Token::Gt => ">",
                    Token::Lt => "<",
                    Token::Eq => "==",
                    Token::NotEq => "!=",

                    _ => panic!("Invalid Infix Token: {:?}", operator_token),
                };

                write!(f, "({}{}{})", left, operator, right)
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
        program.statements.push(Statement::LetStatement(
            "myVar".to_string(),
            Expression::Identifier("5".to_string()),
        ));

        program
            .statements
            .push(Statement::ReturnStatement(Token::Return));

        assert_eq!(program.to_string(), "let myVar = 5;\nreturn (todo);");
    }
}
