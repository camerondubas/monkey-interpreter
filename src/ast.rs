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
                .join(""),
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement(String, Expression),
    ReturnStatement(Token),
    ExpressionStatement(Expression),
    BlockStatement(Vec<Statement>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::LetStatement(name, value) => {
                write!(f, "{} {} {} {};", Token::Let, name, Token::Assign, value)
            }
            Statement::ReturnStatement(_) => write!(f, "{} (todo);", Token::Return),
            Statement::ExpressionStatement(expression) => write!(f, "{}", expression),
            Statement::BlockStatement(statements) => {
                for statement in statements {
                    write!(f, "{}", statement)?;
                }

                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(u64),
    PrefixExpression(Token, Box<Expression>),
    InfixExpression(Box<Expression>, Token, Box<Expression>),
    Boolean(bool),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(value) => write!(f, "{}", value),
            Expression::IntegerLiteral(value) => write!(f, "{}", value),
            Expression::Boolean(value) => write!(f, "{}", value),
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
                    None => format!(""),
                };

                let full = vec![if_str, consequence_str, alternative_str].join(" ");
                let _ = full.trim();

                write!(f, "{}", full)
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

        assert_eq!(program.to_string(), "let myVar = 5;return (todo);");
    }

    #[test]
    fn test_if_strignify() {
        let if_expression = Expression::If(
            Box::new(Expression::Boolean(true)),
            Box::new(Statement::LetStatement(
                "a".to_string(),
                Expression::IntegerLiteral(1),
            )),
            Some(Box::new(Statement::LetStatement(
                "b".to_string(),
                Expression::IntegerLiteral(2),
            ))),
        );

        let output = "if (true) { let a = 1; } else { let b = 2; }";

        assert_eq!(if_expression.to_string(), output.to_string());
    }
}
