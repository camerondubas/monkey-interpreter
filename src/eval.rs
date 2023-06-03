use crate::{
    ast::{Expression, Program, Statement},
    lexer::Token,
    object::{Boolean, Integer, Null, Object},
};

const NULL: Object = Object::Null(Null {});
const TRUE: Object = Object::Boolean(Boolean { value: true });
const FALSE: Object = Object::Boolean(Boolean { value: false });

pub fn eval(program: Program) -> Object {
    eval_statements(program.statements)
}

fn eval_statements(statements: Vec<Statement>) -> Object {
    let mut result = NULL;
    for statement in statements {
        result = match statement {
            Statement::Expression(expression) => eval_expression(expression),
            _ => NULL,
        }
    }

    result
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::IntegerLiteral(integer) => Object::Integer(Integer { value: integer }),
        Expression::Boolean(true) => TRUE,
        Expression::Boolean(false) => FALSE,
        Expression::PrefixExpression(prefix, right) => {
            let right_obj = eval_expression(*right);
            eval_prefix_expression(prefix, right_obj)
        }
        Expression::InfixExpression(left, operator, right) => {
            let left_obj = eval_expression(*left);
            let right_obj = eval_expression(*right);
            eval_infix_expression(operator, left_obj, right_obj)
        }
        _ => NULL,
    }
}

fn eval_prefix_expression(prefix: Token, right: Object) -> Object {
    match prefix {
        Token::Bang => eval_bang_operator_expression(right),
        Token::Minus => eval_minus_operator_expression(right),
        _ => NULL,
    }
}

fn eval_infix_expression(operator: Token, left: Object, right: Object) -> Object {
    match (left.clone(), right.clone(), operator.clone()) {
        (Object::Integer(left), Object::Integer(right), _) => {
            eval_integer_infix_expression(operator, left, right)
        }
        (_, _, Token::Eq) => Object::Boolean(Boolean {
            value: left == right,
        }),
        (_, _, Token::NotEq) => Object::Boolean(Boolean {
            value: left != right,
        }),
        _ => NULL,
    }
}

fn eval_integer_infix_expression(operator: Token, left: Integer, right: Integer) -> Object {
    match operator {
        Token::Plus => Object::Integer(Integer {
            value: left.value + right.value,
        }),
        Token::Minus => Object::Integer(Integer {
            value: left.value - right.value,
        }),
        Token::Asterisk => Object::Integer(Integer {
            value: left.value * right.value,
        }),
        Token::Slash => Object::Integer(Integer {
            value: left.value / right.value,
        }),
        Token::Lt => Object::Boolean(Boolean {
            value: left.value < right.value,
        }),
        Token::Gt => Object::Boolean(Boolean {
            value: left.value > right.value,
        }),
        Token::Eq => Object::Boolean(Boolean {
            value: left.value == right.value,
        }),
        Token::NotEq => Object::Boolean(Boolean {
            value: left.value != right.value,
        }),
        _ => panic!("Not implemented"),
    }
}

fn eval_bang_operator_expression(object: Object) -> Object {
    match object {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_operator_expression(object: Object) -> Object {
    match object {
        Object::Integer(integer) => Object::Integer(Integer {
            value: -integer.value,
        }),
        _ => NULL,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer() {
        let inputs = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Integer(integer) => assert_eq!(integer.value, expected),
                _ => panic!("Expected Integer, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_eval_boolean() {
        let inputs = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Boolean(bool) => assert_eq!(bool.value, expected),
                _ => panic!("Expected Boolean, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_eval_bang() {
        let inputs = vec![
            ("!true", false),
            ("!false", true),
            ("!!true", true),
            ("!!false", false),
            ("!5", false),
            ("!!5", true),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Boolean(bool) => assert_eq!(bool.value, expected),
                _ => panic!("Expected Boolean, got {:?}", evaluated),
            }
        }
    }

    fn test_eval(input: &str) -> Object {
        let program = Parser::from_source(input).parse_program();
        eval(program)
    }
}
