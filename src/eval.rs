use crate::{
    ast::{Expression, Program, Statement},
    lexer::Token,
    object::Object,
};

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

pub fn eval(program: Program) -> Object {
    let mut result = NULL;

    for statement in program.statements {
        result = eval_statement(statement);

        match result {
            Object::Return(ret) => return *ret,
            Object::Error(_) => return result,
            _ => (),
        }
    }

    result
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        Statement::Block(block) => eval_block_statement(block),
        Statement::Return(expression) => {
            let value = eval_expression(expression);
            if value.is_error() {
                return value;
            }

            Object::Return(Box::new(value))
        }
        _ => NULL,
    }
}

fn eval_block_statement(block: Vec<Statement>) -> Object {
    let mut result = NULL;

    for statement in block {
        result = eval_statement(statement);

        match result {
            Object::Return(_) => return result,
            Object::Error(_) => return result,
            _ => (),
        }
    }

    result
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::IntegerLiteral(integer) => Object::Integer(integer),
        Expression::Boolean(true) => TRUE,
        Expression::Boolean(false) => FALSE,
        Expression::PrefixExpression(prefix, right) => {
            let right_obj = eval_expression(*right);
            if right_obj.is_error() {
                return right_obj;
            }
            eval_prefix_expression(prefix, right_obj)
        }
        Expression::InfixExpression(left, operator, right) => {
            let left_obj = eval_expression(*left);
            if left_obj.is_error() {
                return left_obj;
            }

            let right_obj = eval_expression(*right);
            if right_obj.is_error() {
                return right_obj;
            }

            eval_infix_expression(operator, left_obj, right_obj)
        }
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(*condition, *consequence, alternative)
        }
        _ => NULL,
    }
}

fn eval_prefix_expression(prefix: Token, right: Object) -> Object {
    match prefix {
        Token::Bang => eval_bang_operator_expression(right),
        Token::Minus => eval_minus_operator_expression(right),
        _ => unknown_operator_error(None, prefix, right),
    }
}

fn eval_infix_expression(operator: Token, left: Object, right: Object) -> Object {
    match (left.clone(), right.clone(), operator.clone()) {
        (Object::Integer(_), Object::Integer(_), _) => {
            eval_integer_infix_expression(operator, left, right)
        }
        (_, _, Token::Eq) => Object::Boolean(left == right),
        (_, _, Token::NotEq) => Object::Boolean(left != right),
        _ => {
            if left.get_type() != right.get_type() {
                return type_mismatch_error(left, operator, right);
            }

            unknown_operator_error(Some(left), operator, right)
        }
    }
}

fn eval_integer_infix_expression(operator: Token, left_obj: Object, right_obj: Object) -> Object {
    match (left_obj.clone(), right_obj.clone()) {
        (Object::Integer(left), Object::Integer(right)) => match operator {
            Token::Plus => Object::Integer(left + right),
            Token::Minus => Object::Integer(left - right),
            Token::Asterisk => Object::Integer(left * right),
            Token::Slash => Object::Integer(left / right),
            Token::Lt => Object::Boolean(left < right),
            Token::Gt => Object::Boolean(left > right),
            Token::Eq => Object::Boolean(left == right),
            Token::NotEq => Object::Boolean(left != right),
            _ => unknown_operator_error(Some(left_obj), operator, right_obj),
        },
        (Object::Boolean(left), Object::Boolean(right)) => match operator {
            Token::Lt => Object::Boolean(!left & right),
            Token::Gt => Object::Boolean(left & !right),
            Token::Eq => Object::Boolean(left == right),
            Token::NotEq => Object::Boolean(left != right),
            _ => unknown_operator_error(Some(left_obj), operator, right_obj),
        },
        _ => type_mismatch_error(left_obj, operator, right_obj),
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
        Object::Integer(integer) => Object::Integer(-integer),
        _ => unknown_operator_error(None, Token::Minus, object),
    }
}

fn eval_if_expression(
    condition: Expression,
    consequence: Statement,
    alternative: Option<Box<Statement>>,
) -> Object {
    let condition = eval_expression(condition);
    if condition.is_error() {
        return condition;
    }

    let is_truthy = is_truthy(condition);

    match (is_truthy, alternative) {
        (true, _) => eval_statement(consequence),
        (false, Some(alternative)) => eval_statement(*alternative),
        _ => NULL,
    }
}

fn is_truthy(object: Object) -> bool {
    match object {
        TRUE => true,
        FALSE => false,
        Object::Integer(_) => true,
        NULL => false,
        _ => true,
    }
}

fn type_mismatch_error(left: Object, operator: Token, right: Object) -> Object {
    Object::Error(format!(
        "type mismatch: {} {} {}",
        left.get_type(),
        operator,
        right.get_type()
    ))
}

fn unknown_operator_error(left: Option<Object>, operator: Token, right: Object) -> Object {
    match left {
        Some(left) => Object::Error(format!(
            "unknown operator: {} {} {}",
            left.get_type(),
            operator,
            right.get_type()
        )),
        None => Object::Error(format!(
            "unknown operator: {}{}",
            operator,
            right.get_type()
        )),
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
                Object::Integer(integer) => assert_eq!(integer, expected),
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
                Object::Boolean(bool) => assert_eq!(bool, expected),
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
                Object::Boolean(bool) => assert_eq!(bool, expected),
                _ => panic!("Expected Boolean, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_eval_conditional_integer() {
        let inputs = vec![
            ("if (true) { 10 }", 10),
            ("if (1) { 10 }", 10),
            ("if (1 < 2) { 10 }", 10),
            ("if (1 > 2) { 10 } else { 20 }", 20),
            ("if (1 < 2) { 10 } else { 20 }", 10),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Integer(integer) => assert_eq!(integer, expected),
                _ => panic!("Expected Integer, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_eval_conditional_null() {
        let inputs = vec![("if (false) { 10 }", "null"), ("if (1 > 2) { 10 }", "null")];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Null => assert_eq!(Object::Null.to_string(), expected),
                _ => panic!("Expected Null, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_eval_return_statement() {
        let inputs = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10>1) {
                if (10 > 1) {
                    return 10;
                }

                return 1
            }",
                10,
            ),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Integer(integer) => assert_eq!(integer, expected),
                _ => panic!("Expected Integer, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_error_handling() {
        let inputs = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Error(error) => assert_eq!(error, expected),
                _ => panic!("Expected Error, got {:?}. `{}`", evaluated, input),
            }
        }
    }

    fn test_eval(input: &str) -> Object {
        let program = Parser::from_source(input).parse_program();
        eval(program)
    }
}
