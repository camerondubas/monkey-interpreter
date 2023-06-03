use crate::{
    ast::{Expression, Program, Statement},
    object::{Integer, Null, Object},
};

pub fn eval(program: Program) -> Object {
    eval_statements(program.statements)
}

fn eval_statements(statements: Vec<Statement>) -> Object {
    let mut result = Object::Null(Null {});
    for statement in statements {
        result = match statement {
            Statement::Expression(expression) => eval_expression(expression),
            _ => panic!("Not implemented"),
        }
    }

    result
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::IntegerLiteral(integer) => Object::Integer(Integer { value: integer }),
        _ => panic!("Not implemented"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let inputs = vec![("5", 5), ("10", 10)];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Integer(integer) => assert_eq!(integer.value, expected),
                _ => panic!("Expected Integer, got {:?}", evaluated),
            }
        }
    }

    fn test_eval(input: &str) -> Object {
        let program = Parser::from_source(input).parse_program();
        eval(program)
    }
}
