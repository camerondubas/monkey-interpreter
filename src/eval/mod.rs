use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{Expression, Program, Statement},
    lexer::Token,
    object::{
        constants::{FALSE, NULL, TRUE},
        Environment, HashKey, Object,
    },
};

mod builtins;

use self::builtins::get_builtin_fn;

pub fn eval(program: Program, environment: Rc<RefCell<Environment>>) -> Object {
    let mut result = NULL;

    for statement in program.statements {
        result = eval_statement(statement, Rc::clone(&environment));

        match result {
            Object::Return(ret) => return *ret,
            Object::Error(_) => return result,
            _ => (),
        }
    }

    result
}

fn eval_statement(statement: Statement, environment: Rc<RefCell<Environment>>) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression, environment),
        Statement::Block(block) => eval_block_statement(block, environment),
        Statement::Let(identifier, expression) => {
            let value = eval_expression(expression, Rc::clone(&environment));
            if value.is_error() {
                return value;
            }

            environment.borrow_mut().set(identifier, value)
        }
        Statement::Return(expression) => {
            let value = eval_expression(expression, environment);
            if value.is_error() {
                return value;
            }

            Object::Return(Box::new(value))
        }
    }
}

fn eval_block_statement(block: Vec<Statement>, environment: Rc<RefCell<Environment>>) -> Object {
    let mut result = NULL;

    for statement in block {
        result = eval_statement(statement, Rc::clone(&environment));

        if matches!(result, Object::Return(_) | Object::Error(_)) {
            return result;
        }
    }

    result
}

fn eval_expression(expression: Expression, environment: Rc<RefCell<Environment>>) -> Object {
    match expression {
        Expression::IntegerLiteral(integer) => Object::Integer(integer),
        Expression::StringLiteral(value) => Object::String(value),
        Expression::Boolean(true) => TRUE,
        Expression::Boolean(false) => FALSE,
        Expression::Index(left, idx) => eval_index(*left, *idx, environment),
        Expression::ArrayLiteral(items) => eval_array_literal(items, environment),
        Expression::HashLiteral(items) => eval_hash_literal(items, environment),
        Expression::FunctionLiteral(params, body) => {
            Object::Function(params, body, Rc::clone(&environment))
        }
        Expression::CallExpression(function_expression, args) => {
            let function = eval_expression(*function_expression, Rc::clone(&environment));
            if function.is_error() {
                return function;
            }

            let args = eval_expressions(args, environment);

            if args.len() == 1 && args[0].is_error() {
                return args[0].clone();
            }

            apply_function(function, args)
        }
        Expression::Identifier(identifier) => eval_identifier(identifier, environment),
        Expression::PrefixExpression(prefix, right) => {
            let right_obj = eval_expression(*right, environment);
            if right_obj.is_error() {
                return right_obj;
            }
            eval_prefix_expression(prefix, right_obj)
        }
        Expression::InfixExpression(left, operator, right) => {
            let left_obj = eval_expression(*left, Rc::clone(&environment));
            if left_obj.is_error() {
                return left_obj;
            }

            let right_obj = eval_expression(*right, environment);
            if right_obj.is_error() {
                return right_obj;
            }

            eval_infix_expression(operator, left_obj, right_obj)
        }
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(*condition, *consequence, alternative, environment)
        }
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
        (_, _, Token::Eq) => Object::Boolean(left == right),
        (_, _, Token::NotEq) => Object::Boolean(left != right),
        (Object::Integer(l), Object::Integer(r), _) => eval_integer_infix(operator, l, r),
        (Object::String(l), Object::String(r), _) => eval_string_infix(operator, l, r),
        (Object::Boolean(l), Object::Boolean(r), _) => eval_boolean_infix(operator, l, r),
        _ => {
            if left.get_type() != right.get_type() {
                return operator_type_mismatch_error(left, operator, right);
            }

            unknown_operator_error(Some(left), operator, right)
        }
    }
}

fn eval_integer_infix(operator: Token, left: i64, right: i64) -> Object {
    match operator {
        Token::Plus => Object::Integer(left + right),
        Token::Minus => Object::Integer(left - right),
        Token::Asterisk => Object::Integer(left * right),
        Token::Slash => Object::Integer(left / right),
        Token::Lt => Object::Boolean(left < right),
        Token::Gt => Object::Boolean(left > right),
        _ => unknown_operator_error(
            Some(Object::Integer(left)),
            operator,
            Object::Integer(right),
        ),
    }
}

fn eval_string_infix(operator: Token, left: String, right: String) -> Object {
    match operator {
        Token::Plus => Object::String(format!("{}{}", left, right)),
        _ => unknown_operator_error(Some(Object::String(left)), operator, Object::String(right)),
    }
}

fn eval_boolean_infix(operator: Token, left: bool, right: bool) -> Object {
    match operator {
        Token::Lt => Object::Boolean(!left & right),
        Token::Gt => Object::Boolean(left & !right),
        _ => unknown_operator_error(
            Some(Object::Boolean(left)),
            operator,
            Object::Boolean(right),
        ),
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
    environment: Rc<RefCell<Environment>>,
) -> Object {
    let condition = eval_expression(condition, Rc::clone(&environment));
    if condition.is_error() {
        return condition;
    }

    let is_truthy = is_truthy(condition);

    match (is_truthy, alternative) {
        (true, _) => eval_statement(consequence, environment),
        (false, Some(alternative)) => eval_statement(*alternative, environment),
        _ => NULL,
    }
}

fn eval_identifier(identifier: String, environment: Rc<RefCell<Environment>>) -> Object {
    if let Some(env_identifier) = environment.borrow().get(identifier.clone()) {
        return env_identifier;
    }

    if let Some(builtin) = get_builtin_fn(&identifier) {
        return builtin;
    }

    unknown_identifier_error(identifier)
}

fn eval_expressions(
    expressions: Vec<Expression>,
    environment: Rc<RefCell<Environment>>,
) -> Vec<Object> {
    let mut result = vec![];

    for expression in expressions {
        let evaluated = eval_expression(expression, Rc::clone(&environment));
        if evaluated.is_error() {
            return vec![evaluated];
        }

        result.push(evaluated);
    }

    result
}

fn eval_array_literal(items: Vec<Expression>, environment: Rc<RefCell<Environment>>) -> Object {
    let item_objs = items
        .iter()
        .map(|i| eval_expression(i.clone(), Rc::clone(&environment)))
        .collect::<Vec<Object>>();

    Object::Array(item_objs)
}

fn eval_hash_literal(
    items: Vec<(Expression, Expression)>,
    environment: Rc<RefCell<Environment>>,
) -> Object {
    let mut pairs = HashMap::new();

    for (key, value) in items {
        let key = eval_expression(key, Rc::clone(&environment));
        if key.is_error() {
            return key;
        }

        let value = eval_expression(value, Rc::clone(&environment));
        if value.is_error() {
            return value;
        }

        match HashKey::from_obj(key) {
            Ok(hash_key) => pairs.insert(hash_key, value),
            Err(err) => return err,
        };
    }

    Object::Hash(pairs)
}

fn eval_index(left: Expression, idx: Expression, environment: Rc<RefCell<Environment>>) -> Object {
    let left_obj = eval_expression(left, Rc::clone(&environment));
    let idx_obj = eval_expression(idx, environment);

    match left_obj {
        Object::Array(items) => {
            let idx_int = match idx_obj {
                Object::Integer(i) => i,
                Object::Error(_) => return idx_obj,
                _ => return type_mismatch_error(Object::Integer(0), idx_obj),
            };

            match items.get(idx_int as usize) {
                Some(val) => val.clone(),
                None => out_of_range_error(items.len(), idx_int as usize),
            }
        }
        Object::Hash(pairs) => {
            let key = match HashKey::from_obj(idx_obj) {
                Ok(key) => key,
                Err(e) => return e,
            };

            match pairs.get(&key) {
                Some(val) => val.clone(),
                None => Object::Null,
            }
        }
        Object::Error(_) => left_obj,
        _ => type_mismatch_error(Object::Array(vec![]), left_obj),
    }
}

fn apply_function(function: Object, args: Vec<Object>) -> Object {
    match function {
        Object::Function(params, body, env) => {
            let mut extended_env = Environment::new_enclosed(Rc::clone(&env));

            for (idx, param) in params.iter().enumerate() {
                if let Expression::Identifier(param_name) = param {
                    extended_env.set(param_name.clone(), args[idx].clone());
                } else {
                    return Object::Error(format!("not an identifier: {}", param));
                }
            }

            let evaluated = eval_statement(*body, Rc::new(RefCell::new(extended_env)));

            match evaluated {
                Object::Return(value) => *value,
                _ => evaluated,
            }
        }
        Object::BuiltInFunction(builtin) => builtin(args),
        _ => Object::Error(format!("not a function: {}", function.get_type())),
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

fn operator_type_mismatch_error(left: Object, operator: Token, right: Object) -> Object {
    Object::Error(format!(
        "type mismatch: {} {} {}",
        left.get_type(),
        operator,
        right.get_type()
    ))
}

fn type_mismatch_error(expected: Object, actual: Object) -> Object {
    Object::Error(format!(
        "type mismatch: expected ({}), got ({})",
        expected.get_type(),
        actual.get_type()
    ))
}

fn out_of_range_error(len: usize, idx: usize) -> Object {
    Object::Error(format!(
        "out of range error: cannot access index {} for array of length {}",
        len, idx
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

fn unknown_identifier_error(identifier: String) -> Object {
    Object::Error(format!("identifier not found: {}", identifier))
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
    fn test_eval_string() {
        let inputs = vec![
            ("\"5\"", "5"),
            ("\"foo\"", "foo"),
            ("\"test@domain.com\"", "test@domain.com"),
            ("\"S-a=>_ft432||rst+~ascc\"", "S-a=>_ft432||rst+~ascc"),
            (
                "\"(5 + 10 * 2 + 15 / 3) * 2 + -10\"",
                "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            ),
            (r#""some \"nested\" string""#, "some \"nested\" string"),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            println!("STRING {}", evaluated);
            match evaluated {
                Object::String(value) => assert_eq!(value, expected),
                _ => panic!("Expected String, got {:?}", evaluated),
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
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Error(error) => assert_eq!(error, expected),
                _ => panic!("Expected Error, got {:?}. `{}`", evaluated, input),
            }
        }
    }

    #[test]
    fn test_let_statement() {
        let inputs = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
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
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";

        let evaluated = test_eval(input);

        match evaluated {
            Object::Function(params, body, _) => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].to_string(), "x");
                assert_eq!(body.to_string(), "(x + 2)");
            }
            _ => panic!("Expected Function, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_builtin_functions() {
        let inputs = vec![
            ("len(\"\")", 0),
            ("len(\"four\")", 4),
            ("len(\"hello world\")", 11),
            ("len([])", 0),
            ("len([1,2,3])", 3),
            ("let a = \"a\"; len([a])", 1),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Integer(val) => assert_eq!(val, expected),
                _ => panic!("Expected Integer, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_builtin_fn_errors() {
        let inputs = vec![
            ("len()", "`len` expects 1 argument"),
            ("len(1)", "argument to `len` not supported, got Integer(1)"),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Error(err) => assert_eq!(err, expected),
                _ => panic!("Expected Error, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_array_literal() {
        let inputs = vec![
            ("[]", vec![]),
            (
                "[1, true, \"test\"]",
                vec![
                    Object::Integer(1),
                    Object::Boolean(true),
                    Object::String("test".to_string()),
                ],
            ),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Array(val) => assert_eq!(val, expected),
                _ => panic!("Expected Array, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_array_index() {
        let inputs = vec![
            ("[1, 2, 3][0]", 1),
            ("[1, 2, 3][1]", 2),
            ("[1, 2, 3][2]", 3),
            ("let i = 0; [1][i];", 1),
            ("[1, 2, 3][1 + 1];", 3),
            ("let myArray = [1, 2, 3]; myArray[2];", 3),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                6,
            ),
            ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", 2),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Integer(val) => assert_eq!(val, expected),
                _ => panic!("Expected Array, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_array_index_error() {
        let inputs = vec![
            ("[1, 2, 3][3]", out_of_range_error(3, 3)),
            ("[1, 2, 3][5]", out_of_range_error(3, 5)),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input);

            match evaluated {
                Object::Error(val) => assert_eq!(format!("ERROR: {}", val), expected.to_string()),
                _ => panic!("Expected Array, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_hash_literal_simple() {
        let mut expected = HashMap::new();
        expected.insert(HashKey::String("one".to_string()), Object::Integer(1));
        expected.insert(HashKey::String("two".to_string()), Object::Integer(2));
        expected.insert(HashKey::String("three".to_string()), Object::Integer(3));

        let input = "{ \"one\": 1, \"two\": 2, \"three\": 3 }";
        let evaluated = test_eval(input);

        match evaluated {
            Object::Hash(val) => assert_eq!(val, expected),
            _ => panic!("Expected Hash, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_hash_literal_complex() {
        let mut expected = HashMap::new();
        expected.insert(HashKey::Boolean(true), Object::Boolean(true));
        expected.insert(
            HashKey::String("two".to_string()),
            Object::String("two".to_string()),
        );
        expected.insert(
            HashKey::Integer(3),
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
            ]),
        );

        let input = "{ true: true, \"two\": \"two\", 3: [1, 2, 3] }";
        let evaluated = test_eval(input);

        match evaluated {
            Object::Hash(val) => assert_eq!(val, expected),
            _ => panic!("Expected Hash, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_hash_index_inline() {
        let input = "{ true: false, \"two\": \"two\", 3: [1, 2, 3] }[true]";
        let evaluated = test_eval(input);

        match evaluated {
            FALSE => (),
            _ => panic!("Expected Hash, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_hash_index_variable() {
        let input = "let hash = { true: false, \"two\": \"two\", 3: [1, 2, 3] }; hash[\"two\"]";
        let evaluated = test_eval(input);

        match evaluated {
            Object::String(str) => assert_eq!(str, "two".to_string()),
            _ => panic!("Expected Hash, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_function_application() {
        let inputs = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
            (
                "let newAdder = fn(x) {
                fn(y) { x + y };
            }
            let addTwo = newAdder(2);
            addTwo(2);",
                4,
            ),
            (
                "let callbackHell = fn(cb) { 1 + cb() };
                callbackHell(fn() {3 + 4});",
                8,
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

    fn test_eval(input: &str) -> Object {
        let program = Parser::from_source(input).parse_program();
        let environment = Rc::new(RefCell::new(Environment::new()));
        eval(program, environment)
    }
}
