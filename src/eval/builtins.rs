use crate::object::Object;

use super::out_of_range_error;

pub fn get_builtin_fn(ident: &str) -> Option<Object> {
    match ident {
        "len" => Some(Object::BuiltInFunction(len)),
        "first" => Some(Object::BuiltInFunction(first)),
        "last" => Some(Object::BuiltInFunction(last)),
        _ => None,
    }
}

fn len(args: Vec<Object>) -> Object {
    match args.first() {
        Some(Object::Array(items)) => Object::Integer(items.len() as i64),
        Some(Object::String(str)) => Object::Integer(str.len() as i64),
        Some(obj) => Object::Error(format!("argument to `len` not supported, got {:?}", obj)),
        None => Object::Error("`len` expects 1 argument".to_string()),
    }
}

fn first(args: Vec<Object>) -> Object {
    match args.first() {
        Some(Object::Array(items)) => items
            .first()
            .unwrap_or(&out_of_range_error(items.len(), 0))
            .clone(),
        Some(obj) => Object::Error(format!("argument to `first` not supported, got {:?}", obj)),
        None => Object::Error("`first` expects 1 argument".to_string()),
    }
}

fn last(args: Vec<Object>) -> Object {
    match args.first() {
        Some(Object::Array(items)) => items
            .last()
            .unwrap_or(&out_of_range_error(items.len(), 0))
            .clone(),
        Some(obj) => Object::Error(format!("argument to `last` not supported, got {:?}", obj)),
        None => Object::Error("`last` expects 1 argument".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;

    #[test]
    fn test_array_len() {
        let tests = vec![
            (vec![], 0),
            (
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
                3,
            ),
            (vec![Object::String("test".to_string())], 1),
        ];

        for (input, expected) in tests {
            let evaluated = len(vec![Object::Array(input)]);
            assert_eq!(evaluated, Object::Integer(expected));
        }
    }

    #[test]
    fn test_string_len() {
        let tests = vec![("test", 4), ("", 0), ("hello world", 11)];

        for (input, expected) in tests {
            let evaluated = len(vec![Object::String(input.to_string())]);
            assert_eq!(evaluated, Object::Integer(expected));
        }
    }

    #[test]
    fn test_len_no_arg_error() {
        let evaluated = len(vec![]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }

    #[test]
    fn test_len_bad_type() {
        let evaluated = len(vec![Object::Integer(1)]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }

    #[test]
    fn test_array_first() {
        let evaluated = first(vec![Object::Array(vec![
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3),
        ])]);
        assert_eq!(evaluated, Object::Integer(1));
    }

    #[test]
    fn test_first_empty_array() {
        let evaluated = first(vec![Object::Array(vec![])]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }

    #[test]
    fn test_array_last() {
        let evaluated = last(vec![Object::Array(vec![
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3),
        ])]);
        assert_eq!(evaluated, Object::Integer(3));
    }

    #[test]
    fn test_last_empty_array() {
        let evaluated = last(vec![Object::Array(vec![])]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }
}
