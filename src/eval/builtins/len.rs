use crate::object::Object;

pub(crate) fn len(args: Vec<Object>) -> Object {
    match args.as_slice() {
        [Object::Array(items)] => Object::Integer(items.len() as i64),
        [Object::String(str)] => Object::Integer(str.len() as i64),
        [obj] => Object::Error(format!("argument to `len` not supported, got {:?}", obj)),
        _ => Object::Error("`len` expects 1 argument".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;

    #[test]
    fn test_len_array() {
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
    fn test_len_string() {
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
}
