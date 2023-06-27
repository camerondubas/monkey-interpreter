use crate::object::Object;

pub(crate) fn push(args: Vec<Object>) -> Object {
    match args.as_slice() {
        [Object::Array(items), obj] => {
            let mut new_items = items.clone();
            new_items.push(obj.clone());
            Object::Array(new_items)
        }
        [non_array, _] => Object::Error(format!(
            "argument to `push` not supported, got {:?}",
            non_array,
        )),
        _ => Object::Error("`push` expects 2 arguments".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;

    #[test]
    fn test_array_push() {
        let evaluated = push(vec![
            Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
            Object::Integer(3),
        ]);
        assert_eq!(
            evaluated,
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3)
            ])
        );
    }

    #[test]
    fn test_push_no_args_error() {
        let evaluated = push(vec![]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }

    #[test]
    fn test_push_one_arg_error() {
        let evaluated = push(vec![Object::Array(vec![
            Object::Integer(1),
            Object::Integer(2),
        ])]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }

    #[test]
    fn test_push_bad_type_error() {
        let evaluated = push(vec![Object::Integer(2), Object::String("test".to_string())]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }
}
