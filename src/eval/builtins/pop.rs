use crate::object::Object;

pub(crate) fn pop(args: Vec<Object>) -> Object {
    match args.as_slice() {
        [Object::Array(items)] => {
            let mut new_items = items.clone();
            new_items.pop();
            Object::Array(new_items)
        }
        [non_array] => Object::Error(format!(
            "argument to `pop` not supported, got {:?}",
            non_array,
        )),
        _ => Object::Error("`pop` expects 1 argument".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;

    #[test]
    fn test_array_pop() {
        let evaluated = pop(vec![Object::Array(vec![
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3),
        ])]);
        assert_eq!(
            evaluated,
            Object::Array(vec![Object::Integer(1), Object::Integer(2)])
        );
    }

    #[test]
    fn test_pop_no_args_error() {
        let evaluated = pop(vec![]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }

    #[test]
    fn test_pop_bad_type_error() {
        let evaluated = pop(vec![Object::Integer(2)]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }
}
