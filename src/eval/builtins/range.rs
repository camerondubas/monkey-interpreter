use crate::object::Object;

pub(crate) fn range(args: Vec<Object>) -> Object {
    match args.as_slice() {
        [Object::Array(items), Object::Integer(start), Object::Integer(end)] => {
            let start = *start as usize;
            let end = *end as usize;
            if start > end {
                return Object::Error(format!(
                    "start index ({}) cannot be greater than end index ({})",
                    start, end
                ));
            }
            if end > items.len() {
                return Object::Error(format!(
                    "end index ({}) cannot be greater than array length ({})",
                    end,
                    items.len()
                ));
            }
            Object::Array(items[start..=end].to_vec())
        }
        [non_array, _, _] => Object::Error(format!(
            "argument to `range` not supported, got {:?}",
            non_array,
        )),
        _ => Object::Error("`range` expects 3 arguments".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;

    #[test]
    fn test_range() {
        let evaluated = range(vec![
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4),
                Object::Integer(5),
            ]),
            Object::Integer(1),
            Object::Integer(3),
        ]);
        assert_eq!(
            evaluated,
            Object::Array(vec![
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4)
            ])
        );
    }

    #[test]
    fn test_range_no_args_error() {
        let evaluated = range(vec![]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }

    #[test]
    fn test_range_one_arg_error() {
        let evaluated = range(vec![Object::Array(vec![
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3),
        ])]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }

    #[test]
    fn test_range_two_args_error() {
        let evaluated = range(vec![
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
            ]),
            Object::Integer(1),
        ]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }

    #[test]
    fn test_range_bad_type_error() {
        let evaluated = range(vec![
            Object::Integer(2),
            Object::Integer(1),
            Object::Integer(3),
        ]);
        match evaluated {
            Object::Error(_) => (),
            _ => panic!("expected error"),
        }
    }
}
