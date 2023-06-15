use crate::object::Object;

use super::out_of_range_error;

pub fn get_builtin_fn(ident: &str) -> Option<Object> {
    match ident {
        "len" => Some(Object::BuiltInFunction(len)),
        "first" => Some(Object::BuiltInFunction(first)),
        "last" => Some(Object::BuiltInFunction(last)),
        "push" => Some(Object::BuiltInFunction(push)),
        "pop" => Some(Object::BuiltInFunction(pop)),
        "range" => Some(Object::BuiltInFunction(range)),
        _ => None,
    }
}

fn len(args: Vec<Object>) -> Object {
    match args.as_slice() {
        [Object::Array(items)] => Object::Integer(items.len() as i64),
        [Object::String(str)] => Object::Integer(str.len() as i64),
        [obj] => Object::Error(format!("argument to `len` not supported, got {:?}", obj)),
        _ => Object::Error("`len` expects 1 argument".to_string()),
    }
}

fn first(args: Vec<Object>) -> Object {
    match args.as_slice() {
        [Object::Array(items)] => items
            .first()
            .unwrap_or(&out_of_range_error(items.len(), 0))
            .clone(),
        [obj] => Object::Error(format!("argument to `first` not supported, got {:?}", obj)),
        _ => Object::Error("`first` expects 1 argument".to_string()),
    }
}

fn last(args: Vec<Object>) -> Object {
    match args.as_slice() {
        [Object::Array(items)] => items
            .last()
            .unwrap_or(&out_of_range_error(items.len(), 0))
            .clone(),
        [obj] => Object::Error(format!("argument to `last` not supported, got {:?}", obj)),
        _ => Object::Error("`last` expects 1 argument".to_string()),
    }
}

fn push(args: Vec<Object>) -> Object {
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

fn pop(args: Vec<Object>) -> Object {
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

fn range(args: Vec<Object>) -> Object {
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
}
