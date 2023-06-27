use crate::{eval::out_of_range_error, object::Object};

pub(crate) fn last(args: Vec<Object>) -> Object {
    match args.as_slice() {
        [Object::Array(items)] => items
            .last()
            .unwrap_or(&out_of_range_error(items.len(), 0))
            .clone(),
        [obj] => Object::Error(format!("argument to `last` not supported, got {:?}", obj)),
        _ => Object::Error("`last` expects 1 argument".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;

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
