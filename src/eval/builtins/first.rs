use crate::{eval::out_of_range_error, object::Object};

pub(crate) fn first(args: Vec<Object>) -> Object {
    match args.as_slice() {
        [Object::Array(items)] => items
            .first()
            .unwrap_or(&out_of_range_error(items.len(), 0))
            .clone(),
        [obj] => Object::Error(format!("argument to `first` not supported, got {:?}", obj)),
        _ => Object::Error("`first` expects 1 argument".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;

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
}
