use crate::object::Object;

pub(crate) fn values(args: Vec<Object>) -> Object {
    match args.as_slice() {
        [Object::Hash(hash)] => {
            let mut values = vec![];
            hash.iter().for_each(|(_, value)| {
                values.push(value.clone());
            });
            Object::Array(values)
        }
        [non_hash] => Object::Error(format!(
            "argument to `keys` must be a hash, got {:?}",
            non_hash
        )),
        _ => Object::Error("`keys` expects 1 argument".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::object::{HashKey, Object};

    fn count_objects(values: Vec<Object>) -> HashMap<String, i32> {
        let mut counts = HashMap::new();
        for value in values {
            *counts.entry(value.to_string()).or_insert(0) += 1;
        }

        counts
    }

    #[test]
    fn test_values() {
        let mut hash_map = HashMap::new();
        hash_map.insert(HashKey::Integer(1), Object::Integer(2));
        hash_map.insert(HashKey::Boolean(true), Object::Boolean(false));

        let hash = Object::Hash(hash_map);
        let evaluated = values(vec![hash]);

        if let Object::Array(vec) = evaluated {
            assert_eq!(
                count_objects(vec),
                count_objects(vec![Object::Integer(2), Object::Boolean(false)])
            );
        } else {
            panic!("expected array");
        }
    }

    #[test]
    fn test_values_empty() {
        let hash_map = HashMap::new();

        let hash = Object::Hash(hash_map);
        let evaluated = values(vec![hash]);

        if let Object::Array(vec) = evaluated {
            assert_eq!(vec, vec![]);
        } else {
            panic!("expected array");
        }
    }

    #[test]
    fn test_values_invalid() {
        let hash = Object::Integer(1);
        let evaluated = values(vec![hash]);

        if let Object::Error(_) = evaluated {
        } else {
            panic!("expected error");
        }
    }
}
