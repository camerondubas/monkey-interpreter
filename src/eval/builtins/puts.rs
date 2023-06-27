use crate::object::Object;

pub(crate) fn puts(args: Vec<Object>) -> Object {
    for arg in args {
        println!("{}", arg);
    }
    Object::Null
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;

    #[test]
    fn test_puts() {
        let evaluated = puts(vec![Object::String("test".to_string())]);
        assert_eq!(evaluated, Object::Null);
    }

    #[test]
    fn test_puts_no_args() {
        let evaluated = puts(vec![]);
        assert_eq!(evaluated, Object::Null);
    }

    #[test]
    fn test_puts_multiple_args() {
        let evaluated = puts(vec![
            Object::String("test".to_string()),
            Object::String("test2".to_string()),
        ]);
        assert_eq!(evaluated, Object::Null);
    }
}
