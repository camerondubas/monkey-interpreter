use crate::object::Object;

fn len(args: Vec<Object>) -> Object {
    match args.first() {
        Some(Object::String(str)) => Object::Integer(str.len() as i64),
        Some(obj) => Object::Error(format!("argument to `len` not supported, got {:?}", obj)),
        None => Object::Error("len expects 1 argument".to_string()),
    }
}

pub fn get_builtin_fn(ident: &str) -> Option<Object> {
    match ident {
        "len" => Some(Object::BuiltInFunction(len)),
        _ => None,
    }
}
