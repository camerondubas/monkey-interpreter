use crate::object::Object;
mod entries;
mod first;
mod keys;
mod last;
mod len;
mod pop;
mod push;
mod puts;
mod range;
mod values;

use entries::entries;
use first::first;
use keys::keys;
use last::last;
use len::len;
use pop::pop;
use push::push;
use puts::puts;
use range::range;
use values::values;

pub fn get_builtin_fn(ident: &str) -> Option<Object> {
    let builtin = match ident {
        "len" => Object::BuiltInFunction(len),
        "first" => Object::BuiltInFunction(first),
        "last" => Object::BuiltInFunction(last),
        "push" => Object::BuiltInFunction(push),
        "pop" => Object::BuiltInFunction(pop),
        "range" => Object::BuiltInFunction(range),
        "puts" => Object::BuiltInFunction(puts),
        "keys" => Object::BuiltInFunction(keys),
        "values" => Object::BuiltInFunction(values),
        "entries" => Object::BuiltInFunction(entries),
        _ => return None,
    };

    Some(builtin)
}

#[cfg(test)]
mod tests {}
