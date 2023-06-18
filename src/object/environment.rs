use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use super::Object;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: String) -> Option<Object> {
        match (self.store.get(&name), self.outer.as_ref()) {
            (None, Some(outer_env)) => outer_env.borrow().get(name),
            (obj, _) => obj.cloned(),
        }
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        value
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Current Environment: ")?;
        for (key, value) in &self.store {
            writeln!(f, "  - {}: {}", key, value)?;
        }

        if let Some(outer) = &self.outer {
            writeln!(f, "  - Outer:")?;
            for (key, value) in &outer.borrow().store {
                writeln!(f, "    - {}: {}", key, value)?;
            }
        } else {
            writeln!(f, "  - Outer: None")?;
        }

        Ok(())
    }
}
