use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
enum SymbolScope {
    Global,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    name: String,
    scope: SymbolScope,
    pub index: usize,
}
impl Symbol {
    fn new_global(name: &str, index: usize) -> Symbol {
        Symbol {
            name: name.to_string(),
            scope: SymbolScope::Global,
            index,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new_global(name, self.num_definitions);
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define() {
        let mut global_table = SymbolTable::new();
        let tests = vec![
            (global_table.define("a"), Symbol::new_global("a", 0)),
            (global_table.define("b"), Symbol::new_global("b", 1)),
        ];

        for (symbol, expected) in tests {
            assert_eq!(symbol, expected);
        }
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let expected = vec![Symbol::new_global("a", 0), Symbol::new_global("b", 1)];

        for expected_symbol in expected {
            let symbol = global.resolve(&expected_symbol.name);
            assert_eq!(symbol, Some(expected_symbol));
        }
    }
}
