use std::collections::HashMap;

pub type SymbolScope = String;

pub const GLOBAL_SCOPE: &str = "GLOBAL";

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

impl Symbol {
    pub fn new(name: &str, scope: SymbolScope, index: usize) -> Self {
        Self { name: String::from(name), scope, index }
    }
}

pub struct SymbolTable {
    pub store: HashMap<String, Symbol>,
    pub num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        let store: HashMap<String, Symbol> = HashMap::new();

        Self { store, num_definitions: 0 }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new(name.clone(), String::from(GLOBAL_SCOPE), self.num_definitions);
        self.store.insert(String::from(name), symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        match self.store.get(&String::from(name)) {
            Some(s) => Some(s.clone()),
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_define() {
        let expected: HashMap<&str, Symbol> = [
            ("a", Symbol::new("a", String::from(GLOBAL_SCOPE), 0)),
            ("b", Symbol::new("b", String::from(GLOBAL_SCOPE), 1)),
        ].iter().cloned().collect();

        let mut global = SymbolTable::new();

        let a = global.define("a");
        assert_eq!(a, expected["a"]);

        let b = global.define("b");
        assert_eq!(b, expected["b"]);
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let expected = vec![
            Symbol::new("a", String::from(GLOBAL_SCOPE), 0),
            Symbol::new("b", String::from(GLOBAL_SCOPE), 1),
        ];

        for symbol in expected {
            let result = global.resolve(symbol.name.as_str());
            match result {
                Some(res) => assert_eq!(res, symbol),
                None => assert!(false, "Name could not be resolved."),
            };
        }
    }
}