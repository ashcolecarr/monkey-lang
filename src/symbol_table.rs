use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type SymbolScope = String;

pub const GLOBAL_SCOPE: &str = "GLOBAL";
pub const LOCAL_SCOPE: &str = "LOCAL";

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

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub store: HashMap<String, Symbol>,
    pub num_definitions: usize,
    pub outer: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let store: HashMap<String, Symbol> = HashMap::new();

        Self { store, num_definitions: 0, outer: None }
    }

    pub fn new_enclosed(outer: Rc<RefCell<SymbolTable>>) -> Self {
        let mut symbol_table = Self::new();
        symbol_table.outer = Some(outer);

        symbol_table
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new(name.clone(), match self.outer {
            Some(_) => String::from(LOCAL_SCOPE),
            None => String::from(GLOBAL_SCOPE)
        }, self.num_definitions);

        self.store.insert(String::from(name), symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        match self.store.get(&String::from(name)) {
            Some(s) => Some(s.clone()),
            None => {
                match &self.outer {
                    Some(out) => {
                        match out.borrow().resolve(&name) {
                            Some(o) => Some(o.clone()),
                            None => None,
                        }
                    },
                    None => None,
                }
            },
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
            ("c", Symbol::new("c", String::from(LOCAL_SCOPE), 0)),
            ("d", Symbol::new("d", String::from(LOCAL_SCOPE), 1)),
            ("e", Symbol::new("e", String::from(LOCAL_SCOPE), 0)),
            ("f", Symbol::new("f", String::from(LOCAL_SCOPE), 1)),
        ].iter().cloned().collect();

        let mut global = SymbolTable::new();

        let a = global.define("a");
        assert_eq!(a, expected["a"]);

        let b = global.define("b");
        assert_eq!(b, expected["b"]);

        let mut first_local = SymbolTable::new_enclosed(Rc::new(RefCell::new(global)));

        let c = first_local.define("c");
        assert_eq!(c, expected["c"]);

        let d = first_local.define("d");
        assert_eq!(d, expected["d"]);

        let mut second_local = SymbolTable::new_enclosed(Rc::new(RefCell::new(first_local)));

        let e = second_local.define("e");
        assert_eq!(e, expected["e"]);

        let f = second_local.define("f");
        assert_eq!(f, expected["f"]);
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

    #[test]
    fn test_resolve_local() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut local = SymbolTable::new_enclosed(Rc::new(RefCell::new(global)));
        local.define("c");
        local.define("d");

        let expected = vec![
            Symbol::new("a", String::from(GLOBAL_SCOPE), 0),
            Symbol::new("b", String::from(GLOBAL_SCOPE), 1),
            Symbol::new("c", String::from(LOCAL_SCOPE), 0), 
            Symbol::new("d", String::from(LOCAL_SCOPE), 1),
        ];

        for symbol in expected {
            let result = local.resolve(symbol.name.as_str());
            match result {
                Some(res) => assert_eq!(res, symbol),
                None => assert!(false, "Name could not be resolved."),
            };
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        struct SymbolTableTest {
            table: SymbolTable,
            expected_symbols: Vec<Symbol>,
        };

        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(Rc::new(RefCell::new(global)));
        first_local.define("c");
        first_local.define("d");

        let mut second_local = SymbolTable::new_enclosed(Rc::new(RefCell::new(first_local.clone())));
        second_local.define("e");
        second_local.define("f");

        let symbol_table_tests = vec![
            SymbolTableTest {
                table: first_local,
                expected_symbols: vec![
                    Symbol::new("a", String::from(GLOBAL_SCOPE), 0),
                    Symbol::new("b", String::from(GLOBAL_SCOPE), 1),
                    Symbol::new("c", String::from(LOCAL_SCOPE), 0), 
                    Symbol::new("d", String::from(LOCAL_SCOPE), 1),
                ]
            },
            SymbolTableTest {
                table: second_local,
                expected_symbols: vec![
                    Symbol::new("a", String::from(GLOBAL_SCOPE), 0),
                    Symbol::new("b", String::from(GLOBAL_SCOPE), 1),
                    Symbol::new("e", String::from(LOCAL_SCOPE), 0), 
                    Symbol::new("f", String::from(LOCAL_SCOPE), 1),
                ]
            },
        ];

        for symbol_table_test in symbol_table_tests {
            for symbol in symbol_table_test.expected_symbols {
                let result = symbol_table_test.table.resolve(symbol.name.as_str());
                match result {
                    Some(res) => assert_eq!(res, symbol),
                    None => assert!(false, "Name could not be resolved."),
                };
            }
        }
    }
}