use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolScope {
    GlobalScope,
    LocalScope,
    BuiltinScope,
    FreeScope,
    FunctionScope,
}

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
    pub free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let store: HashMap<String, Symbol> = HashMap::new();

        Self { 
            store, 
            num_definitions: 0, 
            outer: None,
            free_symbols: vec![],
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<SymbolTable>>) -> Self {
        let mut symbol_table = Self::new();
        symbol_table.outer = Some(outer);

        symbol_table
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new(name.clone(), match self.outer {
            Some(_) => SymbolScope::LocalScope,
            None => SymbolScope::GlobalScope,
        }, self.num_definitions);

        self.store.insert(String::from(name), symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        match self.store.get(&String::from(name)) {
            Some(s) => return Some(s.clone()),
            None => {
                match &mut self.outer {
                    Some(out) => {
                        let result = out.borrow_mut().resolve(&name);
                        if result.is_none() {
                            return result;
                        }
                        
                        // Unwrap to drop the borrow reference here.
                        let o = result.unwrap();
                        match o.scope {
                            SymbolScope::GlobalScope | SymbolScope::BuiltinScope => Some(o.clone()),
                            _ => {
                                let free = self.define_free(o.clone());

                                Some(free)
                            }
                        }
                    },
                    None => None,
                }
            },
        }
    }

    pub fn define_builtin(&mut self, index: usize, name: &str) -> Symbol {
        let symbol = Symbol::new(name, SymbolScope::BuiltinScope, index);
        self.store.insert(String::from(name), symbol.clone());

        symbol
    }

    fn define_free(&mut self, original: Symbol) -> Symbol {
        self.free_symbols.push(original.clone());

        let symbol = Symbol::new(original.name.as_str(), SymbolScope::FreeScope, self.free_symbols.len() - 1);

        self.store.insert(original.name, symbol.clone());

        symbol
    }

    pub fn define_function_name(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new(name, SymbolScope::FunctionScope, 0);

        self.store.insert(name.to_string(), symbol.clone());

        symbol
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_define() {
        let expected: HashMap<&str, Symbol> = [
            ("a", Symbol::new("a", SymbolScope::GlobalScope, 0)),
            ("b", Symbol::new("b", SymbolScope::GlobalScope, 1)),
            ("c", Symbol::new("c", SymbolScope::LocalScope, 0)),
            ("d", Symbol::new("d", SymbolScope::LocalScope, 1)),
            ("e", Symbol::new("e", SymbolScope::LocalScope, 0)),
            ("f", Symbol::new("f", SymbolScope::LocalScope, 1)),
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
            Symbol::new("a", SymbolScope::GlobalScope, 0),
            Symbol::new("b", SymbolScope::GlobalScope, 1),
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
            Symbol::new("a", SymbolScope::GlobalScope, 0),
            Symbol::new("b", SymbolScope::GlobalScope, 1),
            Symbol::new("c", SymbolScope::LocalScope, 0), 
            Symbol::new("d", SymbolScope::LocalScope, 1),
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
                    Symbol::new("a", SymbolScope::GlobalScope, 0),
                    Symbol::new("b", SymbolScope::GlobalScope, 1),
                    Symbol::new("c", SymbolScope::LocalScope, 0), 
                    Symbol::new("d", SymbolScope::LocalScope, 1),
                ]
            },
            SymbolTableTest {
                table: second_local,
                expected_symbols: vec![
                    Symbol::new("a", SymbolScope::GlobalScope, 0),
                    Symbol::new("b", SymbolScope::GlobalScope, 1),
                    Symbol::new("e", SymbolScope::LocalScope, 0), 
                    Symbol::new("f", SymbolScope::LocalScope, 1),
                ]
            },
        ];

        for mut symbol_table_test in symbol_table_tests {
            for symbol in symbol_table_test.expected_symbols {
                let result = symbol_table_test.table.resolve(symbol.name.as_str());
                match result {
                    Some(res) => assert_eq!(res, symbol),
                    None => assert!(false, "Name could not be resolved."),
                };
            }
        }
    }

    #[test]
    fn test_define_resolve_builtins() {
        let mut global = SymbolTable::new();

        let expected = vec![
            Symbol::new("a", SymbolScope::BuiltinScope, 0),
            Symbol::new("c", SymbolScope::BuiltinScope, 1),
            Symbol::new("e", SymbolScope::BuiltinScope, 2),
            Symbol::new("f", SymbolScope::BuiltinScope, 3),
        ];

        for (i, value) in expected.iter().enumerate() {
            global.define_builtin(i, value.name.as_str());
        }

        let first_local = SymbolTable::new_enclosed(Rc::new(RefCell::new(global.clone())));
        let second_local = SymbolTable::new_enclosed(Rc::new(RefCell::new(first_local.clone())));

        let symbol_tables = vec![global, first_local, second_local];
        for mut table in symbol_tables {
            for symbol in &expected {
                let result = table.resolve(symbol.name.as_str());
                match result {
                    Some(res) => assert_eq!(res, *symbol),
                    None => assert!(false, "Name could not be resolved."),
                };
            }
        }
    }

    #[test]
    fn test_resolve_free() {
        struct SymbolTableTest {
            table: SymbolTable,
            expected_symbols: Vec<Symbol>,
            expected_free_symbols: Vec<Symbol>,
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
                    Symbol::new("a", SymbolScope::GlobalScope, 0),
                    Symbol::new("b", SymbolScope::GlobalScope, 1),
                    Symbol::new("c", SymbolScope::LocalScope, 0), 
                    Symbol::new("d", SymbolScope::LocalScope, 1),
                ],
                expected_free_symbols: vec![],
            },
            SymbolTableTest {
                table: second_local,
                expected_symbols: vec![
                    Symbol::new("a", SymbolScope::GlobalScope, 0),
                    Symbol::new("b", SymbolScope::GlobalScope, 1),
                    Symbol::new("c", SymbolScope::FreeScope, 0), 
                    Symbol::new("d", SymbolScope::FreeScope, 1),
                    Symbol::new("e", SymbolScope::LocalScope, 0), 
                    Symbol::new("f", SymbolScope::LocalScope, 1),
                ],
                expected_free_symbols: vec![
                    Symbol::new("c", SymbolScope::LocalScope, 0), 
                    Symbol::new("d", SymbolScope::LocalScope, 1),
                ],
            },
        ];

        for mut symbol_table_test in symbol_table_tests {
            for symbol in symbol_table_test.expected_symbols {
                let result = symbol_table_test.table.resolve(symbol.name.as_str());
                match result {
                    Some(res) => assert_eq!(res, symbol),
                    None => assert!(false, "Name could not be resolved."),
                };
            }

            assert_eq!(symbol_table_test.table.free_symbols, symbol_table_test.expected_free_symbols);

            for (i, symbol) in symbol_table_test.expected_free_symbols.iter().enumerate() {
                let result = symbol_table_test.table.free_symbols.get(i);
                match result {
                    Some(res) => assert_eq!(res, symbol),
                    None => assert!(false, "Free symbol could not be resolved."),
                };
            }
        }
    }

    #[test]
    fn test_resolve_unresolvable_free() {
        let mut global = SymbolTable::new();
        global.define("a");

        let mut first_local = SymbolTable::new_enclosed(Rc::new(RefCell::new(global)));
        first_local.define("c");

        let mut second_local = SymbolTable::new_enclosed(Rc::new(RefCell::new(first_local.clone())));
        second_local.define("e");
        second_local.define("f");

        let expected = vec![
            Symbol::new("a", SymbolScope::GlobalScope, 0),
            Symbol::new("c", SymbolScope::FreeScope, 0),
            Symbol::new("e", SymbolScope::LocalScope, 0),
            Symbol::new("f", SymbolScope::LocalScope, 1),
        ];

        for symbol in expected {
            let result = second_local.resolve(symbol.name.as_str());
            match result {
                Some(res) => assert_eq!(res, symbol),
                None => assert!(false, "Name could not be resolved."),
            };
        }

        let expected_unresolvable = vec![ String::from("b"), String::from("d")];

        for name in expected_unresolvable {
            let result = second_local.resolve(name.as_str());
            match result {
                Some(_) => assert!(false, "Name should not have resolved."),
                None => {},
            };
        }
    }

    #[test]
    fn test_define_and_resolve_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a");

        let expected = Symbol::new("a", SymbolScope::FunctionScope, 0);

        let result = global.resolve(expected.name.as_str());
        match result {
            Some(res) => assert_eq!(res, expected),
            None => assert!(false, "Name could not be resolved."),
        };
    }

    #[test]
    fn test_shadowing_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a");
        global.define("a");

        let expected = Symbol::new("a", SymbolScope::GlobalScope, 0);

        let result = global.resolve(expected.name.as_str());
        match result {
            Some(res) => assert_eq!(res, expected),
            None => assert!(false, "Name could not be resolved."),
        };
    }
}