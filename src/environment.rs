use super::builtins::Builtins;
use super::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    builtins: Option<Builtins>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(is_outermost: bool) -> Self {
        Self { 
            store: HashMap::new(), 
            builtins: if is_outermost { Some(Builtins::new()) } else { None },
            outer: None 
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        let mut environment = Self::new(false);
        environment.outer = Some(outer);

        environment
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        match self.store.get(name) {
            Some(s) => Some(s.clone()),
            None => {
                match &self.outer {
                    Some(out) => {
                        match out.borrow().get(&name) {
                            Some(o) => Some(o.clone()),
                            None => None,
                        }
                    },
                    None => None,
                }
            },
        }
    }

    pub fn set(&mut self, name: String, val: Object) -> Object {
        self.store.insert(name, val.clone());

        val
    }

    pub fn get_builtin(&self, name: &str) -> Option<Object> {
        match &self.builtins {
            Some(b) => b.get(name),
            None => {
                match &self.outer {
                    Some(out) => out.borrow().get_builtin(name),
                    None => None,
                }
            },
        }
    }
}