use super::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Environment {
    store: HashMap<String, Box<dyn Object>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self { store: HashMap::new(), outer: None }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        let mut env = Self::new();
        env.outer = Some(outer);

        env
    }

    pub fn get(&self, name: &String) -> Option<Box<dyn Object>> {
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

    pub fn set(&mut self, name: String, val: Box<dyn Object>) -> Box<dyn Object> {
        self.store.insert(name, val.clone());

        val
    }
}