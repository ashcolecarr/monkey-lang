use super::object::Object;
use std::collections::HashMap;

pub struct Environment {
    store: HashMap<String, Box<dyn Object>>,
}

impl Environment {
    pub fn new() -> Self {
        Self { store: HashMap::new() }
    }

    pub fn get(&self, name: String) -> Option<Box<dyn Object>> {
        match self.store.get(&name) {
            Some(s) => Some(s.clone()),
            None => None,
        }
    }

    pub fn set(&mut self, name: String, val: Box<dyn Object>) -> Box<dyn Object> {
        self.store.insert(name, val.clone());

        val
    }
}