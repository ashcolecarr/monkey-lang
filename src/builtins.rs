use super::object::*;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Clone)]
pub struct Builtins {
    pub builtins: HashMap<String, BuiltinFunction>,
}

impl Builtins {
    pub fn new() -> Self {
        let mut builtin_object = Self { builtins: HashMap::new() };

        builtin_object.builtins.insert(String::from("len"), Self::len);
        builtin_object.builtins.insert(String::from("puts"), Self::puts);
        builtin_object.builtins.insert(String::from("first"), Self::first);
        builtin_object.builtins.insert(String::from("last"), Self::last);
        builtin_object.builtins.insert(String::from("rest"), Self::rest);
        builtin_object.builtins.insert(String::from("push"), Self::push);

        builtin_object
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.builtins.get(&String::from(name)) {
            Some(b) => Some(Object::Builtin(Builtin::new(b.clone()))),
            None => None,
        }
    }

    // Needed since HashMap does not guarantee order of insertion.
    pub fn get_index(&self, name: &str) -> usize {
        match name {
            "len" => 0,
            "puts" => 1,
            "first" => 2,
            "last" => 3,
            "rest" => 4,
            "push" => 5,
            _ => panic!("Not a valid builtin name."),
        }
    }

    pub fn index(&self, value: usize) -> Option<Object> {
        let builtin = match value {
            0 => self.builtins.get(&String::from("len")),
            1 => self.builtins.get(&String::from("puts")),
            2 => self.builtins.get(&String::from("first")),
            3 => self.builtins.get(&String::from("last")),
            4 => self.builtins.get(&String::from("rest")),
            5 => self.builtins.get(&String::from("push")),
            _ => None,
        };

        match builtin {
            Some(b) => Some(Object::Builtin(Builtin::new(b.clone()))),
            None => None,
        }
    }

    fn len(arguments: &Vec<Object>) -> Object {
        if arguments.len() != 1 {
            return Object::Error(Error::new(
                format!("wrong number of arguments. got {}, want 1", arguments.len()).as_str()));
        }

        match &arguments[0] {
            Object::Array(arr) => Object::Integer(Integer::new(arr.elements.len() as i64)),
            Object::String(s) => Object::Integer(Integer::new(s.value.len() as i64)),
            _ => Object::Error(Error::new(
                format!("argument to \"len\" not supported, got {}", arguments[0].type_of()).as_str())),
        }
    }

    fn first(arguments: &Vec<Object>) -> Object {
        if arguments.len() != 1 {
            return Object::Error(Error::new(
                format!("wrong number of arguments. got {}, want 1", arguments.len()).as_str()));
        }

        match &arguments[0] {
            Object::Array(arr) => {
                if !arr.elements.is_empty() {
                    arr.elements[0].clone()
                } else {
                    Object::Null(Null::new())
                }
            },
            _ => Object::Error(Error::new(
                format!("argument to \"first\" must be ARRAY, got {}", arguments[0].type_of()).as_str())),
        }
    }

    fn last(arguments: &Vec<Object>) -> Object {
        if arguments.len() != 1 {
            return Object::Error(Error::new(
                format!("wrong number of arguments. got {}, want 1", arguments.len()).as_str()));
        }

        match &arguments[0] {
            Object::Array(arr) => {
                if !arr.elements.is_empty() {
                    arr.elements[arr.elements.len() - 1].clone()
                } else {
                    Object::Null(Null::new())
                }
            },
            _ => Object::Error(Error::new(
                format!("argument to \"last\" must be ARRAY, got {}", arguments[0].type_of()).as_str())),
        }
    }

    fn rest(arguments: &Vec<Object>) -> Object {
        if arguments.len() != 1 {
            return Object::Error(Error::new(
                format!("wrong number of arguments. got {}, want 1", arguments.len()).as_str()));
        }

        match &arguments[0] {
            Object::Array(arr) => {
                if !arr.elements.is_empty() {
                    let new_elements: Vec<Object> = arr.elements[1..].to_vec();
                    Object::Array(Array::new(new_elements))
                } else {
                    Object::Null(Null::new())
                }
            },
            _ => Object::Error(Error::new(
                format!("argument to \"rest\" must be ARRAY, got {}", arguments[0].type_of()).as_str())),
        }
    }

    fn push(arguments: &Vec<Object>) -> Object {
        if arguments.len() != 2 {
            return Object::Error(Error::new(
                format!("wrong number of arguments. got {}, want 2", arguments.len()).as_str()));
        }

        match &arguments[0] {
            Object::Array(arr) => {
                let mut new_array = arr.elements.clone();
                new_array.push(arguments[1].clone());

                Object::Array(Array::new(new_array))
            },
            _ => Object::Error(Error::new(
                format!("argument to \"push\" must be ARRAY, got {}", arguments[0].type_of()).as_str())),
        }
    }

    fn puts(arguments: &Vec<Object>) -> Object {
        for arg in arguments {
            println!("{}", arg);
        }

        Object::Null(Null::new())
    }
}

// Rust requires that Hash be implemented here, but it is not a valid key for hashing.
impl Hash for Builtins {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Environment is not valid for hashing.");
    }
}