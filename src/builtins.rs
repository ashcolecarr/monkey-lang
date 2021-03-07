use super::object::*;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Builtins {
    builtins: HashMap<String, BuiltinFunction>,
}

impl Builtins {
    pub fn new() -> Self {
        let mut builtin_object = Self { builtins: HashMap::new() };

        builtin_object.builtins.insert(String::from("len"), Self::len);
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

    fn len(arguments: &Vec<Object>) -> Object {
        if arguments.len() != 1 {
            return Object::Error(Error::new(
                format!("wrong number of arguments. got {}, want 1", arguments.len()).as_str()));
        }

        match &arguments[0] {
            Object::Array(arr) => Object::Integer(Integer::new(arr.elements.len() as i64)),
            Object::StringObject(so) => Object::Integer(Integer::new(so.value.len() as i64)),
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
}