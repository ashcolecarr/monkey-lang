use super::ast::*;
use super::environment::Environment;
use std::any::Any;
use std::cell::RefCell;
use std::collections::hash_map::{DefaultHasher, HashMap};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub const INTEGER_OBJ: &str = "INTEGER";
pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const NULL_OBJ: &str = "NULL";
pub const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";
pub const ERROR_OBJ: &str = "ERROR";
pub const FUNCTION_OBJ: &str = "FUNCTION";
pub const STRING_OBJ: &str = "STRING";
pub const BUILTIN_OBJ: &str = "BUILTIN";
pub const ARRAY_OBJ: &str = "ARRAY";
pub const HASH_OBJ: &str = "HASH";

#[derive(Clone)]
pub enum BuiltinFunction {
    Len,
    First,
    Last,
    Rest,
    Push,
    Puts,
}

type ObjectType = String;

pub trait Object: ObjectClone {
    fn type_of(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub trait ObjectClone {
    fn clone_box(&self) -> Box<dyn Object>;
}

pub trait ObjectHashKey {
    fn hash_key(&self) -> HashKey;
}

impl <T: 'static + Object + Clone> ObjectClone for T {
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Object> {
    fn clone(&self) -> Box<dyn Object> {
        self.clone_box()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HashKey {
    pub obj_type: ObjectType,
    pub value: u64,
}

impl HashKey {
    pub fn new(obj_type: ObjectType, value: u64) -> Self {
        Self { obj_type, value }
    }
}

#[derive(Clone)]
pub struct HashPair {
    pub key: Box<dyn Object>,
    pub value: Box<dyn Object>,
}

impl HashPair {
    pub fn new(key: Box<dyn Object>, value: Box<dyn Object>) -> Self {
        Self { key, value }
    }
}

#[derive(Clone)]
pub struct Integer {
    pub value: i64,
}

impl Integer {
    pub fn new(value: i64) -> Self {
        Self { value }
    }
}

impl Object for Integer {
    fn type_of(&self) -> ObjectType {
        INTEGER_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ObjectHashKey for Integer {
    fn hash_key(&self) -> HashKey {
        HashKey::new(self.type_of(), self.value as u64)
    }
}

#[derive(Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Boolean {
    pub fn new(value: bool) -> Self {
        Self { value }
    }
}

impl Object for Boolean {
    fn type_of(&self) -> ObjectType {
        BOOLEAN_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ObjectHashKey for Boolean {
    fn hash_key(&self) -> HashKey {
        let value = if self.value { 1 } else { 0 };
        HashKey::new(self.type_of(), value)
    }
}

#[derive(Clone)]
pub struct Null { }

impl Null {
    pub fn new() -> Self {
        Self { }
    }
}

impl Object for Null {
    fn type_of(&self) -> ObjectType {
        NULL_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        String::from("null")
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct ReturnValue {
    pub value: Box<dyn Object>,
}

impl ReturnValue {
    pub fn new(value: Box<dyn Object>) -> Self {
        Self { value }
    }
}

impl Object for ReturnValue {
    fn type_of(&self) -> ObjectType {
        RETURN_VALUE_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        format!("{}", self.value.inspect())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct Error {
    pub message: String,
}

impl Error {
    pub fn new(message: String) -> Self {
        Self { message }
    }
}

impl Object for Error {
    fn type_of(&self) -> ObjectType {
        ERROR_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        format!("Error: {}", self.message)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct Function {
    pub parameters: Vec<Box<dyn Expression>>,
    pub body: Box<dyn Statement>,
    pub env: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(parameters: Vec<Box<dyn Expression>>, body: Box<dyn Statement>, env: Rc<RefCell<Environment>>) -> Self {
        Self { parameters, body, env }
    }
}

impl Object for Function {
    fn type_of(&self) -> ObjectType {
        FUNCTION_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        format!("fn({}) {{\n{}\n}}", params.join(", "), self.body.to_string())
    }

    fn as_any(&self) -> &dyn Any where dyn Object: 'static {
        self
    }
}

#[derive(Clone)]
pub struct StringObject {
    pub value: String,
}

impl StringObject {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}

impl Object for StringObject {
    fn type_of(&self) -> ObjectType {
        STRING_OBJ.to_string() 
    }

    fn inspect(&self) -> String {
        format!("{}", self.value.clone())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ObjectHashKey for StringObject {
    fn hash_key(&self) -> HashKey {
        let mut s = DefaultHasher::new();
        self.value.hash(&mut s);

        HashKey::new(self.type_of(), s.finish())
    }
}

#[derive(Clone)]
pub struct Builtin {
    pub builtin_function: BuiltinFunction,
}

impl Builtin {
    pub fn new(function_name: String) -> Option<Self> {
        match function_name.as_str() {
            "len" => Some(Self { builtin_function: BuiltinFunction::Len }),
            "first" => Some(Self { builtin_function: BuiltinFunction::First }),
            "last" => Some(Self { builtin_function: BuiltinFunction::Last }),
            "rest" => Some(Self { builtin_function: BuiltinFunction::Rest }),
            "push" => Some(Self { builtin_function: BuiltinFunction::Push }),
            "puts" => Some(Self { builtin_function: BuiltinFunction::Puts }),
            _ => None,
        }
    }

    pub fn call(&self, args: &Vec<Box<dyn Object>>) -> Box<dyn Object> {
        match self.builtin_function {
            BuiltinFunction::Len => {
                if args.len() != 1 {
                    return Box::new(Error::new(format!("wrong number of arguments. got {}, want 1", args.len())));
                }

                match (args[0].as_any().downcast_ref::<Array>(), args[0].as_any().downcast_ref::<StringObject>()) {
                    (Some(arr), _) => Box::new(Integer::new(arr.elements.len() as i64)),
                    (_, Some(st)) => Box::new(Integer::new(st.value.len() as i64)),
                    _ => Box::new(Error::new(format!("argument to 'len' not supported, got {}", args[0].type_of()))),
                }
            },
            BuiltinFunction::First => {
                if args.len() != 1 {
                    return Box::new(Error::new(format!("wrong number of arguments. got {}, want 1", args.len())));
                }

                match args[0].as_any().downcast_ref::<Array>() {
                    Some(arr) => {
                        if !arr.elements.is_empty() {
                            arr.elements[0].clone()
                        } else {
                            Box::new(Null::new())
                        }
                    },
                    _ => Box::new(Error::new(format!("argument to 'first' must be ARRAY, got {}", args[0].type_of()))),
                }
            },
            BuiltinFunction::Last => {
                if args.len() != 1 {
                    return Box::new(Error::new(format!("wrong number of arguments. got {}, want 1", args.len())));
                }

                match args[0].as_any().downcast_ref::<Array>() {
                    Some(arr) => {
                        if !arr.elements.is_empty() {
                            arr.elements[arr.elements.len() - 1].clone()
                        } else {
                            Box::new(Null::new())
                        }
                    },
                    _ => Box::new(Error::new(format!("argument to 'last' must be ARRAY, got {}", args[0].type_of()))),
                }
            },
            BuiltinFunction::Rest => {
                if args.len() != 1 {
                    return Box::new(Error::new(format!("wrong number of arguments. got {}, want 1", args.len())));
                }

                match args[0].as_any().downcast_ref::<Array>() {
                    Some(arr) => {
                        if !arr.elements.is_empty() {
                            let new_elements: Vec<Box<dyn Object>> = arr.elements[1..].to_vec();
                            Box::new(Array::new(new_elements))
                        } else {
                            Box::new(Null::new())
                        }
                    },
                    _ => Box::new(Error::new(format!("argument to 'rest' must be ARRAY, got {}", args[0].type_of()))),
                }
            },
            BuiltinFunction::Push => {
                if args.len() != 2 {
                    return Box::new(Error::new(format!("wrong number of arguments. got {}, want 2", args.len())));
                }

                match args[0].as_any().downcast_ref::<Array>() {
                    Some(arr) => {
                        let mut new_array = arr.elements.clone();
                        new_array.push(args[1].clone());

                        Box::new(Array::new(new_array))
                    },
                    _ => Box::new(Error::new(format!("argument to 'push' must be ARRAY, got {}", args[0].type_of()))),
                }
            },
            BuiltinFunction::Puts => {
                for arg in args {
                    println!("{}", arg.inspect());
                }

                Box::new(Null::new())
            },
        }
    }
}

impl Object for Builtin {
    fn type_of(&self) -> ObjectType {
        BUILTIN_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        String::from("builtin function")
    }

    fn as_any(&self) -> &dyn Any where dyn Object: 'static {
        self
    }
}

#[derive(Clone)]
pub struct Array {
    pub elements: Vec<Box<dyn Object>>,
}

impl Array {
    pub fn new(elements: Vec<Box<dyn Object>>) -> Self {
        Self { elements }
    }
}

impl Object for Array {
    fn type_of(&self) -> ObjectType {
        ARRAY_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        let elements: Vec<String> = self.elements.iter().map(|e| e.inspect()).collect();
        format!("[{}]", elements.join(", "))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct HashObject {
    pub pairs: HashMap<HashKey, HashPair>,
}

impl HashObject {
    pub fn new(pairs: HashMap<HashKey, HashPair>) -> Self {
        Self { pairs }
    }
}

impl Object for HashObject {
    fn type_of(&self) -> ObjectType {
        HASH_OBJ.to_string() 
    }

    fn inspect(&self) -> String {
        let mut pairs = vec![];
        for (_, pair) in &self.pairs {
            pairs.push(format!("{}: {}", pair.key.inspect(), pair.value.inspect()));
        }
        format!("{{{}}}", pairs.join(", "))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = StringObject::new(String::from("Hello World"));
        let hello2 = StringObject::new(String::from("Hello World"));
        let diff1 = StringObject::new(String::from("My name is johnny"));
        let diff2 = StringObject::new(String::from("My name is johnny"));

        assert_eq!(hello1.hash_key(), hello2.hash_key());
        assert_eq!(diff1.hash_key(), diff2.hash_key());
        assert_ne!(hello1.hash_key(), diff1.hash_key());
    }

    #[test]
    fn test_boolean_hash_key() {
        let true1 = Boolean::new(true);
        let true2 = Boolean::new(true);
        let false1 = Boolean::new(false);
        let false2 = Boolean::new(false);

        assert_eq!(true1.hash_key(), true2.hash_key());
        assert_eq!(false1.hash_key(), false2.hash_key());
        assert_ne!(true1.hash_key(), false1.hash_key());
    }

    #[test]
    fn test_integer_hash_key() {
        let one1 = Integer::new(1);
        let one2 = Integer::new(1);
        let two1 = Integer::new(2);
        let two2 = Integer::new(2);

        assert_eq!(one1.hash_key(), one2.hash_key());
        assert_eq!(two1.hash_key(), two2.hash_key());
        assert_ne!(one1.hash_key(), two1.hash_key());
    }
}