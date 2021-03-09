use super::ast::{BlockStatement, Identifier};
use super::environment::Environment;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

const INTEGER_OBJ: &str = "INTEGER";
const BOOLEAN_OBJ: &str = "BOOLEAN";
const NULL_OBJ: &str = "NULL";
const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";
const ERROR_OBJ: &str = "ERROR";
const FUNCTION_OBJ: &str = "FUNCTION"; 
const STRING_OBJ: &str = "STRING"; 
const BUILTIN_OBJ: &str = "BUILTIN"; 
const ARRAY_OBJ: &str = "ARRAY"; 
const HASH_OBJ: &str = "HASH"; 

pub type BuiltinFunction = fn(arguments: &Vec<Object>) -> Object;

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Object {
    NonPrint, // Non printed debug object to satisfy return requirement.
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    ReturnValue(ReturnValue),
    Error(Error),
    Function(Function),
    String(StringObject),
    Builtin(Builtin),
    Array(Array),
    Hash(HashObject)
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Self::NonPrint => String::new(),
            Self::Integer(i) => format!("{}", i),
            Self::Boolean(b) => format!("{}", b),
            Self::Null(n) => format!("{}", n),
            Self::ReturnValue(rv) => format!("{}", rv),
            Self::Error(e) => format!("{}", e),
            Self::Function(f) => format!("{}", f),
            Self::String(s) => format!("{}", s),
            Self::Builtin(b) => format!("{}", b),
            Self::Array(a) => format!("{}", a),
            Self::Hash(h) => format!("{}", h),
        })
    }
}

impl Object {
    pub fn type_of(&self) -> &str {
        match self {
            Self::NonPrint => "",
            Self::Integer(i) => i.type_of(),
            Self::Boolean(b) => b.type_of(),
            Self::Null(n) => n.type_of(),
            Self::ReturnValue(rv) => rv.type_of(),
            Self::Error(e) => e.type_of(),
            Self::Function(f) => f.type_of(),
            Self::String(so) => so.type_of(),
            Self::Builtin(b) => b.type_of(),
            Self::Array(a) => a.type_of(),
            Self::Hash(h) => h.type_of(),
        }
    }

    pub fn is_hashable(&self) -> bool {
        match self {
            Self::Integer(_) | Self::Boolean(_) | Self::String(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Integer {
    pub value: i64,
}

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

impl Integer {
    pub fn new(value: i64) -> Self {
        Self { value }
    }

    pub fn type_of(&self) -> &str {
        INTEGER_OBJ
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Boolean {
    pub value: bool,
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

impl Boolean {
    pub fn new(value: bool) -> Self {
        Self { value }
    }

    pub fn type_of(&self) -> &str {
        BOOLEAN_OBJ
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Null { }

impl Display for Null {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "null")
    }
}

impl Null {
    pub fn new() -> Self {
        Self { }
    }

    pub fn type_of(&self) -> &str {
        NULL_OBJ
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

impl Display for ReturnValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

impl ReturnValue {
    pub fn new(value: Box<Object>) -> Self {
        Self { value }
    }

    pub fn type_of(&self) -> &str {
        RETURN_VALUE_OBJ
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Error {
    pub message: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Error: {}", self.message)
    }
}

impl Error {
    pub fn new(message: &str) -> Self {
        Self { message: String::from(message) }
    }

    pub fn type_of(&self) -> &str {
        ERROR_OBJ
    }
}

#[derive(Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub environment: Rc<RefCell<Environment>>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let params: Vec<String> = self.parameters.iter().map(|p| format!("{}", p)).collect();

        write!(f, "fn({}) {{\n{}\n}}", params.join(", "), self.body)
    }
}

impl PartialEq for Function {
    fn eq(&self, _other: &Function) -> bool {
        panic!("PartialEq is not supported for Function.");
    }
}
impl Eq for Function {}

impl Hash for Function {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Hash is not supported for Function.");
    }
}

impl Function {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement, environment: Rc<RefCell<Environment>>) -> Self {
        Self { parameters, body, environment }
    }

    pub fn type_of(&self) -> &str {
        FUNCTION_OBJ
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct StringObject {
    pub value: String,
}

impl Display for StringObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

impl StringObject {
    pub fn new(value: &str) -> Self {
        Self { value: String::from(value) }
    }

    pub fn type_of(&self) -> &str {
        STRING_OBJ
    }
}

#[derive(Clone)]
pub struct Builtin {
    pub builtin_function: BuiltinFunction,
}

impl Display for Builtin {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "builtin function")
    }
}

impl PartialEq for Builtin {
    fn eq(&self, _other: &Builtin) -> bool {
        panic!("PartialEq is not supported for Builtin.");
    }
}

impl Eq for Builtin {}

impl Hash for Builtin {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Hash is not supported for Builtin.");
    }
}

impl Builtin {
    pub fn new(builtin_function: BuiltinFunction) -> Self {
        Self { builtin_function }
    }

    pub fn type_of(&self) -> &str {
        BUILTIN_OBJ
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Array {
    pub elements: Vec<Object>,
}

impl Display for Array {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let elems: Vec<String> = self.elements.iter().map(|e| format!("{}", e)).collect();

        write!(f, "[{}]", elems.join(", "))
    }
}

impl Array {
    pub fn new(elements: Vec<Object>) -> Self {
        Self { elements }
    }

    pub fn type_of(&self) -> &str {
        ARRAY_OBJ
    }
}

#[derive(Clone)]
pub struct HashObject {
    pub pairs: HashMap<Object, Object>,
}

impl Display for HashObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let pairs: Vec<String> = self.pairs.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();

        write!(f, "{{{}}}", pairs.join(", "))
    }
}

impl PartialEq for HashObject {
    fn eq(&self, _other: &HashObject) -> bool {
        panic!("PartialEq is not supported for HashObject.");
    }
}

impl Eq for HashObject {}

impl Hash for HashObject {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Hash is not supported for HashObject.");
    }
}

impl HashObject {
    pub fn new(pairs: HashMap<Object, Object>) -> Self {
        Self { pairs }
    }

    pub fn type_of(&self) -> &str {
        HASH_OBJ
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::hash_map::DefaultHasher;

    #[test]
    fn test_string_hash_key() {
        let hello1 = Object::String(StringObject::new("Hello World"));
        let hello2 = Object::String(StringObject::new("Hello World"));
        let diff1 = Object::String(StringObject::new("my name is johnny"));
        let diff2 = Object::String(StringObject::new("my name is johnny"));

        let mut hasher1 = DefaultHasher::new();
        hello1.hash(&mut hasher1);
        let mut hasher2 = DefaultHasher::new();
        hello2.hash(&mut hasher2);
        assert_eq!(hasher1.finish(), hasher2.finish());

        let mut hasher1 = DefaultHasher::new();
        diff1.hash(&mut hasher1);
        let mut hasher2 = DefaultHasher::new();
        diff2.hash(&mut hasher2);
        assert_eq!(hasher1.finish(), hasher2.finish());

        let mut hasher1 = DefaultHasher::new();
        hello1.hash(&mut hasher1);
        let mut hasher2 = DefaultHasher::new();
        diff1.hash(&mut hasher2);
        assert_ne!(hasher1.finish(), hasher2.finish());
    }

    #[test]
    fn test_boolean_hash_key() {
        let hello1 = Object::Boolean(Boolean::new(true));
        let hello2 = Object::Boolean(Boolean::new(true));
        let diff1 = Object::Boolean(Boolean::new(false));
        let diff2 = Object::Boolean(Boolean::new(false));

        let mut hasher1 = DefaultHasher::new();
        hello1.hash(&mut hasher1);
        let mut hasher2 = DefaultHasher::new();
        hello2.hash(&mut hasher2);
        assert_eq!(hasher1.finish(), hasher2.finish());

        let mut hasher1 = DefaultHasher::new();
        diff1.hash(&mut hasher1);
        let mut hasher2 = DefaultHasher::new();
        diff2.hash(&mut hasher2);
        assert_eq!(hasher1.finish(), hasher2.finish());

        let mut hasher1 = DefaultHasher::new();
        hello1.hash(&mut hasher1);
        let mut hasher2 = DefaultHasher::new();
        diff1.hash(&mut hasher2);
        assert_ne!(hasher1.finish(), hasher2.finish());
    }

    #[test]
    fn test_integer_hash_key() {
        let hello1 = Object::Integer(Integer::new(1));
        let hello2 = Object::Integer(Integer::new(1));
        let diff1 = Object::Integer(Integer::new(2));
        let diff2 = Object::Integer(Integer::new(2));

        let mut hasher1 = DefaultHasher::new();
        hello1.hash(&mut hasher1);
        let mut hasher2 = DefaultHasher::new();
        hello2.hash(&mut hasher2);
        assert_eq!(hasher1.finish(), hasher2.finish());

        let mut hasher1 = DefaultHasher::new();
        diff1.hash(&mut hasher1);
        let mut hasher2 = DefaultHasher::new();
        diff2.hash(&mut hasher2);
        assert_eq!(hasher1.finish(), hasher2.finish());

        let mut hasher1 = DefaultHasher::new();
        hello1.hash(&mut hasher1);
        let mut hasher2 = DefaultHasher::new();
        diff1.hash(&mut hasher2);
        assert_ne!(hasher1.finish(), hasher2.finish());
    }
}