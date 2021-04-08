use super::code::Instructions;
use super::object::Closure;

#[derive(Clone)]
pub struct Frame {
    pub closure: Closure,
    pub ip: i64,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(closure: Closure, base_pointer: usize) -> Self {
        Self { closure, ip: -1, base_pointer }
    }

    pub fn instructions(&self) -> Instructions {
        self.closure.fun.instructions.clone()
    }
}