use super::code::Instructions;
use super::object::CompiledFunction;

#[derive(Clone)]
pub struct Frame {
    fun: CompiledFunction,
    pub ip: i64,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(fun: CompiledFunction, base_pointer: usize) -> Self {
        Self { fun, ip: -1, base_pointer }
    }

    pub fn instructions(&self) -> Instructions {
        self.fun.instructions.clone()
    }
}