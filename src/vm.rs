use super::builtins::Builtins;
use super::code::*;
use super::compiler::Bytecode;
use super::frame::Frame;
use super::GLOBALS_SIZE;
use super::object::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

const STACK_SIZE: usize = 2048;
const MAX_FRAMES: usize = 1024;

pub struct VM {
    constants: Vec<Object>,
    stack: Vec<Object>,
    sp: usize, // Always points to the next value; top of stack is sp - 1
    globals: Rc<RefCell<Vec<Object>>>,
    frames: Vec<Frame>,
    frames_index: usize,
    builtins: Builtins,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        let main_fun = CompiledFunction::new(bytecode.instructions, 0, 0);
        let main_closure = Closure::new(main_fun, vec![]);
        let main_frame = Frame::new(main_closure, 0);
        let mut frames = vec![Frame::new(Closure::new(CompiledFunction::new(vec![], 0, 0), vec![]), 0); MAX_FRAMES];
        frames[0] = main_frame;

        Self {
            constants: bytecode.constants,
            stack: vec![Object::NonPrint; STACK_SIZE],
            sp: 0,
            globals: Rc::new(RefCell::new(vec![Object::NonPrint; GLOBALS_SIZE])),
            frames: frames,
            frames_index: 1,
            builtins: Builtins::new(),
        }
    }

    pub fn new_with_globals_store(bytecode: Bytecode, store: Rc<RefCell<Vec<Object>>>) -> Self {
        let mut vm = Self::new(bytecode);
        vm.globals = store;

        vm
    }
    
    pub fn run(&mut self) -> Result<(), String> {
        while self.current_frame().ip < (self.current_frame().instructions().len() - 1) as i64 {
            self.current_frame_mut().ip += 1;

            let ip = self.current_frame().ip as usize;
            let instructions = self.current_frame().instructions();
            let op = OpCode::from(instructions[ip]);

            match op {
                OpCode::OpConstant => {
                    let mut bytes = [0; 2];
                    bytes[0] = self.current_frame().instructions()[ip + 1];
                    bytes[1] = self.current_frame().instructions()[ip + 2];

                    let const_index = read_u16(&bytes);
                    self.current_frame_mut().ip += 2;

                    if let Err(e) = self.push(self.constants[const_index as usize].clone()) {
                        return Err(e);
                    }
                },
                OpCode::OpAdd | OpCode::OpSub | OpCode::OpMul | OpCode::OpDiv => {
                    if let Err(e) = self.execute_binary_operation(&op) {
                        return Err(e);
                    }
                },
                OpCode::OpPop => { self.pop(); },
                OpCode::OpTrue => {
                    if let Err(e) = self.push(Object::Boolean(Boolean::new(true))) {
                        return Err(e);
                    }
                },
                OpCode::OpFalse => {
                    if let Err(e) = self.push(Object::Boolean(Boolean::new(false))) {
                        return Err(e);
                    }
                },
                OpCode::OpEqual | OpCode::OpNotEqual | OpCode::OpGreaterThan => {
                    if let Err(e) = self.execute_comparison(&op) {
                        return Err(e);
                    }
                },
                OpCode::OpBang => {
                    if let Err(e) = self.execute_bang_operator() {
                        return Err(e);
                    }
                },
                OpCode::OpMinus => {
                    if let Err(e) = self.execute_minus_operator() {
                        return Err(e);
                    }
                },
                OpCode::OpJump => {
                    let mut bytes = [0; 2];
                    bytes[0] = self.current_frame().instructions()[ip + 1];
                    bytes[1] = self.current_frame().instructions()[ip + 2];

                    let position = read_u16(&bytes);
                    self.current_frame_mut().ip = position as i64 - 1;
                },
                OpCode::OpJumpNotTruthy => {
                    let mut bytes = [0; 2];
                    bytes[0] = self.current_frame().instructions()[ip + 1];
                    bytes[1] = self.current_frame().instructions()[ip + 2];

                    let position = read_u16(&bytes);
                    self.current_frame_mut().ip += 2;

                    let condition = self.pop();
                    if !self.is_truthy(condition) {
                        self.current_frame_mut().ip = position as i64 - 1;
                    }
                },
                OpCode::OpNull => {
                    if let Err(e) = self.push(Object::Null(Null::new())) {
                        return Err(e);
                    }
                },
                OpCode::OpSetGlobal => {
                    let mut bytes = [0; 2];
                    bytes[0] = self.current_frame().instructions()[ip + 1];
                    bytes[1] = self.current_frame().instructions()[ip + 2];

                    let global_index = read_u16(&bytes);
                    self.current_frame_mut().ip += 2;

                    self.globals.borrow_mut()[global_index as usize] = self.pop();
                },
                OpCode::OpGetGlobal => {
                    let mut bytes = [0; 2];
                    bytes[0] = self.current_frame().instructions()[ip + 1];
                    bytes[1] = self.current_frame().instructions()[ip + 2];

                    let global_index = read_u16(&bytes);
                    self.current_frame_mut().ip += 2;

                    let value = self.globals.borrow()[global_index as usize].clone();
                    if let Err(e) = self.push(value) {
                        return Err(e);
                    }
                },
                OpCode::OpArray => {
                    let mut bytes = [0; 2];
                    bytes[0] = self.current_frame().instructions()[ip + 1];
                    bytes[1] = self.current_frame().instructions()[ip + 2];

                    let num_elements = read_u16(&bytes);
                    self.current_frame_mut().ip += 2;

                    let array = self.build_array(self.sp - num_elements as usize, self.sp);
                    self.sp -= num_elements as usize;

                    if let Err(e) = self.push(array) {
                        return Err(e);
                    }
                },
                OpCode::OpHash => {
                    let mut bytes = [0; 2];
                    bytes[0] = self.current_frame().instructions()[ip + 1];
                    bytes[1] = self.current_frame().instructions()[ip + 2];

                    let num_elements = read_u16(&bytes);
                    self.current_frame_mut().ip += 2;

                    match self.build_hash(self.sp - num_elements as usize, self.sp) {
                        Ok(hash) => {
                            self.sp -= num_elements as usize;

                            if let Err(e) = self.push(hash) {
                                return Err(e);
                            }
                        },
                        Err(e) => return Err(e),
                    }
                },
                OpCode::OpIndex => {
                    let index = self.pop();
                    let left = self.pop();

                    if let Err(e) = self.execute_index_expression(&left, &index) {
                        return Err(e);
                    }
                },
                OpCode::OpCall => {
                    let mut byte = [0; 1];
                    byte[0] = self.current_frame().instructions()[ip + 1];
                    let number_arguments = read_u8(&byte);
                    self.current_frame_mut().ip += 1;

                    if let Err(e) = self.execute_call(number_arguments as usize) {
                        return Err(e);
                    }
                },
                OpCode::OpReturnValue => {
                    let return_value = self.pop();

                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    if let Err(e) = self.push(return_value) {
                        return Err(e);
                    }
                },
                OpCode::OpReturn => {
                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    if let Err(e) = self.push(Object::Null(Null::new())) {
                        return Err(e);
                    }
                },
                OpCode::OpSetLocal => {
                    let mut byte = [0; 1];
                    byte[0] = self.current_frame().instructions()[ip + 1];
                    let local_index = read_u8(&byte);
                    self.current_frame_mut().ip += 1;

                    let frame = self.current_frame().clone();

                    self.stack[frame.base_pointer + local_index as usize] = self.pop();
                },
                OpCode::OpGetLocal => {
                    let mut byte = [0; 1];
                    byte[0] = self.current_frame().instructions()[ip + 1];
                    let local_index = read_u8(&byte);
                    self.current_frame_mut().ip += 1;

                    let frame = self.current_frame().clone();

                    if let Err(e) = self.push(self.stack[frame.base_pointer + local_index as usize].clone()) {
                        return Err(e);
                    }
                },
                OpCode::OpGetBuiltin => {
                    let mut byte = [0; 1];
                    byte[0] = self.current_frame().instructions()[ip + 1];
                    let builtin_index = read_u8(&byte);
                    self.current_frame_mut().ip += 1;

                    let definition = self.builtins.index(builtin_index as usize);

                    match definition {
                        Some(def) => {
                            if let Err(e) = self.push(def) {
                                return Err(e);
                            }
                        },
                        None => return Err(format!("builtin for index {} could not be found.", builtin_index)),
                    };
                },
                OpCode::OpClosure => {
                    let mut bytes = [0; 2];
                    bytes[0] = self.current_frame().instructions()[ip + 1];
                    bytes[1] = self.current_frame().instructions()[ip + 2];
                    let const_index = read_u16(&bytes);

                    let mut byte = [0; 1];
                    byte[0] = self.current_frame().instructions()[ip + 3];
                    let num_free = read_u8(&byte);

                    self.current_frame_mut().ip += 3;

                    if let Err(e) = self.push_closure(const_index as usize, num_free as usize) {
                        return Err(e);
                    }
                },
                OpCode::OpGetFree => {
                    let mut byte = [0; 1];
                    byte[0] = self.current_frame().instructions()[ip + 1];
                    let free_index = read_u8(&byte);
                    self.current_frame_mut().ip += 1;

                    let current_closure = self.current_frame().closure.clone();
                    if let Err(e) = self.push(current_closure.free[free_index as usize].clone()) {
                        return Err(e);
                    }
                },
                OpCode::OpCurrentClosure => {
                    let current_closure = self.current_frame().closure.clone();
                    if let Err(e) = self.push(Object::Closure(current_closure)) {
                        return Err(e);
                    }
                },
            };
        }

        Ok(())
    }

    fn push(&mut self, object: Object) -> Result<(), String> {
        if self.sp >= STACK_SIZE {
            return Err(String::from("stack overflow"));
        }

        self.stack[self.sp] = object;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Object {
        let object = self.stack[self.sp - 1].clone();
        self.sp -= 1;

        object
    }

    fn execute_binary_operation(&mut self, op: &OpCode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();

        match (&left, &right) {
            (Object::Integer(l), Object::Integer(r)) => self.execute_binary_integer_operation(op, &l, &r),
            (Object::String(l), Object::String(r)) => self.execute_binary_string_operation(op, &l, &r),
            _ => Err(format!("unsupported types for binary operation: {} {}", left.type_of(), right.type_of())),
        }
    }

    fn execute_binary_integer_operation(&mut self, op: &OpCode, left: &Integer, right: &Integer) -> Result<(), String> {
        let left_value = left.value; 
        let right_value = right.value; 

        let result = match op {
            OpCode::OpAdd => left_value + right_value,
            OpCode::OpSub => left_value - right_value,
            OpCode::OpMul => left_value * right_value,
            OpCode::OpDiv => left_value / right_value,
            _ => return Err(format!("unknown integer operator: {:?}", op)),
        };

        self.push(Object::Integer(Integer::new(result)))
    }

    fn execute_binary_string_operation(&mut self, op: &OpCode, left: &StringObject, right: &StringObject) -> Result<(), String> {
        let left_value = left.value.clone(); 
        let right_value = right.value.clone(); 

        let result = match op {
            OpCode::OpAdd => left_value + right_value.as_str(),
            _ => return Err(format!("unknown integer operator: {:?}", op)),
        };

        self.push(Object::String(StringObject::new(result.as_str())))
    }

    fn execute_comparison(&mut self, op: &OpCode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();

        if let (Object::Integer(l), Object::Integer(r)) = (&left, &right) {
            return self.execute_integer_comparison(op, &l, &r);
        } else if let (Object::Boolean(l), Object::Boolean(r)) = (&left, &right) {
            return match op {
                OpCode::OpEqual => self.push(self.native_bool_to_boolean_object(r.value == l.value)),
                OpCode::OpNotEqual => self.push(self.native_bool_to_boolean_object(r.value != l.value)),
                _ => Err(format!("unknown operator: {:?} ({} {})", op, left.type_of(), right.type_of())),
            }
        }

        Err(format!("object types are not supported: {} {}", left.type_of(), right.type_of()))
    }

    fn execute_integer_comparison(&mut self, op: &OpCode, left: &Integer, right: &Integer) -> Result<(), String> {
        let left_value = left.value; 
        let right_value = right.value; 

        match op {
            OpCode::OpEqual => self.push(self.native_bool_to_boolean_object(right_value == left_value)),
            OpCode::OpNotEqual => self.push(self.native_bool_to_boolean_object(right_value != left_value)),
            OpCode::OpGreaterThan => self.push(self.native_bool_to_boolean_object(left_value > right_value)),
            _ => Err(format!("unknown operator: {:?}", op)),
        }
    }

    fn execute_bang_operator(&mut self) -> Result<(), String> {
        let operand = self.pop();

        match operand {
            Object::Boolean(b) => {
                if b.value {
                    self.push(Object::Boolean(Boolean::new(false)))
                } else {
                    self.push(Object::Boolean(Boolean::new(true)))
                }
            },
            Object::Null(_) => self.push(Object::Boolean(Boolean::new(true))),
            _ => self.push(Object::Boolean(Boolean::new(false))),
        }
    }

    fn execute_minus_operator(&mut self) -> Result<(), String> {
        let operand = self.pop();

        match operand {
            Object::Integer(i) => {
                self.push(Object::Integer(Integer::new(-i.value)))
            },
            _ => Err(format!("unsupported type for negation: {}", operand.type_of())),
        }
    }

    fn build_array(&self, start_index: usize, end_index: usize) -> Object {
        let mut elements = vec![Object::NonPrint; end_index - start_index];

        for i in start_index..end_index {
            elements[i - start_index] = self.stack[i].clone();
        }

        Object::Array(Array::new(elements))
    }

    fn build_hash(&self, start_index: usize, end_index: usize) -> Result<Object, String> {
        let mut hashed_pairs: HashMap<Object, Object> = HashMap::new();

        for i in (start_index..end_index).step_by(2) {
            let key = self.stack[i].clone();
            let value = self.stack[i + 1].clone();

            if !key.is_hashable() {
                return Err(format!("unusable as hash key: {}", key.type_of()));
            }

            hashed_pairs.insert(key, value);
        }

        Ok(Object::Hash(HashObject::new(hashed_pairs)))
    }

    fn execute_index_expression(&mut self, left: &Object, index: &Object) -> Result<(), String> {
        if let (Object::Array(l), Object::Integer(i)) = (left, index) {
            return self.execute_array_index(&l, &i);
        } else if let Object::Hash(l) = left {
            return self.execute_hash_index(&l, &index)
        }

        Err(format!("index operator not supported: {}", left.type_of()))
    }

    fn execute_array_index(&mut self, array: &Array, index: &Integer) -> Result<(), String> {
        let i = index.value;
        let max = array.elements.len() as i64 - 1;

        if i < 0 || i > max {
            self.push(Object::Null(Null::new()))
        } else {
            self.push(array.elements[i as usize].clone())
        }
    }

    fn execute_hash_index(&mut self, hash: &HashObject, index: &Object) -> Result<(), String> {
        let key = index.clone();
        if !key.is_hashable() {
            return Err(format!("unusable as hash key: {}", key.type_of()));
        }

        match hash.pairs.get(&key) {
            Some(p) => self.push(p.clone()),
            None => self.push(Object::Null(Null::new())),
        }
    }

    fn native_bool_to_boolean_object(&self, input: bool) -> Object {
        Object::Boolean(Boolean::new(input))
    }

    fn is_truthy(&self, object: Object) -> bool {
        match object {
            Object::Boolean(b) => b.value,
            Object::Null(_) => false,
            _ => true,
        }
    }

    fn current_frame(&self) -> Frame {
        self.frames[self.frames_index - 1].clone()
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        &mut self.frames[self.frames_index - 1]
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames[self.frames_index] = frame;
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames_index -= 1;

        self.frames[self.frames_index].clone()
    }

    fn execute_call(&mut self, number_arguments: usize) -> Result<(), String> {
        match self.stack[self.sp - 1 - number_arguments].clone() {
            Object::Closure(cl) => self.call_closure(&cl, number_arguments),
            Object::Builtin(bi) => self.call_builtin(&bi, number_arguments),
            _ => Err(String::from("calling non-closure and non-built-in")),
        }
    }

    fn call_closure(&mut self, closure: &Closure, number_arguments: usize) -> Result<(), String> {
        if number_arguments != closure.fun.num_parameters {
            return Err(format!("wrong number of arguments: want {}, got {}",
                closure.fun.num_parameters, number_arguments));
        }

        let frame = Frame::new(closure.clone(), self.sp - number_arguments);
        self.push_frame(frame.clone());

        self.sp = frame.base_pointer + closure.fun.num_locals;

        Ok(())
    }

    fn call_builtin(&mut self, builtin: &Builtin, number_arguments: usize) -> Result<(), String> {
        let arguments = self.stack[self.sp - number_arguments..self.sp].to_vec();

        let bi = builtin.builtin_function;
        let result = bi(&arguments);
        self.sp = self.sp - number_arguments - 1;

        match result {
            Object::Null(_) => {
                self.push(Object::Null(Null::new()))
            },
            _ => {
                self.push(result)
            }
        }
    }

    fn push_closure(&mut self, const_index: usize, num_free: usize) -> Result<(), String> {
        let constant = &self.constants[const_index];
        match constant {
            Object::CompiledFunction(cf) => {
                let mut free = vec![Object::Null(Null::new()); num_free];
                for i in 0..num_free {
                    free[i] = self.stack[self.sp - num_free + i].clone();
                }
                self.sp = self.sp - num_free;

                let closure = Object::Closure(Closure::new(cf.clone(), free));
                self.push(closure)
            },
            _ => Err(format!("not a function: {}", constant)),
        }
    }

    pub fn last_popped_stack_element(&self) -> Object {
        self.stack[self.sp].clone()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use super::super::ast::*;
    use super::super::compiler::Compiler;
    use super::super::lexer::Lexer;
    use super::super::parser::Parser;

    trait ValueType {
        fn get_type(&self) -> &str;
        fn get_i64(&self) -> i64;
        fn get_string(&self) -> String;
        fn get_bool(&self) -> bool;
        fn get_null(&self) -> Option<i8>;
        fn get_array(&self) -> Vec<i64>;
        fn get_hash(&self) -> Vec<(Integer, i64)>;
        fn get_error(&self) -> Result<(), String>;
    }

    impl ValueType for i64 {
        fn get_type(&self) -> &str { "i64" }
        fn get_i64(&self) -> i64 { *self }
        fn get_string(&self) -> String { panic!("Value is not an i64.") }
        fn get_bool(&self) -> bool { panic!("Value is not an i64.") }
        fn get_null(&self) -> Option<i8> { panic!("Value is not an i64.") }
        fn get_array(&self) -> Vec<i64> { panic!("Value is not an i64.") }
        fn get_hash(&self) -> Vec<(Integer, i64)> { panic!("Value is not an i64.") }
        fn get_error(&self) -> Result<(), String> { panic!("Value is not an i64.") }
    }

    impl ValueType for String {
        fn get_type(&self) -> &str { "String" }
        fn get_i64(&self) -> i64 { panic!("Value is not a String.") }
        fn get_string(&self) -> String { self.clone() }
        fn get_bool(&self) -> bool { panic!("Value is not a String.") }
        fn get_null(&self) -> Option<i8> { panic!("Value is not a String.") }
        fn get_array(&self) -> Vec<i64> { panic!("Value is not a String.") }
        fn get_hash(&self) -> Vec<(Integer, i64)> { panic!("Value is not a String.") }
        fn get_error(&self) -> Result<(), String> { panic!("Value is not a String.") }
    }

    impl ValueType for bool {
        fn get_type(&self) -> &str { "bool" }
        fn get_i64(&self) -> i64 { panic!("Value is not a bool.") }
        fn get_string(&self) -> String { panic!("Value is not a bool.") }
        fn get_bool(&self) -> bool { *self }
        fn get_null(&self) -> Option<i8> { panic!("Value is not a bool.") }
        fn get_array(&self) -> Vec<i64> { panic!("Value is not a bool.") }
        fn get_hash(&self) -> Vec<(Integer, i64)> { panic!("Value is not a bool.") }
        fn get_error(&self) -> Result<(), String> { panic!("Value is not a bool.") }
    }

    impl ValueType for Option<i8> {
        fn get_type(&self) -> &str { "null" }
        fn get_i64(&self) -> i64 { panic!("Value is not null.") }
        fn get_string(&self) -> String { panic!("Value is not null.") }
        fn get_bool(&self) -> bool { panic!("Value is not null.") }
        fn get_null(&self) -> Option<i8> { None }
        fn get_array(&self) -> Vec<i64> { panic!("Value is not null.") }
        fn get_hash(&self) -> Vec<(Integer, i64)> { panic!("Value is not null.") }
        fn get_error(&self) -> Result<(), String> { panic!("Value is not null.") }
    }

    impl ValueType for Vec<i64> {
        fn get_type(&self) -> &str { "Vec" }
        fn get_i64(&self) -> i64 { panic!("Value is not an array.") }
        fn get_string(&self) -> String { panic!("Value is not an array.") }
        fn get_bool(&self) -> bool { panic!("Value is not an array.") }
        fn get_null(&self) -> Option<i8> { panic!("Value is not an array.") }
        fn get_array(&self) -> Vec<i64> { self.clone() }
        fn get_hash(&self) -> Vec<(Integer, i64)> { panic!("Value is not an array.") }
        fn get_error(&self) -> Result<(), String> { panic!("Value is not an array.") }
    }

    impl ValueType for Vec<(Integer, i64)> {
        fn get_type(&self) -> &str { "Hash" }
        fn get_i64(&self) -> i64 { panic!("Value is not a hash.") }
        fn get_string(&self) -> String { panic!("Value is not a hash.") }
        fn get_bool(&self) -> bool { panic!("Value is not a hash.") }
        fn get_null(&self) -> Option<i8> { panic!("Value is not a hash.") }
        fn get_array(&self) -> Vec<i64> { panic!("Value is not a hash.") }
        fn get_hash(&self) -> Vec<(Integer, i64)> { self.clone() }
        fn get_error(&self) -> Result<(), String> { panic!("Value is not a hash.") }
    }

    impl ValueType for Result<(), String> {
        fn get_type(&self) -> &str { "Err" }
        fn get_i64(&self) -> i64 { panic!("Value is not an error.") }
        fn get_string(&self) -> String { panic!("Value is not an error.") }
        fn get_bool(&self) -> bool { panic!("Value is not an error.") }
        fn get_null(&self) -> Option<i8> { panic!("Value is not an error.") }
        fn get_array(&self) -> Vec<i64> { panic!("Value is not an error.") }
        fn get_hash(&self) -> Vec<(Integer, i64)> { panic!("Value is not an error.") }
        fn get_error(&self) -> Result<(), String> { self.clone() }
    }

    struct VMTestCase<'a, T> where T: ValueType {
        input: &'a str,
        expected: T,
    }

    #[test]
    fn test_integer_arithmetic() {
        let vm_test_cases = vec![
            VMTestCase { input: "1", expected: 1 },
            VMTestCase { input: "2", expected: 2 },
            VMTestCase { input: "1 + 2", expected: 3 },
            VMTestCase { input: "1 - 2", expected: -1 },
            VMTestCase { input: "1 * 2", expected: 2 },
            VMTestCase { input: "4 / 2", expected: 2 },
            VMTestCase { input: "50 / 2 * 2 + 10 - 5", expected: 55 },
            VMTestCase { input: "5 + 5 + 5 + 5 - 10", expected: 10 },
            VMTestCase { input: "2 * 2 * 2 * 2 * 2", expected: 32 },
            VMTestCase { input: "5 * 2 + 10", expected: 20 },
            VMTestCase { input: "5 + 2 * 10", expected: 25 },
            VMTestCase { input: "5 * (2 + 10)", expected: 60 },
            VMTestCase { input: "-5", expected: -5 },
            VMTestCase { input: "-10", expected: -10 },
            VMTestCase { input: "-50 + 100 + -50", expected: 0 },
            VMTestCase { input: "(5 + 10 * 2 + 15 / 3) * 2 + -10", expected: 50 },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_boolean_expressions() {
        let vm_test_cases = vec![
            VMTestCase { input: "true", expected: true },
            VMTestCase { input: "false", expected: false },
            VMTestCase { input: "1 < 2", expected: true },
            VMTestCase { input: "1 > 2", expected: false },
            VMTestCase { input: "1 < 1", expected: false },
            VMTestCase { input: "1 > 1", expected: false },
            VMTestCase { input: "1 == 1", expected: true },
            VMTestCase { input: "1 != 1", expected: false },
            VMTestCase { input: "1 == 2", expected: false },
            VMTestCase { input: "1 != 2", expected: true },
            VMTestCase { input: "true == true", expected: true },
            VMTestCase { input: "false == false", expected: true },
            VMTestCase { input: "true == false", expected: false },
            VMTestCase { input: "true != false", expected: true },
            VMTestCase { input: "false != true", expected: true },
            VMTestCase { input: "(1 < 2) == true", expected: true },
            VMTestCase { input: "(1 < 2) == false", expected: false },
            VMTestCase { input: "(1 > 2) == true", expected: false },
            VMTestCase { input: "(1 > 2) == false", expected: true },
            VMTestCase { input: "!true", expected: false },
            VMTestCase { input: "!false", expected: true },
            VMTestCase { input: "!5", expected: false },
            VMTestCase { input: "!!true", expected: true },
            VMTestCase { input: "!!false", expected: false },
            VMTestCase { input: "!!5", expected: true },
            VMTestCase { input: "!(if (false) { 5; })", expected: true },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_conditionals() {
        let vm_test_cases = vec![
            VMTestCase { input: "if (true) { 10 }", expected: 10 },
            VMTestCase { input: "if (true) { 10 } else { 20 }", expected: 10 },
            VMTestCase { input: "if (false) { 10 } else { 20 }", expected: 20 },
            VMTestCase { input: "if (1) { 10 }", expected: 10 },
            VMTestCase { input: "if (1 < 2) { 10 }", expected: 10 },
            VMTestCase { input: "if (1 < 2) { 10 } else { 20 }", expected: 10 },
            VMTestCase { input: "if (1 > 2) { 10 } else { 20 }", expected: 20 },
            VMTestCase { input: "if ((if (false) { 10 })) { 10 } else { 20 }", expected: 20 },
        ];

        let vm_test_cases_2 = vec![
            VMTestCase { input: "if (1 > 2) { 10 }", expected: None },
            VMTestCase { input: "if (false) { 10 }", expected: None },
        ];

        run_vm_tests(vm_test_cases);
        run_vm_tests(vm_test_cases_2);
    }

    #[test]
    fn test_global_let_statements() {
        let vm_test_cases = vec![
            VMTestCase { input: "let one = 1; one", expected: 1 },
            VMTestCase { input: "let one = 1; let two = 2; one + two", expected: 3 },
            VMTestCase { input: "let one = 1; let two = one + one; one + two", expected: 3 },
        ];

        let vm_test_cases_2 = vec![
            VMTestCase { input: "if (1 > 2) { 10 }", expected: None },
            VMTestCase { input: "if (false) { 10 }", expected: None },
        ];

        run_vm_tests(vm_test_cases);
        run_vm_tests(vm_test_cases_2);
    }

    #[test]
    fn test_string_expressions() {
        let vm_test_cases = vec![
            VMTestCase { input: "\"monkey\"", expected: String::from("monkey") },
            VMTestCase { input: "\"mon\" + \"key\"", expected: String::from("monkey") },
            VMTestCase { input: "\"mon\" + \"key\" + \"banana\"", expected: String::from("monkeybanana") },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_array_literals() {
        let vm_test_cases = vec![
            VMTestCase { input: "[]", expected: vec![] },
            VMTestCase { input: "[1, 2, 3]", expected: vec![1, 2, 3] },
            VMTestCase { input: "[1 + 2, 3 * 4, 5 + 6]", expected: vec![3, 12, 11] },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_hash_literals() {
        let vm_test_cases = vec![
            VMTestCase { input: "{}", expected: vec![] },
            VMTestCase { input: "{1: 2, 2: 3}", expected: vec![(Integer::new(1), 2), (Integer::new(2), 3)] },
            VMTestCase { input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}", expected: vec![(Integer::new(2), 4), (Integer::new(6), 16)] },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_index_expressions() {
        let vm_test_cases = vec![
            VMTestCase { input: "[1, 2, 3][1]", expected: 2 },
            VMTestCase { input: "[1, 2, 3][0 + 2]", expected: 3 },
            VMTestCase { input: "[[1, 1, 1]][0][0]", expected: 1 },
            VMTestCase { input: "{1: 1, 2: 2}[1]", expected: 1 },
            VMTestCase { input: "{1: 1, 2: 2}[2]", expected: 2 },
        ];
        let vm_test_cases_2 = vec![
            VMTestCase { input: "[][0]", expected: None },
            VMTestCase { input: "[1, 2, 3][99]", expected: None },
            VMTestCase { input: "[1][-1]", expected: None },
            VMTestCase { input: "{1: 1}[0]", expected: None },
            VMTestCase { input: "{}[0]", expected: None },
        ];

        run_vm_tests(vm_test_cases);
        run_vm_tests(vm_test_cases_2);
    }

    #[test]
    fn test_calling_functions_without_arguments() {
        let vm_test_cases = vec![
            VMTestCase { input: r#"
let fivePlusTen = fn() { 5 + 10; };
fivePlusTen();
"#, expected: 15 },
            VMTestCase { input: r#"
let one = fn() { 1; };
let two = fn() { 2; };
one() + two()
"#, expected: 3 },
            VMTestCase { input: r#"
let a = fn() { 1 };
let b = fn() { a() + 1 };
let c = fn() { b() + 1 };
c();
"#, expected: 3 },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_functions_with_return_statement() {
        let vm_test_cases = vec![
            VMTestCase { input: r#"
let earlyExit = fn() { return 99; 100; };
earlyExit();
"#, expected: 99 },
            VMTestCase { input: r#"
let earlyExit = fn() { return 99; return 100; };
earlyExit();
"#, expected: 99 },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_functions_without_return_value() {
        let vm_test_cases = vec![
            VMTestCase { input: r#"
let noReturn = fn() { };
noReturn();
"#, expected: None },
            VMTestCase { input: r#"
let noReturn = fn() { };
let noReturnTwo = fn() { noReturn(); };
noReturn();
noReturnTwo();
"#, expected: None },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_first_class_functions() {
        let vm_test_cases = vec![
            VMTestCase { input: r#"
let returnsOne = fn() { 1; };
let returnsOneReturner = fn() { returnsOne; };
returnsOneReturner()();
"#, expected: 1 },
            VMTestCase { input: r#"
let returnsOneReturner = fn() { 
    let returnsOne = fn() { 1; };    
    returnsOne; 
};
returnsOneReturner()();
"#, expected: 1 },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_calling_functions_with_bindings() {
        let vm_test_cases = vec![
            VMTestCase { input: r#"
let one = fn() { let one = 1; one };
one();
"#, expected: 1 },
            VMTestCase { input: r#"
let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
oneAndTwo();
"#, expected: 3 },
            VMTestCase { input: r#"
let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
oneAndTwo() + threeAndFour();
"#, expected: 10 },
            VMTestCase { input: r#"
let firstFoobar = fn() { let foobar = 50; foobar; };
let secondFoobar = fn() { let foobar = 100; foobar; };
firstFoobar() + secondFoobar();
"#, expected: 150 },
            VMTestCase { input: r#"
let globalSeed = 50;
let minusOne = fn() {
    let num = 1;
    globalSeed - num;
}
let minusTwo = fn() {
    let num = 2;
    globalSeed - num;
}
minusOne() + minusTwo();
"#, expected: 97 },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_calling_functions_with_arguments_and_bindings() {
        let vm_test_cases = vec![
            VMTestCase { input: r#"
let identity = fn(a) { a; };
identity(4);
"#, expected: 4 },
            VMTestCase { input: r#"
let sum = fn(a, b) { a + b; };
sum(1, 2);
"#, expected: 3 },
            VMTestCase { input: r#"
let sum = fn(a, b) {
    let c = a + b;
    c;
};
sum(1, 2);
"#, expected: 3 },
            VMTestCase { input: r#"
let sum = fn(a, b) {
    let c = a + b;
    c;
};
sum(1, 2) + sum(3, 4);
"#, expected: 10 },
            VMTestCase { input: r#"
let sum = fn(a, b) {
    let c = a + b;
    c;
};
let outer = fn() {
    sum(1, 2) + sum(3, 4);
};
outer();
"#, expected: 10 },
            VMTestCase { input: r#"
let globalNum = 10;

let sum = fn(a, b) {
    let c = a + b;
    c + globalNum;
};

let outer = fn() {
    sum(1, 2) + sum(3, 4) + globalNum;
};

outer() + globalNum;
"#, expected: 50 },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_calling_functions_with_wrong_arguments() {
        let vm_test_cases = vec![
            VMTestCase { input: "fn() { 1; }(1);", expected: String::from("wrong number of arguments: want 0, got 1") },
            VMTestCase { input: "fn(a) { a; }();", expected: String::from("wrong number of arguments: want 1, got 0") },
            VMTestCase { input: "fn(a, b) { a + b; }(1);", expected: String::from("wrong number of arguments: want 2, got 1") },
        ];

        for vm_test_case in vm_test_cases {
            let program = parse(vm_test_case.input);
            let mut compiler = Compiler::new();
            if let Err(e) = compiler.compile(&Node::Program(program)) {
                assert!(false, format!("compiler error: {}", e));
            }

            let mut vm = VM::new(compiler.bytecode());
            match vm.run() {
                Ok(_) => assert!(false, "VM error was expected, but did not appear."),
                Err(e) => assert_eq!(e, vm_test_case.expected),
            }
        }
    }

    #[test]
    fn test_builtin_functions() {
        let vm_test_cases_int = vec![
            VMTestCase { input: "len(\"\")", expected: 0 },
            VMTestCase { input: "len(\"four\")", expected: 4 },
            VMTestCase { input: "len(\"hello world\")", expected: 11 },
            VMTestCase { input: "len([1, 2, 3])", expected: 3 },
            VMTestCase { input: "len([])", expected: 0 },
            VMTestCase { input: "first([1, 2, 3])", expected: 1 },
            VMTestCase { input: "last([1, 2, 3])", expected: 3 },
        ];

        let vm_test_cases_none = vec![
            VMTestCase { input: "puts(\"hello\", \"world!\")", expected: None },
            VMTestCase { input: "first([])", expected: None },
            VMTestCase { input: "last([])", expected: None },
            VMTestCase { input: "rest([])", expected: None },
        ];
        
        let vm_test_cases_error = vec![
            VMTestCase { input: "len(1)", expected: Err(String::from("argument to \"len\" not supported, got INTEGER")) },
            VMTestCase { input: "len(\"one\", \"two\")", expected: Err(String::from("wrong number of arguments. got 2, want 1")) },
            VMTestCase { input: "first(1)", expected: Err(String::from("argument to \"first\" must be ARRAY, got INTEGER")) },
            VMTestCase { input: "last(1)", expected: Err(String::from("argument to \"last\" must be ARRAY, got INTEGER")) },
            VMTestCase { input: "push(1, 1)", expected: Err(String::from("argument to \"push\" must be ARRAY, got INTEGER")) },
        ];

        let vm_test_cases_array = vec![
            VMTestCase { input: "rest([1, 2, 3])", expected: vec![2, 3] },
            VMTestCase { input: "push([], 1)", expected: vec![1] },
        ];

        run_vm_tests(vm_test_cases_int);
        run_vm_tests(vm_test_cases_none);
        run_vm_tests(vm_test_cases_error);
        run_vm_tests(vm_test_cases_array);
    }

    #[test]
    fn test_closures() {
        let vm_test_cases = vec![
            VMTestCase { input: r#"
let newClosure = fn(a) {
    fn() { a; };
};
let closure = newClosure(99);
closure();
"#, expected: 99 },
            VMTestCase { input: r#"
let newAdder = fn(a, b) {
    fn(c) { a + b + c };
};
let adder = newAdder(1, 2);
adder(8);
"#, expected: 11 },
            VMTestCase { input: r#"
let newAdder = fn(a, b) {
    let c = a + b;
    fn(d) { c + d };
};
let adder = newAdder(1, 2);
adder(8);
"#, expected: 11 },
            VMTestCase { input: r#"
let newAdderOuter = fn(a, b) {
    let c = a + b;
    fn(d) {
        let e = d + c;
        fn(f) { e + f; };
    };
};
let newAdderInner = newAdderOuter(1, 2)
let adder = newAdderInner(3);
adder(8);
"#, expected: 14 },
            VMTestCase { input: r#"
let a = 1;
let newAdderOuter = fn(b) {
    fn(c) {
        fn(d) { a + b + c + d };
    };
};
let newAdderInner = newAdderOuter(2)
let adder = newAdderInner(3);
adder(8);
"#, expected: 14 },
            VMTestCase { input: r#"
let newClosure = fn(a, b) {
    let one = fn() { a; };
    let two = fn() { b; };
    fn() { one() + two(); };
};
let closure = newClosure(9, 90);
closure();
"#, expected: 99 },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_recursive_functions() {
        let vm_test_cases = vec![
            VMTestCase { input: r#"
let countDown = fn(x) {
    if (x == 0) {
        return 0;
    } else {
        countDown(x - 1);
    }
};
countDown(1);
"#, expected: 0 },
            VMTestCase { input: r#"
let countDown = fn(x) {
    if (x == 0) {
        return 0;
    } else {
        countDown(x - 1);
    }
};
let wrapper = fn() {
    countDown(1);
};
wrapper();
"#, expected: 0 },
            VMTestCase { input: r#"
let wrapper = fn() {
    let countDown = fn(x) {
        if (x == 0) {
            return 0;
        } else {
            countDown(x - 1);
        }
    };
    countDown(1);
};
wrapper();
"#, expected: 0 },
        ];

        run_vm_tests(vm_test_cases);
    }

    #[test]
    fn test_recursive_fibonacci() {
        let vm_test_cases = vec![
            VMTestCase { input: r#"
let fibonacci = fn(x) {
    if (x == 0) {
        return 0;
    } else {
        if (x == 1) {
            return 1;
        } else {
            fibonacci(x - 1) + fibonacci(x - 2);
        }
    }
};
fibonacci(15);
"#, expected: 610 },
        ];

        run_vm_tests(vm_test_cases);
    }

    fn run_vm_tests<T>(tests: Vec<VMTestCase<T>>) where T: ValueType {
        for test in tests {
            let program = parse(test.input);
            let mut compiler = Compiler::new();
            if let Err(e) = compiler.compile(&Node::Program(program)) {
                assert!(false, format!("compiler error: {}", e));
            }

            let mut vm = VM::new(compiler.bytecode());
            if let Err(e) = vm.run() {
                assert!(false, format!("vm error: {}", e));
            }

            let stack_element = vm.last_popped_stack_element();

            test_expected_object(&test.expected, &stack_element);
        }
    }

    fn parse(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        match parser.parse_program() {
            Some(p) => p,
            None => panic!("Program could not be parsed."),
        }
    }

    fn test_expected_object<T>(expected: &T, actual: &Object) where T: ValueType {
        match expected.get_type() {
            "i64" => test_integer_object(expected.get_i64(), actual),
            "bool" => test_boolean_object(expected.get_bool(), actual),
            "String" => test_string_object(expected.get_string(), actual),
            "null" => {
                match actual {
                    Object::Null(_) => (),
                    _ => assert!(false, "Object was not Null."),
                }
            },
            "Vec" => {
                match actual {
                    Object::Array(arr) => {
                        assert_eq!(arr.elements.len(), expected.get_array().len());
                        for (i, expected_element) in expected.get_array().iter().enumerate() {
                            test_integer_object(*expected_element, &arr.elements[i]);
                        }
                    },
                    _ => assert!(false, "Object was not an Array."),
                }
            },
            "Hash" => {
                match actual {
                    Object::Hash(h) => {
                        assert_eq!(h.pairs.len(), expected.get_hash().len());
                        for (expected_key, expected_value) in expected.get_hash() {
                            match h.pairs.get(&Object::Integer(expected_key)) {
                                Some(p) => test_integer_object(expected_value, &p),
                                None => assert!(false, "Key was not present in Hash."),
                            };
                        }
                    },
                    _ => assert!(false, "Object was not a Hash."),
                };
            },
            "Err" => {
                match actual {
                    Object::Error(e) => assert_eq!(e.message, if let Err(e) = expected.get_error() { e } else { String::new() }),
                    _ => assert!(false, "Object was not an Error."),
                }
            },
            _ => assert!(false, "Object type is not supported."),
        }
    }

    fn test_integer_object(expected: i64, actual: &Object) {
        match actual {
            Object::Integer(i) => assert_eq!(i.value, expected),
            _ => assert!(false, "Object was not an Integer."),
        }
    }

    fn test_boolean_object(expected: bool, actual: &Object) {
        match actual {
            Object::Boolean(b) => assert_eq!(b.value, expected),
            _ => assert!(false, "Object was not a Boolean."),
        }
    }

    fn test_string_object(expected: String, actual: &Object) {
        match actual {
            Object::String(s) => assert_eq!(s.value, expected),
            _ => assert!(false, "Object was not a String."),
        }
    }
}