use super::code::*;
use super::compiler::Bytecode;
use super::object::*;

const STACK_SIZE: usize = 2048;

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    sp: usize, // Always points to the next value; top of stack is sp - 1
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            //stack: Vec::with_capacity(STACK_SIZE),
            stack: vec![Object::NonPrint; STACK_SIZE],
            sp: 0,
        }
    }
    
    pub fn run(&mut self) -> Result<(), String> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = OpCode::from(self.instructions[ip]);

            match op {
                OpCode::OpConstant => {
                    let mut bytes = [0; 2];
                    bytes[0] = self.instructions[ip + 1];
                    bytes[1] = self.instructions[ip + 2];

                    let const_index = read_u16(&bytes);
                    ip += 2;

                    if let Err(e) = self.push(self.constants[const_index as usize].clone()) {
                        return Err(e);
                    }
                },
                OpCode::OpAdd => {
                    let right = self.pop();
                    let left = self.pop();

                    if let (Object::Integer(l), Object::Integer(r)) = (left, right) {
                        let result = l.value + r.value;
                        self.push(Object::Integer(Integer::new(result)));
                    }
                }
                _ => (),
            };

            ip += 1;
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

    pub fn stack_top(&self) -> Option<Object> {
        match self.sp {
            0 => None,
            _ => Some(self.stack[self.sp - 1].clone()),
        }
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
    }

    impl ValueType for i64 {
        fn get_type(&self) -> &str { "i64" }
        fn get_i64(&self) -> i64 { *self }
        fn get_string(&self) -> String { panic!("Value is not an i64.") }
        fn get_bool(&self) -> bool { panic!("Value is not an i64.") }
    }

    impl ValueType for String {
        fn get_type(&self) -> &str { "String" }
        fn get_i64(&self) -> i64 { panic!("Value is not a String.") }
        fn get_string(&self) -> String { self.clone() }
        fn get_bool(&self) -> bool { panic!("Value is not a String.") }
    }

    impl ValueType for bool {
        fn get_type(&self) -> &str { "bool" }
        fn get_i64(&self) -> i64 { panic!("Value is not a bool.") }
        fn get_string(&self) -> String { panic!("Value is not a bool.") }
        fn get_bool(&self) -> bool { *self }
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

            let stack_element = vm.stack_top();

            match stack_element {
                Some(se) => test_expected_object(&test.expected, &se),
                None => assert!(false, "No items were found on the stack."),
            }
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
            _ => assert!(false, "Object type is not supported."),
        }
    }

    fn test_integer_object(expected: i64, actual: &Object) {
        match actual {
            Object::Integer(i) => assert_eq!(i.value, expected),
            _ => assert!(false, "Object was not an Integer."),
        }
    }
}