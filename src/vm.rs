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
                    bytes[0] = self.instructions[ip + 1];
                    bytes[1] = self.instructions[ip + 2];

                    let position = read_u16(&bytes);
                    ip = position as usize - 1;
                },
                OpCode::OpJumpNotTruthy => {
                    let mut bytes = [0; 2];
                    bytes[0] = self.instructions[ip + 1];
                    bytes[1] = self.instructions[ip + 2];

                    let position = read_u16(&bytes);
                    ip += 2;

                    let condition = self.pop();
                    if !self.is_truthy(condition) {
                        ip = position as usize - 1;
                    }
                },
                OpCode::OpNull => {
                    if let Err(e) = self.push(Object::Null(Null::new())) {
                        return Err(e);
                    }
                },
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

    fn execute_binary_operation(&mut self, op: &OpCode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();

        match (&left, &right) {
            (Object::Integer(l), Object::Integer(r)) => self.execute_binary_integer_operation(op, &l, &r),
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
    }

    impl ValueType for i64 {
        fn get_type(&self) -> &str { "i64" }
        fn get_i64(&self) -> i64 { *self }
        fn get_string(&self) -> String { panic!("Value is not an i64.") }
        fn get_bool(&self) -> bool { panic!("Value is not an i64.") }
        fn get_null(&self) -> Option<i8> { panic!("Value is not null.") }
    }

    impl ValueType for String {
        fn get_type(&self) -> &str { "String" }
        fn get_i64(&self) -> i64 { panic!("Value is not a String.") }
        fn get_string(&self) -> String { self.clone() }
        fn get_bool(&self) -> bool { panic!("Value is not a String.") }
        fn get_null(&self) -> Option<i8> { panic!("Value is not null.") }
    }

    impl ValueType for bool {
        fn get_type(&self) -> &str { "bool" }
        fn get_i64(&self) -> i64 { panic!("Value is not a bool.") }
        fn get_string(&self) -> String { panic!("Value is not a bool.") }
        fn get_bool(&self) -> bool { *self }
        fn get_null(&self) -> Option<i8> { panic!("Value is not null.") }
    }

    impl ValueType for Option<i64> {
        fn get_type(&self) -> &str { "null" }
        fn get_i64(&self) -> i64 { panic!("Value is not null.") }
        fn get_string(&self) -> String { panic!("Value is not null.") }
        fn get_bool(&self) -> bool { panic!("Value is not null.") }
        fn get_null(&self) -> Option<i8> { None }
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
            "null" => {
                match actual {
                    Object::Null(_) => (),
                    _ => assert!(false, "Object was not Null."),
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
}