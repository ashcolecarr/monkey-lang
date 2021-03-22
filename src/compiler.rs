use super::ast::*;
use super::code::*;
use super::object::*;

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Bytecode {
    pub fn new(instructions: Instructions, constants: Vec<Object>) -> Self {
        Self { instructions, constants }
    }
}

#[derive(Clone)]
pub struct EmittedInstruction {
    pub opcode: OpCode,
    pub position: usize,
}

impl EmittedInstruction {
    pub fn new(opcode: OpCode, position: usize) -> Self {
        Self { opcode, position }
    }
}

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
    pub last_instruction: EmittedInstruction,
    pub previous_instruction: EmittedInstruction,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            constants: vec![],
            last_instruction: EmittedInstruction::new(OpCode::OpConstant, 0),
            previous_instruction: EmittedInstruction::new(OpCode::OpConstant, 0),
        }
    }

    pub fn compile(&mut self, node: &Node) -> Result<(), String> {
        match node {
            Node::Program(p) => {
                for statement in &p.statements {
                    if let Err(e) = self.compile(&Node::Statement(statement.clone())) {
                        return Err(e);
                    }
                }
            },
            Node::Statement(stmt) => {
                match stmt {
                    Statement::ExpressionStatement(es) => {
                        if let Some(exp) = &es.expression {
                            if let Err(e) = self.compile(&Node::Expression(exp.clone())) {
                                return Err(e);
                            }
                        }

                        self.emit(OpCode::OpPop, vec![]);
                    },
                    Statement::BlockStatement(bs) => {
                        for s in &bs.statements {
                            if let Err(e) = self.compile(&Node::Statement(s.clone())) {
                                return Err(e);
                            }
                        }
                    },
                    _ => (),
                }
            },
            Node::Expression(exp) => {
                match exp {
                    Expression::InfixExpression(ie) => {
                        if ie.operator.as_str() == "<" {
                            if let Err(e) = self.compile(&Node::Expression(*ie.right.clone())) {
                                return Err(e);
                            }

                            if let Err(e) = self.compile(&Node::Expression(*ie.left.clone())) {
                                return Err(e);
                            }

                            self.emit(OpCode::OpGreaterThan, vec![]);

                            return Ok(());
                        }

                        if let Err(e) = self.compile(&Node::Expression(*ie.left.clone())) {
                            return Err(e);
                        }

                        if let Err(e) = self.compile(&Node::Expression(*ie.right.clone())) {
                            return Err(e);
                        }

                        match ie.operator.as_str() {
                            "+" => self.emit(OpCode::OpAdd, vec![]),
                            "-" => self.emit(OpCode::OpSub, vec![]),
                            "*" => self.emit(OpCode::OpMul, vec![]),
                            "/" => self.emit(OpCode::OpDiv, vec![]),
                            ">" => self.emit(OpCode::OpGreaterThan, vec![]),
                            "==" => self.emit(OpCode::OpEqual, vec![]),
                            "!=" => self.emit(OpCode::OpNotEqual, vec![]),
                            _ => return Err(format!("unknown operator {}", ie.operator)),
                        };
                    },
                    Expression::IntegerLiteral(il) => {
                        let integer = Object::Integer(Integer::new(il.value));
                        
                        let position = self.add_constant(integer) as i64;
                        self.emit(OpCode::OpConstant, vec![position]);
                    },
                    Expression::BooleanLiteral(bl) => {
                        if bl.value {
                            self.emit(OpCode::OpTrue, vec![]);
                        } else {
                            self.emit(OpCode::OpFalse, vec![]);
                        }
                    },
                    Expression::PrefixExpression(pe) => {
                        if let Err(e) = self.compile(&Node::Expression(*pe.right.clone())) {
                            return Err(e);
                        }

                        match pe.operator.as_str() {
                            "!" => self.emit(OpCode::OpBang, vec![]),
                            "-" => self.emit(OpCode::OpMinus, vec![]),
                            _ => return Err(format!("unknown operator {}", pe.operator)),
                        };
                    },
                    Expression::IfExpression(ie) => {
                        if let Err(e) = self.compile(&Node::Expression(*ie.condition.clone())) {
                            return Err(e);
                        }

                        // Emit jump-not-truthy instruction with placeholder value.
                        let jump_not_truthy_position = self.emit(OpCode::OpJumpNotTruthy, vec![9999]);

                        if let Err(e) = self.compile(&Node::Statement(Statement::BlockStatement(ie.consequence.clone()))) {
                            return Err(e);
                        }

                        if self.last_instruction_is_pop() {
                            self.remove_last_pop();
                        }

                        // Emit jump instruction with placeholder value.
                        let jump_position = self.emit(OpCode::OpJump, vec![9999]);

                        let after_consequence_position = self.instructions.len();
                        self.change_operand(jump_not_truthy_position, after_consequence_position as i64);

                        match &ie.alternative {
                            Some(alt) => {
                                if let Err(e) = self.compile(&Node::Statement(Statement::BlockStatement(alt.clone()))) {
                                    return Err(e);
                                }
                                
                                if self.last_instruction_is_pop() {
                                    self.remove_last_pop();
                                }

                            },
                            None => {
                                self.emit(OpCode::OpNull, vec![]);
                            },
                        }
                        let after_alternative_position = self.instructions.len();
                        self.change_operand(jump_position, after_alternative_position as i64);
                    },
                    _ => (),
                }
            }
        };

        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode::new(self.instructions.clone(), self.constants.clone())
    }

    fn add_constant(&mut self, object: Object) -> usize {
        self.constants.push(object);

        self.constants.len() - 1
    }

    fn emit(&mut self, op: OpCode, operands: Vec<i64>) -> usize {
        let instruction = make(op.clone(), operands);
        let position = self.add_instruction(instruction);

        self.set_last_instruction(op, position);

        position
    }

    fn add_instruction(&mut self, instruction: Instructions) -> usize {
        let position_new_instruction = self.instructions.len();
        self.instructions.extend(instruction);

        position_new_instruction
    }

    fn set_last_instruction(&mut self, op: OpCode, position: usize) {
        let previous = self.last_instruction.clone();
        let last = EmittedInstruction::new(op, position);

        self.previous_instruction = previous;
        self.last_instruction = last;
    }

    fn last_instruction_is_pop(&self) -> bool {
        self.last_instruction.opcode == OpCode::OpPop
    }

    fn remove_last_pop(&mut self) {
        self.instructions = self.instructions[..self.last_instruction.position].to_vec();
        self.last_instruction = self.previous_instruction.clone();
    }

    fn change_operand(&mut self, op_position: usize, operand: i64) {
        let op = OpCode::from(self.instructions[op_position]);
        let new_instruction = make(op, vec![operand]);

        self.replace_instruction(op_position, new_instruction);
    }

    fn replace_instruction(&mut self, position: usize, new_instruction: Instructions) {
        for (i, _) in new_instruction.iter().enumerate() {
            self.instructions[position + i] = new_instruction[i];
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
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

    struct CompilerTestCase<'a, T> where T: ValueType {
        input: &'a str,
        expected_constants: Vec<T>,
        expected_instructions: Vec<Instructions>,
    }

    #[test]
    fn test_integer_arithmetic() {
        let compiler_test_cases = vec![
            CompilerTestCase {
                input: "1 + 2",
                expected_constants: vec![1, 2],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]), 
                    make(OpCode::OpConstant, vec![1]), 
                    make(OpCode::OpAdd, vec![]),
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "1; 2",
                expected_constants: vec![1, 2],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]), 
                    make(OpCode::OpPop, vec![]),
                    make(OpCode::OpConstant, vec![1]), 
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "1 - 2",
                expected_constants: vec![1, 2],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]), 
                    make(OpCode::OpConstant, vec![1]), 
                    make(OpCode::OpSub, vec![]),
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "1 * 2",
                expected_constants: vec![1, 2],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]), 
                    make(OpCode::OpConstant, vec![1]), 
                    make(OpCode::OpMul, vec![]),
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "2 / 1",
                expected_constants: vec![2, 1],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]), 
                    make(OpCode::OpConstant, vec![1]), 
                    make(OpCode::OpDiv, vec![]),
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "-1",
                expected_constants: vec![1],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]), 
                    make(OpCode::OpMinus, vec![]),
                    make(OpCode::OpPop, vec![]),
                ]
            },
        ];

        run_compiler_tests(compiler_test_cases);
    }

    #[test]
    fn test_boolean_expressions() {
        let compiler_test_cases = vec![
            CompilerTestCase {
                input: "true",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]), 
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "false",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpFalse, vec![]), 
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "1 > 2",
                expected_constants: vec![1, 2],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]), 
                    make(OpCode::OpConstant, vec![1]), 
                    make(OpCode::OpGreaterThan, vec![]), 
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "1 < 2",
                expected_constants: vec![2, 1],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]), 
                    make(OpCode::OpConstant, vec![1]), 
                    make(OpCode::OpGreaterThan, vec![]), 
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "1 == 2",
                expected_constants: vec![1, 2],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]), 
                    make(OpCode::OpConstant, vec![1]), 
                    make(OpCode::OpEqual, vec![]), 
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "1 != 2",
                expected_constants: vec![1, 2],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]), 
                    make(OpCode::OpConstant, vec![1]), 
                    make(OpCode::OpNotEqual, vec![]), 
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "true == false",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]), 
                    make(OpCode::OpFalse, vec![]), 
                    make(OpCode::OpEqual, vec![]), 
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "true != false",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]), 
                    make(OpCode::OpFalse, vec![]), 
                    make(OpCode::OpNotEqual, vec![]), 
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "!true",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]), 
                    make(OpCode::OpBang, vec![]), 
                    make(OpCode::OpPop, vec![]),
                ]
            },
        ];

        run_compiler_tests(compiler_test_cases);
    }

    #[test]
    fn test_conditionals() {
        let compiler_test_cases = vec![
            CompilerTestCase {
                input: "if (true) { 10 }; 3333;",
                expected_constants: vec![10, 3333],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]),
                    make(OpCode::OpJumpNotTruthy, vec![10]),
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpJump, vec![11]),
                    make(OpCode::OpNull, vec![]),
                    make(OpCode::OpPop, vec![]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpPop, vec![]),
                ]
            },
            CompilerTestCase {
                input: "if (true) { 10 } else { 20 }; 3333;",
                expected_constants: vec![10, 20, 3333],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]),
                    make(OpCode::OpJumpNotTruthy, vec![10]),
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpJump, vec![13]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpPop, vec![]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpPop, vec![]),
                ]
            },
        ];

        run_compiler_tests(compiler_test_cases);
    }

    fn run_compiler_tests<T>(tests: Vec<CompilerTestCase<T>>) where T: ValueType {
        for test in tests {
            let program = parse(test.input);
            let mut compiler = Compiler::new();

            match compiler.compile(&Node::Program(program)) {
                Ok(_) => {
                    let bytecode = compiler.bytecode();
                    test_instructions(&test.expected_instructions, &bytecode.instructions.clone());
        
                    test_constants(&test.expected_constants, &bytecode.constants.clone());
                },
                Err(e) => assert!(false, format!("compiler error: {}", e)),
            };
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

    fn test_instructions(expected: &Vec<Instructions>, actual: &Instructions) {
        let concatted = concat_instructions(expected);
        assert_eq!(actual.len(), concatted.len(), "\nactual: {}expected: {}", string(actual), string(&concatted));

        for (i, instruction) in concatted.iter().enumerate() {
            assert_eq!(actual[i], *instruction, "\nactual: {}expected: {}", string(actual), string(&concatted));
        }
    }

    fn concat_instructions(instructions: &Vec<Instructions>) -> Instructions {
        let mut out = vec![];

        for instruction in instructions {
            out.extend(instruction);
        }

        out
    }

    fn test_constants<T>(expected: &Vec<T>, actual: &Vec<Object>) where T: ValueType {
        assert_eq!(expected.len(), actual.len());

        for (i, constant) in expected.iter().enumerate() {
            match constant.get_type() {
                "i64" => test_integer_object(constant.get_i64(), &actual[i]),
                _ => assert!(false, "Constant type is not supported."),
            };
        }
    }

    fn test_integer_object(expected: i64, actual: &Object) {
        match actual {
            Object::Integer(i) => assert_eq!(i.value, expected),
            _ => assert!(false, "Object was not an Integer."),
        }
    }
}