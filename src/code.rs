pub type Instructions = Vec<u8>;

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum OpCode {
    OpConstant = 1,
    OpAdd,
    OpPop,
    OpSub,
    OpMul,
    OpDiv,
    OpTrue,
    OpFalse,
    OpEqual,
    OpNotEqual,
    OpGreaterThan,
    OpMinus,
    OpBang,
    OpJumpNotTruthy,
    OpJump,
    OpNull,
    OpGetGlobal,
    OpSetGlobal,
    OpArray,
    OpHash,
    OpIndex,
    OpCall,
    OpReturnValue,
    OpReturn,
    OpGetLocal,
    OpSetLocal,
    OpGetBuiltin,
    OpClosure,
    OpGetFree,
    OpCurrentClosure,
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        match value {
            1 => OpCode::OpConstant,
            2 => OpCode::OpAdd,
            3 => OpCode::OpPop,
            4 => OpCode::OpSub,
            5 => OpCode::OpMul,
            6 => OpCode::OpDiv,
            7 => OpCode::OpTrue,
            8 => OpCode::OpFalse,
            9 => OpCode::OpEqual,
            10 => OpCode::OpNotEqual,
            11 => OpCode::OpGreaterThan,
            12 => OpCode::OpMinus,
            13 => OpCode::OpBang,
            14 => OpCode::OpJumpNotTruthy,
            15 => OpCode::OpJump,
            16 => OpCode::OpNull,
            17 => OpCode::OpGetGlobal,
            18 => OpCode::OpSetGlobal,
            19 => OpCode::OpArray,
            20 => OpCode::OpHash,
            21 => OpCode::OpIndex,
            22 => OpCode::OpCall,
            23 => OpCode::OpReturnValue,
            24 => OpCode::OpReturn,
            25 => OpCode::OpGetLocal,
            26 => OpCode::OpSetLocal,
            27 => OpCode::OpGetBuiltin,
            28 => OpCode::OpClosure,
            29 => OpCode::OpGetFree,
            30 => OpCode::OpCurrentClosure,
            _ => OpCode::OpNull,
        }
    }
}

pub struct Definition {
    pub name: String,
    pub operand_widths: Vec<usize>,
}

impl Definition {
    pub fn new(name: &str, operand_widths: Vec<usize>) -> Self {
        Self { name: String::from(name), operand_widths }
    }
}

fn lookup(op: &OpCode) -> Definition {
    match op {
        OpCode::OpConstant => Definition::new("OpConstant", vec![2]),
        OpCode::OpAdd => Definition::new("OpAdd", vec![]),
        OpCode::OpPop => Definition::new("OpPop", vec![]),
        OpCode::OpSub => Definition::new("OpSub", vec![]),
        OpCode::OpMul => Definition::new("OpMul", vec![]),
        OpCode::OpDiv => Definition::new("OpDiv", vec![]),
        OpCode::OpTrue => Definition::new("OpTrue", vec![]),
        OpCode::OpFalse => Definition::new("OpFalse", vec![]),
        OpCode::OpEqual => Definition::new("OpEqual", vec![]),
        OpCode::OpNotEqual => Definition::new("OpNotEqual", vec![]),
        OpCode::OpGreaterThan => Definition::new("OpGreaterThan", vec![]),
        OpCode::OpMinus => Definition::new("OpMinus", vec![]),
        OpCode::OpBang => Definition::new("OpBang", vec![]),
        OpCode::OpJumpNotTruthy => Definition::new("OpJumpNotTruthy", vec![2]),
        OpCode::OpJump => Definition::new("OpJump", vec![2]),
        OpCode::OpNull => Definition::new("OpNull", vec![]),
        OpCode::OpGetGlobal => Definition::new("OpGetGlobal", vec![2]),
        OpCode::OpSetGlobal => Definition::new("OpSetGlobal", vec![2]),
        OpCode::OpArray => Definition::new("OpArray", vec![2]),
        OpCode::OpHash => Definition::new("OpHash", vec![2]),
        OpCode::OpIndex => Definition::new("OpIndex", vec![]),
        OpCode::OpCall => Definition::new("OpCall", vec![1]),
        OpCode::OpReturnValue => Definition::new("OpReturnValue", vec![]),
        OpCode::OpReturn => Definition::new("OpReturn", vec![]),
        OpCode::OpGetLocal => Definition::new("OpGetLocal", vec![1]),
        OpCode::OpSetLocal => Definition::new("OpSetLocal", vec![1]),
        OpCode::OpGetBuiltin => Definition::new("OpGetBuiltin", vec![1]),
        OpCode::OpClosure => Definition::new("OpClosure", vec![2, 1]),
        OpCode::OpGetFree => Definition::new("OpGetFree", vec![1]),
        OpCode::OpCurrentClosure => Definition::new("OpCurrentClosure", vec![]),
    }
}

pub fn string(instructions: &Instructions) -> String {
    let mut out = String::new();

    let mut i = 0;
    while i < instructions.len() {
        let definition = lookup(&(OpCode::from(instructions[i])));

        let (operands, read) = read_operands(&definition, &instructions[i + 1..].to_vec());

        out.push_str(format!("{:04} {}\n", i, format_instruction(&definition, &operands)).as_str());

        i += 1 + read;
    }

    out
}

fn format_instruction(definition: &Definition, operands: &Vec<i64>) -> String {
    let operand_count = definition.operand_widths.len();

    if operands.len() != operand_count {
        return format!("ERROR: operand len {} does not match defined {}\n", operands.len(), operand_count);
    }

    match operand_count {
        0 => definition.name.clone(),
        1 => format!("{} {}", definition.name, operands[0]),
        2 => format!("{} {} {}", definition.name, operands[0], operands[1]),
        _ => format!("ERROR: unhandled operand_count for {}\n", definition.name),
    }
}

pub fn make(op: OpCode, operands: Vec<i64>) -> Instructions {
    let definition = lookup(&op);
    
    let mut instruction_len = 1;
    for width in &definition.operand_widths {
        instruction_len += width;
    }
    
    let mut instruction = vec![0; instruction_len];
    instruction[0] = op as u8;
    
    let mut offset = 1;
    for (i, operand) in operands.iter().enumerate() {
        let width = definition.operand_widths[i];
        match width {
            2 => {
                let bytes = u16::to_be_bytes(*operand as u16);
                instruction[offset] = bytes[0];
                instruction[offset + 1] = bytes[1];
            },
            1 => {
                let byte = u8::to_be_bytes(*operand as u8);
                instruction[offset] = byte[0];
            },
            _ => (),
        };
        offset += width as usize;
    }
    
    instruction
}

pub fn read_operands(definition: &Definition, instructions: &Instructions) -> (Vec<i64>, usize) {
    let mut operands = vec![0; definition.operand_widths.len()];
    let mut offset = 0;

    for (i, width) in definition.operand_widths.iter().enumerate() {
        match width {
            2 => operands[i] = {
                let mut bytes = [0; 2];
                bytes[0] = instructions[offset];
                bytes[1] = instructions[offset + 1];

                read_u16(&bytes) as i64
            },
            1 => operands[i] = {
                let mut byte = [0; 1];
                byte[0] = instructions[offset];

                read_u8(&byte) as i64
            },
            _ => (),
        };

        offset += width
    }

    (operands, offset)
}

pub fn read_u16(instructions: &[u8; 2]) -> u16 {
    u16::from_be_bytes(*instructions)
}

pub fn read_u8(instructions: &[u8; 1]) -> u8 {
    u8::from_be_bytes(*instructions)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_make() {
        struct MakeTest {
            op: OpCode,
            operands: Vec<i64>,
            expected: Vec<u8>,
        };

        let make_tests = vec![
            MakeTest { op: OpCode::OpConstant, operands: vec![65534], expected: vec![OpCode::OpConstant as u8, 255, 254] },
            MakeTest { op: OpCode::OpAdd, operands: vec![], expected: vec![OpCode::OpAdd as u8] },
            MakeTest { op: OpCode::OpGetLocal, operands: vec![255], expected: vec![OpCode::OpGetLocal as u8, 255] },
            MakeTest { op: OpCode::OpClosure, operands: vec![65534, 255], expected: vec![OpCode::OpClosure as u8, 255, 254, 255] },
        ];

        for make_test in make_tests {
            let instruction = make(make_test.op, make_test.operands);

            assert_eq!(instruction.len(), make_test.expected.len());
            for (i, exp) in make_test.expected.iter().enumerate() {
                assert_eq!(instruction[i], *exp);
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(OpCode::OpAdd, vec![]),
            make(OpCode::OpGetLocal, vec![1]),
            make(OpCode::OpConstant, vec![2]),
            make(OpCode::OpConstant, vec![65535]),
            make(OpCode::OpClosure, vec![65535, 255]),
        ];

        let expected = r#"0000 OpAdd
0001 OpGetLocal 1
0003 OpConstant 2
0006 OpConstant 65535
0009 OpClosure 65535 255
"#;

        let mut concatted = vec![];
        for instruction in instructions {
            concatted.extend(instruction);
        }

        assert_eq!(string(&concatted), expected);
    }
    
    #[test]
    fn test_read_operands() {
        struct OperandTest {
            op: OpCode,
            operands: Vec<i64>,
            bytes_read: usize,
        };

        let operand_tests = vec![
            OperandTest { op: OpCode::OpConstant, operands: vec![65535], bytes_read: 2 },
            OperandTest { op: OpCode::OpGetLocal, operands: vec![255], bytes_read: 1 },
            OperandTest { op: OpCode::OpClosure, operands: vec![65535, 255], bytes_read: 3 },
        ];

        for operand_test in operand_tests {
            let instruction = make(operand_test.op.clone(), operand_test.operands.clone());

            let definition = lookup(&operand_test.op);

            let (operands_read, n) = read_operands(&definition, &instruction[1..].to_vec());
            assert_eq!(n, operand_test.bytes_read);
            for (i, want) in operand_test.operands.iter().enumerate() {
                assert_eq!(operands_read[i], *want);
            }
        }
    }
}