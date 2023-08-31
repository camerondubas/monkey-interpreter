use std::fmt::Binary;

use self::opcode::Definition;
pub use self::opcode::Opcode;
mod opcode;

#[derive(Default, Debug, Clone)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new() -> Self {
        Instructions(Vec::new())
    }

    pub fn get(&self, offset: usize) -> u8 {
        self.0[offset] // TODO: Handle out of bounds / errors
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn extend(&mut self, instructions: Instructions) {
        self.0.extend(instructions.0);
    }

    #[allow(dead_code)]
    pub fn print(&self) -> String {
        let mut printed_instructions = String::new();
        let mut offset = 0;

        while let Some(instruction) = self.0.get(offset) {
            let opcode = Opcode::from(*instruction);
            let definition = opcode.definition();

            let (operands, read_count) = definition.read_operands(&self.0[offset + 1..]);

            printed_instructions.push_str(&format!(
                "{:04} {}\n",
                offset,
                format_instruction(&definition, &operands)
            ));

            offset += 1 + read_count;
        }

        printed_instructions
    }
}

impl Binary for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for byte in self.0.iter() {
            write!(f, "{:08b}", byte)?;
        }
        Ok(())
    }
}

impl PartialEq<Vec<u8>> for Instructions {
    fn eq(&self, other: &Vec<u8>) -> bool {
        &self.0 == other
    }
}

impl From<Vec<Instructions>> for Instructions {
    fn from(instructions: Vec<Instructions>) -> Self {
        instructions
            .into_iter()
            .fold(Instructions::new(), |mut acc, instruction| {
                acc.extend(instruction);
                acc
            })
    }
}

pub fn make(op: Opcode, operands: &[u16]) -> Instructions {
    let definition = op.definition();
    let mut instruction = Vec::with_capacity(definition.size());

    instruction.push(op.into());

    for (idx, operand) in operands.iter().enumerate() {
        match definition.operand_widths.get(idx) {
            Some(operand_width) => match operand_width {
                2 => {
                    let bytes = operand.to_be_bytes();
                    instruction.push(bytes[0]);
                    instruction.push(bytes[1]);
                }
                _ => panic!("Unhandled operand width"),
            },
            None => panic!("Operand not found"),
        }
    }

    Instructions(instruction)
}

fn format_instruction(definition: &Definition, operands: &[u16]) -> String {
    let operand_count = definition.operand_widths.len();

    if operands.len() != operand_count {
        return format!(
            "ERROR: operand len {} does not match defined {}",
            operands.len(),
            operand_count
        );
    }

    match operand_count {
        0 => definition.name.to_string(),
        1 => format!("{} {}", definition.name, operands[0]),
        _ => format!("ERROR: unhandled operand_count for {}", definition.name),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestMake {
        opcode: Opcode,
        operands: Vec<u16>,
        expected: Vec<u8>,
    }

    #[test]
    fn test_make() {
        let tests: Vec<TestMake> = vec![
            TestMake {
                opcode: Opcode::Constant,
                operands: vec![0],
                expected: vec![Opcode::Constant.into(), 0, 0],
            },
            TestMake {
                opcode: Opcode::Constant,
                operands: vec![1],
                expected: vec![Opcode::Constant.into(), 0, 1],
            },
            TestMake {
                opcode: Opcode::Constant,
                operands: vec![65534],
                expected: vec![Opcode::Constant.into(), 255, 254],
            },
            TestMake {
                opcode: Opcode::Constant,
                operands: vec![65535],
                expected: vec![Opcode::Constant.into(), 255, 255],
            },
            TestMake {
                opcode: Opcode::Add,
                operands: vec![],
                expected: vec![Opcode::Add.into()],
            },
        ];

        for test in tests {
            let instruction = make(test.opcode, &test.operands);
            assert_eq!(instruction, test.expected);
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Opcode::Add, &[]),
            make(Opcode::Constant, &[1]),
            make(Opcode::Constant, &[2]),
            make(Opcode::Constant, &[65535]),
            make(Opcode::Sub, &[]),
            make(Opcode::Mul, &[]),
            make(Opcode::Div, &[]),
        ];

        let expected = "0000 Add
0001 Constant 1
0004 Constant 2
0007 Constant 65535
0010 Sub
0011 Mul
0012 Div
";

        assert_eq!(Instructions::from(instructions).print(), expected);
    }

    #[test]
    fn test_read_operands() {
        let tests: Vec<(Opcode, Vec<u16>, usize)> = vec![(Opcode::Constant, vec![65535], 2)];

        for (opcode, operands, expected_bytes_read) in tests {
            let instruction = make(opcode, &operands);
            let definition = opcode.definition();

            let (operands_read, n) = definition.read_operands(&instruction.0[1..]);
            assert_eq!(n, expected_bytes_read);

            for (i, operand) in operands.iter().enumerate() {
                assert_eq!(operands_read[i], *operand);
            }
        }
    }
}
