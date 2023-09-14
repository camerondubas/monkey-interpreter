use colored::Colorize;
use std::fmt::{Binary, Display};

use self::opcode::Definition;
pub use self::opcode::Opcode;
pub mod make;
mod opcode;

pub(crate) use make::make;

#[derive(Default, Debug, Clone)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new() -> Self {
        Instructions(Vec::new())
    }

    pub fn get(&self, offset: usize) -> u8 {
        self.0[offset] // TODO: Handle out of bounds / errors
    }

    pub fn get_two_bytes(&self, offset: usize) -> u16 {
        u16::from_be_bytes([self.get(offset), self.get(offset + 1)])
    }

    // TODO: clean up setting at index
    pub fn set(&mut self, offset: usize, instructions: Instructions) {
        for (i, byte) in instructions.0.iter().enumerate() {
            self.0[offset + i] = *byte;
        }
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

    pub fn truncate(&mut self, len: usize) {
        self.0.truncate(len);
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

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut offset = 0;

        while let Some(instruction) = self.0.get(offset) {
            let opcode = Opcode::from(*instruction);
            let definition = opcode.definition();

            let (operands, read_count) = definition.read_operands(&self.0[offset + 1..]);

            writeln!(
                f,
                "{} {}",
                format!("{:04}", offset).blue(),
                format_instruction(&definition, &operands)
            )?;

            offset += 1 + read_count;
        }

        Ok(())
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
        1 => format!("{} {}", definition.name, operands[0].to_string().blue()),
        _ => format!("ERROR: unhandled operand_count for {}", definition.name),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestCase {
        opcode: Opcode,
        operands: Vec<u16>,
        expected: Vec<u8>,
    }

    #[test]
    fn test_make() {
        let tests: Vec<TestCase> = vec![
            TestCase {
                opcode: Opcode::Constant,
                operands: vec![0],
                expected: vec![Opcode::Constant.into(), 0, 0],
            },
            TestCase {
                opcode: Opcode::Constant,
                operands: vec![1],
                expected: vec![Opcode::Constant.into(), 0, 1],
            },
            TestCase {
                opcode: Opcode::Constant,
                operands: vec![65534],
                expected: vec![Opcode::Constant.into(), 255, 254],
            },
            TestCase {
                opcode: Opcode::Constant,
                operands: vec![65535],
                expected: vec![Opcode::Constant.into(), 255, 255],
            },
            TestCase {
                opcode: Opcode::Add,
                operands: vec![],
                expected: vec![Opcode::Add.into()],
            },
        ];

        for test in tests {
            let opcode = test.opcode;
            let instruction = make!(opcode, &test.operands);
            assert_eq!(instruction, test.expected);
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make!(Opcode::Add),
            make!(Opcode::Constant, 1),
            make!(Opcode::Constant, 2),
            make!(Opcode::Constant, 65535),
            make!(Opcode::Sub),
            make!(Opcode::Mul),
            make!(Opcode::Div),
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
            let instruction = make!(opcode, &operands);
            let definition = opcode.definition();

            let (operands_read, n) = definition.read_operands(&instruction.0[1..]);
            assert_eq!(n, expected_bytes_read);

            for (i, operand) in operands.iter().enumerate() {
                assert_eq!(operands_read[i], *operand);
            }
        }
    }
}
