#[derive(Default, Clone)]
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

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn extend(&mut self, instructions: Instructions) {
        self.0.extend(instructions.0);
    }

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

#[derive(Clone, Copy)]
pub enum Opcode {
    Constant = 1,
}

pub struct Definition {
    name: String,
    operand_widths: Vec<usize>,
}

impl Definition {
    pub fn size(&self) -> usize {
        let base_size = 1;
        self.operand_widths.iter().sum::<usize>() + base_size
    }

    pub fn read_operands(&self, instructions: &[u8]) -> (Vec<u16>, usize) {
        let mut operands = Vec::new();
        let mut offset = 0;

        for width in self.operand_widths.iter() {
            match width {
                2 => {
                    let operand =
                        u16::from_be_bytes([instructions[offset], instructions[offset + 1]]);
                    operands.push(operand);
                }
                _ => panic!("Unhandled operand width"),
            }
            offset += width;
        }

        (operands, offset)
    }
}

impl Opcode {
    fn definition(&self) -> Definition {
        match self {
            Opcode::Constant => Definition {
                name: "Constant".to_string(),
                operand_widths: vec![2],
            },
        }
    }
}

impl From<u8> for Opcode {
    fn from(opcode: u8) -> Self {
        match opcode {
            1 => Opcode::Constant,
            _ => panic!("Opcode not found"),
        }
    }
}

pub fn make(op: Opcode, operands: &[u16]) -> Instructions {
    let definition = op.definition();
    let mut instruction = Vec::with_capacity(definition.size());

    instruction.push(op as u8);

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

    #[test]
    fn test_constant() {
        let instructions = vec![
            (make(Opcode::Constant, &[0]), vec![1, 0, 0]),
            (make(Opcode::Constant, &[1]), vec![1, 0, 1]),
            (make(Opcode::Constant, &[65534]), vec![1, 255, 254]),
        ];

        for (instruction, expected) in instructions {
            assert_eq!(instruction.0, expected);
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Opcode::Constant, &[1]),
            make(Opcode::Constant, &[2]),
            make(Opcode::Constant, &[65535]),
        ];

        let expected = "0000 Constant 1
0003 Constant 2
0006 Constant 65535
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
