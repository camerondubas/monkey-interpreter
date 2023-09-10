use super::{Instructions, Opcode};

#[macro_export]
macro_rules! make {
    ($opcode:path) => {
        $crate::code::make::make_instruction($opcode, &[])
    };
    ($opcode:path, $($operands:literal),*) => {
        $crate::code::make::make_instruction($opcode, &[$($operands),*])
    };
    ($opcode:path, $operands:expr) => {
        $crate::code::make::make_instruction($opcode, $operands)
    };
}

pub(crate) use make;

pub(crate) fn make_instruction(op: Opcode, operands: &[u16]) -> Instructions {
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
