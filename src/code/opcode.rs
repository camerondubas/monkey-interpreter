// Desc: Opcode definitions for the VM
// Each opcode must have a unique byte value
const CONSTANT: u8 = 1;
const ADD: u8 = 2;
const SUB: u8 = 3;
const MUL: u8 = 4;
const DIV: u8 = 5;
const POP: u8 = 6;
const TRUE: u8 = 7;
const FALSE: u8 = 8;
const EQUAL: u8 = 9;
const NOT_EQUAL: u8 = 10;
const GREATER_THAN: u8 = 11;
const MINUS: u8 = 12;
const BANG: u8 = 13;

#[derive(Clone, Copy, Debug)]
pub enum Opcode {
    Constant,

    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,

    Pop,

    // Booleans
    True,
    False,

    // Comparison operators
    Equal,
    NotEqual,
    GreaterThan,

    // Prefix operators
    Minus,
    Bang,
}

impl Opcode {
    pub(super) fn definition(&self) -> Definition {
        match self {
            Opcode::Constant => Definition::new("Constant", vec![2]),

            Opcode::Add => Definition::new("Add", vec![]),
            Opcode::Sub => Definition::new("Sub", vec![]),
            Opcode::Mul => Definition::new("Mul", vec![]),
            Opcode::Div => Definition::new("Div", vec![]),

            Opcode::Pop => Definition::new("Pop", vec![]),

            Opcode::True => Definition::new("True", vec![]),
            Opcode::False => Definition::new("False", vec![]),

            Opcode::Equal => Definition::new("Equal", vec![]),
            Opcode::NotEqual => Definition::new("NotEqual", vec![]),
            Opcode::GreaterThan => Definition::new("GreaterThan", vec![]),

            Opcode::Minus => Definition::new("Minus", vec![]),
            Opcode::Bang => Definition::new("Bang", vec![]),
        }
    }
}

impl From<Opcode> for u8 {
    fn from(opcode: Opcode) -> Self {
        match opcode {
            Opcode::Constant => CONSTANT,

            Opcode::Add => ADD,
            Opcode::Sub => SUB,
            Opcode::Mul => MUL,
            Opcode::Div => DIV,

            Opcode::Pop => POP,

            Opcode::True => TRUE,
            Opcode::False => FALSE,

            Opcode::Equal => EQUAL,
            Opcode::NotEqual => NOT_EQUAL,
            Opcode::GreaterThan => GREATER_THAN,

            Opcode::Minus => MINUS,
            Opcode::Bang => BANG,
        }
    }
}

impl From<u8> for Opcode {
    fn from(opcode: u8) -> Self {
        match opcode {
            CONSTANT => Opcode::Constant,

            ADD => Opcode::Add,
            SUB => Opcode::Sub,
            MUL => Opcode::Mul,
            DIV => Opcode::Div,

            POP => Opcode::Pop,

            TRUE => Opcode::True,
            FALSE => Opcode::False,

            EQUAL => Opcode::Equal,
            NOT_EQUAL => Opcode::NotEqual,
            GREATER_THAN => Opcode::GreaterThan,

            MINUS => Opcode::Minus,
            BANG => Opcode::Bang,

            _ => panic!("Opcode cannot be made from u8:{}.", opcode),
        }
    }
}

pub struct Definition {
    pub name: String,
    pub operand_widths: Vec<usize>,
}

impl Definition {
    pub fn new(name: &str, operand_widths: Vec<usize>) -> Self {
        Definition {
            name: name.to_string(),
            operand_widths,
        }
    }

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