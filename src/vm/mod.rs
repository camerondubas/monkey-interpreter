use crate::{
    code::{Instructions, Opcode},
    compiler::Bytecode,
    object::Object,
};
use std::{fmt::Display, result::Result};

pub type VirtualMachineResult = Result<(), VirtualMachineError>;
pub enum VirtualMachineError {
    StackOverflow,
}

impl Display for VirtualMachineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            VirtualMachineError::StackOverflow => write!(f, "Stack overflow"),
        }
    }
}

const STACK_SIZE: usize = 2048;

#[derive(Default)]
pub struct VirtualMachine {
    instructions: Instructions,
    constants: Vec<Object>,
    stack: Vec<Object>,
    stack_pointer: usize,
}

impl VirtualMachine {
    pub fn new(bytecode: Bytecode) -> Self {
        VirtualMachine {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            stack: vec![Object::Null; STACK_SIZE],
            stack_pointer: 0,
        }
    }

    pub fn run(&mut self) -> VirtualMachineResult {
        let mut instruction_pointer = 0;

        while instruction_pointer < self.instructions.len() {
            let instruction_opcode = self.instructions.get(instruction_pointer);
            instruction_pointer += 1;

            match Opcode::from(instruction_opcode) {
                Opcode::Constant => {
                    let const_index = u16::from_be_bytes([
                        self.instructions.get(instruction_pointer),
                        self.instructions.get(instruction_pointer + 1),
                    ]);

                    instruction_pointer += 2;
                    self.push(self.constants[const_index as usize].clone())?;
                }
            }
        }
        Ok(())
    }

    pub fn push(&mut self, object: Object) -> VirtualMachineResult {
        if self.stack_pointer >= STACK_SIZE {
            return Err(VirtualMachineError::StackOverflow);
        }

        self.stack[self.stack_pointer] = object;
        self.stack_pointer += 1;

        Ok(())
    }

    pub fn stack_top(&self) -> Object {
        if self.stack_pointer == 0 {
            return Object::Null;
        }

        match self.stack.get(self.stack_pointer - 1) {
            Some(object) => object.clone(),
            None => Object::Null, // TODO: Return an error instead (?)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{compiler::Compiler, parser::Parser};

    struct VMTestCase {
        input: String,
        expected: i64,
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<VMTestCase> = vec![
            VMTestCase {
                input: "1".to_string(),
                expected: 1,
            },
            VMTestCase {
                input: "2".to_string(),
                expected: 2,
            },
            VMTestCase {
                input: "1 + 2".to_string(),
                expected: 2,
            },
        ];

        for test in tests {
            let program = Parser::from_source(&test.input).parse_program();
            let mut compiler = Compiler::new();
            assert!(compiler.compile(program).is_ok());

            let mut vm = VirtualMachine::new(compiler.bytecode());
            assert!(vm.run().is_ok());

            let stack_elem = vm.stack_top();

            assert_eq!(Object::Integer(test.expected), stack_elem, "stack_elem");
        }
    }
}
