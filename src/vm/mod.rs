pub mod error;

use self::error::{Result, VirtualMachineError};
use crate::{
    code::{Instructions, Opcode},
    compiler::Bytecode,
    object::Object,
};

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

    pub fn run(&mut self) -> Result {
        let mut instruction_pointer = 0;

        while instruction_pointer < self.instructions.len() {
            let instruction_opcode = self.instructions.get(instruction_pointer);
            instruction_pointer += 1;

            let opcode = Opcode::from(instruction_opcode);
            match opcode {
                Opcode::Constant => {
                    let const_index = u16::from_be_bytes([
                        self.instructions.get(instruction_pointer),
                        self.instructions.get(instruction_pointer + 1),
                    ]);

                    instruction_pointer += 2;
                    self.push(self.constants[const_index as usize].clone())?;
                }
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    self.execute_binary_operation(opcode)?;
                }
                Opcode::Pop => {
                    self.pop()?;
                }
            }
        }
        Ok(())
    }

    pub fn last_popped_stack_elem(&mut self) -> Object {
        self.stack[self.stack_pointer].clone()
    }

    fn push(&mut self, object: Object) -> Result {
        if self.stack_pointer >= STACK_SIZE {
            return Err(VirtualMachineError::StackOverflow);
        }

        self.stack[self.stack_pointer] = object;
        self.stack_pointer += 1;

        Ok(())
    }

    fn pop(&mut self) -> Result<Object> {
        if self.stack_pointer == 0 {
            return Err(VirtualMachineError::StackUnderflow);
        }

        let object = self.stack[self.stack_pointer - 1].clone();
        self.stack_pointer -= 1;

        Ok(object)
    }

    #[allow(dead_code)]
    fn stack_top(&self) -> Object {
        if self.stack_pointer == 0 {
            return Object::Null;
        }

        match self.stack.get(self.stack_pointer - 1) {
            Some(object) => object.clone(),
            None => Object::Null, // TODO: Return an error instead (?)
        }
    }

    fn execute_binary_operation(&mut self, opcode: Opcode) -> Result<Object> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (&left, &right) {
            (Object::Integer(left), Object::Integer(right)) => match opcode {
                Opcode::Add => self.push(Object::Integer(left + right))?,
                Opcode::Sub => self.push(Object::Integer(left - right))?,
                Opcode::Mul => self.push(Object::Integer(left * right))?,
                Opcode::Div => self.push(Object::Integer(left / right))?,
                _ => return Err(VirtualMachineError::UnknownIntegerOperator(opcode)),
            },
            _ => return Err(VirtualMachineError::UnsupportedAddition(left, right)),
        };

        Ok(Object::Null)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::compile_from_source;

    struct VMTestCase {
        input: String,
        expected: i64,
    }

    impl VMTestCase {
        fn new(input: &str, expected: i64) -> Self {
            VMTestCase {
                input: input.to_string(),
                expected,
            }
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<VMTestCase> = vec![
            VMTestCase::new("1", 1),
            VMTestCase::new("2", 2),
            VMTestCase::new("1 + 2", 3),
            VMTestCase::new("1 + 2 + 6", 9),
            VMTestCase::new("1 - 2", -1),
            VMTestCase::new("1 * 2", 2),
            VMTestCase::new("4 / 2", 2),
            VMTestCase::new("50 / 2 * 2 + 10 - 5", 55),
            VMTestCase::new("5 + 5 + 5 + 5 - 10", 10),
            VMTestCase::new("2 * 2 * 2 * 2 * 2", 32),
            VMTestCase::new("5 * 2 + 10", 20),
            VMTestCase::new("5 + 2 * 10", 25),
            VMTestCase::new("5 * (2 + 10)", 60),
        ];

        for test in tests {
            let compiler = compile_from_source(&test.input);
            let mut vm = VirtualMachine::new(compiler.bytecode());

            if let Err(error) = vm.run() {
                panic!("vm error: {}", error);
            }

            let output = vm.last_popped_stack_elem();
            assert_eq!(Object::Integer(test.expected), output);
        }
    }
}
