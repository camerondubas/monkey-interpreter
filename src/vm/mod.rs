pub mod error;
use self::error::{Result, VirtualMachineError};
use crate::{
    code::{Instructions, Opcode},
    compiler::Bytecode,
    object::{
        constants::{FALSE, NULL, TRUE},
        Object,
    },
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
            stack: vec![NULL; STACK_SIZE],
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
                Opcode::Add
                | Opcode::Sub
                | Opcode::Mul
                | Opcode::Div
                | Opcode::GreaterThan
                | Opcode::Equal
                | Opcode::NotEqual => {
                    self.execute_binary_operation(opcode)?;
                }
                Opcode::True => {
                    self.push(TRUE)?;
                }
                Opcode::False => {
                    self.push(FALSE)?;
                }
                Opcode::Pop => {
                    self.pop()?;
                }
                Opcode::Bang => match self.pop()? {
                    TRUE => self.push(FALSE)?,
                    FALSE => self.push(TRUE)?,
                    _ => self.push(FALSE)?,
                },
                Opcode::Minus => match self.pop()? {
                    Object::Integer(value) => self.push(Object::Integer(-value))?,
                    obj => return Err(VirtualMachineError::UnsupportedMinus(obj)),
                },
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
            return NULL;
        }

        match self.stack.get(self.stack_pointer - 1) {
            Some(object) => object.clone(),
            None => NULL, // TODO: Return an error instead (?)
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

                Opcode::GreaterThan => self.push(Object::from(left > right))?,
                Opcode::Equal => self.push(Object::from(left == right))?,
                Opcode::NotEqual => self.push(Object::from(left != right))?,

                _ => return Err(VirtualMachineError::UnknownIntegerOperator(opcode)),
            },
            (Object::Boolean(left), Object::Boolean(right)) => match opcode {
                Opcode::Equal => self.push(Object::from(left == right))?,
                Opcode::NotEqual => self.push(Object::from(left != right))?,

                _ => return Err(VirtualMachineError::UnknownBooleanOperator(opcode)),
            },

            _ => return Err(VirtualMachineError::UnsupportedAddition(left, right)),
        };

        Ok(NULL)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::compile_from_source;

    struct VMTestCase<T> {
        input: String,
        expected: T,
    }

    impl<T> VMTestCase<T> {
        fn new(input: &str, expected: T) -> Self {
            VMTestCase {
                input: input.to_string(),
                expected,
            }
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<VMTestCase<i64>> = vec![
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
            VMTestCase::new("-5", -5),
            VMTestCase::new("-10", -10),
            VMTestCase::new("-50 + 100 + -50", 0),
            VMTestCase::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
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

    #[test]
    fn test_boolean_expressions() {
        let tests: Vec<VMTestCase<bool>> = vec![
            VMTestCase::new("true", true),
            VMTestCase::new("false", false),
            VMTestCase::new("1 < 2", true),
            VMTestCase::new("1 > 2", false),
            VMTestCase::new("1 < 1", false),
            VMTestCase::new("1 > 1", false),
            VMTestCase::new("1 == 1", true),
            VMTestCase::new("1 != 1", false),
            VMTestCase::new("1 == 2", false),
            VMTestCase::new("1 != 2", true),
            VMTestCase::new("true == true", true),
            VMTestCase::new("false == false", true),
            VMTestCase::new("true == false", false),
            VMTestCase::new("true != false", true),
            VMTestCase::new("(1 < 2) == true", true),
            VMTestCase::new("(1 < 2) == false", false),
            VMTestCase::new("(1 > 2) == true", false),
            VMTestCase::new("(1 > 2) == false", true),
            VMTestCase::new("!true", false),
            VMTestCase::new("!false", true),
            VMTestCase::new("!5", false),
            VMTestCase::new("!!true", true),
            VMTestCase::new("!!false", false),
            VMTestCase::new("!!5", true),
        ];

        for test in tests {
            println!("test: {:?}", test.input);
            let compiler = compile_from_source(&test.input);
            let mut vm = VirtualMachine::new(compiler.bytecode());

            if let Err(error) = vm.run() {
                panic!("vm error: {}", error);
            }

            let output = vm.last_popped_stack_elem();
            assert_eq!(Object::Boolean(test.expected), output);
        }
    }
}
