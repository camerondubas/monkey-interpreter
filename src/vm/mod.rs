pub mod error;
use self::error::{Result, VirtualMachineError};
use crate::{
    code::{Instructions, Opcode},
    compiler::Bytecode,
    object::{
        constants::{FALSE, TRUE},
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

                Opcode::GreaterThan => self.push(self.to_bool_obj(left > right))?,
                Opcode::Equal => self.push(self.to_bool_obj(left == right))?,
                Opcode::NotEqual => self.push(self.to_bool_obj(left != right))?,

                _ => return Err(VirtualMachineError::UnknownIntegerOperator(opcode)),
            },
            (Object::Boolean(left), Object::Boolean(right)) => match opcode {
                Opcode::Equal => self.push(self.to_bool_obj(left == right))?,
                Opcode::NotEqual => self.push(self.to_bool_obj(left != right))?,

                _ => return Err(VirtualMachineError::UnknownBooleanOperator(opcode)),
            },

            _ => return Err(VirtualMachineError::UnsupportedAddition(left, right)),
        };

        Ok(Object::Null)
    }

    fn to_bool_obj(&self, value: bool) -> Object {
        if value {
            TRUE
        } else {
            FALSE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::compile_from_source;

    enum Output {
        Integer(i64),
        Boolean(bool),
    }

    struct VMTestCase {
        input: String,
        expected: Output,
    }

    impl VMTestCase {
        fn int(input: &str, expected: i64) -> Self {
            VMTestCase {
                input: input.to_string(),
                expected: Output::Integer(expected),
            }
        }

        fn bool(input: &str, expected: bool) -> Self {
            VMTestCase {
                input: input.to_string(),
                expected: Output::Boolean(expected),
            }
        }
    }

    impl From<Output> for Object {
        fn from(output: Output) -> Self {
            match output {
                Output::Integer(value) => Object::Integer(value),
                Output::Boolean(value) => Object::Boolean(value),
            }
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<VMTestCase> = vec![
            VMTestCase::int("1", 1),
            VMTestCase::int("2", 2),
            VMTestCase::int("1 + 2", 3),
            VMTestCase::int("1 + 2 + 6", 9),
            VMTestCase::int("1 - 2", -1),
            VMTestCase::int("1 * 2", 2),
            VMTestCase::int("4 / 2", 2),
            VMTestCase::int("50 / 2 * 2 + 10 - 5", 55),
            VMTestCase::int("5 + 5 + 5 + 5 - 10", 10),
            VMTestCase::int("2 * 2 * 2 * 2 * 2", 32),
            VMTestCase::int("5 * 2 + 10", 20),
            VMTestCase::int("5 + 2 * 10", 25),
            VMTestCase::int("5 * (2 + 10)", 60),
            VMTestCase::int("-5", -5),
            VMTestCase::int("-10", -10),
            VMTestCase::int("-50 + 100 + -50", 0),
            VMTestCase::int("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for test in tests {
            let compiler = compile_from_source(&test.input);
            let mut vm = VirtualMachine::new(compiler.bytecode());

            if let Err(error) = vm.run() {
                panic!("vm error: {}", error);
            }

            let output = vm.last_popped_stack_elem();
            assert_eq!(Object::from(test.expected), output);
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let tests: Vec<VMTestCase> = vec![
            VMTestCase::bool("true", true),
            VMTestCase::bool("false", false),
            VMTestCase::bool("1 < 2", true),
            VMTestCase::bool("1 > 2", false),
            VMTestCase::bool("1 < 1", false),
            VMTestCase::bool("1 > 1", false),
            VMTestCase::bool("1 == 1", true),
            VMTestCase::bool("1 != 1", false),
            VMTestCase::bool("1 == 2", false),
            VMTestCase::bool("1 != 2", true),
            VMTestCase::bool("true == true", true),
            VMTestCase::bool("false == false", true),
            VMTestCase::bool("true == false", false),
            VMTestCase::bool("true != false", true),
            VMTestCase::bool("(1 < 2) == true", true),
            VMTestCase::bool("(1 < 2) == false", false),
            VMTestCase::bool("(1 > 2) == true", false),
            VMTestCase::bool("(1 > 2) == false", true),
            VMTestCase::bool("!true", false),
            VMTestCase::bool("!false", true),
            VMTestCase::bool("!5", false),
            VMTestCase::bool("!!true", true),
            VMTestCase::bool("!!false", false),
            VMTestCase::bool("!!5", true),
        ];

        for test in tests {
            println!("test: {:?}", test.input);
            let compiler = compile_from_source(&test.input);
            let mut vm = VirtualMachine::new(compiler.bytecode());

            if let Err(error) = vm.run() {
                panic!("vm error: {}", error);
            }

            let output = vm.last_popped_stack_elem();
            assert_eq!(Object::from(test.expected), output);
        }
    }
}
