pub mod error;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use self::error::{Result, VirtualMachineError};
use crate::{
    code::{Instructions, Opcode},
    compiler::{Bytecode, Constants},
    object::{
        constants::{FALSE, NULL, TRUE},
        HashKey, Object,
    },
};

const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 65536;

pub type Globals = Vec<Object>;
pub fn create_globals_store() -> Globals {
    vec![NULL; GLOBAL_SIZE]
}

#[derive(Default)]
pub struct VirtualMachine {
    instructions: Instructions,
    constants: Constants,
    stack: Vec<Object>,
    stack_pointer: usize,
    globals: Rc<RefCell<Globals>>,
}

impl VirtualMachine {
    pub fn new(bytecode: Bytecode) -> Self {
        VirtualMachine {
            instructions: bytecode.instructions,
            constants: bytecode.constants.borrow().clone(),
            stack: vec![NULL; STACK_SIZE],
            globals: Rc::new(RefCell::new(create_globals_store())),
            stack_pointer: 0,
        }
    }

    pub fn new_with_globals_store(bytecode: Bytecode, globals: Rc<RefCell<Vec<Object>>>) -> Self {
        VirtualMachine {
            instructions: bytecode.instructions,
            constants: bytecode.constants.borrow().clone(),
            stack: vec![NULL; STACK_SIZE],
            globals,
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
                    let const_index = self.instructions.get_two_bytes(instruction_pointer);
                    instruction_pointer += 2;
                    self.push(self.constants[const_index].clone())?;
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
                    NULL => self.push(TRUE)?,
                    _ => self.push(FALSE)?,
                },
                Opcode::Minus => match self.pop()? {
                    Object::Integer(value) => self.push(Object::Integer(-value))?,
                    obj => return Err(VirtualMachineError::UnsupportedMinus(obj)),
                },
                Opcode::JumpNotTruthy => {
                    let jump_position = self.instructions.get_two_bytes(instruction_pointer);
                    instruction_pointer += 2;

                    let condition = self.pop()?;
                    if !condition.is_truthy() {
                        // TODO: look into why we don't have to subtract 1 here
                        instruction_pointer = jump_position;
                    }
                }
                Opcode::Jump => {
                    let jump_position = self.instructions.get_two_bytes(instruction_pointer);

                    // TODO: look into why we don't have to subtract 1 here
                    instruction_pointer = jump_position;
                }
                Opcode::Null => self.push(NULL)?,
                Opcode::GetGlobal => {
                    let global_index = self.instructions.get_two_bytes(instruction_pointer);
                    instruction_pointer += 2;

                    let globals_ref = self.globals.borrow();
                    let global_obj = globals_ref[global_index].clone();
                    drop(globals_ref);

                    self.push(global_obj)?;
                }
                Opcode::SetGlobal => {
                    let global_index = self.instructions.get_two_bytes(instruction_pointer);
                    let global_obj = self.pop()?;
                    instruction_pointer += 2;

                    let mut globals_ref = self.globals.borrow_mut();

                    globals_ref[global_index] = global_obj;
                }
                Opcode::Array => {
                    let num_elements = self.instructions.get_two_bytes(instruction_pointer);
                    instruction_pointer += 2;

                    let array =
                        self.build_array(self.stack_pointer - num_elements, self.stack_pointer)?;

                    self.stack_pointer -= num_elements;

                    self.push(array)?;
                }
                Opcode::Hash => {
                    let num_elements = self.instructions.get_two_bytes(instruction_pointer);
                    instruction_pointer += 2;

                    let hash =
                        self.build_hash(self.stack_pointer - num_elements, self.stack_pointer)?;

                    self.stack_pointer -= num_elements;

                    self.push(hash)?;
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
            (Object::String(left), Object::String(right)) => match opcode {
                Opcode::Add => self.push(Object::String(format!("{}{}", left, right)))?,

                // Opcode::Equal => self.push(Object::from(left == right))?,
                // Opcode::NotEqual => self.push(Object::from(left != right))?,
                _ => return Err(VirtualMachineError::UnknownStringOperator(opcode)),
            },

            _ => return Err(VirtualMachineError::UnsupportedAddition(left, right)),
        };

        Ok(NULL)
    }

    fn build_array(&self, from: usize, to: usize) -> Result<Object> {
        let mut elements = Vec::with_capacity(to - from);

        for i in from..to {
            elements.push(self.stack[i].clone());
        }

        Ok(Object::Array(elements))
    }

    fn build_hash(&self, from: usize, to: usize) -> Result<Object> {
        let mut hash: HashMap<HashKey, Object> = HashMap::with_capacity((to - from) / 2);

        for i in (from..to).step_by(2) {
            let key = self.stack[i].clone();
            let value = self.stack[i + 1].clone();

            let key = HashKey::from_obj(key.clone())
                .map_err(|_| VirtualMachineError::InvalidHashKey(key))?;
            hash.insert(key, value);
        }

        Ok(Object::Hash(hash))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{object::HashKey, test::compile_from_source};

    struct TestCase {
        input: String,
        expected: Object,
    }

    impl TestCase {
        fn new(input: &str, expected: Object) -> Self {
            TestCase {
                input: input.to_string(),
                expected,
            }
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            TestCase::new("1", Object::Integer(1)),
            TestCase::new("2", Object::Integer(2)),
            TestCase::new("1 + 2", Object::Integer(3)),
            TestCase::new("1 + 2 + 6", Object::Integer(9)),
            TestCase::new("1 - 2", Object::Integer(-1)),
            TestCase::new("1 * 2", Object::Integer(2)),
            TestCase::new("4 / 2", Object::Integer(2)),
            TestCase::new("50 / 2 * 2 + 10 - 5", Object::Integer(55)),
            TestCase::new("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            TestCase::new("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            TestCase::new("5 * 2 + 10", Object::Integer(20)),
            TestCase::new("5 + 2 * 10", Object::Integer(25)),
            TestCase::new("5 * (2 + 10)", Object::Integer(60)),
            TestCase::new("-5", Object::Integer(-5)),
            TestCase::new("-10", Object::Integer(-10)),
            TestCase::new("-50 + 100 + -50", Object::Integer(0)),
            TestCase::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            TestCase::new("true", TRUE),
            TestCase::new("false", FALSE),
            TestCase::new("1 < 2", TRUE),
            TestCase::new("1 > 2", FALSE),
            TestCase::new("1 < 1", FALSE),
            TestCase::new("1 > 1", FALSE),
            TestCase::new("1 == 1", TRUE),
            TestCase::new("1 != 1", FALSE),
            TestCase::new("1 == 2", FALSE),
            TestCase::new("1 != 2", TRUE),
            TestCase::new("true == true", TRUE),
            TestCase::new("false == false", TRUE),
            TestCase::new("true == false", FALSE),
            TestCase::new("true != false", TRUE),
            TestCase::new("(1 < 2) == true", TRUE),
            TestCase::new("(1 < 2) == false", FALSE),
            TestCase::new("(1 > 2) == true", FALSE),
            TestCase::new("(1 > 2) == false", TRUE),
            TestCase::new("!true", FALSE),
            TestCase::new("!false", TRUE),
            TestCase::new("!5", FALSE),
            TestCase::new("!!true", TRUE),
            TestCase::new("!!false", FALSE),
            TestCase::new("!!5", TRUE),
            TestCase::new("!(if (false) { 5; })", TRUE),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            TestCase::new("if (true) { 10 }", Object::Integer(10)),
            TestCase::new("if (true) { 10 } else { 20 }", Object::Integer(10)),
            TestCase::new("if (false) { 10 } else { 20 }", Object::Integer(20)),
            TestCase::new("if (1) { 10 }", Object::Integer(10)),
            TestCase::new("if (1 < 2) { 10 }", Object::Integer(10)),
            TestCase::new("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
            TestCase::new("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            TestCase::new("if (1 > 2) { 10 }", NULL),
            TestCase::new("if (false) { 10 }", NULL),
            TestCase::new(
                "if ((if (false) { 10 })) { 10 } else { 20 }",
                Object::Integer(20),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            TestCase::new("let one = 1; one", Object::Integer(1)),
            TestCase::new("let one = 1; let two = 2; one + two", Object::Integer(3)),
            TestCase::new(
                "let one = 1; let two = one + one; one + two",
                Object::Integer(3),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            TestCase::new(r#""monkey""#, Object::String("monkey".to_string())),
            TestCase::new(r#""mon" + "key""#, Object::String("monkey".to_string())),
            TestCase::new(
                r#""mon" + "key" + "banana""#,
                Object::String("monkeybanana".to_string()),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            TestCase::new("[]", Object::Array(vec![])),
            TestCase::new(
                "[1, 2, 3]",
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                ]),
            ),
            TestCase::new(
                "[1 + 2, 3 * 4, 5 + 6]",
                Object::Array(vec![
                    Object::Integer(3),
                    Object::Integer(12),
                    Object::Integer(11),
                ]),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            TestCase::new("{}", Object::Hash(HashMap::new())),
            TestCase::new(
                "{1: 2, 2: 3}",
                Object::Hash(HashMap::from([
                    (HashKey::Integer(1), Object::Integer(2)),
                    (HashKey::Integer(2), Object::Integer(3)),
                ])),
            ),
            TestCase::new(
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                Object::Hash(HashMap::from([
                    (HashKey::Integer(2), Object::Integer(4)),
                    (HashKey::Integer(6), Object::Integer(16)),
                ])),
            ),
        ];

        run_vm_tests(tests);
    }

    fn run_vm_tests(tests: Vec<TestCase>) {
        for test in tests {
            println!(" ");
            println!("test: {:?}", test.input);
            let compiler = compile_from_source(&test.input);
            let mut vm = VirtualMachine::new(compiler.bytecode());

            println!("bytecode: {:?}", compiler.bytecode());
            if let Err(error) = vm.run() {
                panic!("vm error: {}", error);
            }

            let output = vm.last_popped_stack_elem();
            assert_eq!(test.expected, output);
        }
    }
}
