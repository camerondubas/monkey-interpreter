pub mod error;
pub mod symbol_table;

use std::{cell::RefCell, rc::Rc};

pub type Constants = Vec<Object>;

use crate::{
    ast::{Expression, Program, Statement},
    code::{make, Instructions, Opcode},
    lexer::Token,
    object::Object,
};

use self::{
    error::{CompilerError, CompilerResult},
    symbol_table::SymbolTable,
};

#[derive(Default, Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Rc<RefCell<Constants>>,
}

#[derive(Debug, Clone)]
struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

#[derive(Default)]
pub struct Compiler {
    instructions: Instructions,
    constants: Rc<RefCell<Constants>>,
    symbol_table: Rc<RefCell<SymbolTable>>,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler::default()
    }

    pub fn new_with_state(
        symbol_table: Rc<RefCell<SymbolTable>>,
        constants: Rc<RefCell<Vec<Object>>>,
    ) -> Self {
        Compiler {
            constants,
            symbol_table,
            ..Default::default()
        }
    }

    pub fn compile(&mut self, program: Program) -> CompilerResult {
        for statement in program.statements {
            self.compile_statement(statement)?;
        }

        Ok(())
    }

    fn compile_statement(&mut self, statement: Statement) -> CompilerResult {
        match statement {
            Statement::Expression(expression) => {
                self.compile_expression(expression)?;
                self.emit(Opcode::Pop, &[]);
            }
            Statement::Block(statements) => {
                for statement in statements {
                    self.compile_statement(statement)?;
                }
            }
            Statement::Let(identifier, expression) => {
                self.compile_expression(expression)?;
                let symbol = self.symbol_table.borrow_mut().define(identifier.as_str());
                self.emit(Opcode::SetGlobal, &[symbol.index as u16]);
            }
            _ => return Err(CompilerError::UnhandledStatement(statement)),
        };

        Ok(())
    }

    fn compile_expression(&mut self, expression: Expression) -> CompilerResult {
        match expression {
            Expression::InfixExpression(left, operator, right) => {
                self.compile_infix(*left, operator, *right)?;
            }
            Expression::IntegerLiteral(value) => {
                let id = self.add_constant(Object::Integer(value));
                self.emit(Opcode::Constant, &[id]);
            }
            Expression::Boolean(value) => {
                if value {
                    self.emit(Opcode::True, &[]);
                } else {
                    self.emit(Opcode::False, &[]);
                }
            }
            Expression::PrefixExpression(token, expression) => {
                self.compile_expression(*expression)?;
                match token {
                    Token::Bang => self.emit(Opcode::Bang, &[]),
                    Token::Minus => self.emit(Opcode::Minus, &[]),
                    _ => return Err(CompilerError::UnknownPrefixOperator(token)),
                };
            }
            Expression::If(condition, consequence, alternative) => {
                self.compile_if(*condition, *consequence, alternative)?;
            }
            Expression::Identifier(identifier) => {
                let symbol = self
                    .symbol_table
                    .borrow_mut()
                    .resolve(identifier.as_str())
                    .ok_or(CompilerError::UndefinedVariable(identifier.clone()))?;

                self.emit(Opcode::GetGlobal, &[symbol.index as u16]);
            }
            Expression::StringLiteral(value) => {
                let id = self.add_constant(Object::String(value));
                self.emit(Opcode::Constant, &[id]);
            }
            Expression::ArrayLiteral(elements) => {
                let len = elements.len();
                for element in elements {
                    self.compile_expression(element)?;
                }

                self.emit(Opcode::Array, &[len as u16]);
            }
            _ => return Err(CompilerError::UnhandledExpression(expression)),
        }

        Ok(())
    }

    fn compile_infix(
        &mut self,
        left: Expression,
        operator: Token,
        right: Expression,
    ) -> CompilerResult {
        if let Token::Lt = operator {
            self.compile_expression(right)?;
            self.compile_expression(left)?;
            self.emit(Opcode::GreaterThan, &[]);
            return Ok(());
        }

        self.compile_expression(left)?;
        self.compile_expression(right)?;

        match operator {
            Token::Plus => self.emit(Opcode::Add, &[]),
            Token::Minus => self.emit(Opcode::Sub, &[]),
            Token::Asterisk => self.emit(Opcode::Mul, &[]),
            Token::Slash => self.emit(Opcode::Div, &[]),
            Token::Eq => self.emit(Opcode::Equal, &[]),
            Token::NotEq => self.emit(Opcode::NotEqual, &[]),
            Token::Gt => self.emit(Opcode::GreaterThan, &[]),
            _ => return Err(CompilerError::UnknownInfixOperator(operator)),
        };

        Ok(())
    }

    fn compile_if(
        &mut self,
        condition: Expression,
        consequence: Statement,
        alternative: Option<Box<Statement>>,
    ) -> CompilerResult {
        self.compile_expression(condition)?;

        // We use a temp position until we compile the consequence
        // to know how far to jump. TODO: is there a better way to do this?
        let temp_position = 9999;
        let jump_not_truthy_position = self.emit(Opcode::JumpNotTruthy, &[temp_position]);

        self.compile_statement(consequence)?;

        if self.last_instruction_is(Opcode::Pop) {
            self.remove_last_pop();
        }

        let jump_position = self.emit(Opcode::Jump, &[temp_position]);

        let after_consequence_position = self.instructions.len();
        self.change_operand(jump_not_truthy_position, after_consequence_position as u16);

        if let Some(alternative) = alternative {
            self.compile_statement(*alternative)?;

            if self.last_instruction_is(Opcode::Pop) {
                self.remove_last_pop();
            }
        } else {
            self.emit(Opcode::Null, &[]);
        }

        let after_alternative_position = self.instructions.len();
        self.change_operand(jump_position, after_alternative_position as u16);

        Ok(())
    }

    fn last_instruction_is(&self, opcode: Opcode) -> bool {
        if let Some(last_instruction) = &self.last_instruction {
            last_instruction.opcode == opcode
        } else {
            false
        }
    }

    fn remove_last_pop(&mut self) {
        if let Some(last_instruction) = &self.last_instruction {
            self.instructions.truncate(last_instruction.position);
            self.last_instruction = self.previous_instruction.clone();
        }
    }

    fn replace_instruction(&mut self, position: usize, new_instruction: Instructions) {
        self.instructions.set(position, new_instruction)
    }

    fn change_operand(&mut self, position: usize, operand: u16) {
        // this assumes we only change operands for instructions of the same type
        // it may be worth looking into a more robust solution, leaning into rust
        // types / structs / enums
        let opcode = Opcode::from(self.instructions.get(position));
        let new_instruction = make!(opcode, &[operand]);

        self.replace_instruction(position, new_instruction);
    }

    fn emit(&mut self, opcode: Opcode, operands: &[u16]) -> usize {
        let instruction = make!(opcode, operands);
        let position = self.add_instruction(instruction);

        self.set_last_instruction(opcode, position);

        position
    }

    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        self.previous_instruction = self.last_instruction.clone();
        self.last_instruction = Some(EmittedInstruction { opcode, position });
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: Rc::clone(&self.constants),
        }
    }

    fn add_constant(&mut self, integer: Object) -> u16 {
        self.constants.borrow_mut().push(integer);
        (self.constants.borrow_mut().len() - 1) as u16
    }

    fn add_instruction(&mut self, instruction: Instructions) -> usize {
        let position = self.instructions.len();
        self.instructions.extend(instruction);
        position
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::{make, Opcode},
        test::compile_from_source,
    };

    use super::*;

    struct TestCase {
        input: String,
        expected_constants: Vec<Object>,
        expected_instructions: Vec<Instructions>,
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            TestCase {
                input: "1 + 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::Add),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "1; 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Pop),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "((1 + 8 - 1) / 2) * 3".to_string(),
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(8),
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                ],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::Add),
                    make!(Opcode::Constant, 2),
                    make!(Opcode::Sub),
                    make!(Opcode::Constant, 3),
                    make!(Opcode::Div),
                    make!(Opcode::Constant, 4),
                    make!(Opcode::Mul),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "-1".to_string(),
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Minus),
                    make!(Opcode::Pop),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            TestCase {
                input: "true".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![make!(Opcode::True), make!(Opcode::Pop)],
            },
            TestCase {
                input: "false".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![make!(Opcode::False), make!(Opcode::Pop)],
            },
            TestCase {
                input: "1 > 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::GreaterThan),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "1 < 2".to_string(),
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::GreaterThan),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "1 == 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::Equal),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "1 != 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::NotEqual),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "true == false".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    make!(Opcode::True),
                    make!(Opcode::False),
                    make!(Opcode::Equal),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "true != false".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    make!(Opcode::True),
                    make!(Opcode::False),
                    make!(Opcode::NotEqual),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "!true".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![
                    make!(Opcode::True),
                    make!(Opcode::Bang),
                    make!(Opcode::Pop),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_conditionals() {
        let tests: Vec<TestCase> = vec![
            TestCase {
                input: "if (true) { 10 }; 3333;".to_string(),
                expected_constants: vec![Object::Integer(10), Object::Integer(3333)],
                expected_instructions: vec![
                    // 0000
                    make!(Opcode::True),
                    // 0001
                    make!(Opcode::JumpNotTruthy, 10),
                    // 0004
                    make!(Opcode::Constant, 0),
                    // 0007
                    make!(Opcode::Jump, 11),
                    // 0010
                    make!(Opcode::Null),
                    // 0011
                    make!(Opcode::Pop),
                    // 0012
                    make!(Opcode::Constant, 1),
                    // 0015
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "if (true) { 10 } else { 20 }; 3333".to_string(),
                expected_constants: vec![
                    Object::Integer(10),
                    Object::Integer(20),
                    Object::Integer(3333),
                ],
                expected_instructions: vec![
                    // 0000
                    make!(Opcode::True),
                    // 0001
                    make!(Opcode::JumpNotTruthy, 10),
                    // 0004
                    make!(Opcode::Constant, 0),
                    // 0007
                    make!(Opcode::Jump, 13),
                    // 0010
                    make!(Opcode::Constant, 1),
                    // 0013
                    make!(Opcode::Pop),
                    // 0014
                    make!(Opcode::Constant, 2),
                    // 0017
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "if (false) { 10 } else { 20 }; 3333".to_string(),
                expected_constants: vec![
                    Object::Integer(10),
                    Object::Integer(20),
                    Object::Integer(3333),
                ],
                expected_instructions: vec![
                    // 0000
                    make!(Opcode::False),
                    // 0001
                    make!(Opcode::JumpNotTruthy, 10),
                    // 0004
                    make!(Opcode::Constant, 0),
                    // 0007
                    make!(Opcode::Jump, 13),
                    // 0010
                    make!(Opcode::Constant, 1),
                    // 0013
                    make!(Opcode::Pop),
                    // 0014
                    make!(Opcode::Constant, 2),
                    // 0017
                    make!(Opcode::Pop),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            TestCase {
                input: "let one = 1; let two = 2;".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::SetGlobal, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::SetGlobal, 1),
                ],
            },
            TestCase {
                input: "let one = 1; one;".to_string(),
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::SetGlobal, 0),
                    make!(Opcode::GetGlobal, 0),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "let one = 1; let two = one; two;".to_string(),
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::SetGlobal, 0),
                    make!(Opcode::GetGlobal, 0),
                    make!(Opcode::SetGlobal, 1),
                    make!(Opcode::GetGlobal, 1),
                    make!(Opcode::Pop),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            TestCase {
                input: r#""monkey""#.to_string(),
                expected_constants: vec![Object::String("monkey".to_string())],
                expected_instructions: vec![make!(Opcode::Constant, 0), make!(Opcode::Pop)],
            },
            TestCase {
                input: r#""mon" + "key""#.to_string(),
                expected_constants: vec![
                    Object::String("mon".to_string()),
                    Object::String("key".to_string()),
                ],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::Add),
                    make!(Opcode::Pop),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            TestCase {
                input: "[]".to_string(),
                expected_constants: vec![],
                expected_instructions: vec![make!(Opcode::Array, 0), make!(Opcode::Pop)],
            },
            TestCase {
                input: "[1,2,3]".to_string(),
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                ],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::Constant, 2),
                    make!(Opcode::Array, 3),
                    make!(Opcode::Pop),
                ],
            },
            TestCase {
                input: "[1 + 2, 3 - 4, 5 * 6]".to_string(),
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                expected_instructions: vec![
                    make!(Opcode::Constant, 0),
                    make!(Opcode::Constant, 1),
                    make!(Opcode::Add),
                    make!(Opcode::Constant, 2),
                    make!(Opcode::Constant, 3),
                    make!(Opcode::Sub),
                    make!(Opcode::Constant, 4),
                    make!(Opcode::Constant, 5),
                    make!(Opcode::Mul),
                    make!(Opcode::Array, 3),
                    make!(Opcode::Pop),
                ],
            },
        ];

        run_compiler_tests(tests);
    }

    fn run_compiler_tests(tests: Vec<TestCase>) {
        for test in tests {
            println!("test: {}", test.input);
            let compiler = compile_from_source(&test.input);
            let bytecode = compiler.bytecode();

            let expected = Instructions::from(test.expected_instructions);
            assert_eq!(
                expected.print(),
                bytecode.instructions.print(),
                "instructions"
            );
            assert_eq!(
                test.expected_constants,
                *bytecode.constants.borrow(),
                "constants"
            );
        }
    }
}
