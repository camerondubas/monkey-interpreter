pub mod error;

use crate::{
    ast::{Expression, Program, Statement},
    code::{make, Instructions, Opcode},
    lexer::Token,
    object::Object,
};

use self::error::{CompilerError, CompilerResult};

#[derive(Default)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Default)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
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
                Ok(())
            }
            _ => Err(CompilerError::UnhandledStatement(statement)),
        }
    }

    fn compile_expression(&mut self, expression: Expression) -> CompilerResult {
        match expression {
            Expression::InfixExpression(left, operator, right) => {
                self.compile_expression(*left)?;
                self.compile_expression(*right)?;

                match operator {
                    Token::Plus => self.emit(Opcode::Add, &[]),
                    _ => return Err(CompilerError::UnknownOperator(operator)),
                };
            }
            Expression::IntegerLiteral(value) => {
                let integer = Object::Integer(value);
                let id = self.add_constant(integer);

                self.emit(Opcode::Constant, &[id]);
            }
            _ => return Err(CompilerError::UnhandledExpression(expression)),
        }

        Ok(())
    }

    fn emit(&mut self, opcode: Opcode, operands: &[u16]) -> usize {
        let instruction = make(opcode, operands);
        self.add_instruction(instruction)
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn add_constant(&mut self, integer: Object) -> u16 {
        self.constants.push(integer);
        (self.constants.len() - 1) as u16
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
        parser::Parser,
    };

    use super::*;

    struct CompilerTestCase {
        input: String,
        expected_constants: Vec<Object>,
        expected_instructions: Vec<Instructions>,
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            CompilerTestCase {
                input: "1 + 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Add, &[]),
                    make(Opcode::Pop, &[]),
                ],
            },
            CompilerTestCase {
                input: "1; 2".to_string(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::Constant, &[0]),
                    make(Opcode::Pop, &[]),
                    make(Opcode::Constant, &[1]),
                    make(Opcode::Pop, &[]),
                ],
            },
        ];

        for test in tests {
            let program = Parser::from_source(&test.input).parse_program();
            let mut compiler = Compiler::new();
            assert!(compiler.compile(program).is_ok());

            let bytecode = compiler.bytecode();

            let expected = Instructions::from(test.expected_instructions);
            assert_eq!(
                expected.print(),
                bytecode.instructions.print(),
                "instructions"
            );
            assert_eq!(test.expected_constants, bytecode.constants, "constants");
        }
    }
}
