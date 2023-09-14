use std::{fmt::Display, rc::Rc, str::FromStr};

use colored::Colorize;
use monkey_interpreter::{
    compiler::Compiler,
    eval::eval,
    lexer::{Lexer, Token},
    vm::VirtualMachine,
};

use super::{
    error::{ReplError, Result},
    Repl,
};

#[derive(Debug, PartialEq, Clone)]
pub enum ReplMode {
    Lexer,
    Parser,
    Ast,
    Eval,
    Compiler,
    VM,
}

impl Display for ReplMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mode_str = match self {
            ReplMode::Lexer => "lexer",
            ReplMode::Parser => "parser",
            ReplMode::Ast => "ast",
            ReplMode::Eval => "eval",
            ReplMode::Compiler => "compiler",
            ReplMode::VM => "vm",
        };

        write!(f, "{}", mode_str)
    }
}

impl FromStr for ReplMode {
    type Err = ReplError;

    fn from_str(mode_str: &str) -> std::result::Result<Self, Self::Err> {
        let mode = match mode_str {
            "lexer" => ReplMode::Lexer,
            "parser" => ReplMode::Parser,
            "ast" => ReplMode::Ast,
            "eval" => ReplMode::Eval,
            "compiler" => ReplMode::Compiler,
            "vm" => ReplMode::VM,
            _ => return Err(ReplError::UnknownMode(mode_str.to_string())),
        };

        Ok(mode)
    }
}

pub trait Execute {
    fn execute(&self, repl: &mut Repl, line: String) -> Result<()>;
}

impl Execute for ReplMode {
    fn execute(&self, repl: &mut Repl, line: String) -> Result<()> {
        match self {
            ReplMode::Lexer => {
                let mut lexer = Lexer::new(line.as_str());
                loop {
                    let token = lexer.next_token();
                    println!("{:?}: {}", token, token.to_string().blue());
                    if token == Token::Eof {
                        return Ok(());
                    }
                }
            }
            ReplMode::Parser => {
                let program = repl.parse(line)?;
                format!("{:?}", program);
                Ok(())
            }
            ReplMode::Ast => {
                let program = repl.parse(line)?;
                for statement in program.statements {
                    println!("{:?}", statement);
                }

                Ok(())
            }
            ReplMode::Eval => {
                let program = repl.parse(line)?;
                let evaluated = eval(program, Rc::clone(&repl.environment));
                repl.print_obj(evaluated);
                Ok(())
            }
            ReplMode::Compiler => {
                let program = repl.parse(line)?;
                let mut compiler = Compiler::new_with_state(
                    Rc::clone(&repl.symbol_table),
                    Rc::clone(&repl.constants),
                );

                compiler.compile(program)?;

                let bytecode = compiler.bytecode();
                println!("{}", bytecode);

                Ok(())
            }
            ReplMode::VM => {
                let program = repl.parse(line)?;
                let mut compiler = Compiler::new_with_state(
                    Rc::clone(&repl.symbol_table),
                    Rc::clone(&repl.constants),
                );

                compiler.compile(program)?;

                let bytecode = compiler.bytecode();
                let mut vm =
                    VirtualMachine::new_with_globals_store(bytecode, Rc::clone(&repl.globals));

                vm.run()?;

                repl.print_obj(vm.last_popped_stack_elem());
                Ok(())
            }
        }
    }
}
