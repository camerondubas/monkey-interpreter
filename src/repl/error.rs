use std::fmt::Display;

use colored::Colorize;

use crate::{compiler::error::CompilerError, parser::error::ParserError, vm::VirtualMachineError};

pub type Result<T = ()> = std::result::Result<T, ReplError>;
pub enum ReplError {
    Parser(Vec<ParserError>),
    Compiler(CompilerError),
    VM(VirtualMachineError),
    UnknownMode(String),
    UnknownCommand,
    Quit,
}

impl Display for ReplError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReplError::Parser(parser_error) => {
                writeln!(f, "Parser error:")?;
                for parser_error in parser_error {
                    writeln!(f, "  - {}", parser_error)?;
                }

                Ok(())
            }
            ReplError::Compiler(compiler_error) => {
                writeln!(f, "Compiler error:")?;
                writeln!(f, "  - {}", compiler_error)
            }
            ReplError::VM(vm_error) => {
                writeln!(f, "VM error:")?;
                writeln!(f, "  - {}", vm_error)
            }
            ReplError::UnknownMode(mode) => {
                writeln!(f, "Unknown mode \"{}\".", mode.red().bold())
            }
            ReplError::UnknownCommand => {
                writeln!(f, "Unknown command. Type \":help\" for help.")
            }
            ReplError::Quit => writeln!(f, "Bye! 👋"),
        }
    }
}

impl From<Vec<ParserError>> for ReplError {
    fn from(error: Vec<ParserError>) -> Self {
        ReplError::Parser(error)
    }
}

impl From<CompilerError> for ReplError {
    fn from(error: CompilerError) -> Self {
        ReplError::Compiler(error)
    }
}

impl From<VirtualMachineError> for ReplError {
    fn from(error: VirtualMachineError) -> Self {
        ReplError::VM(error)
    }
}
