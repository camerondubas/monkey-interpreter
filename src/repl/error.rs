use colored::Colorize;
use monkey_interpreter::{
    compiler::error::CompilerError, parser::error::ParserError, vm::error::VirtualMachineError,
};
use std::fmt::Display;

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
                writeln!(f, "{}", "Parser error:".to_string().red())?;
                for parser_error in parser_error {
                    writeln!(f, "  - {}", parser_error)?;
                }

                Ok(())
            }
            ReplError::Compiler(compiler_error) => {
                writeln!(f, "{}", "Compiler error:".to_string().red())?;
                writeln!(f, "  - {}", compiler_error)
            }
            ReplError::VM(vm_error) => {
                writeln!(f, "{}", "Virtual Machine error:".to_string().red())?;
                writeln!(f, "  - {}", vm_error)
            }
            ReplError::UnknownMode(mode) => {
                writeln!(f, "Unknown mode \"{}\".", mode.red().bold())
            }
            ReplError::UnknownCommand => {
                writeln!(
                    f,
                    "Unknown command. Type {} to list all commands.",
                    ":help".yellow().italic()
                )
            }
            ReplError::Quit => writeln!(f, "{}", "Bye! 👋".to_string().blue()),
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
