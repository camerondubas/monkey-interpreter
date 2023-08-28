use colored::{ColoredString, Colorize};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::{cell::RefCell, rc::Rc};

use crate::{
    compiler::Compiler,
    eval::eval,
    lexer::{Lexer, Token},
    object::{Environment, Object},
    parser::Parser,
    vm::VirtualMachine,
};

mod commands;
mod mode;
use self::commands::ReplCommand;
use self::mode::ReplMode;

const PROMPT: &str = ">> ";
const PARSING_FAILED_MESSAGE: &str = "Parsing failed. The following errors were found:";

pub struct Repl {
    mode: ReplMode,
    environment: Rc<RefCell<Environment>>,
}

impl Repl {
    pub fn new() -> Self {
        Repl {
            mode: ReplMode::Eval,
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn start(&mut self) -> rustyline::Result<()> {
        let mut rl = DefaultEditor::new()?;
        let _ = rl.load_history("history.txt");

        self.print_welcome();

        loop {
            let readline = rl.readline(format!("{}", PROMPT.green()).as_str());
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str())?;
                    let result = self.handle_input(line);

                    if result.is_err() {
                        break;
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    println!("CTRL-C");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
        }
        rl.save_history("history.txt")?;
        Ok(())
    }

    fn handle_input(&mut self, line: String) -> Result<(), ()> {
        if line.is_empty() {
            return Ok(());
        }

        match line.trim().strip_prefix(':') {
            Some(command) => {
                let command = ReplCommand::from_str(command);
                return command.run(self);
            }
            None => {
                match self.mode {
                    ReplMode::Lexer => self.lex(line),
                    ReplMode::Parser => self.parse(line),
                    ReplMode::Ast => self.ast(line),
                    ReplMode::Eval => self.eval(line),
                    ReplMode::Compiler => self.compile(line),
                };
            }
        }

        Ok(())
    }

    fn set_mode(&mut self, mode_str: &str) -> Result<ReplMode, String> {
        let mode = ReplMode::from_str(mode_str)?;
        self.mode = mode.clone();
        Ok(mode)
    }

    fn compile(&mut self, line: String) {
        let mut parser = Parser::from_source(line.as_str());
        let program = parser.parse_program();

        if parser.errors.is_empty() {
            let mut compiler = Compiler::new();

            match compiler.compile(program) {
                Ok(_) => {
                    let mut vm = VirtualMachine::new(compiler.bytecode());
                    match vm.run() {
                        Ok(_) => {
                            let evaluated = vm.stack_top();
                            let output = self.format_output(evaluated);
                            self.print(output)
                        }
                        Err(e) => println!("{}", e),
                    }
                }
                Err(e) => {
                    println!("{}", e);
                }
            }
        } else {
            println!("{}", PARSING_FAILED_MESSAGE.red());
            for error in parser.errors {
                println!("  - {}", error);
            }
        }
    }

    fn format_output(&mut self, output: Object) -> ColoredString {
        match output {
            Object::Integer(_) => output.to_string().yellow(),
            Object::Boolean(_) => output.to_string().yellow(),
            Object::String(_) => output.to_string().green(),
            Object::Function(_, _, _) => output.to_string().bright_blue(),
            Object::Error(_) => output.to_string().red(),
            _ => output.to_string().white(),
        }
    }

    fn print(&self, str: ColoredString) {
        println!("{}", str.bold());
    }

    fn eval(&mut self, line: String) {
        let mut parser = Parser::from_source(line.as_str());
        let program = parser.parse_program();

        if parser.errors.is_empty() {
            let evaluated = eval(program, Rc::clone(&self.environment));
            let formatted = self.format_output(evaluated);
            self.print(formatted);
        } else {
            println!("{}", PARSING_FAILED_MESSAGE.red());
            for error in parser.errors {
                println!("  - {}", error);
            }
        }
    }

    fn parse(&mut self, line: String) {
        let mut parser = Parser::from_source(line.as_str());
        let program = parser.parse_program();

        if parser.errors.is_empty() {
            println!("{}", program);
        } else {
            println!("{}", PARSING_FAILED_MESSAGE.red());
            for error in parser.errors {
                println!("  - {}", error);
            }
        }
    }

    fn ast(&mut self, line: String) {
        let mut parser = Parser::from_source(line.as_str());
        let program = parser.parse_program();

        if parser.errors.is_empty() {
            for statement in program.statements {
                println!("{:?}", statement);
            }
        } else {
            println!("{}", PARSING_FAILED_MESSAGE.red());
            for error in parser.errors {
                println!("  - {}", error);
            }
        }
    }

    fn lex(&mut self, line: String) {
        let mut lexer = Lexer::new(line.as_str());
        loop {
            let token = lexer.next_token();
            println!("{:?}: {}", token, token.to_string().blue());
            if token == Token::Eof {
                break;
            }
        }
    }

    fn print_welcome(&mut self) {
        println!();
        println!("Welcome to {}.", "Monkey 🍌".green().bold());
        println!();
        println!("Here are some common Commands:");
        println!("  - {}: Full list of commands", "`:help`".yellow().italic());
        println!("  - {}: Print Environment", "`:env`".yellow().italic());
        println!("  - {}: Clear Environment", "`:clear`".yellow().italic());
        println!("  - {}: Exit REPL", "`:exit`".yellow().italic());
        println!();
    }

    fn print_mode(&mut self) {
        println!("{}", format!("Output mode: \"{}\".", &self.mode).blue());
    }

    fn print_help(&mut self) {
        println!("Full list of supported commands:");
        println!("  - {}: Prints this message", "`:help`".yellow().italic());
        println!(
            "  - {}: Changes output mode for the REPL",
            "`:mode <lexer|parser|ast|eval>`".yellow().italic()
        );
        println!(
            "  - {}: Prints the current Environment",
            "`:env`".yellow().italic()
        );
        println!(
            "  - {}: Clears the current Environment",
            "`:clear`".yellow().italic()
        );
        println!("  - {}: Exit REPL", "`:exit`".yellow().italic());
        println!();
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn repl() {
        assert_eq!(1, 1);
    }
}
