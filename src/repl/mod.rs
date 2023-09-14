use colored::Colorize;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::{cell::RefCell, rc::Rc, str::FromStr};

use monkey_interpreter::{
    ast::Program,
    compiler::{symbol_table::SymbolTable, Compiler, Constants},
    eval::eval,
    lexer::{Lexer, Token},
    object::{Environment, Object},
    parser::Parser,
    vm::{create_globals_store, Globals, VirtualMachine},
};

mod commands;
mod error;
mod mode;

use self::{commands::ReplCommand, error::ReplError};
use self::{error::Result, mode::ReplMode};

const PROMPT: &str = ">> ";

pub struct Repl {
    mode: ReplMode,
    environment: Rc<RefCell<Environment>>,
    constants: Rc<RefCell<Constants>>,
    globals: Rc<RefCell<Globals>>,
    symbol_table: Rc<RefCell<SymbolTable>>,
}

impl Repl {
    pub fn new() -> Self {
        Repl {
            mode: ReplMode::Compiler,
            environment: Rc::new(RefCell::new(Environment::new())),
            constants: Rc::new(RefCell::new(vec![])),
            globals: Rc::new(RefCell::new(create_globals_store())),
            symbol_table: Rc::new(RefCell::new(SymbolTable::new())),
        }
    }

    pub fn start(&mut self) -> rustyline::Result<()> {
        let mut rl = DefaultEditor::new()?;
        rl.load_history("history.txt")?;

        self.print_welcome();

        loop {
            let readline = rl.readline(format!("{}", PROMPT.green()).as_str());
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str())?;

                    if let Err(error) = self.handle_input(line) {
                        let should_quit: bool = matches!(error, ReplError::Quit);
                        println!("{}", error);

                        if should_quit {
                            break;
                        }
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

    fn handle_input(&mut self, line: String) -> std::result::Result<(), ReplError> {
        if line.is_empty() {
            return Ok(());
        }

        let command = ReplCommand::from_str(line.as_str())?;
        command.run(self)
    }

    fn set_mode(&mut self, mode_str: &str) -> Result<ReplMode> {
        let mode = ReplMode::from_str(mode_str)?;
        self.mode = mode.clone();
        Ok(mode)
    }

    fn compile(&mut self, line: String) -> Result {
        let program = self.internal_parse(line)?;
        let mut compiler =
            Compiler::new_with_state(Rc::clone(&self.symbol_table), Rc::clone(&self.constants));

        compiler.compile(program)?;

        let bytecode = compiler.bytecode();
        println!("{}", bytecode);

        Ok(())
    }

    fn eval(&mut self, line: String) -> Result {
        let program = self.internal_parse(line)?;
        let evaluated = eval(program, Rc::clone(&self.environment));
        self.print(evaluated);
        Ok(())
    }

    fn parse(&mut self, line: String) -> Result {
        let program = self.internal_parse(line)?;
        format!("{:?}", program);
        Ok(())
    }

    fn ast(&mut self, line: String) -> Result {
        let program = self.internal_parse(line)?;
        for statement in program.statements {
            println!("{:?}", statement);
        }

        Ok(())
    }

    fn lex(&mut self, line: String) -> Result {
        let mut lexer = Lexer::new(line.as_str());
        loop {
            let token = lexer.next_token();
            println!("{:?}: {}", token, token.to_string().blue());
            if token == Token::Eof {
                return Ok(());
            }
        }
    }

    fn internal_parse(&mut self, line: String) -> Result<Program> {
        let mut parser = Parser::from_source(line.as_str());
        let program = parser.parse_program();

        if parser.errors.is_empty() {
            Ok(program)
        } else {
            Err(ReplError::Parser(parser.errors))
        }
    }

    fn print(&self, obj: Object) {
        let formatted = match obj {
            Object::Integer(_) | Object::Boolean(_) => obj.to_string().yellow(),
            Object::String(_) => obj.to_string().green(),
            Object::Function(_, _, _) => obj.to_string().bright_blue(),
            Object::Error(_) => obj.to_string().red(),
            _ => obj.to_string().white(),
        };

        println!("{}", formatted.bold());
    }

    fn print_welcome(&mut self) {
        println!();
        println!("Welcome to {}", "Monkey 🍌".green().bold());
        println!();
        println!("Here are some common commands:");
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
            "`:mode <lexer|parser|ast|eval|compiler>`".yellow().italic()
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
