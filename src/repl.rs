use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::fmt::Display;

use crate::{
    eval::eval,
    lexer::{Lexer, Token},
    object::{Environment, Object},
    parser::Parser,
};
use colored::Colorize;

const PROMPT: &str = ">> ";
const PARSING_FAILED_MESSAGE: &str = "Parsing failed. The following errors were found:";

#[derive(Debug, PartialEq, Clone)]
enum ReplMode {
    Lexer,
    Parser,
    Ast,
    Eval,
}

impl Display for ReplMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mode_str = match self {
            ReplMode::Lexer => "lexer",
            ReplMode::Parser => "parser",
            ReplMode::Ast => "ast",
            ReplMode::Eval => "eval",
        };

        write!(f, "{}", mode_str)
    }
}

pub struct Repl {
    mode: ReplMode,
    tracer_enabled: bool,
    environment: Environment,
}

impl Repl {
    pub fn new() -> Self {
        Repl {
            mode: ReplMode::Eval,
            tracer_enabled: false,
            environment: Environment::new(),
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
        match line.as_str() {
            ":exit" => {
                println!("Bye! 👋");
                return Err(());
            }
            ":tracer" => {
                let message;

                if self.tracer_enabled {
                    self.tracer_enabled = false;
                    message = "Disabled";
                } else {
                    self.tracer_enabled = true;
                    message = "Enabled";
                }

                println!("Tracer {}", message);
                return Ok(());
            }
            line if line.starts_with(":mode") => {
                match self.set_mode(line) {
                    Ok(_) => self.print_mode(),
                    Err(e) => println!("{}", e.red()),
                };
                return Ok(());
            }
            line if line.is_empty() => {
                return Ok(());
            }
            _ => (),
        }

        match self.mode {
            ReplMode::Lexer => self.lex(line),
            ReplMode::Parser => self.parse(line),
            ReplMode::Ast => self.ast(line),
            ReplMode::Eval => self.eval(line),
        };

        Ok(())
    }

    fn set_mode(&mut self, line: &str) -> Result<ReplMode, String> {
        let mode_str = line
            .split(' ')
            .nth(1)
            .ok_or("No mode provided".to_string())?;

        let mode = match mode_str {
            "lexer" => ReplMode::Lexer,
            "parser" => ReplMode::Parser,
            "ast" => ReplMode::Ast,
            "eval" => ReplMode::Eval,
            _ => return Err(format!("{:?} is not a valid mode", mode_str)),
        };

        self.mode = mode.clone();

        Ok(mode)
    }

    fn eval(&mut self, line: String) {
        let mut parser = Parser::from_source(line.as_str());
        let program = parser.parse_program();

        if parser.errors.is_empty() {
            let evaluated = eval(program, &mut self.environment);
            let colorized_string = match evaluated {
                Object::Integer(_) => evaluated.to_string().yellow(),
                Object::Boolean(_) => evaluated.to_string().yellow(),
                Object::String(_) => evaluated.to_string().green(),
                Object::Function(_, _, _) => evaluated.to_string().bright_blue(),
                Object::Error(_) => evaluated.to_string().red(),
                _ => evaluated.to_string().white(),
            };
            println!("{}", colorized_string.bold());
        } else {
            println!("{}", PARSING_FAILED_MESSAGE.red());
            for error in parser.errors {
                println!("  - {}", error);
            }
        }
    }

    fn parse(&mut self, line: String) {
        let mut parser = Parser::from_source(line.as_str());
        parser.trace(self.tracer_enabled);
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
        println!("Commands:");
        println!("  - {}: Toggle tracing", "`:trace`".yellow().italic());
        println!(
            "  - {}: Change output mode",
            "`:mode <lexer|parser|ast|output>`".yellow().italic()
        );
        println!("  - {}: Exit REPL", "`:exit`".yellow().italic());
        println!();
        self.print_mode();
        println!();
    }

    fn print_mode(&mut self) {
        println!("{}", format!("Output mode: \"{}\".", &self.mode).blue());
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn repl() {
        assert_eq!(1, 1);
    }
}
