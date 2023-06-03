use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::fmt::Display;

use crate::{
    lexer::{Lexer, Token},
    parser::Parser,
};
use colored::Colorize;

const PROMPT: &str = ">> ";
const PARSING_FAILED_MESSAGE: &str = "Parsing failed. The following errors were found:";

#[derive(Debug, PartialEq, Clone)]
enum ReplMode {
    Lexer,
    Parser,
    AST,
    Output,
}

impl Display for ReplMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mode_str = match self {
            ReplMode::Lexer => "lexer",
            ReplMode::Parser => "parser",
            ReplMode::AST => "ast",
            ReplMode::Output => "output",
        };

        write!(f, "{}", mode_str)
    }
}

pub struct Repl {
    mode: ReplMode,
    tracer_enabled: bool,
}

impl Repl {
    pub fn new() -> Self {
        Repl {
            mode: ReplMode::Parser,
            tracer_enabled: false,
        }
    }

    pub fn start(&mut self) -> rustyline::Result<()> {
        self.print_welcome();

        let mut rl = DefaultEditor::new()?;

        loop {
            let readline = rl.readline(format!("{}", PROMPT.green()).as_str());
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str())?;
                    let result = self.handle_input(line);

                    if let Err(_) = result {
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
                match self.set_mode(line.clone()) {
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
            ReplMode::Lexer => self.lex(&line),
            ReplMode::Parser => self.parse(&line),
            ReplMode::AST => self.ast(&line),
            ReplMode::Output => unimplemented!("output mode not implemented"),
        };
        return Ok(());
    }

    fn set_mode(&mut self, line: &str) -> Result<ReplMode, String> {
        let mode_str = line
            .split(" ")
            .skip(1)
            .next()
            .ok_or("No mode provided".to_string())?;

        let mode = match mode_str {
            "lexer" => ReplMode::Lexer,
            "parser" => ReplMode::Parser,
            "ast" => ReplMode::AST,
            "output" => return Err(format!("output mode not implemented")),
            _ => return Err(format!("{:?} is not a valid mode", mode_str)),
        };

        self.mode = mode.clone();

        Ok(mode)
    }

    fn parse(&mut self, line: &String) {
        let mut parser = Parser::from_source(line);
        parser.trace(self.tracer_enabled);
        let program = parser.parse_program();

        if parser.errors.is_empty() {
            println!("{}", program.to_string());
        } else {
            println!("{}", PARSING_FAILED_MESSAGE.red());
            for error in parser.errors {
                println!("  - {}", error);
            }
        }
    }

    fn ast(&mut self, line: &String) {
        let mut parser = Parser::from_source(line);
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

    fn lex(&mut self, line: &String) {
        let mut lexer = Lexer::new(&line);
        loop {
            let token = lexer.next_token();
            println!("{:?}: {}", token, token.to_string().blue());
            if token == Token::Eof {
                break;
            }
        }
    }

    fn print_welcome(&mut self) {
        println!("");
        println!("Welcome to {}.", "Monkey 🍌".green().bold());
        println!("");
        println!("Commands:");
        println!("  - {}: {}", "`:trace`".yellow().italic(), "Toggle tracing");
        println!(
            "  - {}: {}",
            "`:mode <lexer|parser|ast|output>`".yellow().italic(),
            "Change output mode"
        );
        println!("  - {}: {}", "`:exit`".yellow().italic(), "Exit REPL");
        println!("");
        self.print_mode();
        println!("");
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
