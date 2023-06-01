use std::{
    fmt::Display,
    io::{self, BufRead, Write},
};

use crate::{
    lexer::{Lexer, Token},
    parser::Parser,
};
use colored::Colorize;

const PROMPT: &str = ">> ";
const PARSING_FAILED_MESSAGE: &str = "Parsing failed. The following errors were found:";
const CHANGE_MODE_MESSAGE: &str = "Change mode with `:mode <lexer|parser|output>;`";
const EXIT_MESSAGE: &str = "Exit with `:exit;`";

#[derive(Debug, PartialEq, Clone)]
enum ReplMode {
    Lexer,
    Parser,
    Output,
}

impl Display for ReplMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mode_str = match self {
            ReplMode::Lexer => "lexer",
            ReplMode::Parser => "parser",
            ReplMode::Output => "output",
        };

        write!(f, "{}", mode_str)
    }
}

pub struct Repl {
    history: Vec<String>,
    mode: ReplMode,
    tracer_enabled: bool,
}

impl Repl {
    pub fn new() -> Self {
        Repl {
            history: Vec::new(),
            mode: ReplMode::Parser,
            tracer_enabled: false,
        }
    }

    pub fn start(&mut self) {
        self.print_welcome();
        let stdin = io::stdin();

        for line in stdin.lock().lines() {
            let line = line.unwrap();

            match line.as_str() {
                ":exit;" => {
                    println!("Bye! 👋");
                    break;
                }
                ":tracer;" => {
                    let message;

                    if self.tracer_enabled {
                        self.tracer_enabled = false;
                        message = "Disabled";
                    } else {
                        self.tracer_enabled = true;
                        message = "Enabled";
                    }

                    println!("Tracer {}", message);
                    self.print_prompt();
                    continue;
                }
                line if line.is_empty() => {
                    self.print_prompt();
                    continue;
                }
                _ => (),
            }

            match &self.set_mode(line.clone()) {
                Ok(opt) => {
                    if opt.is_some() {
                        self.print_mode();
                        self.print_prompt();
                        continue;
                    }
                }
                Err(e) => {
                    println!("{}", e.red());
                    self.print_prompt();
                    continue;
                }
            };

            match self.mode {
                ReplMode::Lexer => self.lex(&line),
                ReplMode::Parser => self.parse(&line),
                ReplMode::Output => unimplemented!("output mode not implemented"),
            }

            self.history.push(line);

            println!("");
            self.print_prompt();
        }
    }

    fn set_mode(&mut self, line: String) -> Result<Option<ReplMode>, String> {
        if !line.starts_with(":mode") || !line.ends_with(";") {
            return Ok(None);
        }

        let mode_str = line
            .split(" ")
            .skip(1)
            .next()
            .ok_or("No mode provided".to_string())?;

        let mode = match mode_str {
            "lexer;" => ReplMode::Lexer,
            "parser;" => ReplMode::Parser,
            "output;" => return Err(format!("output mode not implemented")),
            _ => return Err(format!("{:?} is not a valid mode", mode_str)),
        };

        self.mode = mode.clone();

        Ok(Some(mode))
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
                println!("{:?}", error);
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
        println!("Welcome to {}.", "Monkey".green());
        println!("");
        println!("{}", EXIT_MESSAGE);
        println!("{}", "Enable tracing with `:trace`");
        self.print_mode();
        println!("");
        self.print_prompt();
    }

    fn print_mode(&mut self) {
        let mode_message = format!("Repl mode set to \"{}\".", &self.mode).blue();
        println!("{} {}", mode_message, CHANGE_MODE_MESSAGE.yellow().italic());
    }

    fn print_prompt(&mut self) {
        print!("{}", PROMPT.green());
        io::stdout().flush().unwrap();
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn repl() {
        assert_eq!(1, 1);
    }
}
