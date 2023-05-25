use std::io::{self, BufRead, Write};

use crate::lexer::{Lexer, Token};
use colored::Colorize;

const PROMPT: &str = ">> ";

pub struct Repl {}

impl Repl {
    pub fn start() {
        println!("Welcome to {}.", "Monkey".green());
        println!("Type 'exit;' to exit.");
        print!("{}", PROMPT.green());
        io::stdout().flush().unwrap();

        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let line = line.unwrap();
            if line.as_str() == "exit;" {
                println!("Bye!");
                break;
            }

            let mut lexer = Lexer::new(&line);

            loop {
                let token = lexer.next_token();
                println!("{:?}", token);
                if token == Token::EOF {
                    break;
                }
            }

            println!("");
            print!("{}", PROMPT);
            io::stdout().flush().unwrap();
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn repl() {
        assert_eq!(1, 1);
    }
}
