mod ast;
mod code;
mod compiler;
mod eval;
mod lexer;
mod object;
mod parser;
mod repl;
mod test_utils;
mod vm;

use std::{cell::RefCell, env, fs, rc::Rc};

use parser::Parser;
use repl::Repl;

use crate::object::Environment;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        let mut repl = Repl::new();
        match repl.start() {
            Ok(_) => (),
            Err(e) => println!("{}", e),
        };
        return;
    }

    let file_path = &args[1];
    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");
    let mut parser = Parser::from_source(&contents);
    let program = parser.parse_program();

    if parser.errors.is_empty() {
        eval::eval(program, Rc::new(RefCell::new(Environment::new())));
    } else {
        println!("Parsing Failed");
        for error in parser.errors {
            println!("  - {}", error);
        }
    }
}
