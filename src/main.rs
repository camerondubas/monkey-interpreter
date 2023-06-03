mod ast;
mod lexer;
mod parser;
mod repl;

use repl::Repl;

fn main() {
    let mut repl = Repl::new();
    match repl.start() {
        Ok(_) => (),
        Err(e) => println!("{}", e),
    };
}
