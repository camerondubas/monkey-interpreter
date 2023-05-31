mod ast;
mod lexer;
mod parser;
mod repl;

use parser::Parser;
use repl::Repl;

fn main() {
    let mut repl = Repl::new();
    repl.start();

    // Dummy program to avoid unused code warnings
    let source = "let x = 5;";
    let mut parser = Parser::from_source(source);
    let program = parser.parse_program();

    println!("{:?}", program);
}
