mod ast;
mod lexer;
mod parser;
mod repl;

use repl::Repl;

fn main() {
    Repl::start();

    let source = "let x = 5;";
    let lexer = lexer::Lexer::new(source);
    let mut parser = parser::Parser::new(lexer);
    let program = parser.parse_program();

    println!("{:?}", program);
}
