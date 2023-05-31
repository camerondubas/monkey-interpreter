mod ast;
mod lexer;
mod parser;
mod repl;

use repl::Repl;

fn main() {
    let mut repl = Repl::new();
    repl.start();

    // Dummy program to avoid unused code warnings
    let source = "let x = 5;";
    let lexer = lexer::Lexer::new(source);
    let mut parser = parser::Parser::new(lexer);
    let program = parser.parse_program();

    println!("{:?}", program);
}
