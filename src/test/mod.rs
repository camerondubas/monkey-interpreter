#![allow(unused_imports)]
use crate::compiler::Compiler;

#[cfg(test)]
pub fn compile_from_source(source: &str) -> Compiler {
    use crate::parser::Parser;

    let program = Parser::from_source(source).parse_program();
    let mut compiler = Compiler::new();

    if let Err(error) = compiler.compile(program) {
        panic!("compiler error: {}", error);
    }

    compiler
}
