use std::{cell::RefCell, fs, rc::Rc};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use monkey_interpreter::{
    ast::Program,
    eval::eval,
    lexer::{Lexer, Token},
    object::Environment,
    parser::Parser,
};

fn criterion_benchmark(criterion: &mut Criterion) {
    fn lex(input: &str) {
        let mut lexer = Lexer::new(input);
        loop {
            let token = lexer.next_token();
            if token == Token::Eof {
                break;
            }
        }
    }

    fn parse(input: &str) {
        let mut parser = Parser::from_source(input);
        parser.parse_program();
    }

    fn eval_ast(program: Program) {
        let env = Rc::new(RefCell::new(Environment::new()));
        eval(program, env);
    }

    fn eval_from_source(input: &str) {
        let mut parser = Parser::from_source(input);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Environment::new()));
        eval(program, env);
    }

    let file_path = "./demo/bench.monkey";
    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    criterion.bench_function("Lex source", |bencher| {
        bencher.iter(|| lex(black_box(&contents)))
    });
    criterion.bench_function("Parse tokens", |bencher| {
        bencher.iter(|| parse(black_box(&contents)))
    });
    criterion.bench_function("Eval AST", |bencher| {
        bencher.iter(|| {
            eval_ast(black_box(
                Parser::from_source(black_box(&contents)).parse_program(),
            ))
        })
    });
    criterion.bench_function("Eval from source", |bencher| {
        bencher.iter(|| eval_from_source(black_box(&contents)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
