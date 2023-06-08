use criterion::{black_box, criterion_group, criterion_main, Criterion};
use monkey_interpreter::{
    ast::Program,
    eval::eval,
    lexer::{Lexer, Token},
    object::Environment,
    parser::Parser,
};

const SOURCE: &str = "let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;

\"foobar\"
\"foo bar\"
";

fn criterion_benchmark(c: &mut Criterion) {
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
        let mut env = Environment::new();
        eval(program, &mut env);
    }

    fn eval_from_source(input: &str) {
        let mut parser = Parser::from_source(input);
        let program = parser.parse_program();
        let mut env = Environment::new();
        eval(program, &mut env);
    }

    c.bench_function("Lex source", |b| b.iter(|| lex(black_box(SOURCE))));
    c.bench_function("Parse tokens", |b| b.iter(|| parse(black_box(SOURCE))));
    c.bench_function("Eval AST", |b| {
        b.iter(|| {
            eval_ast(black_box(
                Parser::from_source(black_box(SOURCE)).parse_program(),
            ))
        })
    });
    c.bench_function("Eval from source", |b| {
        b.iter(|| eval_from_source(black_box(SOURCE)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
