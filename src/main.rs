mod lexer;
use lexer::Lexer;

fn main() {
    let mut lexer = Lexer::new("test");
    let first_token = lexer.next_token();
    println!("first_token: {:?}", first_token);
}
