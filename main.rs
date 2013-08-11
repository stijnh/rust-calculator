use std::os;
use lexer::lex;
use parser::parse;

mod lexer;
mod parser;

fn main() {
    let args = os::args();

    match args {
        [_, input] => {
            let tokens = lex(input);
            println(fmt!("tokens: %?", tokens));
            let expr = parse(tokens);
            println(fmt!("expr: %?", expr));
            let res = expr.eval();
            println(fmt!("res: %?", res));
        }
        _ => fail!(fmt!("usage: %s <expr>", args[0]))
    }
}
