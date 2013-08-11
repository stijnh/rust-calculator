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
            let expr = parse(tokens);
            let res = expr.eval();
            println(fmt!("%?", res));
        }
        [bin, .. _] => {
            println(fmt!("usage: %s <expr>", bin));
            println("");
            println("examples: ");
            println(fmt!("%s 1+1", bin));
            println(fmt!("%s max(1,2,3)", bin));
            println(fmt!("%s cos(2*pi)", bin));
            println(fmt!("%s abs(max(-5, -3))", bin));
            println(fmt!("%s e+(3*5)", bin));
        },
        _ => {}
    }
}
