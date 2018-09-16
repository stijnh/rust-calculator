use std::io::{self, BufRead};

mod eval;
mod lexer;
mod parser;

use eval::{evaluate, Context, EvalError};
use lexer::tokenize;
use parser::{parse, ParseError};

fn execute_line(line: &str, ctx: &mut Context) {
    let lexer = tokenize(line);

    let root = match parse(lexer) {
        Ok(x) => x,
        Err(err) => {
            eprintln!("unexpected token: {:?}", err.token);
            return;
        }
    };

    let val = match evaluate(&root, ctx) {
        Ok(x) => x,
        Err(EvalError(msg)) => {
            eprintln!("error: {}", msg);
            return;
        }
    };

    println!("> {:?}", val);
}

fn main() {
    let mut reader = io::stdin();
    let mut ctx = eval::Context::new();

    for line in reader.lock().lines() {
        match &line {
            Ok(line) if line.len() > 0 => {
                execute_line(&line, &mut ctx);
            }
            _ => break,
        }
    }
}
