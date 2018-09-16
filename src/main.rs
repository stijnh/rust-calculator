use std::io;
use std::io::prelude::*;

mod eval;
mod lexer;
mod parser;

use eval::{evaluate, Value, Context, EvalError};
use lexer::tokenize;
use parser::{parse, Span, ParseError};

fn print_parse_error(line: &str, err: &ParseError) {

    eprint!("{}", line);

    let Span(a, b) = err.span;

    let mut msg = String::new();
    (0..a).for_each(|_| msg.push(' '));
    (a..b).for_each(|_| msg.push('^'));
    eprintln!("{}", msg);

    if a == b {
        eprintln!("invalid syntax at {}", a);
    } else {
        eprintln!("invalid syntax at {}:{}", a, b);
    }
}

fn execute_line(line: &str, ctx: &mut Context) {
    let lexer = tokenize(line);

    let root = match parse(lexer) {
        Ok(x) => x,
        Err(err) => {
            print_parse_error(line, &err);
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

    match val {
        Value::Number(x) => {
            println!(" {:?}", x);
        }
    }
}

fn main() {
    let exit_cmds = vec!["exit", "quit", ""];
    let input = io::stdin();
    let mut output = io::stdout();
    let mut ctx = eval::Context::new();

    loop {
        output.write(b">>> ").unwrap();
        output.flush().unwrap();

        let mut line = String::new();
        input.read_line(&mut line).unwrap();

        if exit_cmds.contains(&line.trim()) {
            break;
        }

        execute_line(&line, &mut ctx);
    }
}
