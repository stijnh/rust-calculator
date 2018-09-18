use std::io;
use std::io::prelude::*;

mod eval;
mod funcs;
mod lexer;
mod parser;

use eval::{evaluate, Context, EvalError, Value};
use lexer::tokenize;
use parser::{parse, ParseError, Span};

fn print_parse_error(line: &str, err: &ParseError) {
    eprint!("{}", line);

    let Span(a, b) = err.span;

    let mut msg = String::new();
    (0..a).for_each(|_| msg.push(' '));
    (a..b).for_each(|_| msg.push('^'));
    eprintln!("{}", msg);

    if a + 1 >= b {
        eprintln!("invalid syntax at {}", a);
    } else {
        eprintln!("invalid syntax at {}:{}", a, b - 1);
    }
}

fn execute_line(line: &str, ctx: &mut Context) {
    let lexer = tokenize(line.trim());

    let root = match parse(lexer) {
        Ok(x) => x,
        Err(err) => {
            print_parse_error(line, &err);
            return;
        }
    };

    println!("parsed: {:?}", root);

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
        Value::Function(f) => match f.name() {
            Some(s) => println!(" <function: {}>", s),
            None => println!(" <function>"),
        },
        Value::Boolean(b) => {
            println!(" {:?}", b);
        }
    }
}

fn main() {
    let exit_cmds = vec!["exit", "quit", ""];
    let input = io::stdin();
    let mut output = io::stdout();

    let base = funcs::create();
    let mut ctx = eval::Context::with_parent(&base);

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
