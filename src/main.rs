extern crate itertools;

use std::io;
use std::io::prelude::*;
use self::itertools::join;

mod eval;
mod funcs;
mod lexer;
mod parser;


use eval::{evaluate, Context, EvalError, Value};
use lexer::tokenize;
use parser::{parse, ParseError, Span};

fn print_parse_error(line: &str, err: &ParseError) {
    eprintln!("{}", line);

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

fn format_value(val: &Value) -> String {
    match val {
        Value::Number(x) => format!("{}", x),
        Value::Boolean(x) => format!("{}", x),
        Value::Function(f) => match f.name() {
            Some(s) => format!("<function: {}>", s),
            None => format!("<function>"),
        },
        Value::List(x) => {
            format!("[{}]", join(x.iter().map(format_value), ", "))
        }
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

    println!(" {}", format_value(&val));
}

fn main() {
    let exit_cmds = vec!["exit", "quit"];
    let input = io::stdin();
    let mut output = io::stdout();

    let base = funcs::create();
    let mut ctx = eval::Context::with_parent(&base);

    loop {
        output.write(b">>> ").unwrap();
        output.flush().unwrap();

        let mut buffer = String::new();
        input.read_line(&mut buffer).unwrap();

        let line = &buffer.trim();
        if buffer.is_empty() || exit_cmds.contains(line) {
            break;
        }

        if line.len() == 0 {
            continue;
        }

        execute_line(line, &mut ctx);
    }
}
