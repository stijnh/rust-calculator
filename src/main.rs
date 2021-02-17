extern crate itertools;

#[macro_use]
extern crate afl;

use self::itertools::join;
use std::io;
use std::io::prelude::*;
use std::ops::Deref;

#[macro_use]
mod util;
mod eval;
mod funcs;
mod lexer;
mod parser;

use eval::{evaluate, Context, EvalError, Value};
use lexer::tokenize;
use parser::{parse, ParseError, Span};

fn clean_input(line: &str) -> String {
    line.chars()
        .map(|c| {
            if c.is_ascii() && !c.is_ascii_control() {
                c
            } else {
                '?'
            }
        })
        .collect()
}

fn print_parse_error(line: &str, err: &ParseError) {
    eprintln!("{}", line);

    let t = err.token.name();

    let Span(a, b) = err.span;
    let mut msg = String::new();
    (0..a).for_each(|_| msg.push(' '));
    (a..b).for_each(|_| msg.push('^'));
    eprintln!("{}", msg);

    if a + 1 >= b {
        eprintln!("invalid token '{}' at position {}", t, a);
    } else {
        eprintln!("invalid token '{}' at position {}..{}", t, a, b - 1);
    }
}

fn format_value(val: &Value) -> String {
    match val {
        Value::Number(x) => format!("{}", x),
        Value::Boolean(x) => format!("{}", x),
        Value::Function(f) => match f.name() {
            Some(s) => format!("<function: {}>", s),
            None => "<function>".to_string(),
        },
        Value::List(x) => {
            if x.len() < 50 {
                format!("[{}]", join(x.iter().map(format_value), ", "))
            } else {
                format!("[{}, ...]", join(x[..45].iter().map(format_value), ", "))
            }
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
    let exit_cmds = ["exit", "quit"];
    let input = io::stdin();
    let mut output = io::stdout();

    let base = funcs::create();
    let mut ctx = eval::Context::with_parent(&base);

    loop {
        output.write_all(b">>> ").unwrap();
        output.flush().unwrap();

        let mut buffer = String::new();
        input.read_line(&mut buffer).unwrap();

        let line = clean_input(&buffer.trim());
        if buffer.is_empty() || exit_cmds.contains(&&*line) {
            break;
        }

        if line.is_empty() {
            continue;
        }

        execute_line(&line, &mut ctx);
    }
}
