use lexer::{Comma, LeftParen, RightParen, Number, Ident, Operator, End};
use lexer::Lexer;
use std::float::consts;

#[deriving(Eq)]
pub enum Op {
    Add, Sub, Mul, Div, Mod
}

pub enum Expr {
    Const(float),
    BinOp(~Expr, Op, ~Expr),
    FunCall(~str, ~[~Expr])
}

impl Expr {
    fn eval(&self) -> float {
        match *self {
            Const(x) => x,
            BinOp(ref l, op, ref r) => {
                let a = l.eval();
                let b = r.eval();

                match op {
                    Add => a + b,
                    Sub => a - b,
                    Mul => a * b,
                    Div => a / b,
                    Mod => a % b
                }
            },
            FunCall(ref fun, ref args) => {
                let arr = args.map(|e| e.eval());

                match (fun.to_owned(), arr) {
                    (~"pi", []) => consts::pi,
                    (~"e", []) => consts::e,
                    (~"log", [x]) => x.log10(),
                    (~"log", [x, base]) => x.log(&base),
                    (~"exp", [x]) => x.exp(),
                    (~"sin", [x]) => x.sin(),
                    (~"cos", [x]) => x.cos(),
                    (~"tan", [x]) => x.tan(),
                    (~"sqrt", [x]) => x.sqrt(),
                    (~"abs", [x]) => x.abs(),
                    (~"max", x) => match x.iter().max() {
                        Some(x) => *x,
                        None => 0.0
                    },
                    (~"min", x) => match x.iter().min() {
                        Some(x) => *x,
                        None => 0.0
                    },
                    _ => fail!(fmt!(
                            "unknown function '%?' or wrong number of arguments",
                            fun))
                }
            }
        }
    }
}

pub fn parse(lexer:&Lexer) -> ~Expr {
    let res = parse_binop(lexer, 0);
    lexer.next_expect(~End);
    res
}

fn op_prec(op:Op) -> int {
    match op {
        Add | Sub => 0,
        Mul | Div | Mod => 1
    }
}

fn parse_binop(lexer:&Lexer, prec:int) -> ~Expr {
    let mut expr = parse_monop(lexer);

    loop {
        expr = match *lexer.next() {
            Operator(p) if prec <= op_prec(p) => {
                ~BinOp(expr, p, parse_binop(lexer, op_prec(p)+1))
            }
            _ => break
        }
    }

    lexer.back();
    expr
}

fn parse_monop(lexer:&Lexer) -> ~Expr {
    match *lexer.next() {
        Operator(Sub) => ~BinOp(~Const(0.0), Sub, parse_monop(lexer)),
        Operator(Add) => parse_monop(lexer),
        _ => {
            lexer.back();
            parse_prim(lexer)
        }
    }
}

fn parse_prim(lexer:&Lexer) -> ~Expr {
    match *lexer.next() {
        Number(x) => ~Const(x),
        LeftParen => {
            let expr = parse_binop(lexer, 0);
            lexer.next_expect(~RightParen);
            expr
        }
        Ident(ref name) => {
            let mut args = ~[];

            if lexer.next() == ~LeftParen {
                args.push(parse_binop(lexer, 0));

                while lexer.next() == ~Comma {
                    args.push(parse_binop(lexer, 0));
                }

                lexer.back();
                lexer.next_expect(~RightParen);
            } else {
                lexer.back();
            }

            ~FunCall(name.to_owned(), args)
        },
        t => fail!(fmt!("unexpected token %?", t))
    }
}
