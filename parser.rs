use lexer::{Token, LeftParen, RightParen, Number, Ident, Operator, End};

#[deriving(Eq)]
pub enum Op {
    Add, Sub, Mul, Div, Mod
}

pub enum Expr {
    Const(int),
    BinOp(~Expr, Op, ~Expr),
    FunCall(~str, ~Expr)
}

impl Expr {
    fn eval(&self) -> int {
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
            FunCall(*) => {
                //let a = arg.eval();
                fail!("function calls are not supported yet");
            }
        }
    }
}

pub fn parse(tokens:&[Token]) -> ~Expr {
    let mut i = 0;
    let res = parse_prec(tokens, &mut i, 0);
    if tokens[i] != End {
        fail!(fmt!("unexpected %?, expecting <End>", tokens[i]));
    }
    res
}


fn parse_prec(tokens:&[Token], i:&mut int, prec:int) -> ~Expr {
    let mut expr = match tokens[*i] {
        LeftParen => {
            *i += 1;
            let expr = parse_prec(tokens, i, 0);
            if tokens[*i] != RightParen {
                fail!(fmt!("unexpected %?, expecting ')'", tokens[*i]));
            }
            *i += 1;
            expr
        },
        Number(x) => {
            *i += 1;
            ~Const(x)
        }
        Ident(ref fun) => {
            *i += 1;
            if tokens[*i] != LeftParen {
                fail!(fmt!("unexpected %?, expecting '('", tokens[*i]));
            }
            *i += 1;
            let arg = parse_prec(tokens, i, 0);
            if tokens[*i] != RightParen {
                fail!(fmt!("unexpected %?, expecting ')'", tokens[*i]));
            }
            *i += 1;
            ~FunCall(copy *fun, arg)
        },
        Operator(Add) => {
            *i += 1;
            parse_prec(tokens, i, prec)
        },
        Operator(Sub) => {
            *i += 1;
            let expr = parse_prec(tokens, i, prec);
            ~BinOp(~Const(0), Sub, expr)

        },
        _ => fail!(fmt!("unexpected %?", tokens[*i]))
    };

    loop {
        *i += 1;
        expr = match tokens[*i - 1] {
            Operator(Add) if prec <= 0 => ~BinOp(expr, Add, parse_prec(tokens, i, 1)),
            Operator(Sub) if prec <= 0 => ~BinOp(expr, Sub, parse_prec(tokens, i, 1)),
            Operator(Mul) if prec <= 1 => ~BinOp(expr, Mul, parse_prec(tokens, i, 2)),
            Operator(Div) if prec <= 1 => ~BinOp(expr, Div, parse_prec(tokens, i, 2)),
            Operator(Mod) if prec <= 1 => ~BinOp(expr, Mod, parse_prec(tokens, i, 2)),
            _ => break
        }
    }
    *i -= 1;
    expr
}
