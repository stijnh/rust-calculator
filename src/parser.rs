use eval::Value;
pub use lexer::Op;
pub use lexer::Span;
use lexer::{Lexer, Token};
use std::f64;

#[derive(Clone, Debug)]
pub enum Node {
    Immediate(Value),
    MonOp(Op, Box<Node>),
    BinOp(Op, Box<Node>, Box<Node>),
    Apply(Box<Node>, Vec<Node>),
    Load(String),
    Store(String, Box<Node>),
}

#[derive(Debug)]
pub struct ParseError {
    pub token: Token,
    pub span: Span,
}

fn unexpected_token(lexer: &Lexer) -> Result<Node, ParseError> {
    Err(ParseError {
        token: lexer.peek(),
        span: lexer.span(),
    })
}

fn unexpected_prev_token(lexer: &mut Lexer) -> Result<Node, ParseError> {
    lexer.prev();
    unexpected_token(lexer)
}

fn parse_primitive(lexer: &mut Lexer) -> Result<Node, ParseError> {
    match lexer.next() {
        Token::True => Ok(Node::Immediate(Value::Boolean(true))),
        Token::False => Ok(Node::Immediate(Value::Boolean(false))),
        Token::Ident(name) => Ok(Node::Load(name)),
        Token::Number(num) => {
            if let Ok(x) = num.parse() {
                Ok(Node::Immediate(Value::Number(x)))
            } else {
                unexpected_prev_token(lexer)
            }
        }
        Token::LeftParen => {
            let expr = parse_expr(lexer)?;

            match lexer.next() {
                Token::RightParen => Ok(expr),
                _ => unexpected_prev_token(lexer),
            }
        }
        _ => unexpected_prev_token(lexer),
    }
}

fn parse_apply(lexer: &mut Lexer) -> Result<Node, ParseError> {
    let mut out = parse_primitive(lexer)?;

    while lexer.peek() == Token::LeftParen {
        lexer.next();
        let mut args = vec![];

        while lexer.peek() != Token::RightParen {
            args.push(parse_expr(lexer)?);

            if lexer.peek() == Token::Comma {
                lexer.next();
            } else {
                break;
            }
        }

        if lexer.next() != Token::RightParen {
            return unexpected_prev_token(lexer);
        }

        out = Node::Apply(Box::new(out), args);
    }

    Ok(out)
}

fn parse_monop(lexer: &mut Lexer) -> Result<Node, ParseError> {
    if let Token::Operator(op) = lexer.peek() {
        if op == Op::Add || op == Op::Sub || op == Op::Not {
            lexer.next();
            let arg = parse_monop(lexer)?;
            return Ok(Node::MonOp(op, Box::new(arg)));
        }
    }

    parse_apply(lexer)
}

fn op_prec(op: Op) -> i32 {
    match op {
        Op::Or => 1,
        Op::And => 2,
        Op::Lt | Op::Gt | Op::Lte | Op::Gte => 3,
        Op::Eq | Op::Neq => 4,
        Op::Add | Op::Sub => 5,
        Op::Mul | Op::Div => 6,
        _ => -1,
    }
}

fn parse_binop(lexer: &mut Lexer, prec: i32) -> Result<Node, ParseError> {
    let mut lhs = parse_monop(lexer)?;

    loop {
        match lexer.peek() {
            Token::Operator(op) if prec <= op_prec(op) => {
                lexer.next();
                let rhs = parse_binop(lexer, op_prec(op) + 1)?;
                lhs = Node::BinOp(op, Box::new(lhs), Box::new(rhs));
            }
            _ => break Ok(lhs),
        }
    }
}

fn parse_expr(lexer: &mut Lexer) -> Result<Node, ParseError> {
    parse_binop(lexer, 0)
}

fn parse_statement(lexer: &mut Lexer) -> Result<Node, ParseError> {
    let lhs = parse_expr(lexer)?;
    let out = match (lhs, lexer.peek()) {
        (Node::Load(var), Token::Assign) => {
            lexer.next();
            let val = parse_expr(lexer)?;
            Node::Store(var, Box::new(val))
        }
        (out, _) => out,
    };

    match lexer.peek() {
        Token::End => Ok(out),
        _ => unexpected_token(lexer),
    }
}

pub fn parse(mut lexer: Lexer) -> Result<Node, ParseError> {
    parse_statement(&mut lexer)
}
