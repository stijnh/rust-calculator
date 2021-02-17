use eval::Value;
pub use lexer::Op;
pub use lexer::Span;
use lexer::{Lexer, Token};

#[derive(Clone, Debug)]
pub enum Node {
    Immediate(Value),
    MonOp(Op, Box<Node>),
    BinOp(Op, Box<Node>, Box<Node>),
    Apply(Box<Node>, Vec<Node>),
    Index(Box<Node>, Box<Node>),
    Lambda(Vec<String>, Box<Node>),
    Cond(Box<Node>, Box<Node>, Box<Node>),
    List(Vec<Node>),
    Var(String),
    VarDef(String, Box<Node>),
    FunDef(String, Vec<String>, Box<Node>),
    Range(Option<Box<Node>>, Option<Box<Node>>, Option<Box<Node>>),
}

#[derive(Debug)]
pub struct ParseError {
    pub token: Token,
    pub span: Span,
}

fn unexpected_token(lexer: &Lexer) -> Result<Node, ParseError> {
    raise!(ParseError {
        token: lexer.peek(),
        span: lexer.span(),
    })
}

fn unexpected_prev_token(lexer: &mut Lexer) -> Result<Node, ParseError> {
    lexer.prev();
    unexpected_token(lexer)
}

fn expect_token(lexer: &mut Lexer, token: &Token) -> Result<(), ParseError> {
    if lexer.next() != *token {
        unexpected_prev_token(lexer)?;
    }

    Ok(())
}

fn parse_list(lexer: &mut Lexer, closing: &Token) -> Result<Vec<Node>, ParseError> {
    let mut args = vec![];

    while lexer.peek() != *closing {
        args.push(parse_expr(lexer)?);

        if lexer.peek() == Token::Comma {
            lexer.next();
        } else {
            break;
        }
    }

    expect_token(lexer, closing)?;
    Ok(args)
}

fn parse_primitive(lexer: &mut Lexer) -> Result<Node, ParseError> {
    let out = match lexer.next() {
        Token::True => Node::Immediate(Value::Boolean(true)),
        Token::False => Node::Immediate(Value::Boolean(false)),
        Token::Ident(name) => Node::Var(name),
        Token::Number(num) => {
            if let Ok(x) = num.parse() {
                Node::Immediate(Value::Number(x))
            } else {
                return unexpected_prev_token(lexer);
            }
        }
        Token::LeftParen => {
            let expr = parse_expr(lexer)?;
            expect_token(lexer, &Token::RightParen)?;
            expr
        }
        Token::LeftBracket => {
            let args = parse_list(lexer, &Token::RightBracket)?;
            Node::List(args)
        }
        _ => return unexpected_prev_token(lexer),
    };

    Ok(out)
}

fn parse_apply(lexer: &mut Lexer) -> Result<Node, ParseError> {
    let mut out = parse_primitive(lexer)?;

    loop {
        out = match lexer.next() {
            Token::LeftParen => {
                let args = parse_list(lexer, &Token::RightParen)?;
                Node::Apply(Box::new(out), args)
            }
            Token::LeftBracket => {
                let index = parse_expr(lexer)?;
                expect_token(lexer, &Token::RightBracket)?;
                Node::Index(Box::new(out), Box::new(index))
            }
            _ => {
                lexer.prev();
                break;
            }
        }
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

fn parse_cond(lexer: &mut Lexer) -> Result<Node, ParseError> {
    let mut lhs = parse_binop(lexer, 0)?;

    loop {
        if lexer.next() != Token::If {
            lexer.prev();
            break Ok(lhs);
        }

        let cond = parse_expr(lexer)?;
        expect_token(lexer, &Token::Else)?;
        let rhs = parse_expr(lexer)?;

        lhs = Node::Cond(Box::new(cond), Box::new(lhs), Box::new(rhs));
    }
}

fn parse_lambda(lexer: &mut Lexer) -> Result<Node, ParseError> {
    let out = match (lexer.next(), lexer.next(), lexer.next(), lexer.next()) {
        // Case 1: x => body
        (Token::Ident(x), Token::Arrow, _, _) => {
            lexer.prev();
            lexer.prev();
            let args = vec![x];
            let body = parse_lambda(lexer)?;
            Node::Lambda(args, Box::new(body))
        }

        // Case 2: () => body
        (Token::LeftParen, Token::RightParen, Token::Arrow, _) => {
            lexer.prev();
            let args = vec![];
            let body = parse_lambda(lexer)?;
            Node::Lambda(args, Box::new(body))
        }

        // Case 3: (x) => body
        (Token::LeftParen, Token::Ident(x), Token::RightParen, Token::Arrow) => {
            let args = vec![x];
            let body = parse_lambda(lexer)?;
            Node::Lambda(args, Box::new(body))
        }

        // Case 4: (x, y, z) => body
        (Token::LeftParen, Token::Ident(x), Token::Comma, Token::Ident(y)) => {
            let mut args = vec![x, y];

            loop {
                match lexer.next() {
                    Token::Comma => (),
                    Token::RightParen => break,
                    _ => return unexpected_prev_token(lexer),
                };

                match lexer.next() {
                    Token::Ident(x) => args.push(x),
                    _ => return unexpected_prev_token(lexer),
                }
            }

            if lexer.next() != Token::Arrow {
                return unexpected_prev_token(lexer);
            }

            let body = parse_lambda(lexer)?;
            Node::Lambda(args, Box::new(body))
        }

        _ => {
            lexer.prev();
            lexer.prev();
            lexer.prev();
            lexer.prev();
            parse_cond(lexer)?
        }
    };

    Ok(out)
}

fn parse_expr(lexer: &mut Lexer) -> Result<Node, ParseError> {
    parse_lambda(lexer)
}

fn parse_statement(lexer: &mut Lexer) -> Result<Node, ParseError> {
    let lhs = parse_expr(lexer)?;

    Ok(match (lhs, lexer.next()) {
        (out, Token::End) => out,
        (Node::Var(var), Token::Assign) => {
            let body = parse_statement(lexer)?;
            match body {
                Node::Lambda(args, body) => Node::FunDef(var, args, body),
                body => Node::VarDef(var, Box::new(body)),
            }
        }
        (Node::Apply(lhs, args), Token::Assign) => {
            let var = match *lhs {
                Node::Var(var) => var,
                _ => {
                    unexpected_prev_token(lexer)?;
                    unreachable!();
                }
            };

            let mut params = vec![];

            for arg in args {
                if let Node::Var(name) = arg {
                    params.push(name);
                } else {
                    return unexpected_prev_token(lexer);
                }
            }

            let body = parse_statement(lexer)?;
            Node::FunDef(var, params, Box::new(body))
        }
        _ => unexpected_prev_token(lexer)?,
    })
}

pub fn parse(mut lexer: Lexer) -> Result<Node, ParseError> {
    parse_statement(&mut lexer)
}
