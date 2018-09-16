use parser::{Node, Op};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
}

#[derive(Debug)]
pub struct EvalError(pub String);

pub struct Context<'a> {
    parent: Option<&'a Context<'a>>,
    scope: HashMap<String, Value>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Context {
            parent: None,
            scope: HashMap::new(),
        }
    }

    pub fn with_parent(ctx: &'a Context) -> Self {
        Context {
            parent: Some(ctx),
            scope: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        if let Some(x) = self.scope.get(key) {
            return Some(x.clone());
        }

        if let Some(p) = self.parent {
            return p.get(key);
        }

        return None;
    }

    pub fn set(&mut self, key: &str, val: Value) {
        self.scope.insert(key.into(), val);
    }
}

fn evaluate_binop(op: Op, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    let (x, y) = match (lhs, rhs) {
        (Value::Number(x), Value::Number(y)) => (x, y),
        _ => return Err(EvalError(format!("invalid cast to number"))),
    };

    let z = match op {
        Op::Add => x + y,
        Op::Sub => x - y,
        Op::Mul => x * y,
        Op::Div => x / y,
    };

    Ok(Value::Number(z))
}

fn evaluate_monop(op: Op, arg: &Value) -> Result<Value, EvalError> {
    let x = match arg {
        Value::Number(x) => *x,
        _ => return Err(EvalError(format!("invalid cast to number"))),
    };

    let y = match op {
        Op::Add => x,
        Op::Sub => -x,
        _ => return Err(EvalError(format!("invalid unary operator '{}'", op.name()))),
    };

    Ok(Value::Number(y))
}

fn evaluate_node(node: &Node, ctx: &Context) -> Result<Value, EvalError> {
    match node {
        Node::Immediate(val) => Ok(val.clone()),
        Node::Load(var) => {
            if let Some(val) = ctx.get(var) {
                Ok(val)
            } else {
                Err(EvalError(format!("undefined variable '{}'", var)))
            }
        }
        Node::BinOp(op, lhs, rhs) => {
            let x = evaluate_node(lhs, ctx)?;
            let y = evaluate_node(rhs, ctx)?;
            evaluate_binop(*op, &x, &y)
        }
        Node::MonOp(op, arg) => {
            let x = evaluate_node(arg, ctx)?;
            evaluate_monop(*op, &x)
        }
    }
}

pub fn evaluate(root: &Node, ctx: &Context) -> Result<Value, EvalError> {
    evaluate_node(root, ctx)
}
