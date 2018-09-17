use parser::{Node, Op};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub trait Func {
    fn name(&self) -> Option<&str>;
    fn call(&self, args: &Vec<Value>) -> Result<Value, EvalError>;
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Function(Rc<Box<Func>>),
}

impl Value {
    fn as_number(&self) -> Option<f64> {
        match self {
            Value::Number(x) => Some(*x),
            Value::Boolean(true) => Some(1.0),
            Value::Boolean(false) => Some(0.0),
            _ => None,
        }
    }

    fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Boolean(x) => Some(*x),
            Value::Number(x) => Some(*x != 0.0),
            _ => None,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(x) => write!(f, "Number({:?})", x),
            Value::Boolean(x) => write!(f, "Boolean({:?})", x),
            Value::Function(_) => write!(f, "Function(...)"),
        }
    }
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
    let (x, y) = match (lhs.as_number(), rhs.as_number()) {
        (Some(x), Some(y)) => (x, y),
        _ => return Err(EvalError(format!("invalid cast to number"))),
    };

    let z = match op {
        Op::Add => x + y,
        Op::Sub => x - y,
        Op::Mul => x * y,
        Op::Div => x / y,
        _ => {
            return Err(EvalError(format!(
                "invalid binary operator '{}'",
                op.name()
            )))
        }
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

fn evaluate_apply(fun: &Value, args: &Vec<Value>) -> Result<Value, EvalError> {
    let fun = match fun {
        Value::Function(f) => f,
        _ => return Err(EvalError(format!("invalid cast to function"))),
    };

    fun.call(args)
}

fn evaluate_node(node: &Node, ctx: &mut Context) -> Result<Value, EvalError> {
    match node {
        Node::Immediate(val) => Ok(val.clone()),
        Node::Store(key, arg) => {
            let val = evaluate_node(arg, ctx)?;
            ctx.set(key, val.clone());
            Ok(val)
        }
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
        Node::Apply(fun, args) => {
            let f = evaluate_node(fun, ctx)?;
            let mut vals = vec![];
            for arg in args {
                vals.push(evaluate_node(arg, ctx)?);
            }
            evaluate_apply(&f, &vals)
        }
    }
}

pub fn evaluate(root: &Node, ctx: &mut Context) -> Result<Value, EvalError> {
    evaluate_node(root, ctx)
}
