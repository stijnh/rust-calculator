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
    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Number(x) => Some(*x),
            Value::Boolean(true) => Some(1.0),
            Value::Boolean(false) => Some(0.0),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Boolean(x) => Some(*x),
            Value::Number(x) => Some(*x != 0.0),
            _ => None,
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Value::Boolean(_) => "boolean",
            Value::Number(_) => "number",
            Value::Function(_) => "function",
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

fn compare_values(op: Op, lhs: &Value, rhs: &Value) -> Option<bool> {
    match op {
        Op::Gt => compare_values(Op::Lt, rhs, lhs),
        Op::Gte => compare_values(Op::Lte, rhs, lhs),
        Op::Neq => compare_values(Op::Eq, lhs, rhs).map(|b| !b),
        Op::Lte => 
            if let Some(true) = compare_values(Op::Eq, lhs, rhs) {
                Some(true)
            } else {
                compare_values(Op::Lt, lhs, rhs)
            },
        Op::Eq => {
            match (lhs, rhs) {
                (Value::Number(x), Value::Number(y)) => Some(x == y),
                (Value::Boolean(x), Value::Boolean(y)) => Some(x == y),
                _ => None
            }
        },
        _ => None
    }
}

fn evaluate_binop(op: Op, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    use Value::Number as N;
    use Value::Boolean as B;

    if let Some(b) = compare_values(op, lhs, rhs) {
        return Ok(B(b));
    }

    match (op, lhs.as_bool()) {
        (Op::And, Some(true))  => return Ok(rhs.clone()),
        (Op::And, Some(false)) => return Ok(lhs.clone()),
        (Op::Or, Some(true))  => return Ok(lhs.clone()),
        (Op::Or, Some(false)) => return Ok(rhs.clone()),
        _ => ()
    }

    let out = match (op, lhs.clone(), rhs.clone()) {
        (Op::Add, N(x), N(y)) => N(x + y),
        (Op::Sub, N(x), N(y)) => N(x - y),
        (Op::Mul, N(x), N(y)) => N(x * y),
        (Op::Div, N(x), N(y)) => N(x / y),
        (Op::Eq, N(x), N(y)) =>  B(x == y),
        (Op::Lt, N(x), N(y)) =>  B(x < y),

        (Op::Mul, B(x), B(y)) => B(x && y),
        (Op::Add, B(x), B(y)) => B(x || y),
        (Op::Eq, B(x), B(y)) =>  B(x == y),
        (Op::Lt, B(x), B(y)) =>  B(x < y),
        _ => {
            return Err(EvalError(format!("invalid binary operator '{}' for types {} and {}",
                                         op.name(), lhs.type_name(), rhs.type_name())));
        }
    };

    Ok(out)
}

fn evaluate_monop(op: Op, arg: &Value) -> Result<Value, EvalError> {
    use Value::Number as N;
    use Value::Boolean as B;

    let out = match (op, arg.clone()) {
        (Op::Add, N(x)) => N(x),
        (Op::Sub, N(x)) => N(-x),
        (Op::Not, N(x)) => B(x == 0.0),
        (Op::Not, B(x)) => B(!x),
        _ => {
            return Err(EvalError(format!("invalid unary operator '{}' for type {}",
                                         op.name(), arg.type_name())));
        }
    };

    Ok(out)
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
