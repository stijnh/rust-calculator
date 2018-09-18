extern crate itertools;

use self::itertools::chain;
use parser::{Node, Op};
use std::collections::HashMap;
use std::rc::Rc;
use std::{cmp, fmt};

pub trait Func {
    fn name(&self) -> Option<&str>;
    fn call(&self, args: &Vec<Value>) -> Result<Value, EvalError>;
}

struct ClosureFunc {
    name: Option<String>,
    params: Vec<String>,
    body: Node,
}

impl Func for ClosureFunc {
    fn name(&self) -> Option<&str> {
        match self.name {
            Some(ref x) => Some(&x),
            None => None
        }
    }

    fn call(&self, args: &Vec<Value>) -> Result<Value, EvalError> {
        if self.params.len() != args.len() {
            return Err(EvalError(format!("wrong number of arguments")));
        }

        let mut ctx = Context::new();
        for (param, arg) in self.params.iter().zip(args) {
            ctx.set(param, arg.clone());
        }

        evaluate_node(&self.body, &mut ctx)
    }
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
            Value::Function(_) => Some(true),
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

impl cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<cmp::Ordering> {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => x.partial_cmp(y),
            (Value::Boolean(x), Value::Boolean(y)) => x.partial_cmp(y),
            (Value::Function(x), Value::Function(y)) => {
                if Rc::ptr_eq(x, y) {
                    Some(cmp::Ordering::Equal)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl cmp::PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        self.partial_cmp(other) == Some(cmp::Ordering::Equal)
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
    use Value::Boolean as B;
    use Value::Number as N;

    if let Some(b) = match op {
        Op::Eq => Some(lhs == rhs),
        Op::Neq => Some(lhs != rhs),
        Op::Lt => lhs.partial_cmp(rhs).map(|x| x == cmp::Ordering::Less),
        Op::Gt => lhs.partial_cmp(rhs).map(|x| x == cmp::Ordering::Greater),
        Op::Lte => lhs.partial_cmp(rhs).map(|x| x != cmp::Ordering::Greater),
        Op::Gte => lhs.partial_cmp(rhs).map(|x| x != cmp::Ordering::Less),
        _ => None,
    } {
        return Ok(B(b));
    }

    match (op, lhs.as_bool()) {
        (Op::And, Some(true)) => return Ok(rhs.clone()),
        (Op::And, Some(false)) => return Ok(lhs.clone()),
        (Op::Or, Some(true)) => return Ok(lhs.clone()),
        (Op::Or, Some(false)) => return Ok(rhs.clone()),
        _ => (),
    }

    let out = match (op, lhs.clone(), rhs.clone()) {
        (Op::Add, N(x), N(y)) => N(x + y),
        (Op::Sub, N(x), N(y)) => N(x - y),
        (Op::Mul, N(x), N(y)) => N(x * y),
        (Op::Div, N(x), N(y)) => N(x / y),

        (Op::Mul, B(x), B(y)) => B(x && y),
        (Op::Add, B(x), B(y)) => B(x || y),
        _ => {
            return Err(EvalError(format!(
                "invalid binary operator '{}' for types {} and {}",
                op.name(),
                lhs.type_name(),
                rhs.type_name()
            )));
        }
    };

    Ok(out)
}

fn evaluate_monop(op: Op, arg: &Value) -> Result<Value, EvalError> {
    use Value::Boolean as B;
    use Value::Number as N;

    let out = match (op, arg.clone()) {
        (Op::Add, N(x)) => N(x),
        (Op::Sub, N(x)) => N(-x),
        (Op::Not, N(x)) => B(x == 0.0),
        (Op::Not, B(x)) => B(!x),
        _ => {
            return Err(EvalError(format!(
                "invalid unary operator '{}' for type {}",
                op.name(),
                arg.type_name()
            )));
        }
    };

    Ok(out)
}

fn evaluate_apply(fun: &Value, args: &Vec<Value>) -> Result<Value, EvalError> {
    let fun = match fun {
        Value::Function(f) => f,
        _ => {
            return Err(EvalError(format!(
                "value of type {} is not callable",
                fun.type_name()
            )))
        }
    };

    fun.call(args)
}

fn bind_vars(node: &Node, bound: &Vec<String>, ctx: &Context) -> Result<Node, EvalError> {
    let out = match node {
        Node::Load(var) => {
            if bound.contains(var) {
                Node::Load(var.clone())
            } else if let Some(val) = ctx.get(var) {
                Node::Immediate(val.clone())
            } else {
                return Err(EvalError(format!("undefined variable '{}'", var)))
            }
        },
        Node::Lambda(args, body) => {
            let new_bound = chain(args, bound).cloned().collect::<Vec<String>>();
            Node::Lambda(args.clone(), Box::new(bind_vars(body, &new_bound, ctx)?))
        }
        Node::BinOp(op, x, y) => {
            Node::BinOp(*op, 
                        Box::new(bind_vars(x, bound, ctx)?), 
                        Box::new(bind_vars(y, bound, ctx)?))
        }
        Node::MonOp(op, x) => {
            Node::MonOp(*op, Box::new(bind_vars(x, bound, ctx)?))
        }
        Node::Apply(fun, args) => {
            let fun = Box::new(bind_vars(fun, bound, ctx)?);
            let mut vals = vec![];
            for arg in args {
                vals.push(bind_vars(arg, bound, ctx)?);
            }
            Node::Apply(fun, vals)
        }
        Node::Immediate(_) => node.clone(),
        Node::Store(_, _) => {
            return Err(EvalError("assignment within lambda is not allowed".into()))
        }
    };

    Ok(out)
}

fn evaluate_lambda(name: Option<&str>, params: &Vec<String>, body: &Node, ctx: &Context) -> Result<Value, EvalError> {
    let fun = ClosureFunc {
        name: name.map(|x| format!("user-defined {}", x)),
        params: params.clone(),
        body: bind_vars(body, params, ctx)?
    };

    Ok(Value::Function(Rc::new(Box::new(fun))))
}

fn evaluate_node(node: &Node, ctx: &mut Context) -> Result<Value, EvalError> {
    match node {
        Node::Immediate(val) => Ok(val.clone()),
        Node::Store(key, arg) => {
            let val = match arg.as_ref() {
                Node::Lambda(args, body) => evaluate_lambda(Some(key), args, body, ctx)?,
                _ => evaluate_node(arg, ctx)?
            };

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
        Node::Lambda(args, body) => evaluate_lambda(None, args, body, ctx)
    }
}

pub fn evaluate(root: &Node, ctx: &mut Context) -> Result<Value, EvalError> {
    evaluate_node(root, ctx)
}

#[cfg(test)]
mod test {
    use super::{evaluate, Context, EvalError, Value};
    use lexer::tokenize;
    use parser::{parse, ParseError};

    fn check(line: &str, expected: Value) {
        let lexer = tokenize(line);
        let root = parse(lexer).unwrap();
        let output = evaluate(&root, &mut Context::new()).unwrap();

        assert_eq!(output, expected);
    }

    #[test]
    fn test_operator() {
        check("1 + 2 * 3", Value::Number(7.0));
        check("1 * 2 + 3", Value::Number(5.0));

        check("2 * 3", Value::Number(6.0));
        check("4 / 2", Value::Number(2.0));
        check("1 + 2", Value::Number(3.0));
        check("2 - 1", Value::Number(1.0));

        check("true * true", Value::Boolean(true));
        check("true * false", Value::Boolean(false));
        check("true + false", Value::Boolean(true));
        check("false + false", Value::Boolean(false));

        check("1 and 0", Value::Number(0.0));
        check("0 and 2", Value::Number(0.0));
        check("3 or 0", Value::Number(3.0));
        check("0 or 4", Value::Number(4.0));

        check("1 and 2 or 3 and 4", Value::Number(2.0));
        check("0 and 2 or 3 and 4", Value::Number(4.0));
        check("0 and 0 or 3 and 4", Value::Number(4.0));
        check("0 and 0 or 0 and 4", Value::Number(0.0));
        check("0 and 0 or 0 and 0", Value::Number(0.0));

        check("1 or 2 and 3 or 4", Value::Number(1.0));
        check("0 or 2 and 3 or 4", Value::Number(3.0));
        check("0 or 0 and 3 or 4", Value::Number(4.0));
        check("0 or 0 and 0 or 4", Value::Number(4.0));
        check("0 or 0 and 0 or 0", Value::Number(0.0));
    }

    #[test]
    fn test_cmp() {
        check("1 == 1", Value::Boolean(true));
        check("1 != 1", Value::Boolean(false));
        check("1 <= 1", Value::Boolean(true));
        check("1 >= 1", Value::Boolean(true));
        check("1 <= 1", Value::Boolean(true));
        check("1 >= 1", Value::Boolean(true));

        check("1 == 2", Value::Boolean(false));
        check("1 != 2", Value::Boolean(true));
        check("1 <= 2", Value::Boolean(true));
        check("1 >= 2", Value::Boolean(false));
        check("1 <= 2", Value::Boolean(true));
        check("1 >= 2", Value::Boolean(false));
    }

    #[test]
    fn test_lambda() {
        check("(x => x)(1)", Value::Number(1.0));
        check("((x) => x)(1)", Value::Number(1.0));
        check("(x => y => x + y)(1)(2)", Value::Number(3.0));
        check("(x => y => z => x+y+z)(1)(2)(3)", Value::Number(6.0));
        check("((x, y) => x + y)(1, 2)", Value::Number(3.0));
    }
}
