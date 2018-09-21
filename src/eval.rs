extern crate itertools;

use self::itertools::chain;
use parser::{Node, Op};
use std::collections::HashMap;
use std::f64::EPSILON;
use std::rc::Rc;
use std::{cmp, fmt};
use std::cell::RefCell;

pub trait Func {
    fn name(&self) -> Option<&str>;
    fn call(&self, args: &[Value]) -> Result<Value, EvalError>;
}

struct ClosureFunc {
    name: Option<String>,
    params: Vec<String>,
    body: RefCell<Node>,
}

impl Func for ClosureFunc {
    fn name(&self) -> Option<&str> {
        match self.name {
            Some(ref x) => Some(&x),
            None => None,
        }
    }

    fn call(&self, args: &[Value]) -> Result<Value, EvalError> {
        if self.params.len() != args.len() {
            return Err(EvalError("wrong number of arguments".to_string()));
        }

        let mut ctx = Context::new();
        for (param, arg) in self.params.iter().zip(args) {
            ctx.set(param, arg.clone());
        }

        evaluate_node(&self.body.borrow(), &mut ctx)
    }
}


#[derive(Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    List(Rc<Vec<Value>>),
    Function(Rc<dyn Func>),
}

impl Value {
    pub fn from_number(x: f64) -> Value {
        Value::Number(x)
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            Value::Number(x) => Some(*x),
            Value::Boolean(true) => Some(1.0),
            Value::Boolean(false) => Some(0.0),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Boolean(x) => *x,
            Value::Number(x) => *x != 0.0,
            Value::Function(_) => true,
            Value::List(lst) => !lst.is_empty(),
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Value::Boolean(_) => "boolean",
            Value::Number(_) => "number",
            Value::Function(_) => "function",
            Value::List(_) => "list",
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(x) => write!(f, "Number({:?})", x),
            Value::Boolean(x) => write!(f, "Boolean({:?})", x),
            Value::Function(_) => write!(f, "Function(...)"),
            Value::List(x) => write!(f, "{:?}", x),
        }
    }
}

impl cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<cmp::Ordering> {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => x.partial_cmp(y),
            (Value::Boolean(x), Value::Boolean(y)) => x.partial_cmp(y),
            (Value::List(x), Value::List(y)) => x.partial_cmp(y),
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

        None
    }

    pub fn set(&mut self, key: &str, val: Value) {
        self.scope.insert(key.into(), val);
    }
}

fn evaluate_binop(op: Op, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    use Value::Boolean as B;
    use Value::List as L;
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

    let out = match (op, lhs.clone(), rhs.clone()) {
        (Op::Add, L(x), L(y)) => {
            let mut tmp = vec![];
            tmp.extend(x.iter().cloned());
            tmp.extend(y.iter().cloned());
            L(Rc::new(tmp))
        }

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

    let out = match (op, arg) {
        (Op::Add, N(x)) => N(*x),
        (Op::Sub, N(x)) => N(-*x),
        (Op::Not, x) => B(!x.as_bool()),
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

fn evaluate_apply(fun: &Value, args: &[Value]) -> Result<Value, EvalError> {
    if let Value::Function(f) = fun {
        f.call(args)
    } else {
        Err(EvalError(format!(
            "value of type {} is not callable",
            fun.type_name()
        )))
    }
}

fn evaluate_index(list: &Value, index: &Value) -> Result<Value, EvalError> {
    match (list, index) {
        (Value::List(list), Value::Number(f)) => {
            let i = f.round() as i64;
            let n = list.len();

            if !f.is_finite() || (*f - i as f64).abs() <= EPSILON {
                return Err(EvalError(format!("{} cannot be used as index", f)));
            }

            match list.get(i as usize) {
                Some(v) => Ok(v.clone()),
                _ => Err(EvalError(format!(
                    "index {} is out of bounds for list of size {}",
                    i, n
                ))),
            }
        }
        (Value::List(_), x) => Err(EvalError(format!(
            "value of type {} cannot be used as index",
            x.type_name()
        ))),
        (x, _) => Err(EvalError(format!(
            "value of type {} cannot be indexed",
            x.type_name()
        ))),
    }
}

fn bind_vars(node: &Node, bound: &[String], ctx: &Context) -> Result<Node, EvalError> {
    let out = match node {
        Node::Var(var) => {
            if bound.contains(var) {
                Node::Var(var.clone())
            } else if let Some(val) = ctx.get(var) {
                Node::Immediate(val.clone())
            } else {
                return Err(EvalError(format!("undefined variable '{}'", var)));
            }
        }
        Node::Lambda(args, body) => {
            let new_bound: Vec<_> = chain(args, bound).cloned().collect();
            Node::Lambda(args.clone(), box bind_vars(body, &new_bound, ctx)?)
        }
        Node::BinOp(op, x, y) => Node::BinOp(
            *op,
            box bind_vars(x, bound, ctx)?,
            box bind_vars(y, bound, ctx)?,
        ),
        Node::MonOp(op, x) => Node::MonOp(*op, box bind_vars(x, bound, ctx)?),
        Node::Apply(fun, args) => {
            let fun = box bind_vars(fun, bound, ctx)?;
            let mut vals = vec![];
            for arg in args {
                vals.push(bind_vars(arg, bound, ctx)?);
            }
            Node::Apply(fun, vals)
        }
        Node::Index(lhs, rhs) => Node::Index(
            box bind_vars(lhs, bound, ctx)?,
            box bind_vars(rhs, bound, ctx)?,
        ),
        Node::List(args) => {
            let mut vals = vec![];
            for arg in args {
                vals.push(bind_vars(arg, bound, ctx)?);
            }
            Node::List(vals)
        }
        Node::Immediate(_) => node.clone(),
        Node::VarDef(_, _) | Node::FunDef(_, _, _) => {
            return Err(EvalError("assignment within lambda is not allowed".into()))
        }
    };

    Ok(out)
}

fn evaluate_lambda(
    name: Option<&str>,
    params: &[String],
    body: &Node,
    ctx: &Context,
) -> Result<Value, EvalError> {
    let fun = ClosureFunc {
        name: name.map(|x| format!("user-defined {}", x)),
        params: params.to_owned(),
        body: bind_vars(body, params, ctx)?.into(),
    };

    Ok(Value::Function(Rc::new(fun)))
}

fn evaluate_node(node: &Node, ctx: &mut Context) -> Result<Value, EvalError> {
    match node {
        Node::Immediate(val) => Ok(val.clone()),
        Node::VarDef(key, arg) => {
            let val = evaluate_node(arg, ctx)?;
            ctx.set(key, val.clone());
            Ok(val)
        }
        Node::FunDef(var, params, body) => {
            let dummy = ClosureFunc {
                name: Some(var.clone()),
                params: params.to_owned(),
                body: Node::Var("?".into()).into()
            };
            let cell = Rc::new(dummy);
            let fun = Value::Function(cell.clone());

            *cell.body.borrow_mut() = {
                let mut child_ctx = Context::with_parent(ctx);
                child_ctx.set(var, fun.clone());
                bind_vars(body, params, &child_ctx)?
            };

            ctx.set(var, fun.clone());
            Ok(fun)
        }
        Node::Var(var) => {
            if let Some(val) = ctx.get(var) {
                Ok(val)
            } else {
                Err(EvalError(format!("undefined variable '{}'", var)))
            }
        }
        Node::BinOp(Op::And, lhs, rhs) => {
            let x = evaluate_node(lhs, ctx)?;
            if x.as_bool() {
                evaluate_node(rhs, ctx)
            } else {
                Ok(x)
            }
        }
        Node::BinOp(Op::Or, lhs, rhs) => {
            let x = evaluate_node(lhs, ctx)?;
            if !x.as_bool() {
                evaluate_node(rhs, ctx)
            } else {
                Ok(x)
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
        Node::Index(lhs, rhs) => {
            let lhs = evaluate_node(lhs, ctx)?;
            let rhs = evaluate_node(rhs, ctx)?;
            evaluate_index(&lhs, &rhs)
        }
        Node::List(args) => {
            let mut vals = vec![];
            for arg in args {
                vals.push(evaluate_node(arg, ctx)?);
            }
            Ok(Value::List(Rc::new(vals)))
        }
        Node::Lambda(args, body) => evaluate_lambda(None, args, body, ctx),
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
