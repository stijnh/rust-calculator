use eval::{Context, EvalError, Func, Value};
use std::f64::consts;
use std::rc::Rc;

fn set_const(ctx: &mut Context, key: &str, val: f64) {
    ctx.set(key, Value::Number(val))
}

struct ClosureFunction<F>(String, F);
impl<F> Func for ClosureFunction<F>
where
    F: Fn(&Vec<Value>) -> Result<Value, EvalError>,
{
    fn name(&self) -> Option<&str> {
        Some(&self.0)
    }

    fn call(&self, args: &Vec<Value>) -> Result<Value, EvalError> {
        self.1(args)
    }
}

fn set_unary<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(f64) -> f64,
{
    let f = ClosureFunction(key.to_string(), move |args: &Vec<Value>| {
        if args.len() != 1 {
            return Err(EvalError(format!(
                "expected 1 argument, {} arguments given",
                args.len()
            )));
        }

        let x = match args[0] {
            Value::Number(x) => x,
            _ => return Err(EvalError("invalid cast to number".into())),
        };

        Ok(Value::Number(fun(x)))
    });

    ctx.set(key, Value::Function(Rc::new(Box::new(f))));
}

fn set_binary<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(f64, f64) -> f64,
{
    let f = ClosureFunction(key.to_string(), move |args: &Vec<Value>| {
        if args.len() != 2 {
            return Err(EvalError(format!(
                "expected 1 argument, {} arguments given",
                args.len()
            )));
        }

        let (x, y) = match (&args[0], &args[1]) {
            (Value::Number(x), Value::Number(y)) => (*x, *y),
            _ => return Err(EvalError("invalid cast to number".into())),
        };

        Ok(Value::Number(fun(x, y)))
    });

    ctx.set(key, Value::Function(Rc::new(Box::new(f))));
}

pub fn create() -> Context<'static> {
    let mut ctx = Context::new();

    {
        let c = &mut ctx;
        set_const(c, "pi", consts::PI);
        set_const(c, "e", consts::E);
        set_const(c, "e", consts::E);

        set_unary(c, "sin", |x| x.sin());
        set_unary(c, "cos", |x| x.cos());
        set_unary(c, "ln", |x| x.ln());
        set_unary(c, "log10", |x| x.log10());
        set_unary(c, "log2", |x| x.log2());
        set_unary(c, "abs", |x| x.abs());
        set_unary(c, "ceil", |x| x.ceil());
        set_unary(c, "floor", |x| x.floor());
        set_unary(c, "round", |x| x.round());
        set_unary(c, "sqrt", |x| x.sqrt());
        set_unary(c, "exp", |x| x.exp());

        set_binary(c, "pow", |x, y| x.powf(y));
        set_binary(c, "log", |x, y| x.log(y));
    }

    ctx
}
