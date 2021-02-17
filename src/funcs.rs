extern crate rand;

use self::rand::random;
use eval::{Callable, Context, EvalError, Value};
use std::cmp;
use std::f64::{self, consts};
use std::rc::Rc;

fn set_const(ctx: &mut Context, key: &str, val: f64) {
    ctx.set(key, Value::Number(val))
}

fn check_args_n<'a>(name: &str, args: &'a [Value], n: usize) -> Result<&'a [Value], EvalError> {
    let k = args.len();

    if k != n {
        raise!(
            EvalError,
            "{} expected {} argument{}, {} argument{} given",
            name,
            n,
            if n == 1 { "" } else { "s" },
            k,
            if k == 1 { "" } else { "s" }
        );
    }

    Ok(args)
}

fn check_args_1<'a>(name: &str, args: &'a [Value]) -> Result<&'a Value, EvalError> {
    let args = check_args_n(name, args, 1)?;
    Ok(&args[0])
}

fn check_args_2<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 2], EvalError> {
    let args = check_args_n(name, args, 2)?;
    Ok([&args[0], &args[1]])
}

fn check_args_3<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 3], EvalError> {
    let args = check_args_n(name, args, 3)?;
    Ok([&args[0], &args[1], &args[2]])
}

fn cast_function(arg: &Value) -> Result<&dyn Callable, EvalError> {
    match arg {
        Value::Function(fun) => Ok(&**fun),
        _ => raise!(EvalError, "invalid cast of {} to function", arg.type_name()),
    }
}

fn cast_list(arg: &Value) -> Result<&[Value], EvalError> {
    match arg {
        Value::List(list) => Ok(list),
        _ => raise!(EvalError, "invalid cast of {} to list", arg.type_name()),
    }
}

fn cast_float(arg: &Value) -> Result<f64, EvalError> {
    match arg {
        Value::Number(x) => Ok(*x),
        Value::Boolean(true) => Ok(1.0),
        Value::Boolean(false) => Ok(0.0),
        _ => raise!(EvalError, "invalid cast of {} to number", arg.type_name()),
    }
}

struct ClosureFunction<F>(String, F);
impl<F> Callable for ClosureFunction<F>
where
    F: Fn(&[Value], &mut Context) -> Result<Value, EvalError>,
{
    fn name(&self) -> Option<&str> {
        Some(&self.0)
    }

    fn call(&self, args: &[Value], ctx: &mut Context) -> Result<Value, EvalError> {
        self.1(args, ctx)
    }
}

fn set_closure<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(&[Value], &mut Context) -> Result<Value, EvalError>,
{
    let f = ClosureFunction(key.to_string(), fun);
    ctx.set(key, Value::Function(Rc::new(f)));
}

fn set_unary<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(f64) -> f64,
{
    let name = key.to_owned();
    set_closure(ctx, key, move |args, _| {
        let x = check_args_1(&name, args)?;
        let x = cast_float(x)?;

        Ok(Value::Number(fun(x)))
    });
}

fn set_binary<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(f64, f64) -> f64,
{
    let name = key.to_owned();
    set_closure(ctx, key, move |args, _| {
        let [x, y] = check_args_2(&name, args)?;
        let x = cast_float(x)?;
        let y = cast_float(y)?;

        Ok(Value::Number(fun(x, y)))
    });
}

fn set_util(ctx: &mut Context) {
    for (key, ord) in &[
        ("max", cmp::Ordering::Less),
        ("min", cmp::Ordering::Greater),
    ] {
        let ord = *ord;

        set_closure(ctx, key, move |args, _| {
            if args.is_empty() {
                raise!(EvalError, "{} of empty sequence", key);
            }

            let args = match args {
                [Value::List(list)] => list,
                x => x,
            };

            let mut best = args[0].clone();

            for arg in args {
                if let Some(x) = best.partial_cmp(arg) {
                    if x == ord {
                        best = arg.clone();
                    }
                } else {
                    raise!(
                        EvalError,
                        "types '{}' and '{}' cannot be compared",
                        best.type_name(),
                        arg.type_name()
                    );
                }
            }

            Ok(best)
        });
    }

    set_closure(ctx, "rand", |args, _| {
        let a = args.get(0).map(cast_float).transpose()?;
        let b = args.get(1).map(cast_float).transpose()?;

        let (a, b) = match (a, b) {
            (Some(x), Some(y)) => (x, y),
            (Some(x), None) => (0.0, x),
            _ => (0.0, 1.0),
        };

        let out = (b - a) * random::<f64>() + a;
        Ok(Value::Number(out))
    });

    set_closure(ctx, "range", |args, _| {
        let a = args.get(0).map(cast_float).transpose()?;
        let b = args.get(1).map(cast_float).transpose()?;

        let (a, b) = match (a, b) {
            (Some(x), Some(y)) => (x, y),
            (Some(x), None) => (0.0, x),
            _ => (0.0, 0.0),
        };

        let mut i = 0;
        let mut list = vec![];

        while a + (i as f64) < b {
            list.push(Value::Number(a + (i as f64)));
            i += 1;
        }

        Ok(Value::List(list.into()))
    });

    set_closure(ctx, "map", |args, ctx| {
        let [fun, list] = check_args_2("map", args)?;
        let fun = cast_function(fun)?;
        let list = cast_list(list)?;

        let out = list
            .iter()
            .map(|x| ctx.call(fun, &[x.clone()]))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Value::List(out.into()))
    });

    set_closure(ctx, "linspace", |args, _| {
        let [lbnd, ubnd, steps] = check_args_3("linspace", args)?;
        let lbnd = cast_float(lbnd)?;
        let ubnd = cast_float(ubnd)?;
        let steps = cast_float(steps)?;
        let n = steps.floor() as i64;

        if n <= 2 {
            raise!(
                EvalError,
                "number of steps cannot be less than 2, got {}",
                steps
            );
        }

        let list = (0..n)
            .map(|i| (i as f64) / ((n - 1) as f64))
            .map(|v| (1.0 - v) * lbnd + v * ubnd)
            .map(|v| Value::Number(v))
            .collect::<Vec<_>>();

        Ok(Value::List(list.into()))
    });

    set_closure(ctx, "sort", |args, _| {
        let list = check_args_1("sort", args)?;
        let mut vec = cast_list(list)?.to_vec();
        vec.sort_by(|a, b| a.partial_cmp(b).unwrap_or(cmp::Ordering::Equal));

        Ok(Value::List(vec.into()))
    });

    set_closure(ctx, "length", |args, _| {
        let list = check_args_1("length", args)?;
        let n = cast_list(list)?.len();

        Ok(Value::Number(n as f64))
    });

    //set_closure(ctx, "sum", |args| {
    //    check_args_n("sum", args, 1)?;
    //    let mut list = cast_list(&args[0])?.to_vec();
    //    Ok(Value::List(list.into()))
    //});
}

pub fn create() -> Context<'static> {
    let mut ctx = Context::new();

    {
        let c = &mut ctx;
        set_const(c, "pi", consts::PI);
        set_const(c, "e", consts::E);
        set_const(c, "nan", f64::NAN);
        set_const(c, "inf", f64::INFINITY);

        set_unary(c, "asin", |x| x.asin());
        set_unary(c, "acos", |x| x.acos());
        set_unary(c, "atan", |x| x.atan());
        set_unary(c, "sin", |x| x.sin());
        set_unary(c, "cos", |x| x.cos());
        set_unary(c, "tan", |x| x.tan());
        set_unary(c, "ln", |x| x.ln());
        set_unary(c, "log10", |x| x.log10());
        set_unary(c, "log2", |x| x.log2());
        set_unary(c, "abs", |x| x.abs());
        set_unary(c, "ceil", |x| x.ceil());
        set_unary(c, "floor", |x| x.floor());
        set_unary(c, "round", |x| x.round());
        set_unary(c, "sqrt", |x| x.sqrt());
        set_unary(c, "exp", |x| x.exp());
        set_unary(c, "float", |x| x);
        set_unary(c, "sign", |x| x.signum());

        set_binary(c, "pow", |x, y| x.powf(y));
        set_binary(c, "log", |x, y| x.log(y));
        set_binary(c, "hypot", |x, y| x.hypot(y));
        set_binary(c, "atan2", |x, y| x.atan2(y));

        set_util(c);
    }

    ctx
}
