extern crate rand;

use self::rand::random;
use eval::{Context, EvalError, Func, Value};
use std::cmp;
use std::f64::{self, consts};
use std::rc::Rc;

fn set_const(ctx: &mut Context, key: &str, val: f64) {
    ctx.set(key, Value::Number(val))
}

fn check_num_args(name: &str, args: &[Value], want: usize) -> Result<(), EvalError> {
    let (x, y) = (want, args.len());

    if x != y {
        let buffer = format!(
            "{} expected {} argument{}, {} argument{} given",
            name,
            x,
            if x == 1 { "" } else { "s" },
            y,
            if y == 1 { "" } else { "s" }
        );

        Err(EvalError(buffer))
    } else {
        Ok(())
    }
}

fn cast_function(arg: &Value) -> Result<&dyn Func, EvalError> {
    match arg {
        Value::Function(fun) => Ok(&**fun),
        _ => Err(EvalError(format!(
                    "invalid cast of {} to function",
                    arg.type_name())))
    }
}

fn cast_list(arg: &Value) -> Result<&[Value], EvalError> {
    match arg {
        Value::List(list) => Ok(list),
        _ => Err(EvalError(format!(
                    "invalid cast of {} to list",
                    arg.type_name())))
    }
}

fn cast_float(arg: &Value) -> Result<f64, EvalError> {
    match arg {
        Value::Number(x) => Ok(*x),
        Value::Boolean(true) => Ok(1.0),
        Value::Boolean(false) => Ok(0.0),
        _ => Err(EvalError(format!(
                    "invalid cast of {} to number",
                    arg.type_name())))
    }
}

struct ClosureFunction<F>(String, F);
impl<F> Func for ClosureFunction<F>
where
    F: Fn(&[Value]) -> Result<Value, EvalError>,
{
    fn name(&self) -> Option<&str> {
        Some(&self.0)
    }

    fn call(&self, args: &[Value]) -> Result<Value, EvalError> {
        self.1(args)
    }
}

fn set_closure<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(&[Value]) -> Result<Value, EvalError>,
{
    let f = ClosureFunction(key.to_string(), fun);

    ctx.set(key, Value::Function(Rc::new(f)));
}

fn set_unary<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(f64) -> f64,
{
    let name = key.to_owned();
    set_closure(ctx, key, move |args: &[Value]| {
        check_num_args(&name, args, 1)?;
        let x = cast_float(&args[0])?;

        Ok(Value::Number(fun(x)))
    });
}

fn set_binary<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(f64, f64) -> f64,
{
    let name = key.to_owned();
    set_closure(ctx, key, move |args: &[Value]| {
        check_num_args(&name, args, 2)?;
        let x = cast_float(&args[0])?;
        let y = cast_float(&args[1])?;

        Ok(Value::Number(fun(x, y)))
    });
}

fn set_util(ctx: &mut Context) {
    for (key, ord) in &[
        ("max", cmp::Ordering::Less),
        ("min", cmp::Ordering::Greater),
    ] {
        let ord = *ord;

        set_closure(ctx, key, move |args| {
            if args.is_empty() {
                return Err(EvalError(format!("{} of empty sequence", key)));
            }

            let args = match args {
                [Value::List(list)] => list,
                x => x
            };

            let mut best = args[0].clone();

            for arg in args {
                if let Some(x) = best.partial_cmp(arg) {
                    if x == ord {
                        best = arg.clone();
                    }
                } else {
                    return Err(EvalError(format!(
                        "types '{}' and '{}' cannot be compared",
                        best.type_name(),
                        arg.type_name()
                    )));
                }
            }

            Ok(best)
        });
    }

    set_closure(ctx, "rand", |args| {
        let a = args.get(0).map(cast_float).transpose()?.unwrap_or(0.0);
        let b = args.get(1).map(cast_float).transpose()?.unwrap_or(1.0);

        let out = (b - a) * random::<f64>() + a;
        Ok(Value::Number(out))
    });

    set_closure(ctx, "range", |args| {
        check_num_args("range", args, 1)?;
        let n = cast_float(&args[0])?;

        let mut i = 0;
        let mut list = vec![];

        while (i as f64) < n {
            list.push(Value::Number(i as f64));
            i += 1;
        }

        Ok(Value::List(list.into()))
    });

    set_closure(ctx, "map", |args| {
        check_num_args("map", args, 2)?;
        let fun = cast_function(&args[0])?;
        let list = cast_list(&args[1])?;

        let out = list
            .iter()
            .map(|x| fun.call(&[x.clone()]))
            .collect::<Result<Vec<Value>, _>>()?;

        Ok(Value::List(out.into()))
    });

    set_closure(ctx, "linspace", |args| {
        check_num_args("linspace", args, 3)?;
        let lbnd = cast_float(&args[0])?;
        let ubnd = cast_float(&args[1])?;
        let steps = cast_float(&args[2])?;
        let n = steps.floor() as i64;

        if n <= 2 {
            return Err(EvalError(format!("number of steps cannot be less than 2, got {}", steps)));
        }

        let list = (0..n).map(|i| (i as f64) / ((n - 1) as f64))
                         .map(|v| (1.0 - v) * lbnd + v * ubnd)
                         .map(|v| Value::Number(v))
                         .collect::<Vec<_>>();

        Ok(Value::List(list.into()))
    });

    set_closure(ctx, "sort", |args| {
        check_num_args("sort", args, 1)?;
        let mut list = cast_list(&args[0])?.to_vec();
        list.sort_by(|a, b| a.partial_cmp(b).unwrap_or(cmp::Ordering::Equal));

        Ok(Value::List(list.into()))
    });
}

pub fn create() -> Context<'static> {
    let mut ctx = Context::new();

    {
        let c = &mut ctx;
        set_const(c, "pi", consts::PI);
        set_const(c, "e", consts::E);
        set_const(c, "nan", f64::NAN);
        set_const(c, "inf", f64::INFINITY);
        set_const(c, "ninf", -f64::INFINITY);

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

        set_binary(c, "pow", |x, y| x.powf(y));
        set_binary(c, "log", |x, y| x.log(y));
        set_binary(c, "hypot", |x, y| x.hypot(y));
        set_binary(c, "atan2", |x, y| x.atan2(y));

        set_util(c);
    }

    ctx
}
