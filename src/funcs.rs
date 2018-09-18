extern crate rand;

use self::rand::random;
use eval::{Context, EvalError, Func, Value};
use std::cmp;
use std::f64::consts;
use std::rc::Rc;

fn set_const(ctx: &mut Context, key: &str, val: f64) {
    ctx.set(key, Value::Number(val))
}

fn check_num_args(args: &Vec<Value>, want: usize) -> Result<(), EvalError> {
    let (x, y) = (want, args.len());

    if x != y {
        let buffer = format!(
            "expected {} argument{}, {} argument{} given",
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

fn check_number(arg: &Value) -> Result<f64, EvalError> {
    if let Some(x) = arg.as_number() {
        Ok(x)
    } else {
        Err(EvalError(format!(
            "invalid cast of {} to number",
            arg.type_name()
        )))
    }
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

fn set_closure<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(&Vec<Value>) -> Result<Value, EvalError>,
{
    let f = ClosureFunction(key.to_string(), fun);

    ctx.set(key, Value::Function(Rc::new(Box::new(f))));
}

fn set_unary<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(f64) -> f64,
{
    set_closure(ctx, key, move |args: &Vec<Value>| {
        check_num_args(args, 1)?;
        let x = check_number(&args[0])?;

        Ok(Value::Number(fun(x)))
    });
}

fn set_binary<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(f64, f64) -> f64,
{
    set_closure(ctx, key, move |args: &Vec<Value>| {
        check_num_args(args, 2)?;
        let x = check_number(&args[0])?;
        let y = check_number(&args[1])?;

        Ok(Value::Number(fun(x, y)))
    });
}

pub fn create() -> Context<'static> {
    let mut ctx = Context::new();

    {
        let c = &mut ctx;
        set_const(c, "pi", consts::PI);
        set_const(c, "e", consts::E);

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

        for (key, ord) in vec![
            ("max", cmp::Ordering::Less),
            ("min", cmp::Ordering::Greater),
        ] {
            set_closure(c, key, move |args| {
                if args.len() == 0 {
                    return Err(EvalError(format!("{} of empty sequence", key)));
                }

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

        set_closure(c, "rand", |args| {
            let (a, b) = match args.len() {
                0 => (0.0, 1.0),
                1 => (0.0, check_number(&args[0])?),
                _ => (check_number(&args[0])?, check_number(&args[1])?),
            };

            let out = (b - a) * random::<f64>() + a;
            Ok(Value::Number(out))
        });
    }

    ctx
}
