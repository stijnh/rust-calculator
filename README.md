# Rust calculator

Simple command-line based calculator written in Rust. Supports many arithmetic functions, boolean logic, lists, and basic lambda functions.

## Value types

Supports four value types:

* Numbers: `1`, `3.2`, `-200` 
* Booleans: `true` and `false`
* Lists: `[1, true, -4.1]`
* Functions


## User-defined functions
Functions are first-call citizens and can be used like any other value.
They can be defined using the lambda syntax.

```
>>> foo = x => x + 1
```

Alternatively, they can be defined using the following syntax.

```
>>> foo(x) = x + 1
```

Note that this syntax allows recursive definitions which is not possible with the lamda syntax.

```
>>> factorial(n) = factorial(n - 1) * n if n > 0 else 1
```



## Predefined constants and functions.

Supports the following constants.

* `pi`
* `e`
* `nan`
* `inf`

The following common mathematical functions are supported.

* `sin(x)`, `cos(x)`, `tan(x)`
* `asin(x)`, `acos(x)`, `atan(x)`
* `ln(x)`, `log10(x)`, `log2(x)`, `log(x, base)`
* `round(x)`, `floor(x)`, `ceil(x)`
* `sqrt(x)`, `exp(x)`, `pow(x, e)`
* `abs(x)`

The following Python-like utility functions are included.

* `min(...)`: minimum of arguments.
* `max(...)`: maximum of arguments.
* `rand()`, `rand(stop)`, `rand(start, stop)`: random float (default start is 0.0, stop is 1.0).
* `range(stop)`, `range(start, stop)`: list `[start, start+1, ..., stop-1, stop]`.
* `map(f, list)`: applies `f` to each element of `list`.
* `sort(list)`: sort elements of `list`
* `length(list)`: length of `list`



## Examples

```
$ cargo run
>>> 1 + 1
 2
>>> factorial(n) = factorial(n - 1) * n if n > 0 else 1
 <function: factorial>
>>> factorial(6)
 720
>>> x = range(5)
 [0, 1, 2, 3, 4]
>>> map(factorial, x)
 [1, 1, 2, 6, 24]
>>> factorial(1000)
 error: stack overflow, call depth cannot exceed 512
```
