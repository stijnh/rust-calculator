# Rust calculator

Simple command-line based calculator written in Rust. Supports many arithmetic functions, boolean logic, lists, and basic functions/lambdas.

## Value types

The calculator supports four value types:

* Numbers (examples: `1`, `3.2`, `-200`, `1.3333`) 
* Booleans (`true` and `false`)
* Lists (`[1, true, -4.1]`, `[]`, `[sin, cos, tan]`, `[true, false]`)
* Functions (pre-defined and user-defined)

## Arithmetic Options

Supports the following operations:

* Arithmetic
    * Addition: `a + b`
    * Subtraction: `a - b`
    * Multiplication: `a * b`
    * Division: `a / b`
* Relational
    * Equal: `a == b`, `a != b`
    * Compare: `a < b`, `a <= b`
* Logic
    * Conjunction: `a and b`
    * Disjunction: `a or b`
    * Negation: `not a`
    * Ternary: `a if b else c`


## Defining functions
Functions are first-call citizens and can be used like any other value.
They can be passed around like any other value, making thing like this possible.

```
>>> foo = sin == cos
  false
>>> index = -(sin if foo else cos)(pi)
  1  
>>> [sin, cos][index](pi)
  -1
```


New functions can be defined using the lambda syntax.

```
>>> foo = x => x + 1
```

Alternatively, they can be defined using the following special syntax.

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
* `rand()`, `rand(stop)`, `rand(start, stop)`: random float (default range is 0.0 to 1.0).
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
