# Rust calculator

Simple command-line based calculator written in Rust. Supports basic arithmetic floating-point operations (addition, subtaction, multiplication, division, modulo) and the following functions:

* `pi()`
* `e()`
* `log(x)` and `log(x, base)`
* `exp(x)`
* `sin(x)`
* `cos(x)`
* `tan(x)`
* `sqrt(x)`
* `abs(x)`
* `max(a, b, c, ...)`
* `min(a, b, c, ...)`

Note that parentheses are optional for nullary functions, so `pi()` can also be written as simply `pi`.


## Examples

```
$ ./calc 42
42
$ ./calc sin(2 * pi)
0
$ ./calc abs(max(-3.2, -5.7))
3.2
$ ./calc log(e, e)
1
```
