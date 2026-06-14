# Create or modify custom variables or functions

Custom functions are user-defined functions that can be used throughout
a stock-and-flow model. `custom_func()` adds or changes a function. This
is a convenience wrapper around
[`update()`](https://rdrr.io/r/stats/update.html) with `type = "func"`.

## Usage

``` r
custom_func(object, name, eqn = 0, label = name, doc = "")
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`sdbuildR`](https://kcevers.github.io/sdbuildR/reference/sdbuildR.md).

- name:

  Name of the function variable. The equation will be assigned to this
  name.

- eqn:

  Equation of the function variable. A character vector. Defaults to
  `0`.

- label:

  Name of variable used for plotting. Defaults to the same as name.

- doc:

  Documentation. Defaults to "".

## Value

A stock-and-flow model object of class
[`sdbuildR`](https://kcevers.github.io/sdbuildR/reference/sdbuildR.md)

## See also

[`update()`](https://kcevers.github.io/sdbuildR/reference/update.sdbuildR.md),
[`discard()`](https://kcevers.github.io/sdbuildR/reference/discard.md),
[`change_name()`](https://kcevers.github.io/sdbuildR/reference/change_name.md)

## Examples

``` r

# Simple function
sfm <- sdbuildR() |>
  custom_func(double, eqn = "function(x) x * 2") |>
  constant(a, eqn = double(2))

# Function with defaults
sfm <- sdbuildR() |>
  custom_func(scale, eqn = "function(x, factor = 10) x * factor") |>
  constant(b, eqn = scale(2))

# If the logistic() function did not exist, you could create it yourself:
sfm <- sdbuildR() |>
  custom_func(my_logistic, eqn = "function(x, slope = 1, midpoint = .5){
   1 / (1 + exp(-slope*(x-midpoint)))
 }") |>
  constant(c_, eqn = my_logistic(2, slope = 50))
```
