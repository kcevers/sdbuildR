# Add or modify constants

Constants are time-independent variables that do not change over the
course of a simulation. `constant()` adds or changes a constant
variable. This is a convenience wrapper around
[`update()`](https://rdrr.io/r/stats/update.html) with
`type = "constant"`. See the **Constants** section of
[`update()`](https://rdrr.io/r/stats/update.html) for more details.

## Usage

``` r
constant(object, name, eqn = 0, label = name, doc = "", non_negative = FALSE)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`sdbuildR`](https://kcevers.github.io/sdbuildR/reference/sdbuildR.md).

- name:

  Variable name. Accepts a bare symbol (e.g., `population`), a string
  (`"population"`), or a vector via
  [`c()`](https://rdrr.io/r/base/c.html) (e.g., `c(a, b)` or
  `c("a", "b")`). Use `!!` to inject from a variable.

- eqn:

  Equation (or initial value in the case of stocks). Accepts a bare
  expression (e.g., `a * b + 1`), a string (`"a * b + 1"`), or a numeric
  value. Use `!!` to inject from a variable. Defaults to `0`.

- label:

  Name of variable used for plotting. Defaults to the same as name.

- doc:

  Description of variable. Defaults to `""` (no description).

- non_negative:

  If TRUE, variable is enforced to be non-negative (i.e., strictly 0 or
  positive). Defaults to `FALSE`.

## Value

A stock-and-flow model object of class
[`sdbuildR`](https://kcevers.github.io/sdbuildR/reference/sdbuildR.md)

## See also

[`update()`](https://rdrr.io/r/stats/update.html),
[`discard()`](https://kcevers.github.io/sdbuildR/reference/discard.md),
[`change_name()`](https://kcevers.github.io/sdbuildR/reference/change_name.md)

## Examples

``` r

# Create constants for model parameters
sfm <- sdbuildR() |>
  constant(growth_rate, eqn = 0.1, label = "Growth Rate") |>
  constant(carrying_capacity, eqn = 1000, label = "Carrying Capacity")
```
