# Add or modify auxiliaries

Auxiliaries are dynamic variables used for intermediate calculations in
the system. `auxiliary()` adds or changes an auxiliary variable. This is
a convenience wrapper around
[`update()`](https://rdrr.io/r/stats/update.html) with `type = "aux"`.
See the **Auxiliaries** section of
[`update()`](https://kcevers.github.io/sdbuildR/reference/update.sdbuildR.md)
for more details.

## Usage

``` r
auxiliary(object, name, eqn = 0, label = name, doc = "", non_negative = FALSE)

aux(object, name, eqn = 0, label = name, doc = "", non_negative = FALSE)
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

[`update()`](https://kcevers.github.io/sdbuildR/reference/update.sdbuildR.md),
[`discard()`](https://kcevers.github.io/sdbuildR/reference/discard.md),
[`change_name()`](https://kcevers.github.io/sdbuildR/reference/change_name.md)

## Examples

``` r

# Create an auxiliary for an intermediate calculation
sfm <- sdbuildR() |>
  stock(population, eqn = 100) |>
  constant(carrying_capacity, eqn = 1000) |>
  auxiliary(density, eqn = population / carrying_capacity, label = "Density")
```
