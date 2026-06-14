# Add or modify flows

Flows move material and information through the system, increasing or
decreasing stocks. `flow()` adds or changes a flow variable. This is a
convenience wrapper around
[`update()`](https://rdrr.io/r/stats/update.html) with `type = "flow"`.
See the **Flows** section of
[`update()`](https://rdrr.io/r/stats/update.html) for more details.

## Usage

``` r
flow(
  object,
  name,
  eqn = 0,
  to = NULL,
  from = NULL,
  label = name,
  doc = "",
  non_negative = FALSE
)
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

- to:

  Target of flow. Accepts a bare symbol or string. Must be a stock in
  the model. Defaults to `NULL` to indicate no target.

- from:

  Source of flow. Accepts a bare symbol or string. Must be a stock in
  the model. Defaults to `NULL` to indicate no source.

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

# Create a flow into a stock
sfm <- sdbuildR() |>
  stock(population, eqn = 100) |>
  flow(births, eqn = population * 0.1, to = population) |>
  flow(deaths, eqn = population * 0.05, from = population)
```
