# Add or modify stocks

Stocks accumulate material or information over time, defining the state
of the system. `stock()` adds or changes a stock variable. This is a
convenience wrapper around
[`update()`](https://rdrr.io/r/stats/update.html) with `type = "stock"`.
See the **Stocks** section of
[`update()`](https://rdrr.io/r/stats/update.html) for more details.

## Usage

``` r
stock(object, name, eqn = 0, label = name, doc = "", non_negative = FALSE)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

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
[`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md)

## See also

[`update()`](https://rdrr.io/r/stats/update.html),
[`discard()`](https://kcevers.github.io/sdbuildR/reference/discard.md),
[`change_name()`](https://kcevers.github.io/sdbuildR/reference/change_name.md)

## Examples

``` r

# Create a stock with an initial value
sfm <- stockflow() |>
  stock(population, eqn = 100, label = "Population")

# Multiple stocks
sfm <- stockflow() |>
  stock(susceptible, eqn = 999, label = "susceptible") |>
  stock(infected, eqn = 1, label = "infected") |>
  stock(recovered, eqn = 0, label = "recovered")
```
