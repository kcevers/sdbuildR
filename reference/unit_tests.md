# Display unit tests defined on a stock-and-flow model

Returns an overview of all unit tests attached to the model. The result
has a [`print()`](https://rdrr.io/r/base/print.html) method.

## Usage

``` r
unit_tests(object, test = NULL, label = NULL, ignore_case = TRUE)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

- test:

  Integer vector of test number(s) to display (1-based). Defaults to
  `NULL` (show all tests). Can be combined with `label` (intersection).

- label:

  Character vector of regex patterns for partial, case-insensitive label
  matching. A test is included if its label matches *any* pattern. E.g.,
  `c("non-neg", "beta")` returns tests matching either fragment. Can be
  combined with `test` (intersection).

- ignore_case:

  Logical; whether `label` matching is case-insensitive. Default `TRUE`.

## Value

An object of class `unit_tests_stockflow`, printed automatically.

## See also

[`unit_test()`](https://kcevers.github.io/sdbuildR/reference/unit_test.md),
[`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md)

## Examples

``` r
sfm <- stockflow("SIR") |>
  unit_test(expr = all(susceptible >= 0)) |>
  unit_test(
    label = "recovered increases over time",
    expr = all(diff(recovered) >= 0)
  )

unit_tests(sfm)
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 2 tests • 2/2 active • 0/2 include conditions
#> • 1. susceptible is at least 0 (for all values)
#>   `all(susceptible >= 0)`
#> • 2. recovered increases over time
#>   `all(diff(recovered) >= 0)`
unit_tests(sfm, test = 1L)
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 1 tests • 1/1 active • 0/1 include conditions
#> • 1. susceptible is at least 0 (for all values)
#>   `all(susceptible >= 0)`
unit_tests(sfm, label = "increases")
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 1 tests • 1/1 active • 0/1 include conditions
#> • 2. recovered increases over time
#>   `all(diff(recovered) >= 0)`
```
