# Remove a unit test from a stock-and-flow model

Remove one or more unit tests by `test` (integer position as shown by
[`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md))
or by `label` (character). Warns if a label or index is not found.
Remaining tests are renumbered sequentially after removal.

## Usage

``` r
discard_unit_test(object, label, test)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`stockflow`](https://kcevers.github.io/sdbuildR/reference/stockflow.md).

- label:

  Character label(s) of the test(s) to remove. Supports NSE (bare symbol
  or string). For backward compatibility, integer values passed via
  `label` are also accepted.

- test:

  Integer index/indices of the test(s) to remove. Corresponds to the
  order shown by
  [`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md).

## Value

The model object with the specified test(s) removed.

## See also

[`unit_test()`](https://kcevers.github.io/sdbuildR/reference/unit_test.md),
[`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md)

## Examples

``` r
sfm <- stockflow("SIR") |>
  unit_test(label = "susceptible is non-negative", expr = all(susceptible >= 0)) |>
  unit_test(label = "recovered increases", expr = all(diff(recovered) >= 0))

# Remove by test
sfm <- discard_unit_test(sfm, test = 1)

# Remove by label
sfm <- discard_unit_test(sfm, label = "recovered increases")
```
