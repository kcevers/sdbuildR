# Print last rows of verify results

Wrapper around [`tail()`](https://rdrr.io/r/utils/head.html) that first
converts the results to a data frame using
[`as.data.frame.verify_sdbuildR()`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.verify_sdbuildR.md).

## Usage

``` r
# S3 method for class 'verify_sdbuildR'
tail(x, n = 6L, ...)
```

## Arguments

- x:

  A `verify_sdbuildR` object.

- n:

  Number of rows. Defaults to 6.

- ...:

  Other arguments passed to
  [`as.data.frame.verify_sdbuildR()`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.verify_sdbuildR.md).

## Value

A `data.frame`.

## Examples

``` r
sfm <- sdbuildR("SIR") |>
  unit_test(expr = all(susceptible >= 0))
res <- verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 1/1 test passed.
#> ✔ 1. susceptible is at least 0 (for all values)
tail(res)
#>   test                                      label status outcome
#> 1    1 susceptible is at least 0 (for all values)   pass    TRUE
#>                expr_str condition conditions message
#> 1 all(susceptible >= 0)         1                   
```
