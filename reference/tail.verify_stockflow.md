# Print last rows of verify results

Wrapper around [`tail()`](https://rdrr.io/r/utils/head.html) that first
converts the results to a data frame using
[`as.data.frame.verify_stockflow()`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.verify_stockflow.md).

## Usage

``` r
# S3 method for class 'verify_stockflow'
tail(x, n = 6L, ...)
```

## Arguments

- x:

  A `verify_stockflow` object.

- n:

  Number of rows. Defaults to 6.

- ...:

  Other arguments passed to
  [`as.data.frame.verify_stockflow()`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.verify_stockflow.md).

## Value

A `data.frame`.

## Examples

``` r
sfm <- stockflow("SIR") |>
  unit_test(expr = all(susceptible >= 0))
res <- verify(sfm)
tail(res)
#>   test                                      label status outcome
#> 1    1 susceptible is at least 0 (for all values)   pass    TRUE
#>                expr_str condition conditions message
#> 1 all(susceptible >= 0)         1                   
```
