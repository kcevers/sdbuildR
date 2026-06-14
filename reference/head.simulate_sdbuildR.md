# Print first rows of a simulation

Print the first rows of a simulation data frame of a stock-and-flow
model. This is a wrapper around
[`head()`](https://rdrr.io/r/utils/head.html) that first converts the
simulation results to a data frame using
[as.data.frame()](https://kcevers.github.io/sdbuildR/reference/as.data.frame.simulate_sdbuildR.md).

## Usage

``` r
# S3 method for class 'simulate_sdbuildR'
head(x, n = 6L, ...)
```

## Arguments

- x:

  Output of
  [`simulate()`](https://kcevers.github.io/sdbuildR/reference/simulate.sdbuildR.md).

- n:

  Number of rows to print. Defaults to 6.

- ...:

  Other arguments passed to
  [`as.data.frame.simulate_sdbuildR()`](https://kcevers.github.io/sdbuildR/reference/as.data.frame.simulate_sdbuildR.md).

## Value

A data.frame with the first rows of the simulation results.

## Examples

``` r
sfm <- sdbuildR("SIR")
sim <- simulate(sfm)
head(sim)
#>   time variable    value
#> 1 0.00 infected 1.000000
#> 2 0.01 infected 1.019000
#> 3 0.02 infected 1.038361
#> 4 0.03 infected 1.058089
#> 5 0.04 infected 1.078193
#> 6 0.05 infected 1.098678
```
