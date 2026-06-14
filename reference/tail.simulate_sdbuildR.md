# Print last rows of a simulation

Print the last rows of a simulation data frame of a stock-and-flow
model. This is a wrapper around
[`tail()`](https://rdrr.io/r/utils/head.html) that first converts the
simulation results to a data frame using
[as.data.frame()](https://kcevers.github.io/sdbuildR/reference/as.data.frame.simulate_sdbuildR.md).

## Usage

``` r
# S3 method for class 'simulate_sdbuildR'
tail(x, n = 6L, ...)
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

A data.frame with the last rows of the simulation results.

## Examples

``` r
sfm <- sdbuildR("SIR")
sim <- simulate(sfm)
tail(sim)
#>       time    variable      value
#> 5998 19.95 susceptible 0.02911698
#> 5999 19.96 susceptible 0.02897038
#> 6000 19.97 susceptible 0.02882466
#> 6001 19.98 susceptible 0.02867981
#> 6002 19.99 susceptible 0.02853584
#> 6003 20.00 susceptible 0.02839274
```
