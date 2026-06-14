# Summarise simulation results

Returns a data frame with per-variable summary statistics (min, mean,
max, and final value) over the simulated time range.

## Usage

``` r
# S3 method for class 'simulate_sdbuildR'
summary(object, ...)
```

## Arguments

- object:

  A simulation result of class
  [`simulate_sdbuildR`](https://kcevers.github.io/sdbuildR/reference/simulate.sdbuildR.md)

- ...:

  Additional arguments (unused)

## Value

A `data.frame` with columns `variable`, `min`, `mean`, `max`, `final`.

## See also

[`print.simulate_sdbuildR()`](https://kcevers.github.io/sdbuildR/reference/print.simulate_sdbuildR.md),
[`simulate.sdbuildR()`](https://kcevers.github.io/sdbuildR/reference/simulate.sdbuildR.md)

## Examples

``` r
sfm <- sdbuildR("SIR")
sim <- simulate(sfm)
summary(sim)
#>      variable        min     mean      max        final
#> 1    infected 1.00000000 37468.98 80112.72 2.504960e+04
#> 2   recovered 0.00000000 31977.39 74950.37 7.495037e+04
#> 3 susceptible 0.02839274 30553.63 99999.00 2.839274e-02
```
