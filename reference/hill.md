# Hill function

Computes the Hill function with configurable slope, midpoint, and upper
asymptote.

## Usage

``` r
hill(x, slope = 1, midpoint = 0.5, upper = 1)
```

## Arguments

- x:

  Value at which to evaluate the function

- slope:

  Slope of Hill function at the midpoint. Defaults to 1.

- midpoint:

  Midpoint of Hill function where the output is `upper/2`. Defaults to
  0.5.

- upper:

  Upper asymptote (maximal value) of the Hill function. Defaults to 1.

## Value

Numeric value given by \$\$f(x) = \frac{upper \cdot
x^{slope}}{midpoint^{slope} + x^{slope}}\$\$

## Details

The Hill function is a smooth S-shaped curve (when slope \> 1) bounded
between 0 and `upper`. It transitions from near 0 to near `upper` around
the `midpoint`, with the steepness of this transition controlled by
`slope`. See
<https://en.wikipedia.org/wiki/Hill_equation_%28biochemistry%29> for
more details.

## Examples

``` r
hill(0)
#> [1] 0

# Adjust parameters
hill(0, slope = 5, midpoint = 0.5, upper = 10)
#> [1] 0
```
