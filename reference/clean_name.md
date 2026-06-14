# Clean variable name(s)

Clean variable name(s) to create syntactically valid, unique names for
use in R and Julia.

## Usage

``` r
clean_name(new, protected = NULL)
```

## Arguments

- new:

  Vector of names to transform to valid names

- protected:

  Optional vector of protected names, e.g., existing names in model

## Value

Vector of cleaned names

## Examples

``` r
sfm <- sdbuildR("predator_prey")
# As the variable name "predator" is already taken, clean_name() will create
# a unique name
clean_name("predator", as.data.frame(sfm)[["name"]])
#> [1] "predator_1"
```
