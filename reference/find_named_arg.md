# Find a named argument in a list of call arguments

Searches by name; also handles positional fallback for the second
argument when the name is absent (e.g.,
`expect_equal(x, y, tolerance = 0.01)`).

## Usage

``` r
find_named_arg(args, name)
```

## Arguments

- args:

  List of call arguments (from `as.list(e[-1])`)

- name:

  Character name of the argument to find

## Value

The argument value, or NULL if not found
