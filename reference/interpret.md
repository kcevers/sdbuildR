# Recursively interpret a parsed R expression

Recursively interpret a parsed R expression

## Usage

``` r
interpret(e, parent_op = NULL)
```

## Arguments

- e:

  A language object (from parse())

- parent_op:

  The operator of the parent call (used for precedence decisions)

## Value

A human-readable string
