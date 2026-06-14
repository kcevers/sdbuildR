# Determine whether parentheses are needed to preserve meaning

Parentheses are needed when the inner operator has lower precedence than
the parent, because without them the human reader might misinterpret the
grouping.

## Usage

``` r
needs_parens(inner_op, outer_op)
```

## Arguments

- inner_op:

  The operator inside the parentheses (or NULL)

- outer_op:

  The operator outside (parent context, or NULL)

## Value

logical
