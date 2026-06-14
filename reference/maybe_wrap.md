# Conditionally wrap a result string in parentheses

Used by infix operators to add parens when the current operator has
lower precedence than the parent context.

## Usage

``` r
maybe_wrap(result, current_op, parent_op)
```

## Arguments

- result:

  The human-readable string for this sub-expression

- current_op:

  The current operator

- parent_op:

  The parent operator (from the recursive call)

## Value

Possibly parenthesized string
