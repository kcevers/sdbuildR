# Add or modify unit tests

Unit tests are assertions about model behavior that can be evaluated
against simulation results. For example, you might assert that a stock
remains non-negative, or that a certain variable reaches a threshold by
the end of the simulation. Unit tests can be added to a model such that
they can be evaluated with
[`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md).
All unit tests can be displayed with
[`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md).

## Usage

``` r
unit_test(object, test, expr, label, conditions = list(), active = TRUE)
```

## Arguments

- object:

  Stock-and-flow model, object of class
  [`sdbuildR`](https://kcevers.github.io/sdbuildR/reference/sdbuildR.md).

- test:

  Integer number of the test to modify. Must be a positive integer (a
  warning is issued and the value rounded when a non-integer is
  supplied). When `test` exceeds the current number of tests a warning
  is issued and a new test is appended instead. Can be omitted when
  adding a new test.

- expr:

  An expression to evaluate against simulation results. Variable names
  in the expression refer to model variables; each resolves to a numeric
  vector of time-series values. Required when adding a new test;
  optional when modifying (keeps the current expression if omitted).

- label:

  A descriptive label for the test. If omitted when adding,
  auto-generated from `expr`. If omitted when modifying, the current
  label is kept. Labels must be unique.

- conditions:

  A named list of constant or initial stock overrides used when
  evaluating this test. If non-empty,
  [`verify.sdbuildR()`](https://kcevers.github.io/sdbuildR/reference/verify.sdbuildR.md)
  will re-simulate the model with these parameter values before
  evaluating `expr`.

- active:

  If `FALSE`, the test is defined but skipped during
  [`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md).
  Defaults to `TRUE`.

## Value

The model object with the unit test added or modified, invisibly.

## Details

The `expr` argument accepts a plain logical expression:

- **Logical**: `all(S >= 0)`, `cor(D, C) < -.5`.

When `label` is omitted, a human-readable label is generated
automatically by parsing the expression (e.g., `all(S >= 0)` →
`"S is at least 0 (for all values)"`).

## Adding vs. modifying

- **Add** a new test: omit `test` (and provide a `label` that does not
  match any existing test, or omit `label` to auto-generate one).

- **Modify** an existing test by number: supply `test` (integer).

- **Modify** an existing test by label: supply a `label` that matches an
  existing test (without specifying `test`).

When modifying, only the arguments you explicitly supply are changed;
all other fields keep their current value.

## Uniqueness

Labels must be unique across all unit tests. An error is thrown if a new
or modified label would create a duplicate. Expressions must also be
unique; an error is thrown if an identical `expr` already exists on
another test.

## See also

[`verify()`](https://kcevers.github.io/sdbuildR/reference/verify.md),
[`unit_tests()`](https://kcevers.github.io/sdbuildR/reference/unit_tests.md),
[`discard_unit_test()`](https://kcevers.github.io/sdbuildR/reference/discard_unit_test.md)

## Examples

``` r
sfm <- sdbuildR("SIR") |>
  unit_test(expr = all(susceptible >= 0))

# Run unit tests
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 1/1 test passed.
#> ✔ 1. susceptible is at least 0 (for all values)

# Add test with label
sfm <- unit_test(sfm,
  label = "recovered increases",
  expr = all(diff(recovered) >= 0)
)
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 2/2 tests passed.
#> ✔ 1. susceptible is at least 0 (for all values)
#> ✔ 2. recovered increases

# Add test with conditions
sfm <- unit_test(sfm,
  expr = all(infected == infected[1]),
  label = "When infection_rate is zero, no one gets infected",
  conditions = list(infection_rate = 0)
)
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 2/3 tests passed.
#> ✔ 1. susceptible is at least 0 (for all values)
#> ✔ 2. recovered increases
#> ✖ 3. When infection_rate is zero, no one gets infected
#>   Expected: TRUE Actual: FALSE

# View all tests
unit_tests(sfm)
#> 
#> ── Stock-and-Flow Unit Tests ───────────────────────────────────────────────────
#> 3 tests • 3/3 active • 1/3 include conditions
#> • 1. susceptible is at least 0 (for all values)
#>   `all(susceptible >= 0)`
#> • 2. recovered increases
#>   `all(diff(recovered) >= 0)`
#> • 3. When infection_rate is zero, no one gets infected
#>   `all(infected == infected[1])`
#>   Conditions: infection_rate = 0

# Deactivate test test 1
sfm <- unit_test(sfm, test = 1, active = FALSE)
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 1/2 tests passed.
#> ℹ 1. susceptible is at least 0 (for all values)
#>   Test is inactive.
#> ✔ 2. recovered increases
#> ✖ 3. When infection_rate is zero, no one gets infected
#>   Expected: TRUE Actual: FALSE

# Modify test by label, e.g., to change the expression
sfm <- unit_test(sfm,
  label = "recovered increases over time",
  expr = all(diff(recovered) > -1)
)
verify(sfm)
#> 
#> ── Stock-and-Flow Unit Test Results ────────────────────────────────────────────
#> 2/3 tests passed.
#> ℹ 1. susceptible is at least 0 (for all values)
#>   Test is inactive.
#> ✔ 2. recovered increases
#> ✖ 3. When infection_rate is zero, no one gets infected
#>   Expected: TRUE Actual: FALSE
#> ✔ 4. recovered increases over time
```
