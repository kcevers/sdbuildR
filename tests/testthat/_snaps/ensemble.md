# ensemble() error: invalid n [plain]

    Code
      ensemble(sfm, n = 0)
    Condition
      Error in `ensemble()`:
      x The `n` argument must be greater than "0".

# ensemble() error: invalid n [ansi]

    Code
      ensemble(sfm, n = 0)
    Condition
      [1m[33mError[39m in `ensemble()`:[22m
      [1m[22m[31mx[39m The `n` argument must be greater than [34m"0"[39m.

# ensemble() error: invalid quantiles [plain]

    Code
      ensemble(sfm, quantiles = 0.5)
    Condition
      Error in `ensemble()`:
      x The `quantiles` argument must have at least "2" unique values.
      i Received 1 unique value(s).
      > Provide at least 2 quantiles, e.g., `quantiles = c(0.025, 0.975)`.

# ensemble() error: invalid quantiles [ansi]

    Code
      ensemble(sfm, quantiles = 0.5)
    Condition
      [1m[33mError[39m in `ensemble()`:[22m
      [1m[22m[31mx[39m The `quantiles` argument must have at least [34m"2"[39m unique values.
      [36mi[39m Received [34m1[39m unique value(s).
      > Provide at least 2 quantiles, e.g., `quantiles = c(0.025, 0.975)`.

# ensemble() error: non-numeric range [plain]

    Code
      ensemble(sfm, range = list(S = "abc"))
    Condition
      Error in `ensemble()`:
      x All `range` elements must be <numeric> vectors.
      > Example: `range = list(param1 = c(1, 2, 3))`.

# ensemble() error: non-numeric range [ansi]

    Code
      ensemble(sfm, range = list(S = "abc"))
    Condition
      [1m[33mError[39m in `ensemble()`:[22m
      [1m[22m[31mx[39m All `range` elements must be [34m<numeric>[39m vectors.
      > Example: `range = list(param1 = c(1, 2, 3))`.

