# ensemble() error: invalid n [plain]

    Code
      ensemble(sfm, n = 0)
    Condition
      Error in `ensemble()`:
      x The `n` argument must be greater than 0.

# ensemble() error: invalid n [ansi]

    Code
      ensemble(sfm, n = 0)
    Condition
      [1m[33mError[39m in `ensemble()`:[22m
      [1m[22m[31mx[39m The `n` argument must be greater than [34m0[39m.

# ensemble() error: invalid quantiles [plain]

    Code
      ensemble(sfm, n = 3, quantiles = 0.5)
    Condition
      Error in `.validate_ensemble_quantiles()`:
      ! Invalid `quantiles` argument.
      x The `quantiles` argument must have at least 2 unique values.
      > Provide at least 2 quantiles, e.g., `quantiles = c(0.025, 0.975)`.

# ensemble() error: invalid quantiles [ansi]

    Code
      ensemble(sfm, n = 3, quantiles = 0.5)
    Condition
      [1m[33mError[39m in `.validate_ensemble_quantiles()`:[22m
      [1m[22m[33m![39m Invalid `quantiles` argument.
      [31mx[39m The `quantiles` argument must have at least [34m2[39m unique values.
      > Provide at least 2 quantiles, e.g., `quantiles = c(0.025, 0.975)`.

# ensemble() error: non-numeric conditions [plain]

    Code
      ensemble(sfm, n = 3, conditions = list(S = "abc"))
    Condition
      Error in `ensemble()`:
      x All `conditions` elements must be <numeric> vectors.
      > Example: `conditions = list(param1 = c(1, 2, 3))`.

# ensemble() error: non-numeric conditions [ansi]

    Code
      ensemble(sfm, n = 3, conditions = list(S = "abc"))
    Condition
      [1m[33mError[39m in `ensemble()`:[22m
      [1m[22m[31mx[39m All `conditions` elements must be [34m<numeric>[39m vectors.
      > Example: `conditions = list(param1 = c(1, 2, 3))`.

# print() success output matches snapshot [plain]

    Code
      print(sims)
    Message
      
      -- Stock-and-Flow Ensemble Simulation: Demo model ------------------------------
      i 3 total simulations • 1 condition • 3 per condition
      v Completed in 1.234 seconds
      i Individual simulations saved: no
      i Time points saved per simulation: 11

# print() success with conditions lists changed parameters [plain]

    Code
      print(sims)
    Message
      
      -- Stock-and-Flow Ensemble Simulation: Demo model ------------------------------
      i 12 total simulations • 4 conditions • 3 per condition
      v Completed in 1.234 seconds
      i Parameters changed across conditions: contact_rate, infection_rate
      i Individual simulations saved: yes
      i Time points saved per simulation: 11

