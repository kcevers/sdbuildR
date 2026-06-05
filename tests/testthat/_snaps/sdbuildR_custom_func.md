# logistic() validates parameters [plain]

    Code
      logistic(0, slope = "a")
    Condition
      Error in `logistic()`:
      ! Invalid `slope` parameter.
      x The `slope` parameter must be numeric.

---

    Code
      logistic(0, midpoint = "b")
    Condition
      Error in `logistic()`:
      ! Invalid `midpoint` parameter.
      x The `midpoint` parameter must be numeric.

---

    Code
      logistic(0, upper = "c")
    Condition
      Error in `logistic()`:
      ! Invalid `upper` parameter.
      x The `upper` parameter must be numeric.

# logistic() validates parameters [ansi]

    Code
      logistic(0, slope = "a")
    Condition
      [1m[33mError[39m in `logistic()`:[22m
      [1m[22m[33m![39m Invalid `slope` parameter.
      [31mx[39m The `slope` parameter must be numeric.

---

    Code
      logistic(0, midpoint = "b")
    Condition
      [1m[33mError[39m in `logistic()`:[22m
      [1m[22m[33m![39m Invalid `midpoint` parameter.
      [31mx[39m The `midpoint` parameter must be numeric.

---

    Code
      logistic(0, upper = "c")
    Condition
      [1m[33mError[39m in `logistic()`:[22m
      [1m[22m[33m![39m Invalid `upper` parameter.
      [31mx[39m The `upper` parameter must be numeric.

