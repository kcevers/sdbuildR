# logistic() validates parameters [plain]

    Code
      logistic(0, slope = "a")
    Condition
      Error in `logistic()`:
      ! slope must be numeric!

---

    Code
      logistic(0, midpoint = "b")
    Condition
      Error in `logistic()`:
      ! midpoint must be numeric!

---

    Code
      logistic(0, upper = "c")
    Condition
      Error in `logistic()`:
      ! upper must be numeric!

# logistic() validates parameters [ansi]

    Code
      logistic(0, slope = "a")
    Condition
      [1m[33mError[39m in `logistic()`:[22m
      [33m![39m slope must be numeric!

---

    Code
      logistic(0, midpoint = "b")
    Condition
      [1m[33mError[39m in `logistic()`:[22m
      [33m![39m midpoint must be numeric!

---

    Code
      logistic(0, upper = "c")
    Condition
      [1m[33mError[39m in `logistic()`:[22m
      [33m![39m upper must be numeric!

