# Tests for custom functions

# Mathematical functions ----

test_that("round_IM() rounds .5 correctly", {
  expect_equal(round_IM(0.5), 1)
  expect_equal(round_IM(-0.5), 0)
  expect_equal(round_IM(1.5), 2)
  expect_equal(round_IM(2.5), 3)
})

test_that("round_IM() handles digits parameter", {
  # Note: round_IM only handles .5 for integer rounding (digits=0)
  # For other digits, it uses standard round()
  expect_equal(round_IM(1.234, digits = 2), 1.23)
  expect_equal(round_IM(2.567, digits = 1), 2.6)
})

test_that("round_IM() handles integers", {
  expect_equal(round_IM(1), 1)
  expect_equal(round_IM(0), 0)
})

test_that("logit() computes correctly", {
  expect_equal(logit(0.5), 0)
  expect_true(logit(0.9) > 0)
  expect_true(logit(0.1) < 0)
})

test_that("expit() computes correctly", {
  expect_equal(expit(0), 0.5)
  expect_true(expit(10) > 0.9)
  expect_true(expit(-10) < 0.1)
})

test_that("expit() is inverse of logit()", {
  expect_equal(expit(logit(0.5)), 0.5)
  expect_equal(expit(logit(0.3)), 0.3, tolerance = 1e-10)
  expect_equal(expit(logit(0.7)), 0.7, tolerance = 1e-10)
})

# Random functions ----

test_that("rbool() returns logical", {
  withr::local_seed(123)
  result <- rbool(0.5)
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("rbool() respects extreme probabilities", {
  withr::local_seed(123)
  expect_true(rbool(1))
  expect_false(rbool(0))
})

test_that("rdist() returns single sample", {
  withr::local_seed(123)
  result <- rdist(c(1, 2, 3), c(0.5, 0.25, 0.25))
  expect_length(result, 1)
  expect_true(result %in% c(1, 2, 3))
})

test_that("rdist() respects probabilities", {
  withr::local_seed(123)
  result <- rdist(c("a", "b"), c(1, 0))
  expect_equal(result, "a")
})

# Vector/string functions ----

test_that("indexof() finds value in vector", {
  expect_equal(indexof(c("a", "b", "c"), "b"), 2)
  expect_equal(indexof(c(1, 2, 3), 1), 1)
  expect_equal(indexof(c("x", "y", "z"), "z"), 3)
})

test_that("indexof() returns 0 when not found in vector", {
  expect_equal(indexof(c("a", "b", "c"), "d"), 0)
  expect_equal(indexof(c(1, 2, 3), 4), 0)
})

test_that("indexof() finds substring in string", {
  expect_equal(indexof("haystack", "hay"), 1)
  expect_equal(indexof("haystack", "stack"), 4)
  expect_equal(indexof("haystack", "a"), 2)
})

test_that("indexof() returns 0 when substring not found", {
  expect_equal(indexof("haystack", "needle"), 0)
  expect_equal(indexof("test", "z"), 0)
})

test_that("length_IM() counts vector elements", {
  expect_equal(length_IM(c("a", "b", "c")), 3)
  expect_equal(length_IM(c(1, 2, 3, 4)), 4)
  expect_equal(length_IM(numeric(0)), 0)
})

test_that("length_IM() counts string characters", {
  expect_equal(length_IM("abcdef"), 6)
  expect_equal(length_IM("test"), 4)
  expect_equal(length_IM(""), 0)
  expect_equal(length_IM(NA_character_), NA_integer_)
})

test_that("contains_IM() checks vector membership", {
  expect_true(contains_IM(c("a", "b", "c"), "b"))
  expect_false(contains_IM(c("a", "b", "c"), "d"))
  expect_true(contains_IM(c(1, 2, 3), 2))
})

test_that("contains_IM() checks substring in string", {
  expect_true(contains_IM("haystack", "hay"))
  expect_true(contains_IM("abcdef", "bc"))
  expect_false(contains_IM("haystack", "needle"))
})

# rem(), mod(), %REM%, logistic(), sigmoid(), and nonnegative() values are
# covered more thoroughly in the "Extra tests" sections further below.

# Error message tests ----

cli::test_that_cli(configs = c("plain", "ansi"), "logistic() validates parameters", {
  expect_snapshot(logistic(0, slope = "a"), error = TRUE)
  expect_snapshot(logistic(0, midpoint = "b"), error = TRUE)
  expect_snapshot(logistic(0, upper = "c"), error = TRUE)
})

# Extra tests for sdbuildR_custom_func.R
# Covers: ramp(), pulse(), step(), seasonal(), saveat_func(),
#         rem(), mod(), %REM%, logistic(), sigmoid(), hill(), nonnegative()


# ============================================================================
# ramp()
# ============================================================================

test_that("ramp: is 0 strictly before start", {
  times <- seq(0, 10, by = 0.01)
  f <- ramp(times, start = 3, finish = 7, height = 4)
  expect_equal(f(0), 0, tolerance = 1e-9)
  expect_equal(f(2.9), 0, tolerance = 1e-9)
})

test_that("ramp: equals height at and after finish", {
  times <- seq(0, 10, by = 0.01)
  f <- ramp(times, start = 3, finish = 7, height = 4)
  expect_equal(f(7), 4, tolerance = 1e-6)
  expect_equal(f(10), 4, tolerance = 1e-6)
})

test_that("ramp: midpoint equals height/2", {
  times <- seq(0, 10, by = 0.001)
  f <- ramp(times, start = 0, finish = 10, height = 10)
  expect_equal(f(5), 5, tolerance = 0.05)
})

test_that("ramp: negative height produces a decreasing ramp", {
  times <- seq(0, 10, by = 0.01)
  f <- ramp(times, start = 2, finish = 8, height = -6)
  expect_equal(f(0), 0, tolerance = 1e-9)
  expect_equal(f(10), -6, tolerance = 1e-6)
})

test_that("ramp: errors when finish < start", {
  expect_error(ramp(seq(0, 10, 0.1), start = 7, finish = 3, height = 1))
})

test_that("ramp: returns an approxfun (callable function)", {
  times <- seq(0, 10, by = 0.1)
  f <- ramp(times, start = 2, finish = 8, height = 3)
  expect_true(is.function(f))
  expect_true(is.numeric(f(5)))
})


# ============================================================================
# pulse()
# ============================================================================

test_that("pulse: is 0 strictly before start", {
  times <- seq(0, 20, by = 0.001)
  f <- pulse(times, start = 5, height = 3, width = 2)
  expect_equal(f(4.99), 0, tolerance = 1e-6)
  expect_equal(f(0), 0, tolerance = 1e-6)
})

test_that("pulse: equals height during [start, start+width)", {
  times <- seq(0, 20, by = 0.001)
  f <- pulse(times, start = 5, height = 3, width = 2)
  expect_equal(f(5.5), 3, tolerance = 1e-6)
  expect_equal(f(6.9), 3, tolerance = 1e-6)
})

test_that("pulse: returns to 0 after start+width", {
  times <- seq(0, 20, by = 0.001)
  f <- pulse(times, start = 5, height = 3, width = 2)
  expect_equal(f(7.5), 0, tolerance = 1e-6)
  expect_equal(f(15), 0, tolerance = 1e-6)
})

test_that("pulse: repeats correctly at each interval", {
  times <- seq(0, 30, by = 0.001)
  f <- pulse(times, start = 0, height = 1, width = 1, repeat_interval = 5)
  expect_equal(f(0.5), 1, tolerance = 1e-6) # first pulse
  expect_equal(f(5.5), 1, tolerance = 1e-6) # second pulse
  expect_equal(f(10.5), 1, tolerance = 1e-6) # third pulse
  expect_equal(f(3), 0, tolerance = 1e-6) # gap between pulses
})

test_that("pulse: warns when width >= repeat_interval", {
  times <- seq(0, 10, by = 0.1)
  expect_warning(
    pulse(times, start = 0, height = 1, width = 5, repeat_interval = 4)
  )
})

test_that("pulse: errors when width <= 0", {
  times <- seq(0, 10, by = 0.1)
  expect_error(pulse(times, start = 0, height = 1, width = 0))
  expect_error(pulse(times, start = 0, height = 1, width = -1))
})


# ============================================================================
# step()
# ============================================================================

test_that("step: is 0 strictly before start", {
  times <- seq(0, 10, by = 0.001)
  f <- step(times, start = 5, height = 2)
  expect_equal(f(4.99), 0, tolerance = 1e-6)
  expect_equal(f(0), 0, tolerance = 1e-6)
})

test_that("step: equals height at and after start", {
  times <- seq(0, 10, by = 0.001)
  f <- step(times, start = 5, height = 2)
  expect_equal(f(5), 2, tolerance = 1e-6)
  expect_equal(f(9.9), 2, tolerance = 1e-6)
})

test_that("step: default height is 1", {
  times <- seq(0, 10, by = 0.1)
  f <- step(times, start = 3)
  expect_equal(f(5), 1, tolerance = 1e-6)
})

test_that("step: negative height produces a downward step", {
  times <- seq(0, 10, by = 0.1)
  f <- step(times, start = 5, height = -3)
  expect_equal(f(0), 0, tolerance = 1e-6)
  expect_equal(f(8), -3, tolerance = 1e-6)
})

test_that("step: returns a callable function", {
  f <- step(seq(0, 10, 0.1), start = 3, height = 5)
  expect_true(is.function(f))
})


# ============================================================================
# seasonal()
# ============================================================================

test_that("seasonal: output is bounded within [-1, 1]", {
  times <- seq(0, 100, by = 0.01)
  f <- seasonal(times, period = 1)
  vals <- vapply(seq(0, 100, by = 0.5), f, numeric(1))
  expect_lte(max(vals), 1 + 1e-6)
  expect_gte(min(vals), -1 - 1e-6)
})

test_that("seasonal: peak occurs at shift and every period thereafter", {
  times <- seq(0, 10, by = 0.001)
  f <- seasonal(times, period = 2, shift = 0)
  expect_equal(f(0), 1, tolerance = 1e-6) # cos(0) = 1
  expect_equal(f(2), 1, tolerance = 1e-6) # one period later
  expect_equal(f(4), 1, tolerance = 1e-6)
})

test_that("seasonal: shift moves the peak by the shift amount", {
  times <- seq(0, 10, by = 0.001)
  f <- seasonal(times, period = 4, shift = 1)
  expect_equal(f(1), 1, tolerance = 1e-6) # peak at t = shift
})

test_that("seasonal: errors when period <= 0", {
  times <- seq(0, 10, 0.1)
  expect_error(seasonal(times, period = 0))
  expect_error(seasonal(times, period = -1))
})


# ============================================================================
# saveat_func()
# ============================================================================

test_that("saveat_func: linearly interpolates x = 2*time correctly", {
  df <- data.frame(time = c(0, 1, 2, 3, 4), x = c(0, 2, 4, 6, 8))
  result <- saveat_func(df, "time", c(0.5, 1.5, 2.5))
  expect_equal(result$time, c(0.5, 1.5, 2.5))
  expect_equal(result$x, c(1.0, 3.0, 5.0), tolerance = 1e-9)
})

test_that("saveat_func: at original times returns exact original values", {
  df <- data.frame(time = c(0, 1, 2), x = c(10, 20, 30))
  result <- saveat_func(df, "time", c(0, 1, 2))
  expect_equal(result$x, c(10, 20, 30), tolerance = 1e-9)
})

test_that("saveat_func: preserves all non-time columns", {
  df <- data.frame(time = 0:4, a = 0:4, b = (0:4)^2)
  result <- saveat_func(df, "time", c(1.0, 2.0))
  expect_setequal(names(result), names(df))
  expect_equal(nrow(result), 2)
})

test_that("saveat_func: new_times with 1 entry gives 1-row data frame", {
  df <- data.frame(time = c(0, 1, 2), x = c(0, 1, 2))
  result <- saveat_func(df, "time", 1.0)
  expect_equal(nrow(result), 1)
  expect_equal(result$x, 1, tolerance = 1e-9)
})


# ============================================================================
# rem() / mod() / %REM% — sign-difference semantics
# ============================================================================

test_that("rem: sign follows dividend (truncated division)", {
  expect_equal(rem(7, 3), 1)
  expect_equal(rem(-7, 3), -1) # negative dividend → negative result
  expect_equal(rem(7, -3), 1) # positive dividend → positive result
  expect_equal(rem(-7, -3), -1)
})

test_that("mod: result matches R %% operator (sign follows divisor)", {
  expect_equal(mod(7, 3), 7 %% 3)
  expect_equal(mod(-7, 3), -7 %% 3)
  expect_equal(mod(7, -3), 7 %% -3)
  expect_equal(mod(-7, -3), -7 %% -3)
})

test_that("rem and mod agree for positive operands", {
  expect_equal(rem(9, 4), mod(9, 4))
  expect_equal(rem(10, 5), mod(10, 5))
})

test_that("rem and mod DISAGREE for negative dividend", {
  # For -7 / 3: rem = -1 (sign follows -7), mod = 2 (sign follows 3)
  expect_equal(rem(-7, 3), -1)
  expect_equal(mod(-7, 3), 2)
  expect_false(rem(-7, 3) == mod(-7, 3))
})

test_that("%REM% operator matches rem() exactly", {
  expect_equal(7L %REM% 3L, rem(7, 3))
  expect_equal(-7L %REM% 3L, rem(-7, 3))
  expect_equal(7L %REM% -3L, rem(7, -3))
  expect_equal(-7L %REM% -3L, rem(-7, -3))
})


# ============================================================================
# logistic() / sigmoid()
# ============================================================================

test_that("logistic: output is bounded [0, upper]", {
  vals <- logistic(seq(-200, 200, by = 5), upper = 5)
  expect_gte(min(vals), 0)
  expect_lte(max(vals), 5)
})

test_that("logistic: equals upper/2 at the midpoint", {
  expect_equal(logistic(3, midpoint = 3, upper = 1), 0.5, tolerance = 1e-9)
  expect_equal(logistic(7, midpoint = 7, upper = 10), 5.0, tolerance = 1e-9)
  expect_equal(logistic(0, midpoint = 0, upper = 1), 0.5, tolerance = 1e-9)
})

test_that("logistic: is monotonically increasing", {
  x <- seq(-10, 10, by = 0.1)
  vals <- logistic(x)
  expect_true(all(diff(vals) >= 0))
})

test_that("sigmoid is identical to logistic for the same arguments", {
  expect_equal(sigmoid(0), logistic(0))
  expect_equal(sigmoid(2, slope = 3), logistic(2, slope = 3))
  expect_equal(sigmoid(-1, upper = 2), logistic(-1, upper = 2))
})

test_that("logistic: larger slope gives steeper transition", {
  mid <- 0
  # At x = 0.5 (past midpoint), steep logistic should be closer to upper
  slow <- logistic(0.5, slope = 0.5, midpoint = mid, upper = 1)
  fast <- logistic(0.5, slope = 5.0, midpoint = mid, upper = 1)
  expect_lt(slow, fast)
})


# ============================================================================
# hill()
# ============================================================================

test_that("hill: equals upper/2 at the midpoint (EC50 property)", {
  expect_equal(hill(0.5, slope = 1, midpoint = 0.5, upper = 1), 0.5, tolerance = 1e-9)
  expect_equal(hill(2, slope = 1, midpoint = 2, upper = 4), 2.0, tolerance = 1e-9)
  expect_equal(hill(5, slope = 2, midpoint = 5, upper = 3), 1.5, tolerance = 1e-9)
})

test_that("hill: output is bounded [0, upper]", {
  vals <- hill(seq(0, 100, by = 0.1), slope = 2, midpoint = 5, upper = 3)
  expect_gte(min(vals), 0)
  expect_lte(max(vals), 3 + 1e-9)
})

test_that("hill: is monotonically increasing for positive x", {
  x <- seq(0.01, 50, by = 0.1)
  vals <- hill(x, slope = 2, midpoint = 5, upper = 10)
  expect_true(all(diff(vals) >= 0))
})

test_that("hill: x = 0 produces 0 (regardless of parameters)", {
  expect_equal(hill(0, slope = 2, midpoint = 1, upper = 5), 0, tolerance = 1e-9)
})


# ============================================================================
# nonnegative()
# ============================================================================

test_that("nonnegative: returns NA when input is NA", {
  expect_true(is.na(nonnegative(NA)))
})

test_that("nonnegative: clamps negative values to 0", {
  expect_equal(nonnegative(-5), 0)
  expect_equal(nonnegative(-0.1), 0)
  expect_equal(nonnegative(-1e6), 0)
})

test_that("nonnegative: passes through non-negative values unchanged", {
  expect_equal(nonnegative(3), 3)
  expect_equal(nonnegative(0), 0)
  expect_equal(nonnegative(100), 100)
})


# ============================================================================
# ricker()
# ============================================================================

test_that("ricker: peaks at x = location with value upper", {
  expect_equal(ricker(1, location = 1, upper = 1), 1, tolerance = 1e-9)
  expect_equal(ricker(2, location = 2, upper = 5), 5, tolerance = 1e-9)
  expect_equal(ricker(3, location = 3, upper = 10, shape = 2), 10, tolerance = 1e-9)
})

test_that("ricker: equals 0 at x = 0", {
  expect_equal(ricker(0, location = 2, upper = 5), 0, tolerance = 1e-9)
  expect_equal(ricker(0, location = 1, upper = 1, shape = 0.5), 0, tolerance = 1e-9)
})

test_that("ricker: location is the global maximum (hump shape)", {
  x <- seq(0, 20, by = 0.01)
  vals <- ricker(x, location = 4, upper = 3, shape = 1.5)
  expect_equal(x[which.max(vals)], 4, tolerance = 0.01)
  # increasing before the peak, decreasing after
  expect_true(all(diff(vals[x <= 4]) >= 0))
  expect_true(all(diff(vals[x >= 4]) <= 0))
})

test_that("ricker: upper scales the curve with upper", {
  x <- seq(0.1, 10, by = 0.1)
  expect_equal(ricker(x, location = 2, upper = 6), 6 * ricker(x, location = 2, upper = 1),
    tolerance = 1e-9
  )
})

test_that("ricker: shape = 1 reduces to the standard Ricker a*x*exp(-b*x)", {
  # f(x) = upper * (x/location) * exp(1 - x/location)
  #      = (upper * e / location) * x * exp(-x / location)
  location <- 2
  upper <- 1
  a <- upper * exp(1) / location
  b <- 1 / location
  x <- c(0.5, 1, 3, 5)
  expect_equal(
    ricker(x, location = location, upper = upper, shape = 1),
    a * x * exp(-b * x),
    tolerance = 1e-9
  )
})

test_that("ricker: standard coefficients a, b map to location, upper", {
  # Start from textbook coefficients, convert to our parameters via
  # location = 1/b, upper = a/(b*e), and check the curves agree.
  a <- 2.5
  b <- 0.4
  location <- 1 / b
  upper <- a / (b * exp(1))
  x <- c(0, 0.5, 1, 2.5, 5, 9)
  expect_equal(
    ricker(x, location = location, upper = upper, shape = 1),
    a * x * exp(-b * x),
    tolerance = 1e-9
  )
})

test_that("ricker: location and upper are the peak position and height of a*x*exp(-b*x)", {
  # The standard Ricker peaks at x = 1/b with value a/(b*e); these must equal
  # the location and upper recovered from a = upper*e/location, b = 1/location.
  location <- 3
  upper <- 7
  a <- upper * exp(1) / location
  b <- 1 / location
  expect_equal(1 / b, location, tolerance = 1e-9) # peak position
  expect_equal(a / (b * exp(1)), upper, tolerance = 1e-9) # peak height
})

test_that("ricker: shape = alpha matches generalized form C * x^alpha * exp(-(alpha/location)*x)", {
  location <- 2
  upper <- 3
  alpha <- 2.5
  C <- upper * exp(alpha) / location^alpha
  x <- c(0.5, 1, 2, 4, 6)
  expect_equal(
    ricker(x, location = location, upper = upper, shape = alpha),
    C * x^alpha * exp(-(alpha / location) * x),
    tolerance = 1e-9
  )
})

test_that("ricker: larger shape narrows the peak (smaller values off-peak)", {
  # Off the peak the inner term is in (0, 1), so a larger exponent lowers it
  x <- 8 # away from the peak at location = 4
  broad <- ricker(x, location = 4, upper = 1, shape = 0.5)
  base <- ricker(x, location = 4, upper = 1, shape = 1)
  narrow <- ricker(x, location = 4, upper = 1, shape = 3)
  expect_true(narrow < base)
  expect_true(base < broad)
})

test_that("ricker: is vectorized over x", {
  vals <- ricker(seq(0, 5, by = 0.5), location = 2)
  expect_length(vals, 11)
  expect_type(vals, "double")
})

test_that("ricker: validates parameters", {
  expect_error(ricker(1, location = "a"))
  expect_error(ricker(1, upper = "b"))
  expect_error(ricker(1, shape = "c"))
})

test_that("ricker: a, b equals the standard curve a*x*exp(-b*x) at shape = 1", {
  a <- 2.5
  b <- 0.4
  x <- c(0, 0.5, 1, 2.5, 5, 9)
  expect_equal(ricker(x, a = a, b = b), a * x * exp(-b * x), tolerance = 1e-9)
})

test_that("ricker: a, b equals the expanded form a*x^shape*exp(-b*x) for any shape", {
  a <- 1.8
  b <- 0.6
  shape <- 2.5
  x <- c(0.5, 1, 2, 4, 6)
  expect_equal(
    ricker(x, a = a, b = b, shape = shape),
    a * x^shape * exp(-b * x),
    tolerance = 1e-9
  )
})

test_that("ricker: a, b maps to location = shape/b, upper = a*(location/e)^shape", {
  a <- 1.8
  b <- 0.6
  shape <- 2.5
  location <- shape / b
  upper <- a * (location / exp(1))^shape
  x <- c(0.5, 1, 2, 4)
  expect_equal(
    ricker(x, a = a, b = b, shape = shape),
    ricker(x, location = location, upper = upper, shape = shape),
    tolerance = 1e-9
  )
})

test_that("ricker: errors on incomplete or conflicting parameterization", {
  expect_error(ricker(1, a = 2)) # b missing
  expect_error(ricker(1, b = 0.5)) # a missing
  expect_error(ricker(1, a = 2, b = 0.5, location = 3)) # conflict
  expect_error(ricker(1, a = 2, b = 0.5, upper = 4)) # conflict
  expect_error(ricker(1, a = "x", b = 0.5)) # non-numeric
})
