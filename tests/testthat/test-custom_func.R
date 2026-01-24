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
  set.seed(123)
  result <- rbool(0.5)
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("rbool() respects extreme probabilities", {
  set.seed(123)
  expect_true(rbool(1))
  expect_false(rbool(0))
})

test_that("rdist() returns single sample", {
  set.seed(123)
  result <- rdist(c(1, 2, 3), c(0.5, 0.25, 0.25))
  expect_length(result, 1)
  expect_true(result %in% c(1, 2, 3))
})

test_that("rdist() respects probabilities", {
  set.seed(123)
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

# Modulus and remainder functions ----

test_that("rem() computes remainder correctly", {
  expect_equal(rem(7, 3), 1)
  expect_equal(rem(10, 5), 0)
  expect_equal(rem(8, 3), 2)
})

test_that("rem() handles negative values differently than mod()", {
  # When a is negative
  expect_equal(rem(-7, 3), -1)
  expect_equal(mod(-7, 3), 2)
  
  # When b is negative
  expect_equal(rem(7, -3), 1)
  expect_equal(mod(7, -3), -2)
})

test_that("rem() and mod() agree for positive values", {
  expect_equal(rem(7, 3), mod(7, 3))
  expect_equal(rem(10, 4), mod(10, 4))
})

test_that("%REM% operator works", {
  expect_equal(7 %REM% 3, rem(7, 3))
  expect_equal(-7 %REM% 3, rem(-7, 3))
})

# Logistic functions ----

test_that("logistic() computes at midpoint", {
  # At midpoint, output should be upper/2
  expect_equal(logistic(0, midpoint = 0, upper = 1), 0.5)
  expect_equal(logistic(5, midpoint = 5, upper = 10), 5)
})

test_that("logistic() respects upper asymptote", {
  # Values should be bounded by upper
  expect_true(logistic(100, upper = 1) < 1)
  expect_true(logistic(100, upper = 10) < 10)
})

test_that("logistic() handles slope parameter", {
  # Higher slope means steeper transition, so at a given x-midpoint distance,
  # higher slope gets closer to the upper asymptote (farther from midpoint)
  expect_true(abs(logistic(1, slope = 1) - 0.5) < abs(logistic(1, slope = 10) - 0.5))
})

test_that("sigmoid() is alias for logistic()", {
  expect_equal(sigmoid(0), logistic(0))
  expect_equal(sigmoid(1, slope = 2, midpoint = 1, upper = 5), 
               logistic(1, slope = 2, midpoint = 1, upper = 5))
})

test_that("logistic() returns numeric", {
  expect_type(logistic(0), "double")
  expect_length(logistic(0), 1)
})

# Non-negative function ----

test_that("nonnegative() clamps negative values to zero", {
  expect_equal(nonnegative(-5), 0)
  expect_equal(nonnegative(-0.1), 0)
  expect_equal(nonnegative(0), 0)
})

test_that("nonnegative() preserves positive values", {
  expect_equal(nonnegative(5), 5)
  expect_equal(nonnegative(0.1), 0.1)
  expect_equal(nonnegative(100), 100)
})

# Error message tests ----

cli::test_that_cli(configs = c("plain", "ansi"), "logistic() validates parameters", {
  expect_snapshot(logistic(0, slope = "a"), error = TRUE)
  expect_snapshot(logistic(0, midpoint = "b"), error = TRUE)
  expect_snapshot(logistic(0, upper = "c"), error = TRUE)
})


