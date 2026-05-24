# Tests for conv_julia_distributions.R
# Covers: conv_distribution_julia(), conv_seq_julia(), conv_sample_julia()
# These internal helpers are accessible via devtools::load_all()


# ============================================================================
# conv_distribution_julia()
# ============================================================================

test_that("conv_distribution_julia: rnorm(10) → rand(Normal(...), 10)", {
  result <- conv_distribution_julia(
    arg          = c("10", "0.0", "1.0"),
    R_func       = "rnorm",
    julia_func   = "rand",
    distribution = "Normal"
  )
  expect_equal(result, "rand(Normal(0.0, 1.0), 10)")
})

test_that("conv_distribution_julia: runif(5) → rand(Uniform(...), 5)", {
  result <- conv_distribution_julia(
    arg          = c("5", "0.0", "1.0"),
    R_func       = "runif",
    julia_func   = "rand",
    distribution = "Uniform"
  )
  expect_equal(result, "rand(Uniform(0.0, 1.0), 5)")
})

test_that("conv_distribution_julia: n=1 omits size argument for rand", {
  result <- conv_distribution_julia(
    arg          = c("1", "0.0", "1.0"),
    R_func       = "rnorm",
    julia_func   = "rand",
    distribution = "Normal"
  )
  expect_equal(result, "rand(Normal(0.0, 1.0))")
  expect_false(grepl(",\\s*1\\)", result))
})

test_that("conv_distribution_julia: n=1 does NOT omit size for non-rand funcs", {
  result <- conv_distribution_julia(
    arg          = c("1", "0.5"),
    R_func       = "qnorm",
    julia_func   = "Distributions.quantile.",
    distribution = "Normal"
  )
  # For quantile function, n=1 should not trigger the size-omission rule
  expect_false(identical(result, "Distributions.quantile.(Normal(0.5))"))
})

test_that("conv_distribution_julia: non-integer first argument throws an error", {
  expect_error(
    conv_distribution_julia(
      arg          = c("abc", "0.0", "1.0"),
      R_func       = "rnorm",
      julia_func   = "rand",
      distribution = "Normal"
    )
  )
})


# ============================================================================
# conv_seq_julia()
# ============================================================================

test_that("conv_seq_julia: seq_along(x) → range(1.0, length(x))", {
  result <- conv_seq_julia(
    arg     = list(along.with = "myvec"),
    R_func  = "seq_along",
    julia_func = "range"
  )
  expect_equal(result, "range(1.0, length(myvec))")
})

test_that("conv_seq_julia: seq_len(n) → range(1.0, n)", {
  result <- conv_seq_julia(
    arg     = list(length.out = "5"),
    R_func  = "seq_len",
    julia_func = "range"
  )
  expect_equal(result, "range(1.0, 5)")
})

test_that("conv_seq_julia: seq(from, to, by) → range(from, to, step=by)", {
  result <- conv_seq_julia(
    arg     = list(from = "0.0", to = "10.0", by = "2.0"),
    R_func  = "seq",
    julia_func = "range"
  )
  expect_equal(result, "range(0.0, 10.0, step=2.0)")
})

test_that("conv_seq_julia: seq(from, to, length.out) → range(..., round_(n))", {
  result <- conv_seq_julia(
    arg     = list(from = "0.0", to = "1.0", length.out = "5"),
    R_func  = "seq",
    julia_func = "range"
  )
  expect_equal(result, "range(0.0, 1.0, round_(5))")
})

test_that("conv_seq_julia: seq with length.out=1 and from != to → returns just 'from'", {
  result <- conv_seq_julia(
    arg     = list(from = "3.0", to = "7.0", length.out = "1"),
    R_func  = "seq",
    julia_func = "range"
  )
  expect_equal(result, "3.0")
})

test_that("conv_seq_julia: seq with length.out=1 and from == to → range(from, to, round_(1))", {
  result <- conv_seq_julia(
    arg     = list(from = "3.0", to = "3.0", length.out = "1"),
    R_func  = "seq",
    julia_func = "range"
  )
  expect_equal(result, "range(3.0, 3.0, round_(1))")
})

test_that("conv_seq_julia: seq without by or length.out defaults to step=1.0", {
  result <- conv_seq_julia(
    arg     = list(from = "1.0", to = "5.0"),
    R_func  = "seq",
    julia_func = "range"
  )
  expect_match(result, "step=1\\.0")
})


# ============================================================================
# conv_sample_julia()
# ============================================================================

test_that("conv_sample_julia: basic sample without replacement wraps size in round_()", {
  result <- conv_sample_julia(
    arg      = list(x = "myvec", size = "3", replace = "FALSE"),
    R_func   = "sample",
    julia_func = "StatsBase.sample"
  )
  expect_equal(result, "StatsBase.sample(myvec, round_(3), replace=false)")
})

test_that("conv_sample_julia: replace=TRUE maps to julia true", {
  result <- conv_sample_julia(
    arg      = list(x = "myvec", size = "3", replace = "TRUE"),
    R_func   = "sample",
    julia_func = "StatsBase.sample"
  )
  expect_match(result, "replace=true")
  expect_false(grepl("replace=false", result))
})

test_that("conv_sample_julia: replace is case-insensitive (TRUE, True, true all work)", {
  r1 <- conv_sample_julia(list(x = "v", size = "1", replace = "TRUE"),  "sample", "StatsBase.sample")
  r2 <- conv_sample_julia(list(x = "v", size = "1", replace = "True"),  "sample", "StatsBase.sample")
  r3 <- conv_sample_julia(list(x = "v", size = "1", replace = "true"),  "sample", "StatsBase.sample")
  expect_equal(r1, r2)
  expect_equal(r2, r3)
})

test_that("conv_sample_julia: weighted sample uses StatsBase.pweights()", {
  result <- conv_sample_julia(
    arg      = list(x = "myvec", size = "2", replace = "TRUE", prob = "myprobs"),
    R_func   = "sample",
    julia_func = "StatsBase.sample"
  )
  expect_match(result, "StatsBase\\.pweights\\(myprobs\\)")
  expect_match(result, "replace=true")
  expect_match(result, "round_\\(2\\)")
})

test_that("conv_sample_julia: sample.int generates integer range seq(1.0, n)", {
  result <- conv_sample_julia(
    arg      = list(n = "10", size = "3", replace = "FALSE"),
    R_func   = "sample.int",
    julia_func = "StatsBase.sample"
  )
  expect_match(result, "seq\\(1\\.0,\\s*10\\)")
  expect_match(result, "round_\\(3\\)")
  expect_match(result, "replace=false")
})
