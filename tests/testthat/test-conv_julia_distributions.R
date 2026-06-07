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


# ---------------------------------------------------------------------------
# rate -> scale reparameterization (Exponential, Gamma)
# R parameterizes these by `rate`; Julia's Distributions use `scale = 1/rate`.
# The real conversion path passes a *named* argument list (from sort_args()).
# ---------------------------------------------------------------------------

test_that("reparam_rate_distribution: Exponential inverts rate to scale", {
  out <- reparam_rate_distribution(
    arg          = list(n = "10", rate = "4.0"),
    distribution = "Distributions.Exponential"
  )
  expect_equal(out, list(n = "10", rate = "1 / (4.0)"))
})

test_that("reparam_rate_distribution: Gamma drops the redundant rate", {
  out <- reparam_rate_distribution(
    arg          = list(n = "10", shape = "2.0", rate = "4.0", scale = "0.25"),
    distribution = "Distributions.Gamma"
  )
  expect_equal(out, list(n = "10", shape = "2.0", scale = "0.25"))
})

test_that("reparam_rate_distribution: no-op for other distributions", {
  arg <- list(n = "10", mean = "0.0", sd = "1.0")
  expect_equal(
    reparam_rate_distribution(arg, "Distributions.Normal"),
    arg
  )
})

test_that("reparam_rate_distribution: no-op on unnamed input", {
  arg <- list("10", "2.0", "0.25")
  expect_equal(reparam_rate_distribution(arg, "Distributions.Gamma"), arg)
})

test_that("conv_distribution_julia: rgamma keeps scale, drops rate", {
  result <- conv_distribution_julia(
    arg          = list(n = "10", shape = "2.0", rate = "4.0", scale = "0.25"),
    R_func       = "rgamma",
    julia_func   = "rand",
    distribution = "Distributions.Gamma"
  )
  expect_equal(result, "rand(Distributions.Gamma(2.0, 0.25), 10)")
})

test_that("conv_distribution_julia: rexp inverts rate to scale", {
  result <- conv_distribution_julia(
    arg          = list(n = "10", rate = "4.0"),
    R_func       = "rexp",
    julia_func   = "rand",
    distribution = "Distributions.Exponential"
  )
  expect_equal(result, "rand(Distributions.Exponential(1 / (4.0)), 10)")
})

test_that("conv_distribution_julia: pexp inverts rate in cdf form", {
  result <- conv_distribution_julia(
    arg          = list(q = "2.0", rate = "4.0", lower.tail = "TRUE", log.p = "FALSE"),
    R_func       = "pexp",
    julia_func   = "Distributions.cdf.",
    distribution = "Distributions.Exponential"
  )
  expect_equal(result, "Distributions.cdf.(Distributions.Exponential(1 / (4.0)), 2)")
})

test_that("conv_distribution_julia: pgamma drops rate in cdf form", {
  result <- conv_distribution_julia(
    arg          = list(
      q = "2.0", shape = "2.0", rate = "4.0", scale = "0.25",
      lower.tail = "TRUE", log.p = "FALSE"
    ),
    R_func       = "pgamma",
    julia_func   = "Distributions.cdf.",
    distribution = "Distributions.Gamma"
  )
  expect_equal(result, "Distributions.cdf.(Distributions.Gamma(2.0, 0.25), 2)")
})

test_that("conv_distribution_julia: named args work for non-rate distributions", {
  result <- conv_distribution_julia(
    arg          = list(n = "10", mean = "0.0", sd = "1.0"),
    R_func       = "rnorm",
    julia_func   = "rand",
    distribution = "Distributions.Normal"
  )
  expect_equal(result, "rand(Distributions.Normal(0.0, 1.0), 10)")
})


# ============================================================================
# conv_seq_julia()
# ============================================================================

test_that("conv_seq_julia: seq_along(x) → range(1.0, length(x))", {
  result <- conv_seq_julia(
    arg = list(along.with = "myvec"),
    R_func = "seq_along",
    julia_func = "range"
  )
  expect_equal(result, "range(1.0, length(myvec))")
})

test_that("conv_seq_julia: seq_len(n) → range(1.0, n)", {
  result <- conv_seq_julia(
    arg = list(length.out = "5"),
    R_func = "seq_len",
    julia_func = "range"
  )
  expect_equal(result, "range(1.0, 5)")
})

test_that("conv_seq_julia: seq(from, to, by) → range(from, to, step=by)", {
  result <- conv_seq_julia(
    arg = list(from = "0.0", to = "10.0", by = "2.0"),
    R_func = "seq",
    julia_func = "range"
  )
  expect_equal(result, "range(0.0, 10.0, step=2.0)")
})

test_that("conv_seq_julia: seq(from, to, length.out) → range(..., round_(n))", {
  result <- conv_seq_julia(
    arg = list(from = "0.0", to = "1.0", length.out = "5"),
    R_func = "seq",
    julia_func = "range"
  )
  expect_equal(result, "range(0.0, 1.0, round_(5))")
})

test_that("conv_seq_julia: seq with length.out=1 and from != to → returns just 'from'", {
  result <- conv_seq_julia(
    arg = list(from = "3.0", to = "7.0", length.out = "1"),
    R_func = "seq",
    julia_func = "range"
  )
  expect_equal(result, "3.0")
})

test_that("conv_seq_julia: seq with length.out=1 and from == to → range(from, to, round_(1))", {
  result <- conv_seq_julia(
    arg = list(from = "3.0", to = "3.0", length.out = "1"),
    R_func = "seq",
    julia_func = "range"
  )
  expect_equal(result, "range(3.0, 3.0, round_(1))")
})

test_that("conv_seq_julia: seq without by or length.out defaults to step=1.0", {
  result <- conv_seq_julia(
    arg = list(from = "1.0", to = "5.0"),
    R_func = "seq",
    julia_func = "range"
  )
  expect_match(result, "step=1\\.0")
})


# ============================================================================
# conv_sample_julia()
# ============================================================================

test_that("conv_sample_julia: basic sample without replacement wraps size in round_()", {
  result <- conv_sample_julia(
    arg = list(x = "myvec", size = "3", replace = "FALSE"),
    R_func = "sample",
    julia_func = "StatsBase.sample"
  )
  expect_equal(result, "StatsBase.sample(myvec, round_(3), replace=false)")
})

test_that("conv_sample_julia: replace=TRUE maps to julia true", {
  result <- conv_sample_julia(
    arg = list(x = "myvec", size = "3", replace = "TRUE"),
    R_func = "sample",
    julia_func = "StatsBase.sample"
  )
  expect_match(result, "replace=true")
  expect_false(grepl("replace=false", result))
})

test_that("conv_sample_julia: replace is case-insensitive (TRUE, True, true all work)", {
  r1 <- conv_sample_julia(list(x = "v", size = "1", replace = "TRUE"), "sample", "StatsBase.sample")
  r2 <- conv_sample_julia(list(x = "v", size = "1", replace = "True"), "sample", "StatsBase.sample")
  r3 <- conv_sample_julia(list(x = "v", size = "1", replace = "true"), "sample", "StatsBase.sample")
  expect_equal(r1, r2)
  expect_equal(r2, r3)
})

test_that("conv_sample_julia: weighted sample uses StatsBase.pweights()", {
  result <- conv_sample_julia(
    arg = list(x = "myvec", size = "2", replace = "TRUE", prob = "myprobs"),
    R_func = "sample",
    julia_func = "StatsBase.sample"
  )
  expect_match(result, "StatsBase\\.pweights\\(myprobs\\)")
  expect_match(result, "replace=true")
  expect_match(result, "round_\\(2\\)")
})

test_that("conv_sample_julia: sample.int generates integer range seq(1.0, n)", {
  result <- conv_sample_julia(
    arg = list(n = "10", size = "3", replace = "FALSE"),
    R_func = "sample.int",
    julia_func = "StatsBase.sample"
  )
  expect_match(result, "seq\\(1\\.0,\\s*10\\)")
  expect_match(result, "round_\\(3\\)")
  expect_match(result, "replace=false")
})

