# ==============================================================================
# R -> Julia argument-mapping contract
# ==============================================================================
#
# When sdbuildR translates an R model equation to Julia it ALWAYS emits a
# positional call: argument *names* are dropped and the values are placed by
# position. There are two paths in `sort_args()`:
#
#   * fill_defaults = TRUE  -> resolve against R's formals, fill the unprovided
#                              defaults, and reorder into formal order. This is
#                              the ONLY way a NAMED optional argument (e.g.
#                              `upper = 10`) can land in the right Julia slot, so
#                              the Julia target MUST be positional in R's formal
#                              order.
#   * fill_defaults = FALSE -> pass the user's arguments through in the given
#                              order with names dropped. Safe only when the user
#                              cannot name an argument out of position (≤ 1
#                              optional arg, in first position).
#
# Consequence (the contract this file guards):
#   Any custom Julia function with optional parameters must be (a) positional, in
#   R's formal order, and (b) registered with fill_defaults = TRUE.
#
# This suite is self-contained: it builds its own converter helper and asserts
# both the table-level registration invariants and the actual emitted Julia for
# every function whose argument mapping is non-trivial. It also pins the
# regressions for the bugs found while establishing the contract:
#   * ricker(): keyword-only Julia def + missing fill_defaults
#   * diff():   differences = silently became lag
#   * seasonal(): Julia signature had an extra leading `dt`
#   * paste0(): collapse = silently became an extra concatenation part
# ==============================================================================

# Build a converter over a fixed set of known variable names. Anything not in
# `var_names` is treated as a function/global, which is exactly what we want for
# the bare data symbols (x, a, b, ...) used below.
arg_map_conv <- function(extra = character()) {
  var_names <- unique(c(
    "x", "y", "a", "b", "tbl", "cond", "times", "n", "s", "p1", "p2", extra
  ))
  function(eqn) convert_equations_julia("aux", "z", eqn, var_names)[["eqn"]]
}


# ------------------------------------------------------------------------------
# Structural invariants: the registration table itself
# ------------------------------------------------------------------------------

test_that("custom funcs with optional args are registered fill_defaults = TRUE", {
  sd <- syntax_julia[["syntax_df"]]

  # Every function that has more than one user-nameable optional argument, or
  # whose Julia target is positional and order-sensitive, must fill defaults.
  need_fill <- c(
    "ricker", "hill", "logistic", "sigmoid",
    "diff", "rep", "grep",
    "step", "ramp", "pulse", "seasonal"
  )

  flagged <- as.logical(sd[["fill_defaults"]][match(need_fill, sd[["R_first_iter"]])])
  expect_true(
    all(flagged),
    info = paste0("Not fill_defaults: ", toString(need_fill[!flagged]))
  )
})

test_that("paste0() is registered with its dedicated variadic handler", {
  sd <- syntax_julia[["syntax_df"]]
  expect_equal(sd[["syntax"]][sd[["R_first_iter"]] == "paste0"], "syntax_paste")
})

test_that("emitted custom-function calls are positional (names never leak)", {
  conv <- arg_map_conv()
  # If a Julia keyword name ever leaked through, the output would contain " = ".
  # (paste0's collapse is folded into join(), so it must not leak either.)
  positional_only <- c(
    "ricker(x, upper = 10)",
    "hill(x, midpoint = 2)",
    "logistic(x, slope = 2)",
    "diff(x, differences = 2)",
    "seasonal(times, period = 2)",
    "paste0(p1, p2, collapse = \"-\")"
  )
  for (e in positional_only) {
    expect_false(grepl(" = ", conv(e), fixed = TRUE), info = e)
  }
})


# ------------------------------------------------------------------------------
# ricker(): peak parameterization and the a/b coefficient form
# ------------------------------------------------------------------------------

test_that("ricker: named args map to (location, upper, shape, a, b) slots", {
  conv <- arg_map_conv()
  expect_equal(conv("ricker(x)"), "ricker.(x, 1.0, 1.0, 1.0)")
  # Regression: `upper = 10` must be the 2nd slot, not the 1st (location).
  expect_equal(conv("ricker(x, upper = 10)"), "ricker.(x, 1.0, 10.0, 1.0)")
  expect_equal(conv("ricker(x, location = 2, shape = 3)"), "ricker.(x, 2.0, 1.0, 3.0)")
})

test_that("ricker: a/b coefficient form fills location/upper defaults positionally", {
  conv <- arg_map_conv()
  # location/upper are filled with their defaults (1.0, 1.0); the Julia ricker
  # lets a, b override them, so this is correct rather than a conflict.
  expect_equal(conv("ricker(x, a = 2.5, b = 0.4)"), "ricker.(x, 1.0, 1.0, 1.0, 2.5, 0.4)")
  expect_equal(conv("ricker(x, a = 2.5, b = 0.4, shape = 2)"), "ricker.(x, 1.0, 1.0, 2.0, 2.5, 0.4)")
})


# ------------------------------------------------------------------------------
# hill() / logistic(): named args out of order
# ------------------------------------------------------------------------------

test_that("hill/logistic: named args map to (slope, midpoint, upper) slots", {
  conv <- arg_map_conv()
  expect_equal(conv("hill(x, upper = 10)"), "hill.(x, 1.0, 0.5, 10.0)")
  expect_equal(conv("hill(x, midpoint = 2)"), "hill.(x, 1.0, 2.0, 1.0)")
  expect_equal(conv("logistic(x, midpoint = 5, upper = 2)"), "logistic.(x, 1.0, 5.0, 2.0)")
  # sigmoid is an alias of logistic
  expect_equal(conv("sigmoid(x, upper = 2)"), "logistic.(x, 1.0, 0.0, 2.0)")
})


# ------------------------------------------------------------------------------
# Signal builders: Julia signature must match R formal order
# ------------------------------------------------------------------------------

test_that("step/ramp/pulse: named args map into the make_* signature", {
  conv <- arg_map_conv()
  expect_equal(conv("step(times, 5, height = 2)"), "make_step(times, 5.0, 2.0)")
  expect_equal(conv("ramp(times, 2, 5, height = 10)"), "make_ramp(times, 2.0, 5.0, 10.0)")
  expect_equal(conv("pulse(times, 5, height = 2, width = 3)"), "make_pulse(times, 5.0, 2.0, 3.0)")
})

test_that("seasonal: maps to make_seasonal(times, period, shift) (regression)", {
  conv <- arg_map_conv()
  # Regression: make_seasonal previously had an extra leading `dt`, so the
  # converter's (times, period, shift) call was off by one and crashed.
  expect_equal(conv("seasonal(times, period = 2, shift = 1)"), "make_seasonal(times, 2.0, 1.0)")
  expect_equal(conv("seasonal(times)"), "make_seasonal(times, 1.0, 0.0)")
})


# ------------------------------------------------------------------------------
# diff(): lag vs differences (regression)
# ------------------------------------------------------------------------------

test_that("diff: lag/differences map by name, not by silent position", {
  conv <- arg_map_conv()
  expect_equal(conv("diff(x)"), "r_diff(x, 1.0, 1.0)")
  expect_equal(conv("diff(x, 2)"), "r_diff(x, 2.0, 1.0)") # positional lag
  expect_equal(conv("diff(x, lag = 2)"), "r_diff(x, 2.0, 1.0)")
  # Regression: differences must NOT land in the lag slot.
  expect_equal(conv("diff(x, differences = 2)"), "r_diff(x, 1.0, 2.0)")
  expect_equal(conv("diff(x, lag = 2, differences = 3)"), "r_diff(x, 2.0, 3.0)")
  expect_equal(conv("diff(x, differences = 3, lag = 2)"), "r_diff(x, 2.0, 3.0)")
})


# ------------------------------------------------------------------------------
# rep() / grep(): named-options mapping
# ------------------------------------------------------------------------------

test_that("rep: each/times/length.out map to r_rep(x, times, length_out, each)", {
  conv <- arg_map_conv()
  expect_equal(conv("rep(x, each = 2)"), "r_rep(x, 1.0, -1.0, 2.0)")
  expect_equal(conv("rep(x, times = 3)"), "r_rep(x, 3.0, -1.0, 1.0)")
})

test_that("grep: value= flag lands in the correct r_grep slot", {
  conv <- arg_map_conv()
  expect_equal(conv("grep(\"a\", x)"), "r_grep(\"a\", x, false, false, false, false, false, false)")
  expect_equal(conv("grep(\"a\", x, value = TRUE)"), "r_grep(\"a\", x, false, false, true, false, false, false)")
})


# ------------------------------------------------------------------------------
# paste0(): collapse must become join() (regression)
# ------------------------------------------------------------------------------

test_that("paste0: element-wise concatenation broadcasts string()", {
  conv <- arg_map_conv()
  expect_equal(conv("paste0(p1, p2)"), "string.(p1, p2)")
  expect_equal(conv("paste0(\"a\", x)"), "string.(\"a\", x)")
})

test_that("paste0: collapse becomes join(), not an extra string() part", {
  conv <- arg_map_conv()
  # Regression: collapse = "-" previously dropped its name and became a third
  # part, i.e. string.(p1, p2, "-").
  expect_equal(conv("paste0(p1, p2, collapse = \"-\")"), "join(string.(p1, p2), \"-\")")
  # collapse = "" is a real (empty-separator) collapse, distinct from no collapse
  expect_equal(conv("paste0(p1, collapse = \"\")"), "join(string.(p1), \"\")")
  # collapse = NULL is the R default: no collapse
  expect_equal(conv("paste0(p1, p2, collapse = NULL)"), "string.(p1, p2)")
})


# ------------------------------------------------------------------------------
# No-fill functions: a single optional in first position is safe
# ------------------------------------------------------------------------------

test_that("no-fill funcs with one first-position optional map correctly", {
  conv <- arg_map_conv()
  expect_equal(conv("sort(x, decreasing = TRUE)"), "r_sort(x, true)")
  expect_equal(conv("round(x, digits = 2)"), "round_.(x, 2.0)")
  expect_equal(conv("upper.tri(x, diag = TRUE)"), "r_upper_tri(x, true)")
})
