# Tests for summary.sdbuildR and print.summary_sdbuildR

test_that("summary() returns summary_sdbuildR class", {
  sfm <- sdbuildR()
  expect_s3_class(summary(sfm), "summary_sdbuildR")
})

test_that("summary() has dependencies, n_errors, n_warnings fields", {
  summ <- summary(sdbuildR("SIR"))
  expect_true(all(c("dependencies", "n_errors", "n_warnings") %in% names(summ)))
})

test_that("summary() dependencies is a named list", {
  summ <- summary(sdbuildR("SIR"))
  expect_type(summ$dependencies, "list")
  expect_true(length(names(summ$dependencies)) > 0)
})

test_that("summary() only includes variables that have at least one dependency", {
  sfm <- sdbuildR() |>
    build("S", type = "stock", eqn = "100") |>
    build("k", type = "constant", eqn = "0.1") |>
    build("Flow1", type = "flow", from = "S", eqn = "k * S")
  summ <- summary(sfm)
  expect_false("k"    %in% names(summ$dependencies)) # constant — no deps
  expect_false("S"    %in% names(summ$dependencies)) # stock — no deps
  expect_true("Flow1" %in% names(summ$dependencies)) # flow — depends on k and S
  expect_true("k" %in% summ$dependencies[["Flow1"]])
  expect_true("S" %in% summ$dependencies[["Flow1"]])
})

test_that("summary() n_errors and n_warnings are zero for valid model", {
  sfm <- sdbuildR() |>
    build("S", type = "stock", eqn = "100") |>
    build("Flow1", type = "flow", from = "S", eqn = "0.1 * S")
  summ <- summary(sfm)
  expect_equal(summ$n_errors,   0L)
  expect_equal(summ$n_warnings, 0L)
})

test_that("summary() n_errors > 0 for model with errors", {
  sfm <- sdbuildR() # empty model — no stocks (error)
  summ <- summary(sfm)
  expect_gt(summ$n_errors, 0)
})

test_that("summary() n_warnings > 0 for model with warnings", {
  sfm <- sdbuildR() |>
    build("S", type = "stock") # disconnected stock — warning
  summ <- summary(sfm)
  expect_gt(summ$n_warnings, 0)
})

test_that("print(summary(sfm)) produces output without error", {
  sfm <- sdbuildR("SIR")
  expect_no_error(print(summary(sfm)))
})

test_that("print(summary(sfm)) shows Dependencies section", {
  sfm <- sdbuildR("SIR")
  expect_snapshot(print(summary(sfm)))
})

test_that("print(summary(sfm)) shows Diagnostics section", {
  sfm <- sdbuildR("SIR")
  expect_snapshot(print(summary(sfm)))
})

test_that("print(summary(sfm)) reports no issues for valid model", {
  sfm <- sdbuildR() |>
    build("S", type = "stock", eqn = "100") |>
    build("Flow1", type = "flow", from = "S", eqn = "0.1 * S")
  expect_snapshot(print(summary(sfm)))
})

test_that("print(summary(sfm)) reports errors for invalid model", {
  sfm <- sdbuildR() # empty — no stocks
  expect_snapshot(print(summary(sfm)))
})
