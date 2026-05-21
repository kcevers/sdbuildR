# print.simulate_sdbuildR() -----------------------------------------------

test_that("print.simulate_sdbuildR() returns x invisibly", {
  sim <- silence(sir_sim())
  expect_invisible(print(sim))
})

test_that("print.simulate_sdbuildR() snapshot: successful simulation", {
  sim <- silence(sir_sim())
  expect_snapshot(print(sim))
})

test_that("print.simulate_sdbuildR() snapshot: named model", {
  sfm <- sdbuildR("SIR") |> meta(name = "My SIR Model")
  sim <- silence(simulate(sfm))
  expect_snapshot(print(sim))
})

test_that("print.simulate_sdbuildR() snapshot: failed simulation", {
  sim <- sdbuildR:::new_simulate_sdbuildR(
    success       = FALSE,
    error_message = "something went wrong"
  )
  expect_snapshot(print(sim))
})


# summary.simulate_sdbuildR() ---------------------------------------------

test_that("summary.simulate_sdbuildR() returns a data frame", {
  sim <- silence(sir_sim())
  expect_s3_class(summary(sim), "data.frame")
})

test_that("summary.simulate_sdbuildR() has correct columns", {
  result <- summary(silence(sir_sim()))
  expect_true(all(c("variable", "min", "mean", "max", "final") %in% names(result)))
})

test_that("summary.simulate_sdbuildR() has one row per variable", {
  sim <- silence(sir_sim())
  result <- summary(sim)
  vars <- unique(sim$df$variable)
  expect_equal(nrow(result), length(vars))
  expect_equal(sort(result$variable), sort(vars))
})

test_that("summary.simulate_sdbuildR() errors on failed simulation", {
  sim <- sdbuildR:::new_simulate_sdbuildR(
    success       = FALSE,
    error_message = "something went wrong"
  )
  expect_error(summary(sim), class = "rlang_error")
})
