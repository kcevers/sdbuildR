# print.simulate_stockflow() -----------------------------------------------

test_that("print.simulate_stockflow() returns x invisibly", {
  sim <- silence(sir_sim())
  expect_invisible(print(sim))
})

test_that("print.simulate_stockflow() snapshot: successful simulation", {
  sim <- silence(sir_sim())
  expect_snapshot(print(sim))
})

test_that("print.simulate_stockflow() snapshot: named model", {
  sfm <- stockflow("SIR") |> meta(name = "My SIR Model")
  sim <- silence(simulate(sfm))
  expect_snapshot(print(sim))
})

test_that("print.simulate_stockflow() snapshot: failed simulation", {
  sim <- new_simulate_stockflow(
    success       = FALSE,
    error_message = "something went wrong"
  )
  expect_snapshot(print(sim))
})


# summary.simulate_stockflow() ---------------------------------------------

test_that("summary.simulate_stockflow() returns a data frame", {
  sim <- silence(sir_sim())
  expect_s3_class(summary(sim), "data.frame")
})

test_that("summary.simulate_stockflow() has correct columns", {
  result <- summary(silence(sir_sim()))
  expect_true(all(c("variable", "min", "mean", "max", "final") %in% names(result)))
})

test_that("summary.simulate_stockflow() has one row per variable", {
  sim <- silence(sir_sim())
  result <- summary(sim)
  vars <- unique(sim$df$variable)
  expect_equal(nrow(result), length(vars))
  expect_equal(sort(result$variable), sort(vars))
})

test_that("summary.simulate_stockflow() errors on failed simulation", {
  sim <- new_simulate_stockflow(
    success       = FALSE,
    error_message = "something went wrong"
  )
  expect_error(summary(sim), class = "rlang_error")
})
