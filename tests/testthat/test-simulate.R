# simulate() tests --------------------------------------------------------

test_that("simulate() requires stocks for simulation", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "aux1", type = "aux", eqn = "5")
  sfm2 <- sim_specs(sfm1, language = "R")

  expect_error(
    simulate(sfm2),
    "no stocks|Cannot simulate|Model has.*problem",
    ignore.case = TRUE
  )
})


test_that("simulate() with R language works on simple model", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Pop", type = "stock", eqn = "100")
  sfm2 <- update(sfm1, "Growth", type = "flow", from = "Pop", eqn = "Pop * 0.05")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 10, dt = 1)

  result <- simulate(sfm3)

  expect_s3_class(result, "simulate_sdbuildR")
  expect_true("df" %in% names(result))
  expect_true(nrow(result$df) > 0)
})

test_that("simulate() result has correct structure", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock", eqn = "10")
  sfm2 <- update(sfm1, "Flow1", type = "flow", from = "Stock1", eqn = "Stock1 * 0.1")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 5, dt = 1)

  result <- simulate(sfm3)

  # Check required fields
  expect_true("df" %in% names(result))
  expect_true("object" %in% names(result))
  expect_true("init" %in% names(result))
  expect_true("constants" %in% names(result))
  expect_true("success" %in% names(result))
  expect_equal(result$success, TRUE)
})

test_that("simulate() returns data frame with time column", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "S", type = "stock", eqn = "100")
  sfm2 <- update(sfm1, "Flow", type = "flow", from = "S", eqn = "0")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 10, dt = 1)

  result <- simulate(sfm3)
  df <- result$df

  expect_true("time" %in% colnames(df))
  expect_equal(min(df$time), 0)
  expect_equal(max(df$time), 10)
})

test_that("simulate() respects save_at interval", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "X", type = "stock", eqn = "1")
  sfm2 <- update(sfm1, "Flow", type = "flow", from = "X", eqn = "0")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 10, dt = 0.1, save_at = 1)

  result <- simulate(sfm3, only_stocks = FALSE)

  # With save_at = 1, should have roughly (10-0)/1 + 1 = 11 time points
  # But may have more depending on solver output
  expect_true(nrow(result$df) >= 11)
  expect_true(nrow(result$df) <= 25)
})

test_that("simulate() with constants", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "rate", type = "constant", eqn = "0.05")
  sfm2 <- update(sfm1, "Stock", type = "stock", eqn = "100")
  sfm3 <- update(sfm2, "Flow", type = "flow", from = "Stock", eqn = "Stock * rate")
  sfm4 <- sim_specs(sfm3, language = "R", start = 0, stop = 10, dt = 1)

  result <- simulate(sfm4, only_stocks = FALSE)

  expect_true(result$success)
  expect_true("time" %in% colnames(result$df))
  expect_true(ncol(result$df) >= 2) # At least time + one stock
})

test_that("simulate() with auxiliaries", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "S", type = "stock", eqn = "100")
  sfm2 <- update(sfm1, "rate", type = "aux", eqn = "0.1")
  sfm3 <- update(sfm2, "Flow", type = "flow", from = "S", eqn = "S * rate")
  sfm4 <- sim_specs(sfm3, language = "R", start = 0, stop = 5, dt = 1)

  result <- simulate(sfm4, only_stocks = FALSE)

  expect_true(result$success)
  expect_true("time" %in% colnames(result$df))
  expect_true(ncol(result$df) >= 2) # At least time + stocks
})

test_that("simulate() with multiple stocks", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "S", type = "stock", eqn = "100")
  sfm <- update(sfm, "I", type = "stock", eqn = "10")
  sfm <- update(sfm, "infection", type = "flow", from = "S", to = "I", eqn = "0")
  sfm <- sim_specs(sfm, language = "R", start = 0, stop = 5, dt = 1)

  sim <- expect_successful_simulation(sfm, only_stocks = FALSE)

  expect_true(length(unique(sim$df$variable)) == 3) # time + at least 2 stocks
})

test_that("simulate() returns constants in result", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "const_val", type = "constant", eqn = "42")
  sfm2 <- update(sfm1, "Stock", type = "stock", eqn = "10")
  sfm3 <- update(sfm2, "Flow", type = "flow", from = "Stock", eqn = "0")
  sfm4 <- sim_specs(sfm3, language = "R", start = 0, stop = 5, dt = 1)

  result <- simulate(sfm4)

  expect_true("constants" %in% names(result))
  expect_true("const_val" %in% names(result$constants))
  expect_equal(result$constants[["const_val"]], 42)
})

test_that("simulate() returns initial values", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Pop", type = "stock", eqn = "500")
  sfm2 <- update(sfm1, "Flow", type = "flow", from = "Pop", eqn = "0")
  sfm3 <- sim_specs(sfm2, language = "R", start = 0, stop = 10, dt = 1)

  result <- simulate(sfm3)

  expect_true("init" %in% names(result))
  expect_equal(unname(result$init["Pop"]), 500)
})


test_that("simulate() with graphical function dependency", {
  sfm <- sdbuildR()
  sfm <- update(sfm,
    name = "gf1",
    type = "lookup",
    xpts = c(0, 10, 20),
    ypts = c(0, 100, 50)
  )
  sfm <- update(sfm, "Stock1", type = "stock", eqn = "50")
  sfm <- update(sfm, "Flow1", type = "flow", from = "Stock1", eqn = "gf1(Stock1)")
  sfm <- sim_specs(sfm, language = "R", start = 0, stop = 10, dt = 1)

  expect_successful_simulation(sfm)

  # Without source
  sfm <- update(sfm, "gf1", source = NA)

  expect_successful_simulation(sfm)
})

test_that("simulate() filters output to vars", {
  sfm <- sdbuildR("SIR") |>
    sim_specs(language = "R", vars = c("Susceptible", "Infection_Rate"))

  sim <- simulate(sfm)
  expect_true(sim$success)
  expect_equal(sort(unique(sim$df$variable)), c("Infection_Rate", "Susceptible"))
})

test_that("simulate() vars overrides only_stocks", {
  sfm <- sdbuildR("SIR") |>
    sim_specs(language = "R", only_stocks = TRUE, vars = c("Infection_Rate"))

  sim <- simulate(sfm)
  expect_true(sim$success)
  expect_equal(unique(sim$df$variable), "Infection_Rate")
})

test_that("simulate() with Julia filters output to vars", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("SIR") |>
    sim_specs(
      language = "Julia",
      start = 0,
      stop = 5,
      dt = 0.1,
      save_at = 1,
      vars = c("Susceptible", "Infection_Rate")
    )

  sim <- simulate(sfm)
  expect_true(sim$success)
  expect_equal(sort(unique(sim$df$variable)), c("Infection_Rate", "Susceptible"))
})

test_that("simulate() with Julia vars overrides only_stocks", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("SIR") |>
    sim_specs(
      language = "Julia",
      start = 0,
      stop = 5,
      dt = 0.1,
      save_at = 1,
      only_stocks = TRUE,
      vars = c("Infection_Rate")
    )

  sim <- simulate(sfm)
  expect_true(sim$success)
  expect_equal(unique(sim$df$variable), "Infection_Rate")
})
