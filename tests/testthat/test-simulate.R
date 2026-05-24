# simulate() tests --------------------------------------------------------

test_that("simulate() requires stocks for simulation", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "aux1", type = "aux", eqn = "5")
  sfm2 <- sim_settings(sfm1, language = "R")

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
  sfm3 <- sim_settings(sfm2, language = "R", start = 0, stop = 10, dt = 1)

  result <- simulate(sfm3)

  expect_s3_class(result, "simulate_sdbuildR")
  expect_true("df" %in% names(result))
  expect_true(nrow(result$df) > 0)
})

test_that("simulate() result has correct structure", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock", eqn = "10")
  sfm2 <- update(sfm1, "Flow1", type = "flow", from = "Stock1", eqn = "Stock1 * 0.1")
  sfm3 <- sim_settings(sfm2, language = "R", start = 0, stop = 5, dt = 1)

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
  sfm3 <- sim_settings(sfm2, language = "R", start = 0, stop = 10, dt = 1)

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
  sfm3 <- sim_settings(sfm2, language = "R", start = 0, stop = 10, dt = 0.1, save_at = 1)

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
  sfm4 <- sim_settings(sfm3, language = "R", start = 0, stop = 10, dt = 1)

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
  sfm4 <- sim_settings(sfm3, language = "R", start = 0, stop = 5, dt = 1)

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
  sfm <- sim_settings(sfm, language = "R", start = 0, stop = 5, dt = 1)

  sim <- expect_successful_simulation(sfm, only_stocks = FALSE)

  expect_true(length(unique(sim$df$variable)) == 3) # time + at least 2 stocks
})

test_that("simulate() returns constants in result", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "const_val", type = "constant", eqn = "42")
  sfm2 <- update(sfm1, "Stock", type = "stock", eqn = "10")
  sfm3 <- update(sfm2, "Flow", type = "flow", from = "Stock", eqn = "0")
  sfm4 <- sim_settings(sfm3, language = "R", start = 0, stop = 5, dt = 1)

  result <- simulate(sfm4)

  expect_true("constants" %in% names(result))
  expect_true("const_val" %in% names(result$constants))
  expect_equal(result$constants[["const_val"]], 42)
})

test_that("simulate() returns initial values", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Pop", type = "stock", eqn = "500")
  sfm2 <- update(sfm1, "Flow", type = "flow", from = "Pop", eqn = "0")
  sfm3 <- sim_settings(sfm2, language = "R", start = 0, stop = 10, dt = 1)

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
  sfm <- sim_settings(sfm, language = "R", start = 0, stop = 10, dt = 1)

  expect_successful_simulation(sfm)

  # Without source
  sfm <- update(sfm, "gf1", source = NA)

  expect_successful_simulation(sfm)
})

test_that("simulate() filters output to vars", {
  sfm <- sdbuildR("SIR") |>
    sim_settings(language = "R", vars = c("susceptible", "new_infections"))

  sim <- simulate(sfm)
  expect_true(sim$success)
  expect_equal(sort(unique(sim$df$variable)), c("new_infections", "susceptible"))
})

test_that("simulate() vars overrides only_stocks", {
  sfm <- sdbuildR("SIR") |>
    sim_settings(language = "R", only_stocks = TRUE, vars = c("new_infections"))

  sim <- simulate(sfm)
  expect_true(sim$success)
  expect_equal(unique(sim$df$variable), "new_infections")
})

test_that("simulate() with Julia filters output to vars", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("SIR") |>
    sim_settings(
      language = "Julia",
      start = 0,
      stop = 5,
      dt = 0.1,
      save_at = 1,
      vars = c("susceptible", "new_infections")
    )

  sim <- simulate(sfm)
  expect_true(sim$success)
  expect_equal(sort(unique(sim$df$variable)), c("new_infections", "susceptible"))
})

test_that("simulate() with Julia vars overrides only_stocks", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("SIR") |>
    sim_settings(
      language = "Julia",
      start = 0,
      stop = 5,
      dt = 0.1,
      save_at = 1,
      only_stocks = TRUE,
      vars = c("new_recoveries")
    )

  sim <- simulate(sfm)
  expect_true(sim$success)
  expect_equal(unique(sim$df$variable), "new_recoveries")
})

# Precision / accuracy tests for simulate.R
# Covers: analytical solutions, conservation laws, save_at, save_n, seed


# ============================================================================
# Analytical solution: exponential decay (make_verifiable_sfm)
# dS/dt = -rate * S  →  S(t) = S0 * exp(-rate * t)
# ============================================================================

test_that("exponential decay: simulation matches analytical solution to within 1%", {
  sfm  <- make_verifiable_sfm()   # S, drain = rate*S (outflow), rate = 0.1; language = "R"
  sim  <- simulate(sfm)
  expect_true(sim$success)

  wide  <- as.data.frame(sim, direction = "wide")
  S0    <- wide[wide[["time"]] == 0, "S"]
  rate  <- 0.1

  S_sim  <- wide$S
  S_true <- S0 * exp(-rate * wide$time)

  max_err <- max(abs(S_sim - S_true) / S_true)
  expect_lt(max_err, 0.01, label = "Max relative error < 1%")
})


# ============================================================================
# Analytical solution: exponential growth (bank_account with R language)
# dB/dt = interest_rate * B  →  B(t) = B0 * exp(interest_rate * t)
# ============================================================================

test_that("bank_account: exponential growth matches analytical solution within 1%", {
  sfm_template <- templates("bank_account")
  sfm  <- sim_settings(sfm_template, start = 0, stop = 50, dt = 0.001, language = "R")
  sim  <- simulate(sfm, seed = 42)
  expect_true(sim$success)

  wide       <- as.data.frame(sim, direction = "wide")
  stock_name <- as.data.frame(sfm_template, type = "stock")$name
  B0         <- wide[wide[["time"]] == 0, stock_name]
  r          <- as.numeric(as.data.frame(sfm_template, type = "constant")$eqn)

  B_sim  <- wide[[stock_name]]
  B_true <- B0 * exp(r * wide$time)

  max_err <- max(abs(B_sim - B_true) / B_true)
  expect_lt(max_err, 0.01, label = "Max relative error < 1%")
})


# ============================================================================
# Conservation law: SIR — S + I + R = constant at every timestep
# ============================================================================

test_that("SIR: total population is conserved at every timestep", {
  skip_if_julia_not_ready()
  sfm  <- sim_settings(templates("SIR"), only_stocks = TRUE)
  sim  <- simulate(sfm, seed = 42)
  expect_true(sim$success)
  wide       <- as.data.frame(sim, direction = "wide")
  stock_cols <- setdiff(names(wide), "time")
  N          <- rowSums(wide[, stock_cols, drop = FALSE])
  expect_equal(diff(range(N)), 0, tolerance = 1e-3)
})


# ============================================================================
# Convergence: logistic_model reaches carrying capacity K within 2%
# ============================================================================

test_that("logistic_model: stock reaches K within 2% at long times", {
  sfm        <- sim_settings(templates("logistic_model"), stop = 300, dt = 0.1,
                              language = "R")
  sim        <- simulate(sfm, seed = 42)
  expect_true(sim$success)
  wide       <- as.data.frame(sim, direction = "wide")
  stock_name <- as.data.frame(templates("logistic_model"), type = "stock")$name
  const_df   <- as.data.frame(templates("logistic_model"), type = "constant")
  K_val      <- max(as.numeric(const_df$eqn), na.rm = TRUE)
  final_mean <- mean(tail(wide[[stock_name]], 10))
  expect_equal(final_mean, K_val, tolerance = K_val * 0.02)
})


# ============================================================================
# save_at: exact output times
# ============================================================================

test_that("simulate with save_at vector returns ONLY exactly those times", {
  target_times <- c(0, 1, 2.5, 5, 10)
  sfm  <- sim_settings(
    sim_settings(make_verifiable_sfm(), save_at = target_times),
    start = 0, stop = 10, dt = 0.01
  )
  sim  <- simulate(sfm)
  wide <- as.data.frame(sim, direction = "wide")
  expect_equal(sort(wide$time), target_times, tolerance = 1e-9)
  expect_equal(nrow(wide), length(target_times))
})

test_that("simulate with scalar save_at returns regular grid of times", {
  sfm  <- sim_settings(make_verifiable_sfm(), start = 0, stop = 10,
                       dt = 0.01, save_at = 1)
  sim  <- simulate(sfm)
  wide <- as.data.frame(sim, direction = "wide")
  expected_times <- seq(0, 10, by = 1)
  expect_equal(sort(wide$time), expected_times, tolerance = 1e-9)
})


# ============================================================================
# save_n: exact row count
# ============================================================================

test_that("simulate with save_n returns exactly N rows", {
  n_save <- 25
  sfm    <- sim_settings(make_verifiable_sfm(), start = 0, stop = 10,
                         save_n = n_save, dt = 0.001)
  sim    <- simulate(sfm)
  expect_equal(nrow(as.data.frame(sim, direction = "wide")), n_save)
})

test_that("simulate with save_n = 1 returns a single-row data frame", {
  sfm <- sim_settings(make_verifiable_sfm(), start = 0, stop = 5,
                      save_n = 1, dt = 0.01)
  sim <- simulate(sfm)
  expect_equal(nrow(as.data.frame(sim, direction = "wide")), 1)
})


# ============================================================================
# Seed reproducibility
# ============================================================================

test_that("simulation is reproducible with seed and random static and dynamic elements", {
  sfm  <- sim_settings(sdbuildR("SIR"), language = "R",
  # runif() should only be done with euler
   method = "euler") |>
          update(c(susceptible, infected, recovered), eqn = runif(1, 1, 1000)) |>
          update(new_infections, eqn = runif(1, 0.01, 0.5) * susceptible * infected) |>
          update(new_recoveries, eqn = runif(1, 0.01, 0.5) * infected)
  sim1 <- simulate(sfm, seed = 42)
  sim2 <- simulate(sfm, seed = 42)
  expect_equal(
    as.data.frame(sim1, direction = "wide"),
    as.data.frame(sim2, direction = "wide")
  )
})


test_that("NULL seed still produces a successful simulation", {
  sfm <- sim_settings(make_verifiable_sfm(), seed = NULL)
  sim <- simulate(sfm)
  expect_true(sim$success)
})
