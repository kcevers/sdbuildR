# Test Helper Functions
# These functions reduce code duplication in tests

#' Helper to skip test if Julia is not ready
#'
skip_if_julia_not_ready <- function() {
  testthat::skip_on_cran()

  env_setup <- tryCatch(
    {
      suppressWarnings({
        is_julia_env_setup()
      })
    },
    error = function(e) {
      return(FALSE)
    }
  )

  if (!env_setup) {
    testthat::skip()
  }

  invisible()
}


#' Expect successful simulation
#'
#' Helper to verify a simulation completes successfully
#'
#' @param sfm A stock-and-flow model
#' @param ... Additional arguments passed to simulate()
#' @returns Simulation result
expect_successful_simulation <- function(sfm, ...) {
  sim <- expect_no_error(simulate(sfm, ...))
  expect_true(sim$success)
  expect_true(nrow(sim$df) > 0)
  expect_true("time" %in% colnames(sim$df))

  # Time range should be correct
  expect_equal(max(sim$df$time), as.numeric(sfm$sim_settings$stop))

  invisible(sim)
}


silence <- function(expr) {
  suppressMessages(suppressWarnings(expr))
}

#' Build a standard SIR model
#'
#' Helper to create the canonical SIR model used across plot tests.
sir_model <- function() {
  sdbuildR("SIR")
}


#' Simulate the SIR model
#'
#' Creates and simulates the SIR model, allowing overrides such as only_stocks.
sir_sim <- function(..., only_stocks = TRUE, seed = 123) {
  simulate(sir_model(), only_stocks = only_stocks, seed = seed, ...)
}


#' Expect a plotly object
#'
#' Small helper to assert an object is a plotly visualization.
expect_plotly <- function(x) {
  testthat::expect_s3_class(x, "plotly")
}


#' Create a basic stock-and-flow model for tests
#'
#' Returns an sdbuildR model with one stock S (eqn=1) and one flow Flow1 (eqn=S, to=S)
#' to reduce duplication across tests. Note: uses "Flow1" instead of "F" to avoid
#' name conflict with R's FALSE constant.
make_basic_sfm <- function() {
  sdbuildR() |>
    update("S", type = "stock", eqn = "1") |>
    update("Flow1", type = "flow", eqn = "S", to = "S")
}


# Skip tests if internet not available or on CRAN
skip_if_no_internet <- function() {
  if (!has_internet()) {
    testthat::skip("No internet connection")
  }
  if (Sys.getenv("NOT_CRAN") != "true") {
    testthat::skip("Not run on CRAN")
  }
}


# Local helper: model with stock, flow, constant, language = Julia
# Validation in ensemble() fails before Julia execution, so no Julia needed
make_ensemble_error_sfm <- function() {
  sdbuildR() |>
    update("S", type = "stock", eqn = "1") |>
    update("Flow1", type = "flow", eqn = "S", to = "S") |>
    update("k", type = "constant", eqn = "0.5") |>
    sim_settings(language = "Julia")
}


# Helper: standard sfm for method tests
make_jl_ensemble_sfm <- function() {
  sdbuildR("Crielaard2022") |>
    sim_settings(start = 0, stop = 10, dt = 0.1, save_at = 1, language = "Julia")
}


# Helper: small model for R ensemble tests (no Julia required)
make_r_ensemble_sfm <- function() {
  sdbuildR("SIR") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1, save_at = 1)
}

make_r_ensemble_random_sfm <- function() {
  sdbuildR("SIR") |>
    update("Susceptible", eqn = "runif(1, 900, 1100)") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1, save_at = 1)
}


# Helper: a small model with a stock, flow, and constant
make_verifiable_sfm <- function() {
  sdbuildR() |>
    update("S", type = "stock", eqn = "100") |>
    update("drain", type = "flow", eqn = "rate * S", from = "S") |>
    update("rate", type = "constant", eqn = "0.1") |>
    sim_settings(stop = 10, dt = 0.1, save_at = 1)
}
