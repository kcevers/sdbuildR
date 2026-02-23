# Test Helper Functions
# These functions reduce code duplication in tests

#' Helper to skip test if Julia is not ready
#'
skip_if_julia_not_ready <- function() {
  testthat::skip_on_cran()
  if (!is_julia_ready()) {
    testthat::skip()
  }
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
  expect_equal(min(sim$df$time), as.numeric(sfm$sim_specs$save_from))
  expect_equal(max(sim$df$time), as.numeric(sfm$sim_specs$stop))
  expect_equal(mean(diff(unique(sim$df$time))), as.numeric(sfm$sim_specs$save_at))

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
sir_sim <- function(..., only_stocks = TRUE) {
  simulate(sir_model(), only_stocks = only_stocks, ...)
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
    build("S", type = "stock", eqn = "1") |>
    build("Flow1", type = "flow", eqn = "S", to = "S")
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
make_ensemble_sfm <- function() {
  sdbuildR() |>
    build("S", type = "stock", eqn = "1") |>
    build("Flow1", type = "flow", eqn = "S", to = "S") |>
    build("k", type = "constant", eqn = "0.5") |>
    sim_specs(language = "Julia")
}

