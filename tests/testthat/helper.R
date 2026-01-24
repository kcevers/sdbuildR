# Test Helper Functions
# These functions reduce code duplication in tests


#' Skip test if Julia is not ready
#'
#' Combines skip_on_cran() and skip_if_not(julia_status()$status == "ready")
#' for tests that require Julia
skip_if_julia_not_ready <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not(julia_status()$status == "ready")
}

#' Get variable attribute from new flat data frame structure
#'
#' Helper to access variable attributes in the new flat data frame structure.
#' Replaces old nested access like sfm[["model"]][["variables"]][["flow"]][["name"]][["to"]]
#' with get_var_attr(sfm, "name", "flow", "to")
#'
#' @param sfm A stock-and-flow model
#' @param name Name of the variable
#' @param type Type of variable (optional, can be NULL to search all types)
#' @param attr Attribute to extract (e.g., "to", "from", "eqn", "units")
#' @returns Value of the attribute, or NULL if not found
get_var_attr <- function(sfm, name, type = NULL, attr) {
  if (is.null(type)) {
    row <- sfm[["variables"]][sfm[["variables"]][["name"]] == name, ]
  } else {
    row <- sfm[["variables"]][sfm[["variables"]][["name"]] == name & 
                              sfm[["variables"]][["type"]] == type, ]
  }
  
  if (nrow(row) == 0) return(NULL)
  
  value <- row[[attr]]
  if (length(value) == 1) return(value[[1]])
  return(value)
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
  invisible(sim)
}


#' Create a basic stock-and-flow model for tests
#'
#' Returns an xmile model with one stock S (eqn=1) and one flow Flow1 (eqn=S, to=S)
#' to reduce duplication across tests. Note: uses "Flow1" instead of "F" to avoid 
#' name conflict with R's FALSE constant.
make_basic_sfm <- function() {
  xmile() |>
    build("S", type = "stock", eqn = "1") |>
    build("Flow1", type = "flow", eqn = "S", to = "S")
}
