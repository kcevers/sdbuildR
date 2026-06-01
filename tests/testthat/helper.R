# Test Helper Functions
expect_snapshot_plot <- function(name, code, fileext = NULL, width = 4, height = 4) {
  # Announce the file before touching skips or running `code`. This way,
  # if the skips are active, testthat will not auto-delete the corresponding snapshot file.
  withr::local_pdf(NULL)

  pl <- code
  if (is.null(fileext)) {
    if (inherits(pl, "plotly")) {
      fileext <- ".png"
      plotly_object <- TRUE
    } else if (inherits(pl, "grViz")) {
      fileext <- ".svg"
      plotly_object <- FALSE
    } else {
      stop("Unable to determine file extension for plot snapshot. Please specify fileext.")
    }
  }

  name <- paste0(name, fileext)
  announce_snapshot_file(name = name)

  skip_on_cran()
  # skip_on_os("mac")
  skip_if(
    plotly_object && !has_internet(),
    "No internet connection for plot snapshot test"
  )

  if (FALSE) {
    skip("Temporarily skip for faster testing")
  }

  if (!plotly_object) {
    # For non-plotly objects, we can directly export to the target format
    path <- tempfile(fileext = fileext)

    export_plot(pl, file = path, width = width, height = height)
    expect_snapshot_file(path, name)
  } else {
    # stable assertion on structure
    json <- plotly::plotly_json(pl, jsonedit = FALSE)
    # normalize plotly's random internal IDs
    json <- gsub('"[a-f0-9]{8,}"', '"ID"', json)

    json_path <- tempfile(fileext = ".json")
    writeLines(json, json_path)
    expect_snapshot_file(json_path, name = paste0(name, ".json"))

    # visual artifact for manual inspection, never fails
    img_path <- tempfile(fileext = ".png")
    tryCatch(
      export_plot(pl, file = img_path, width = width, height = height),
      stockflow_export_error = function(e) {
        testthat::skip(paste("Plot export unavailable:", conditionMessage(e)))
      }
    )
    expect_snapshot_file(img_path,
      name = paste0(name, ".png"),
      compare = function(old, new) TRUE
    )
  }

  invisible()
}


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
    skip("No internet connection")
  }
  skip_on_cran()
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
    update("susceptible", eqn = "runif(1, 900, 1100)") |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1, save_at = 1)
}


# Helper: a small model with a stock, flow, and constant
# Pass language = "Julia" to use the Julia backend instead of R.
make_verifiable_sfm <- function(language = "R") {
  sdbuildR() |>
    update("S", type = "stock", eqn = runif(1, 1, 100)) |>
    update("drain", type = "flow", eqn = "rate * S", from = "S") |>
    update("rate", type = "constant", eqn = "0.1") |>
    sim_settings(stop = 10, dt = 0.1, save_at = 1, language = language, seed = 123)
}


# Helper: build a verify_sdbuildR result with configurable tests.
#   n_tests = 1 : one test ("S non-negative")
#   n_tests = 2 : adds a conditioned test ("S constant at zero rate")
#   with_fail   : adds an intentionally failing test instead of the conditioned one
make_verify_model <- function(n_tests = 1, with_fail = FALSE) {
  sfm <- make_verifiable_sfm() |>
    update(S, eqn = 10) |>
    unit_test(label = "S non-negative", expr = "all(S >= 0)")

  if (n_tests >= 2 && !with_fail) {
    sfm <- sfm |> unit_test(
      label = "S constant at zero rate",
      expr = "all(diff(S) == 0)",
      conditions = list(rate = 0)
    )
  } else if (with_fail) {
    sfm <- sfm |> unit_test(label = "S always zero", expr = "all(S == 0)")
  }

  silence(verify(sfm))
}


# Helper: deterministic R ensemble (seed fixed for snapshot reproducibility).
# save_sims = TRUE  → individual trajectories are stored in the result.
# conditions          → optional named list passed to ensemble().
make_r_ens <- function(n = 5, save_sims = FALSE, conditions = NULL, ...) {
  sfm <- make_r_ensemble_random_sfm() |> sim_settings(seed = 42)
  args <- list(sfm, n = n, save_sims = save_sims, verbose = FALSE, ...)
  if (!is.null(conditions)) args$conditions <- conditions
  silence(do.call(ensemble, args))
}

make_r_ens_2cond <- function(n = 3, ...) {
  make_r_ens(n = n, conditions = list("contact_rate" = c(1.5, 2.5)), ...)
}

##### Plotly attribute helpers #####
plotly_traces <- function(pl) {
  plotly::plotly_build(pl)[["x"]][["data"]]
}

# `%||%` <- function(x, y) {
#   if (is.null(x)) y else x
# }

trace_group <- function(trace) {
  group <- trace[["legendgroup"]]
  if (is.null(group) || !nzchar(as.character(group)[1L])) {
    return(NA_character_)
  }
  as.character(group)[1L]
}

trace_name <- function(trace) {
  name <- trace[["name"]]
  if (is.null(name) || !nzchar(as.character(name)[1L])) {
    return(NA_character_)
  }
  as.character(name)[1L]
}

trace_visible <- function(trace) {
  visible <- trace[["visible"]]
  if (is.null(visible)) {
    return("TRUE")
  }
  if (is.logical(visible)) {
    return(if (isTRUE(visible[1L])) "TRUE" else "FALSE")
  }
  as.character(visible)[1L]
}

trace_showlegend <- function(trace) {
  showlegend <- trace[["showlegend"]]
  if (is.null(showlegend)) {
    return(FALSE)
  }
  isTRUE(showlegend)
}

trace_color <- function(trace) {
  color <- NULL

  if (!is.null(trace[["line"]])) {
    color <- trace[["line"]][["color"]]
  }
  if (is.null(color) && !is.null(trace[["marker"]])) {
    color <- trace[["marker"]][["color"]]
  }
  if (is.null(color) && !is.null(trace[["fillcolor"]])) {
    color <- trace[["fillcolor"]]
  }

  if (is.null(color) || !nzchar(as.character(color)[1L])) {
    return(NA_character_)
  }

  as.character(color)[1L]
}


#' Normalize a Plotly color string to #RRGGBB uppercase when possible
#'
#' Accepts hex (#RRGGBB), rgba()/rgb() and named R colours; returns uppercase
#' #RRGGBB or NA_character_. Used by tests to compare colours robustly.
normalize_color_string <- function(col) {
  if (is.null(col)) {
    return(NA_character_)
  }
  col_chr <- as.character(col)
  if (!nzchar(col_chr)) {
    return(NA_character_)
  }

  # rgba(...) or rgb(...)
  if (grepl("^rgba?\\(", col_chr)) {
    nums <- as.numeric(strsplit(gsub("rgba?\\(|\\)", "", col_chr), ",")[[1L]])
    if (length(nums) >= 3) {
      r <- nums[1]
      g <- nums[2]
      b <- nums[3]
      if (max(nums, na.rm = TRUE) <= 1) {
        r <- round(r * 255)
        g <- round(g * 255)
        b <- round(b * 255)
      }
      return(toupper(grDevices::rgb(r, g, b, maxColorValue = 255)))
    }
  }

  # Hex (#RRGGBB or #RRGGBBAA) -> strip alpha if present
  if (grepl("^#", col_chr)) {
    if (nchar(col_chr) >= 7) {
      return(toupper(substr(col_chr, 1, 7)))
    }
    return(toupper(col_chr))
  }

  # Named colours via col2rgb
  rgb_val <- tryCatch(grDevices::col2rgb(col_chr), error = function(e) NULL)
  if (!is.null(rgb_val)) {
    return(toupper(grDevices::rgb(rgb_val[1, 1], rgb_val[2, 1], rgb_val[3, 1], maxColorValue = 255)))
  }

  NA_character_
}

plotly_trace_summary <- function(pl) {
  traces <- plotly_traces(pl)

  if (!length(traces)) {
    return(data.frame())
  }

  data.frame(
    trace = seq_along(traces),
    name = vapply(traces, trace_name, character(1)),
    legendgroup = vapply(traces, trace_group, character(1)),
    showlegend = vapply(traces, trace_showlegend, logical(1)),
    visible = vapply(traces, trace_visible, character(1)),
    color = vapply(traces, trace_color, character(1)),
    stringsAsFactors = FALSE
  )
}

plotly_legend_items <- function(pl) {
  out <- plotly_trace_summary(pl)
  out[out$showlegend, c("trace", "name", "legendgroup", "visible", "color")]
}

plotly_visible_legend_items <- function(pl) {
  out <- plotly_legend_items(pl)
  out[out$visible != "legendonly", , drop = FALSE]
}

plotly_legendonly_items <- function(pl) {
  out <- plotly_legend_items(pl)
  out[out$visible == "legendonly", , drop = FALSE]
}

plotly_layout_attrs <- function(pl) {
  # plotly::plotly_build(pl)[["x"]][["layoutAttrs"]]
  plotly::plotly_build(pl)[["x"]][["layout"]]
}

is_subplot <- function(pl) {
  check <- plotly::plotly_build(pl)[["x"]][["subplot"]]
  if (is.null(check)) {
    FALSE
  } else {
    check
  }
}
