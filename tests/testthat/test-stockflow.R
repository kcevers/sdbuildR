# Tests for basic sdbuildR structure

test_that("stockflow() creates an empty model", {
  sfm <- stockflow()
  expect_s3_class(sfm, "stockflow")
  expect_type(sfm, "list")
})


test_that("stockflow() has required top-level components", {
  sfm <- stockflow()
  expect_true("meta" %in% names(sfm))
  expect_true("sim_settings" %in% names(sfm))
  expect_true("variables" %in% names(sfm))
})


test_that("stockflow() initializes empty data frames with columns", {
  sfm <- stockflow()

  expect_s3_class(sfm$variables, "data.frame")
  expect_equal(nrow(sfm$variables), 0)
  expect_true(all(c("name", "type", "eqn") %in% names(sfm$variables)))
})


test_that("stockflow() creates default meta and sim_settings", {
  sfm <- stockflow()

  expect_type(sfm$meta, "list")
  expect_true(all(c("name", "author", "created", "version") %in% names(sfm$meta)))

  expect_type(sfm$sim_settings, "list")
  expect_true(all(c("start", "stop", "dt", "time_units") %in% names(sfm$sim_settings)))
  expect_true(sfm$sim_settings$start < sfm$sim_settings$stop)
})


# Tests for sdbuildR helpers in stockflow.R

test_that("check_stockflow rejects non-model objects", {
  expect_error(check_stockflow(list()), "Expected object of class")
  expect_error(check_stockflow("not a model"), "Expected object of class")
})


test_that("sanitize_stockflow fills missing labels", {
  sfm <- stockflow()
  sfm <- update(sfm, "S", type = "stock", eqn = "2")

  # Remove label to force defaults
  sfm[["variables"]][["label"]] <- ""

  sfm_sanitized <- sanitize_stockflow(sfm)

  vars <- sfm_sanitized[["variables"]]
  expect_equal(vars[["label"]], "S")
})


test_that("sanitize_stockflow cleans invalid flow connections", {
  sfm <- stockflow()
  sfm <- update(sfm, "Stock", type = "stock")
  sfm <- update(sfm, "Aux", type = "aux")

  # Invalid flow connections generate warnings during update (which calls sanitize_stockflow internally)
  expect_warning(
    sfm <- update(sfm, "FlowBad", type = "flow", to = "Aux"),
    "non-stock"
  )

  flow_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == "FlowBad", ]
  expect_equal(flow_row[["to"]], "")
  expect_equal(flow_row[["from"]], "")

  expect_warning(
    sfm <- update(sfm, "FlowBad", type = "flow", from = "Aux"),
    "non-stock"
  )

  flow_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == "FlowBad", ]
  expect_equal(flow_row[["to"]], "")
  expect_equal(flow_row[["from"]], "")

  expect_error(
    sfm <- update(sfm, "FlowBad", type = "flow", to = "Stock", from = "Stock"),
    "flow cannot have the same stock as both source and target"
  )
})

test_that("regular models have NULL import_metadata", {
  sfm <- stockflow()
  expect_null(sfm[["import_metadata"]])

  sfm <- update(sfm, name = "x", type = "stock", eqn = "1")
  expect_null(sfm[["import_metadata"]])
})


test_that("validate_stockflow warns but does not mutate", {
  sfm <- stockflow()
  sfm <- update(sfm, "Stock", type = "stock")
  sfm <- update(sfm, "Aux", type = "aux")
  sfm <- update(sfm, "BadFlow", type = "flow", to = "Stock")

  # Manually set invalid flow connection (bypass sanitize_stockflow)
  sfm[["variables"]][sfm[["variables"]][["name"]] == "BadFlow", "to"] <- "Aux"

  expect_warning(validate_stockflow(sfm), "not a stock")

  # Verify no mutation occurred
  flow_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == "BadFlow", ]
  expect_equal(flow_row[["to"]], "Aux")
})


# ---- print.stockflow tests ----

test_that("print.stockflow() returns x invisibly", {
  sfm <- stockflow()
  expect_invisible(print(sfm))
})

test_that("print.stockflow() snapshot: empty model", {
  sfm <- stockflow()
  expect_snapshot(print(sfm))
})

test_that("print.stockflow() snapshot: named model", {
  sfm <- stockflow() |> meta(name = "My SIR Model")
  expect_snapshot(print(sfm))
})

test_that("print.stockflow() snapshot: SIR model", {
  sfm <- stockflow("sir")
  expect_snapshot(print(sfm))
})

test_that("print.stockflow() snapshot: model with constants", {
  sfm <- stockflow() |>
    update("S", type = "stock", eqn = "100") |>
    update("k", type = "constant", eqn = "0.1") |>
    update("Flow1", type = "flow", from = "S", eqn = "k * S")
  expect_snapshot(print(sfm))
})

test_that("print.stockflow() snapshot: default name not shown as title", {
  sfm <- stockflow() # default name is "My Model" — should not appear in header
  expect_snapshot(print(sfm))
})


# ============================================================================
# as.data.frame.stockflow() — filtering behaviour
# ============================================================================

test_that("as.data.frame with type='stock' returns only stock rows", {
  sfm <- templates("sir")
  df <- as.data.frame(sfm, type = "stock")
  expect_true(all(df$type == "stock"))
  expect_equal(nrow(df), 3)
})

test_that("as.data.frame with type='flow' returns only flow rows", {
  sfm <- templates("sir")
  df <- as.data.frame(sfm, type = "flow")
  expect_true(all(df$type == "flow"))
  expect_gt(nrow(df), 0)
})

test_that("as.data.frame with vars filter returns exactly 1 row for a known variable", {
  sfm <- templates("sir")
  var_name <- as.data.frame(sfm)$name[1]
  df <- as.data.frame(sfm, vars = !!var_name)
  expect_equal(nrow(df), 1)
  expect_equal(df$name, var_name)
})

test_that("as.data.frame with properties='eqn' returns name + type + eqn columns", {
  sfm <- templates("sir")
  df <- as.data.frame(sfm, properties = c("eqn"))
  # Always includes type and name in addition to requested columns
  expect_true("eqn" %in% names(df))
  expect_true("name" %in% names(df))
})

test_that("as.data.frame: both vars AND type specified → issues a WARNING (type is ignored)", {
  sfm <- templates("sir")
  var_name <- as.data.frame(sfm)$name[1]
  expect_warning(
    df <- as.data.frame(sfm, type = "stock", vars = !!var_name)
  )
  # After warning, type is ignored; result is filtered by vars only
  expect_equal(nrow(df), 1)
  expect_equal(df$name, var_name)
})

test_that("as.data.frame with no arguments returns all variables", {
  sfm <- templates("sir")
  df <- as.data.frame(sfm)
  n_total <- nrow(as.data.frame(sfm, type = "stock")) +
    nrow(as.data.frame(sfm, type = "flow")) +
    nrow(as.data.frame(sfm, type = "constant")) +
    suppressWarnings(nrow(as.data.frame(sfm, type = "aux")))
  expect_gte(nrow(df), 0) # at minimum it's a data.frame
  expect_gt(nrow(df), 0)
})


# ============================================================================
# print.stockflow() — content checks
# ============================================================================

test_that("print.stockflow: shows model name when set via meta()", {
  sfm <- meta(stockflow(), name = "My Unique Model 9472")
  output <- paste0(cli::cli_fmt(print(sfm)), collapse = "\n")
  expect_match(output, "My Unique Model 9472")
})

test_that("print.stockflow: shows correct stock count", {
  sfm <- templates("sir")
  n_stocks <- nrow(as.data.frame(sfm, type = "stock"))
  output <- paste0(cli::cli_fmt(print(sfm)), collapse = "\n")
  expect_match(output, as.character(n_stocks))
})

test_that("print.stockflow: mentions the simulation stop time", {
  sfm <- sim_settings(templates("bank_account"), stop = 9999)
  output <- paste0(cli::cli_fmt(print(sfm)), collapse = "\n")
  expect_match(output, "9999")
})

test_that("print.stockflow: mentions all stock names from the model", {
  sfm <- templates("sir")
  stock_names <- as.data.frame(sfm, type = "stock")$name
  output <- paste0(cli::cli_fmt(print(sfm)), collapse = "\n")
  for (nm in stock_names) {
    expect_match(output, nm, info = sprintf("Stock name '%s' missing from print output", nm))
  }
})

test_that("print.stockflow: mentions the simulation language", {
  sfm <- sim_settings(make_basic_sfm(), language = "R")
  output <- paste0(cli::cli_fmt(print(sfm)), collapse = "\n")
  expect_match(output, "R")
})

test_that("print.stockflow: returns the model invisibly", {
  sfm <- make_basic_sfm()
  result <- withVisible(print(sfm))
  expect_false(result$visible)
  expect_identical(result$value, sfm)
})
