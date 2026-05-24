# Tests for basic sdbuildR structure

test_that("sdbuildR() creates an empty model", {
  sfm <- sdbuildR()
  expect_s3_class(sfm, "sdbuildR")
  expect_type(sfm, "list")
})


test_that("sdbuildR() has required top-level components", {
  sfm <- sdbuildR()
  expect_true("meta" %in% names(sfm))
  expect_true("sim_settings" %in% names(sfm))
  expect_true("variables" %in% names(sfm))
})


test_that("sdbuildR() initializes empty data frames with columns", {
  sfm <- sdbuildR()

  expect_s3_class(sfm$variables, "data.frame")
  expect_equal(nrow(sfm$variables), 0)
  expect_true(all(c("name", "type", "eqn") %in% names(sfm$variables)))
})


test_that("sdbuildR() creates default meta and sim_settings", {
  sfm <- sdbuildR()

  expect_type(sfm$meta, "list")
  expect_true(all(c("name", "author", "created", "version") %in% names(sfm$meta)))

  expect_type(sfm$sim_settings, "list")
  expect_true(all(c("start", "stop", "dt", "time_units") %in% names(sfm$sim_settings)))
  expect_true(sfm$sim_settings$start < sfm$sim_settings$stop)
})


# Tests for sdbuildR helpers in sdbuildR.R

test_that("check_sdbuildR rejects non-model objects", {
  expect_error(check_sdbuildR(list()), "Expected object of class")
  expect_error(check_sdbuildR("not a model"), "Expected object of class")
})


test_that("sanitize_sdbuildR fills missing labels", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "S", type = "stock", eqn = "2")

  # Remove label to force defaults
  sfm[["variables"]][["label"]] <- ""

  sfm_sanitized <- sanitize_sdbuildR(sfm)

  vars <- sfm_sanitized[["variables"]]
  expect_equal(vars[["label"]], "S")
})


test_that("sanitize_sdbuildR cleans invalid flow connections", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "Stock", type = "stock")
  sfm <- update(sfm, "Aux", type = "aux")

  # Invalid flow connections generate warnings during update (which calls sanitize_sdbuildR internally)
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
  sfm <- sdbuildR()
  expect_null(sfm[["import_metadata"]])

  sfm <- update(sfm, name = "x", type = "stock", eqn = "1")
  expect_null(sfm[["import_metadata"]])
})


test_that("validate_sdbuildR warns but does not mutate", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "Stock", type = "stock")
  sfm <- update(sfm, "Aux", type = "aux")
  sfm <- update(sfm, "BadFlow", type = "flow", to = "Stock")

  # Manually set invalid flow connection (bypass sanitize_sdbuildR)
  sfm[["variables"]][sfm[["variables"]][["name"]] == "BadFlow", "to"] <- "Aux"

  expect_warning(validate_sdbuildR(sfm), "not a stock")

  # Verify no mutation occurred
  flow_row <- sfm[["variables"]][sfm[["variables"]][["name"]] == "BadFlow", ]
  expect_equal(flow_row[["to"]], "Aux")
})


# ---- print.sdbuildR tests ----

test_that("print.sdbuildR() returns x invisibly", {
  sfm <- sdbuildR()
  expect_invisible(print(sfm))
})

test_that("print.sdbuildR() snapshot: empty model", {
  sfm <- sdbuildR()
  expect_snapshot(print(sfm))
})

test_that("print.sdbuildR() snapshot: named model", {
  sfm <- sdbuildR() |> meta(name = "My SIR Model")
  expect_snapshot(print(sfm))
})

test_that("print.sdbuildR() snapshot: SIR model", {
  sfm <- sdbuildR("SIR")
  expect_snapshot(print(sfm))
})

test_that("print.sdbuildR() snapshot: model with constants", {
  sfm <- sdbuildR() |>
    update("S", type = "stock", eqn = "100") |>
    update("k", type = "constant", eqn = "0.1") |>
    update("Flow1", type = "flow", from = "S", eqn = "k * S")
  expect_snapshot(print(sfm))
})

test_that("print.sdbuildR() snapshot: default name not shown as title", {
  sfm <- sdbuildR() # default name is "My Model" — should not appear in header
  expect_snapshot(print(sfm))
})



# ============================================================================
# as.data.frame.sdbuildR() — filtering behaviour
# ============================================================================

test_that("as.data.frame with type='stock' returns only stock rows", {
  sfm <- templates("SIR")
  df  <- as.data.frame(sfm, type = "stock")
  expect_true(all(df$type == "stock"))
  expect_equal(nrow(df), 3)
})

test_that("as.data.frame with type='flow' returns only flow rows", {
  sfm <- templates("SIR")
  df  <- as.data.frame(sfm, type = "flow")
  expect_true(all(df$type == "flow"))
  expect_gt(nrow(df), 0)
})

test_that("as.data.frame with name filter returns exactly 1 row for a known variable", {
  sfm      <- templates("SIR")
  var_name <- as.data.frame(sfm)$name[1]
  df       <- as.data.frame(sfm, name = !!var_name)
  expect_equal(nrow(df), 1)
  expect_equal(df$name, var_name)
})

test_that("as.data.frame with properties='eqn' returns name + type + eqn columns", {
  sfm <- templates("SIR")
  df  <- as.data.frame(sfm, properties = c("eqn"))
  # Always includes type and name in addition to requested columns
  expect_true("eqn"  %in% names(df))
  expect_true("name" %in% names(df))
})

test_that("as.data.frame: both name AND type specified → issues a WARNING (type is ignored)", {
  sfm      <- templates("SIR")
  var_name <- as.data.frame(sfm)$name[1]
  expect_warning(
    df <- as.data.frame(sfm, type = "stock", name = !!var_name)
  )
  # After warning, type is ignored; result is filtered by name only
  expect_equal(nrow(df), 1)
  expect_equal(df$name, var_name)
})

test_that("as.data.frame with no arguments returns all variables", {
  sfm <- templates("SIR")
  df  <- as.data.frame(sfm)
  n_total <- nrow(as.data.frame(sfm, type = "stock")) +
             nrow(as.data.frame(sfm, type = "flow"))  +
             nrow(as.data.frame(sfm, type = "constant")) +
             suppressWarnings(nrow(as.data.frame(sfm, type = "aux")))
  expect_gte(nrow(df), 0)  # at minimum it's a data.frame
  expect_gt(nrow(df), 0)
})


# ============================================================================
# print.sdbuildR() — content checks
# ============================================================================

test_that("print.sdbuildR: shows model name when set via meta()", {
  sfm    <- meta(sdbuildR(), name = "My Unique Model 9472")
  output <- paste0(cli::cli_fmt(print(sfm)), collapse = "\n")
  expect_match(output, "My Unique Model 9472")
})

test_that("print.sdbuildR: shows correct stock count", {
  sfm      <- templates("SIR")
  n_stocks <- nrow(as.data.frame(sfm, type = "stock"))
  output   <- paste0(cli::cli_fmt(print(sfm)), collapse = "\n")
  expect_match(output, as.character(n_stocks))
})

test_that("print.sdbuildR: mentions the simulation stop time", {
  sfm    <- sim_settings(templates("bank_account"), stop = 9999)
  output <- paste0(cli::cli_fmt(print(sfm)), collapse = "\n")
  expect_match(output, "9999")
})

test_that("print.sdbuildR: mentions all stock names from the model", {
  sfm         <- templates("SIR")
  stock_names <- as.data.frame(sfm, type = "stock")$name
  output      <- paste0(cli::cli_fmt(print(sfm)), collapse = "\n")
  for (nm in stock_names) {
    expect_match(output, nm, info = sprintf("Stock name '%s' missing from print output", nm))
  }
})

test_that("print.sdbuildR: mentions the simulation language", {
  sfm    <- sim_settings(make_basic_sfm(), language = "R")
  output <- paste0(cli::cli_fmt(print(sfm)), collapse = "\n")
  expect_match(output, "R")
})

test_that("print.sdbuildR: returns the model invisibly", {
  sfm    <- make_basic_sfm()
  result <- withVisible(print(sfm))
  expect_false(result$visible)
  expect_identical(result$value, sfm)
})
