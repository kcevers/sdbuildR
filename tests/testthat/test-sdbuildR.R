# Tests for basic sdbuildR structure

test_that("sdbuildR() creates an empty model", {
  sfm <- sdbuildR()
  expect_s3_class(sfm, "sdbuildR")
  expect_type(sfm, "list")
})


test_that("sdbuildR() has required top-level components", {
  sfm <- sdbuildR()
  expect_true("meta" %in% names(sfm))
  expect_true("sim_specs" %in% names(sfm))
  expect_true("variables" %in% names(sfm))
})


test_that("sdbuildR() initializes empty data frames with columns", {
  sfm <- sdbuildR()

  expect_s3_class(sfm$variables, "data.frame")
  expect_equal(nrow(sfm$variables), 0)
  expect_true(all(c("name", "type", "eqn") %in% names(sfm$variables)))
})


test_that("sdbuildR() creates default meta and sim_specs", {
  sfm <- sdbuildR()

  expect_type(sfm$meta, "list")
  expect_true(all(c("name", "author", "created", "version") %in% names(sfm$meta)))

  expect_type(sfm$sim_specs, "list")
  expect_true(all(c("start", "stop", "dt", "time_units") %in% names(sfm$sim_specs)))
  expect_true(sfm$sim_specs$start < sfm$sim_specs$stop)
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
