test_that("summary() detects model with no stocks", {
  sfm <- sdbuildR() |> update("aux1", type = "aux", eqn = "5")

  result <- summary(sfm)
  expect_s3_class(result, "summary_sdbuildR")
  expect_equal(result$no_stocks$problem, "error")
})

test_that("summary() detects stocks not connected to flows", {
  sfm <- sdbuildR() |>
    update("Stock1", type = "stock") |>
    update("Stock2", type = "stock")

  result <- summary(sfm)
  expect_equal(result$no_flows$problem, "warning")
  expect_equal(result$disconnected_stocks$problem, "warning")
  expect_true("Stock1" %in% result$disconnected_stocks$stocks)
  expect_true("Stock2" %in% result$disconnected_stocks$stocks)
  expect_snapshot(print(result))
})

test_that("summary() detects flows not connected to any stock", {
  sfm <- sdbuildR() |> update("Orphan_Flow", type = "flow")

  result <- summary(sfm)
  expect_equal(result$disconnected_flows$problem, "error")
  expect_true("Orphan_Flow" %in% result$disconnected_flows$flows)
})

test_that("summary() detects flows connected to non-existent stocks", {
  sfm <- sdbuildR() |> update("Bad_Flow", type = "flow", to = "NonExistentStock")

  result <- summary(sfm)
  expect_equal(result$bad_flow_connections$problem, "error")
  expect_true("Bad_Flow" %in% result$bad_flow_connections$flows)
})

test_that("summary() warns about zero equations", {
  sfm <- sdbuildR() |>
    update("Stock1", type = "stock") |>
    update("Flow1", type = "flow", from = "Stock1", eqn = "0")

  result <- summary(sfm)
  expect_equal(result$zero_equations$problem, "warning")
  expect_true("Flow1" %in% result$zero_equations$variables)
  expect_snapshot(print(result))
})

test_that("summary() detects undefined variable references", {
  sfm <- sdbuildR() |>
    update("Stock1", type = "stock") |>
    update("Flow1", type = "flow", from = "Stock1", eqn = "undefined_var * 2")

  result <- summary(sfm)
  expect_equal(result$undefined_vars$problem, "error")
  undefined_all <- unlist(lapply(result$undefined_vars$refs, `[[`, "undefined"))
  expect_true("undefined_var" %in% undefined_all)
})

test_that("summary() detects circular dependencies in static variables", {
  suppressWarnings(sfm <- sdbuildR() |>
    update("const1", type = "constant", eqn = "const2") |>
    update("const2", type = "constant", eqn = "const1") |>
    update("Stock1", type = "stock") |>
    update("Flow1", type = "flow", from = "Stock1", eqn = "0"))

  result <- summary(sfm)
  expect_equal(result$circular_static$problem, "error")
})

test_that("summary() detects circular dependencies in dynamic variables", {
  suppressWarnings(sfm <- sdbuildR() |>
    update("Stock1", type = "stock") |>
    update("aux1", type = "aux", eqn = "aux2") |>
    update("aux2", type = "aux", eqn = "aux1") |>
    update("Flow1", type = "flow", from = "Stock1", eqn = "0"))

  result <- summary(sfm)
  expect_equal(result$circular_dynamic$problem, "error")
})

test_that("summary() returns summary_sdbuildR with all 10 checks", {
  sfm <- sdbuildR() |> update("Stock1", type = "stock")

  result <- summary(sfm)
  expect_s3_class(result, "summary_sdbuildR")
  expected_names <- c(
    "no_stocks", "no_flows", "disconnected_stocks",
    "disconnected_flows", "bad_flow_connections",
    "zero_equations", "undefined_vars",
    "circular_static", "circular_dynamic",
    "unit_test_refs"
  )
  expect_true(all(expected_names %in% names(result)))
  expect_equal(length(result), length(expected_names))
})

test_that("summary() passes with valid model", {
  sfm <- sdbuildR() |>
    update("Stock1", type = "stock", eqn = "100") |>
    update("Flow1", type = "flow", from = "Stock1", eqn = "Stock1 * 0.1")

  result <- summary(sfm)
  expect_s3_class(result, "summary_sdbuildR")
  expect_true(all(vapply(result, `[[`, character(1), "problem") == "none"))
})

test_that("print.summary_sdbuildR() shows header and 'No problems detected!' for valid model", {
  sfm <- sdbuildR("predator_prey")
  result <- summary(sfm)
  expect_snapshot(print(result))
})

test_that("summary() does not flag func argument names as undefined", {
  sfm <- sdbuildR() |>
    custom_func("double", eqn = "function(x) x * 2") |>
    update("Stock1", type = "stock", eqn = "100") |>
    update("Flow1", type = "flow", from = "Stock1", eqn = "double(Stock1)")

  result <- summary(sfm)
  expect_equal(result$undefined_vars$problem, "none")
})

test_that("summary() does not flag func argument names with defaults as undefined", {
  sfm <- sdbuildR() |>
    custom_func("scale", eqn = "function(x, factor = 10) x * factor") |>
    update("Stock1", type = "stock", eqn = "100") |>
    update("Flow1", type = "flow", from = "Stock1", eqn = "scale(Stock1)")

  result <- summary(sfm)
  expect_equal(result$undefined_vars$problem, "none")
})


# ==============================================================================
# unit_test_refs check
# ==============================================================================

test_that("summary() is clean for model with valid unit tests", {
  sfm <- sdbuildR("SIR") |>
    unit_test(label = "S non-neg", expr = all(susceptible >= 0))

  result <- summary(sfm)
  expect_equal(result[["unit_test_refs"]][["problem"]], "none")
})


test_that("summary() warns when unit test eqn reference undefined variable", {
  sfm <- make_verifiable_sfm() |>
    unit_test(label = "valid", expr = all(S >= 0) & all(drain > 0))

  # Delete one of the variables used in expr
  expect_warning(sfm <- discard(sfm, "drain"), "still reference")

  result <- summary(sfm)
  expect_equal(result[["unit_test_refs"]][["problem"]], "warning")
  expect_true(any(vapply(
    result[["unit_test_refs"]][["refs"]],
    function(r) "drain" %in% r[["undefined"]],
    logical(1)
  )))
  expect_snapshot(print(result))
})
