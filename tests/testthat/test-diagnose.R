test_that("diagnose() detects model with no stocks", {
  sfm <- sdbuildR() |> build("aux1", type = "aux", eqn = "5")

  result <- diagnose(sfm)
  expect_s3_class(result, "diagnose_sdbuildR")
  expect_equal(result$no_stocks$problem, "error")
})

test_that("diagnose() detects stocks not connected to flows", {
  sfm <- sdbuildR() |>
    build("Stock1", type = "stock") |>
    build("Stock2", type = "stock")

  result <- diagnose(sfm)
  expect_equal(result$no_flows$problem,            "warning")
  expect_equal(result$disconnected_stocks$problem, "warning")
  expect_true("Stock1" %in% result$disconnected_stocks$stocks)
  expect_true("Stock2" %in% result$disconnected_stocks$stocks)
})

test_that("diagnose() detects flows not connected to any stock", {
  sfm <- sdbuildR() |> build("Orphan_Flow", type = "flow")

  result <- diagnose(sfm)
  expect_equal(result$disconnected_flows$problem, "error")
  expect_true("Orphan_Flow" %in% result$disconnected_flows$flows)
})

test_that("diagnose() detects flows connected to non-existent stocks", {
  sfm <- sdbuildR() |> build("Bad_Flow", type = "flow", to = "NonExistentStock")

  result <- diagnose(sfm)
  expect_equal(result$bad_flow_connections$problem, "error")
  expect_true("Bad_Flow" %in% result$bad_flow_connections$flows)
})

test_that("diagnose() warns about zero equations", {
  sfm <- sdbuildR() |>
    build("Stock1", type = "stock") |>
    build("Flow1",  type = "flow", from = "Stock1", eqn = "0")

  result <- diagnose(sfm)
  expect_equal(result$zero_equations$problem, "warning")
  expect_true("Flow1" %in% result$zero_equations$variables)
})

test_that("diagnose() detects undefined variable references", {
  sfm <- sdbuildR() |>
    build("Stock1", type = "stock") |>
    build("Flow1",  type = "flow", from = "Stock1", eqn = "undefined_var * 2")

  result <- diagnose(sfm)
  expect_equal(result$undefined_vars$problem, "error")
  undefined_all <- unlist(lapply(result$undefined_vars$refs, `[[`, "undefined"))
  expect_true("undefined_var" %in% undefined_all)
})

test_that("diagnose() detects circular dependencies in static variables", {
  suppressWarnings(sfm <- sdbuildR() |>
    build("const1", type = "constant", eqn = "const2") |>
    build("const2", type = "constant", eqn = "const1") |>
    build("Stock1", type = "stock") |>
    build("Flow1",  type = "flow", from = "Stock1", eqn = "0"))

  result <- diagnose(sfm)
  expect_equal(result$circular_static$problem, "error")
})

test_that("diagnose() detects circular dependencies in dynamic variables", {
  suppressWarnings(sfm <- sdbuildR() |>
    build("Stock1", type = "stock") |>
    build("aux1",   type = "aux", eqn = "aux2") |>
    build("aux2", type = "aux", eqn = "aux1") |>
    build("Flow1",  type = "flow", from = "Stock1", eqn = "0"))

  result <- diagnose(sfm)
  expect_equal(result$circular_dynamic$problem, "error")
})

test_that("diagnose() returns diagnose_sdbuildR with all 11 checks", {
  sfm <- sdbuildR() |> build("Stock1", type = "stock")

  result <- diagnose(sfm)
  expect_s3_class(result, "diagnose_sdbuildR")
  expected_names <- c(
    "no_stocks", "no_flows", "disconnected_stocks",
    "disconnected_flows", "bad_flow_connections",
    "zero_equations", "undefined_vars",
    "circular_static", "circular_dynamic", "undefined_units"
  )
  expect_true(all(expected_names %in% names(result)))
})

test_that("diagnose() passes with valid model", {
  sfm <- sdbuildR() |>
    build("Stock1", type = "stock", eqn = "100") |>
    build("Flow1",  type = "flow", from = "Stock1", eqn = "Stock1 * 0.1")

  result <- diagnose(sfm)
  expect_s3_class(result, "diagnose_sdbuildR")
  expect_true(all(vapply(result, `[[`, character(1), "problem") == "none"))
})

test_that("print.diagnose_sdbuildR() shows 'No problems detected!' for valid model", {
  sfm <- sdbuildR("predator_prey") # Valid model with stocks and flows
  result <- diagnose(sfm)
  expect_snapshot(print(result))
})

test_that("diagnose() does not flag func argument names as undefined", {
  sfm <- sdbuildR() |>
    custom_func("double", eqn = "function(x) x * 2") |>
    build("Stock1", type = "stock", eqn = "100") |>
    build("Flow1", type = "flow", from = "Stock1", eqn = "double(Stock1)")

  result <- diagnose(sfm)
  expect_equal(result$undefined_vars$problem, "none")
})

test_that("diagnose() does not flag func argument names with defaults as undefined", {
  sfm <- sdbuildR() |>
    custom_func("scale", eqn = "function(x, factor = 10) x * factor") |>
    build("Stock1", type = "stock", eqn = "100") |>
    build("Flow1", type = "flow", from = "Stock1", eqn = "scale(Stock1)")

  result <- diagnose(sfm)
  expect_equal(result$undefined_vars$problem, "none")
})

