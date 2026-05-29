# ============================================================================
# BASIC FUNCTIONALITY TESTS
# ============================================================================

test_that("plot() warns on empty model", {
  sfm <- sdbuildR()

  expect_warning(plot(sfm), "Model contains no variables")
})

test_that("plot() method exists for sdbuildR objects", {
  # Check that plot method exists
  expect_true("plot.sdbuildR" %in% methods("plot"))
})

test_that("plot() returns DiagrammeR grViz object", {
  sfm <- sdbuildR("SIR")

  result <- plot(sfm)

  # Should return an htmlwidget (DiagrammeR graph)
  expect_true("grViz" %in% class(result))
  expect_true("htmlwidget" %in% class(result))
})

# ============================================================================
# PARAMETER VALIDATION TESTS
# ============================================================================

test_that("plot() checks vars argument", {
  sfm <- sdbuildR("SIR")

  expect_error(
    plot(sfm, vars = 123),
    "vars"
  )
  expect_error(
    plot(sfm, vars = character(0)),
    "Invalid"
  )
  expect_error(
    plot(sfm, vars = c("susceptible", "NonExistentVar")),
    "NonExistentVar.*not.*variable"
  )
})

# ============================================================================
# VISUAL REGRESSION TESTS (expect_snapshot_plot)
# ============================================================================

test_that("plot() creates diagram for SIR template", {
  sfm <- sdbuildR("SIR")

  expect_snapshot_plot(
    "sdbuildR-SIR-model-diagram",
    plot(sfm)
  )
})

test_that("plot() creates diagram for simple single-stock model", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock", label = "Population")
  sfm2 <- update(sfm1, "Flow1", type = "flow", label = "Birth", from = "Stock1")

  expect_snapshot_plot("sdbuildR-simple-stock-flow", plot(sfm2))
})

test_that("plot() creates diagram with auxiliary variables and dependencies", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "S", type = "stock")
  sfm2 <- update(sfm1, "I", type = "stock")
  sfm3 <- update(sfm2, "infection_rate", type = "aux", eqn = "S * I * 0.001")

  expect_snapshot_plot("sdbuildR-diagram-with-dependencies", plot(sfm3))
})

test_that("plot() with show_dependencies = FALSE hides dependency arrows", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "S", type = "stock")
  sfm2 <- update(sfm1, "aux1", type = "aux", eqn = "S * 2")

  expect_snapshot_plot("sdbuildR-no-dependencies", plot(sfm2, show_dependencies = FALSE))
})

test_that("plot() with show_constants = TRUE displays constants", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- update(sfm1, "const1", type = "constant", eqn = "5")

  expect_snapshot_plot("sdbuildR-with-constants", plot(sfm2, show_constants = TRUE))
})

test_that("plot() with show_constants = FALSE hides constants", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- update(sfm1, "const1", type = "constant", eqn = "5")

  expect_snapshot_plot("sdbuildR-without-constants", plot(sfm2, show_constants = FALSE))
})

test_that("plot() with show_aux = FALSE hides auxiliary variables", {
  sfm <- sdbuildR("SIR")

  expect_snapshot_plot("sdbuildR-no-auxiliaries", plot(sfm, show_aux = FALSE))
})

test_that("plot() filters variables correctly", {
  sfm <- sdbuildR("SIR")

  expect_snapshot_plot(
    "sdbuildR-filtered-variables",
    plot(sfm, vars = c("susceptible", "infected"))
  )

  expect_snapshot_plot(
    "sdbuildR-single-variable-filter",
    plot(sfm, vars = "susceptible")
  )
})

test_that("plot() applies custom stock color", {
  sfm <- sdbuildR("SIR")

  expect_snapshot_plot("sdbuildR-custom-stock-color", plot(sfm, stock_col = "#FF6B6B"))
})

test_that("plot() applies custom flow color", {
  sfm <- sdbuildR("SIR")

  expect_snapshot_plot("sdbuildR-custom-flow-color", plot(sfm, flow_col = "#4ECDC4"))
})

test_that("plot() applies custom dependency color", {
  sfm <- sdbuildR("SIR")

  expect_snapshot_plot("sdbuildR-custom-dependency-color", plot(sfm, dependency_col = "#FFE66D"))
})

test_that("plot() with custom font size", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock", label = "Population")

  expect_snapshot_plot("sdbuildR-large-font", plot(sfm1, font_size = 8))
})

test_that("plot() with custom wrap width", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "VeryLongStockNameThatShouldWrap",
    type = "stock", label = "Very Long Stock Name That Should Wrap"
  )

  expect_snapshot_plot("sdbuildR-wrap-width-small", plot(sfm1, wrap_width = 10))
})

test_that("plot() with format_label = FALSE preserves original labels", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock_1", type = "stock", label = "Stock_1")

  expect_snapshot_plot("sdbuildR-format-label-false", plot(sfm1, format_label = FALSE))
})

test_that("plot() with format_label = TRUE removes underscores", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock_1", type = "stock", label = "Stock_1")

  expect_snapshot_plot("sdbuildR-format-label-true", plot(sfm1, format_label = TRUE))
})
