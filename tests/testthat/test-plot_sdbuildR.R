# ============================================================================
# BASIC FUNCTIONALITY TESTS
# ============================================================================

test_that("plot() warns on empty model", {
  sfm <- sdbuildR()

  expect_warning(plot(sfm), "Model contains no variables")
})

test_that("plot() method exists for sdbuildR objects", {
  sfm <- sdbuildR()

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
    "Empty"
  )
  expect_error(
    plot(sfm, vars = c("susceptible", "NonExistentVar")),
    "NonExistentVar.*not.*variable"
  )
})

# ============================================================================
# VISUAL REGRESSION TESTS (vdiffr)
# ============================================================================

test_that("plot() creates diagram for SIR template", {
  skip_on_os("mac")

  sfm <- sdbuildR("SIR")

  vdiffr::expect_doppelganger(
    "sdbuildR-SIR-model-diagram",
    plot(sfm)
  )
})

test_that("plot() creates diagram for simple single-stock model", {
  skip_on_os("mac")

  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock", label = "Population")
  sfm2 <- update(sfm1, "Flow1", type = "flow", label = "Birth", from = "Stock1")

  vdiffr::expect_doppelganger(
    "sdbuildR-simple-stock-flow",
    plot(sfm2)
  )
})

test_that("plot() creates diagram with auxiliary variables and dependencies", {
  skip_on_os("mac")

  sfm <- sdbuildR()
  sfm1 <- update(sfm, "S", type = "stock")
  sfm2 <- update(sfm1, "I", type = "stock")
  sfm3 <- update(sfm2, "infection_rate", type = "aux", eqn = "S * I * 0.001")

  vdiffr::expect_doppelganger(
    "sdbuildR-diagram-with-dependencies",
    plot(sfm3)
  )
})

test_that("plot() with show_dependencies = FALSE hides dependency arrows", {
  skip_on_os("mac")

  sfm <- sdbuildR()
  sfm1 <- update(sfm, "S", type = "stock")
  sfm2 <- update(sfm1, "aux1", type = "aux", eqn = "S * 2")

  vdiffr::expect_doppelganger(
    "sdbuildR-no-dependencies",
    plot(sfm2, show_dependencies = FALSE)
  )
})

test_that("plot() with show_constants = TRUE displays constants", {
  skip_on_os("mac")

  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- update(sfm1, "const1", type = "constant", eqn = "5")

  vdiffr::expect_doppelganger(
    "sdbuildR-with-constants",
    plot(sfm2, show_constants = TRUE)
  )
})

test_that("plot() with show_constants = FALSE hides constants", {
  skip_on_os("mac")

  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- update(sfm1, "const1", type = "constant", eqn = "5")

  vdiffr::expect_doppelganger(
    "sdbuildR-without-constants",
    plot(sfm2, show_constants = FALSE)
  )
})

test_that("plot() with show_aux = FALSE hides auxiliary variables", {
  skip_on_os("mac")

  sfm <- sdbuildR("SIR")

  vdiffr::expect_doppelganger(
    "sdbuildR-no-auxiliaries",
    plot(sfm, show_aux = FALSE)
  )
})

test_that("plot() filters variables correctly", {
  skip_on_os("mac")

  sfm <- sdbuildR("SIR")

  vdiffr::expect_doppelganger(
    "sdbuildR-filtered-variables",
    plot(sfm, vars = c("susceptible", "infected"))
  )

  vdiffr::expect_doppelganger(
    "sdbuildR-single-variable-filter",
    plot(sfm, vars = "susceptible")
  )
})

test_that("plot() applies custom stock color", {
  skip_on_os("mac")

  sfm <- sdbuildR("SIR")

  vdiffr::expect_doppelganger(
    "sdbuildR-custom-stock-color",
    plot(sfm, stock_col = "#FF6B6B")
  )
})

test_that("plot() applies custom flow color", {
  skip_on_os("mac")

  sfm <- sdbuildR("SIR")

  vdiffr::expect_doppelganger(
    "sdbuildR-custom-flow-color",
    plot(sfm, flow_col = "#4ECDC4")
  )
})

test_that("plot() applies custom dependency color", {
  skip_on_os("mac")

  sfm <- sdbuildR("SIR")

  vdiffr::expect_doppelganger(
    "sdbuildR-custom-dependency-color",
    plot(sfm, dependency_col = "#FFE66D")
  )
})

test_that("plot() with custom font size", {
  skip_on_os("mac")

  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock", label = "Population")

  vdiffr::expect_doppelganger(
    "sdbuildR-large-font",
    plot(sfm1, font_size = 8)
  )
})

test_that("plot() with custom wrap width", {
  skip_on_os("mac")

  sfm <- sdbuildR()
  sfm1 <- update(sfm, "VeryLongStockNameThatShouldWrap",
    type = "stock", label = "Very Long Stock Name That Should Wrap"
  )

  vdiffr::expect_doppelganger(
    "sdbuildR-wrap-width-small",
    plot(sfm1, wrap_width = 10)
  )
})

test_that("plot() with format_label = FALSE preserves original labels", {
  skip_on_os("mac")

  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock_1", type = "stock", label = "Stock_1")

  vdiffr::expect_doppelganger(
    "sdbuildR-format-label-false",
    plot(sfm1, format_label = FALSE)
  )
})

test_that("plot() with format_label = TRUE removes underscores", {
  skip_on_os("mac")

  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock_1", type = "stock", label = "Stock_1")

  vdiffr::expect_doppelganger(
    "sdbuildR-format-label-true",
    plot(sfm1, format_label = TRUE)
  )
})
