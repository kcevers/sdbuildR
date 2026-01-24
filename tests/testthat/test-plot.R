# Tests for plotting functions

test_that("plot() warns on empty model", {
  sfm <- xmile()
  
  expect_warning(plot(sfm), "Model contains no variables")
})

test_that("plot() method exists for xmile objects", {
  sfm <- xmile()
  
  # Check that plot method exists
  expect_true("plot.sdbuildR_xmile" %in% methods("plot"))
})

test_that("plot() creates diagram for SIR template", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  
  sfm <- xmile("SIR")
  
  vdiffr::expect_doppelganger(
    "SIR-model-diagram",
    plot(sfm)
  )
})

test_that("plot() creates diagram for simple model", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock", label = "Population")
  sfm2 <- build(sfm1, "Flow1", type = "flow", label = "Birth", from = "Stock1")
  
  vdiffr::expect_doppelganger(
    "simple-model-diagram",
    plot(sfm2)
  )
})

test_that("plot() with dependencies shown", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  
  sfm <- xmile()
  sfm1 <- build(sfm, "S", type = "stock")
  sfm2 <- build(sfm1, "I", type = "stock")
  sfm3 <- build(sfm2, "infection_rate", type = "aux", eqn = "S * I * 0.001")
  
  vdiffr::expect_doppelganger(
    "diagram-with-dependencies",
    plot(sfm3)
  )
})

test_that("plot() returns graphical object", {
  sfm <- xmile("SIR")
  
  result <- plot(sfm)
  
  # Should return an htmlwidget (DiagrammeR graph)
  expect_true("htmlwidget" %in% class(result) || "grViz" %in% class(result))
})

test_that("plot() with show_dependencies = FALSE", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  
  sfm <- xmile()
  sfm1 <- build(sfm, "S", type = "stock")
  sfm2 <- build(sfm1, "aux", type = "aux", eqn = "S * 2")
  
  vdiffr::expect_doppelganger(
    "no-dependencies",
    plot(sfm2, show_dependencies = FALSE)
  )
})

test_that("plot() with show_constants = TRUE", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  
  sfm <- xmile()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- build(sfm1, "const1", type = "constant", eqn = "5")
  
  vdiffr::expect_doppelganger(
    "with-constants",
    plot(sfm2, show_constants = TRUE)
  )
})

test_that("plot() variable filtering works", {
  sfm <- xmile("SIR")
  
  vdiffr::expect_doppelganger(
    "filtered-variables",
    plot(sfm, vars = c("Susceptible", "Infected"))
  )
})

test_that("plot() with custom colors", {
  skip_on_os("mac")
  
  sfm <- xmile("SIR")

  vdiffr::expect_doppelganger(
    "custom-colors",
    plot(sfm, stock_col = "blue", flow_col = "orange", aux_col = "purple")
  )

})

