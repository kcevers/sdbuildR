test_that("plot() creates a basic plot for simulation", {
  sim <- sir_sim()
  result <- plot(sim)
  expect_plotly(result)
})

test_that("plot() method exists for simulate_sdbuildR objects", {
  expect_true("plot.simulate_sdbuildR" %in% methods("plot"))
})

# ============================================================================
# PARAMETER VALIDATION TESTS
# ============================================================================

test_that("plot() validates showlegend as logical", {
  sim <- sir_sim()

  expect_error(
    plot(sim, showlegend = "yes"),
    "showlegend"
  )
})

test_that("plot() validates vars as character vector", {
  sim <- sir_sim()

  expect_error(
    plot(sim, vars = 123),
    "vars"
  )
  expect_error(
    plot(sim, vars = character(0)),
    "Empty"
  )
})

test_that("plot() validates variable existence in simulation", {
  sim <- sir_sim()

  expect_error(
    plot(sim, vars = c("Susceptible", "NonExistent")),
    "NonExistent.*not.*variable"
  )
})

test_that("plot() validates font_family as character", {
  sim <- sir_sim()

  expect_error(
    plot(sim, font_family = 123),
    "font_family"
  )
})

test_that("plot() validates font_size as positive number", {
  sim <- sir_sim()

  expect_error(
    plot(sim, font_size = 0),
    "must be a positive number"
  )
})

test_that("plot() validates wrap_width as positive integer", {
  sim <- sir_sim()

  expect_error(
    plot(sim, wrap_width = -10),
    "must be a positive integer"
  )
})

test_that("plot() validates palette as character", {
  sim <- sir_sim()

  expect_error(
    plot(sim, palette = 123),
    "palette"
  )
})

test_that("plot() validates colors as character vector", {
  sim <- sir_sim()

  expect_error(
    plot(sim, colors = 123),
    "colors"
  )

  # SIR has 3+ variables, provide only 1 color
  expect_error(
    plot(sim, colors = "#FF0000"),
    "Insufficient colors provided"
  )
})

# ============================================================================
# VISUAL REGRESSION TESTS (vdiffr)
# ============================================================================

test_that("plot() creates standard line plot for SIR simulation", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sim <- sir_sim()

  vdiffr::expect_doppelganger(
    "sim-SIR-default",
    plot(sim)
  )
})

test_that("plot() respects showlegend", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sim <- sir_sim()

  vdiffr::expect_doppelganger(
    "sim-showlegend-true",
    plot(sim, showlegend = TRUE)
  )
  vdiffr::expect_doppelganger(
    "sim-showlegend-false",
    plot(sim, showlegend = FALSE)
  )
})

test_that("plot() respects vars argument", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sim <- sir_sim()

  vdiffr::expect_doppelganger(
    "sim-single-variable",
    plot(sim, vars = "Susceptible")
  )

  vdiffr::expect_doppelganger(
    "sim-filtered-vars",
    plot(sim, vars = c("Susceptible", "Infected"))
  )
})

test_that("plot() with custom palette", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sim <- sir_sim()

  vdiffr::expect_doppelganger(
    "sim-custom-palette",
    plot(sim, palette = "Set 2")
  )
})

test_that("plot() with custom colors vector", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sim <- sir_sim()

  custom_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A")

  vdiffr::expect_doppelganger(
    "sim-custom-colors",
    plot(sim, colors = custom_colors)
  )
})

test_that("plot() with custom font family", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sim <- sir_sim()

  vdiffr::expect_doppelganger(
    "sim-custom-font-family",
    plot(sim, font_family = "Arial")
  )
})

test_that("plot() with custom font size", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sim <- sir_sim()

  vdiffr::expect_doppelganger(
    "sim-large-font-size",
    plot(sim, font_size = 20)
  )
})

test_that("plot() with custom wrap width", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sfm <- sdbuildR()
  sfm <- build(sfm,
    name = "a",
    label = "Very Long Stock Name That Should Wrap", type = "stock"
  )
  stock_name_clean <- sfm$variables$name[1]
  sfm <- build(sfm,
    name = "b",
    label = "Long Flow Name That Should Also Wrap", type = "flow",
    from = !!stock_name_clean
  )
  sim <- simulate(sfm, only_stocks = FALSE)

  vdiffr::expect_doppelganger(
    "sim-wrap-width-narrow",
    plot(sim, wrap_width = 10)
  )
})

test_that("plot() with custom title, axis labels, and limits", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sim <- sir_sim()

  vdiffr::expect_doppelganger(
    "sim-custom-title-axes-limits",
    plot(sim,
      main = "Custom Simulation Title",
      xlab = "Custom X", ylab = "Custom Y", xlim = c(0, 50), ylim = c(0, 800)
    )
  )
})

test_that("plot() respects add_constants", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sfm <- sdbuildR()
  sfm <- build(sfm, "Stock1", type = "stock")
  sfm <- build(sfm, "const_val", type = "constant", eqn = "100")
  sim <- simulate(sfm)

  vdiffr::expect_doppelganger(
    "sim-with-constants",
    plot(sim, add_constants = TRUE)
  )

  vdiffr::expect_doppelganger(
    "sim-without-constants",
    plot(sim, add_constants = FALSE)
  )
})

# ============================================================================
# EDGE CASES AND COMPLEX SCENARIOS
# ============================================================================

test_that("plot() shows legend for single-variable plot", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sfm <- sdbuildR()
  sfm <- build(sfm, "Stock1", type = "stock")
  sim <- simulate(sfm)

  vdiffr::expect_doppelganger(
    "sim-single-var-legend",
    plot(sim, showlegend = TRUE)
  )
})

test_that("plot() works with both stocks and flow variables", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  # SIR has Susceptible (stock), Infected (stock), Recovered (stock)
  sim <- sir_sim(only_stocks = FALSE)

  vdiffr::expect_doppelganger(
    "sim-only-stocks-false",
    plot(sim, showlegend = TRUE)
  )
})

test_that("plot() handles variables with duplicate display labels", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sfm <- sdbuildR()
  sfm <- build(sfm, "var1", type = "stock", label = "Same")
  sfm <- build(sfm, "var2", type = "stock", label = "Same")
  sim <- simulate(sfm)

  vdiffr::expect_doppelganger(
    "sim-duplicate-labels",
    plot(sim)
  )
})

test_that("plot() respects vars filtering for constants", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sfm <- sdbuildR()
  sfm <- build(sfm, "S", type = "stock")
  sfm <- build(sfm, "I", type = "stock")
  sfm <- build(sfm, "const1", type = "constant", eqn = "50")
  sfm <- build(sfm, "const2", type = "constant", eqn = "100")
  sim <- simulate(sfm)

  # Request only S and const1
  vdiffr::expect_doppelganger(
    "sim-constants-filtered-vars",
    plot(sim, vars = c("S", "const1"), add_constants = TRUE)
  )
})

test_that("plot() with vars = constant automatically enables add_constants", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  sfm <- sdbuildR()
  sfm <- build(sfm, "Stock1", type = "stock")
  sfm <- build(sfm, "const_val", type = "constant", eqn = "75")
  sim <- simulate(sfm)

  # Even without add_constants = TRUE, specifying a constant in vars should include it
  vdiffr::expect_doppelganger(
    "sim-vars-constant-add-constants",
    plot(sim, vars = c("Stock1", "const_val"), add_constants = FALSE)
  )
})

# ============================================================================
# DEFAULT BEHAVIOR TESTS
# ============================================================================

test_that("plot() uses default titles", {
  sfm <- sir_model()
  sfm$meta$name <- "My Model"
  sim <- simulate(sfm)

  result <- plot(sim)
  layout <- result$x$layoutAttrs[[1]]
  expect_equal(layout$title, sfm$meta$name)
  expect_true(grepl("^Time", layout$xaxis$title))
  expect_equal(layout$yaxis$title, "")

  # Should produce a plot object without error
  expect_plotly(result)
})
