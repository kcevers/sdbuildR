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
    plot(sim, vars = c("susceptible", "NonExistent")),
    "NonExistent.*not.*variable"
  )
})

test_that("plot() warns and continues when some vars are missing from data", {
  sim <- sir_sim(only_stocks = TRUE)

  expect_warning(
    pl <- plot(sim, vars = c("susceptible", "new_infections")),
    "not available in the simulated data"
  )
  expect_plotly(pl)
})


test_that("plot() errors when all requested vars are missing from data", {
  sim <- sir_sim(only_stocks = TRUE)

  expect_error(
    plot(sim, vars = c("new_recoveries", "new_infections")),
    "not available in the simulated data"
  )
})


test_that("plot() does not error when all requested vars are constants", {
  sim <- sir_sim(only_stocks = TRUE)

  expect_no_error(
    plot(sim, vars = c("infection_rate", "recovery_rate"))
  )
})

test_that("plot() does not error when some requested vars are constants", {
  sim <- sir_sim(only_stocks = TRUE)

  expect_no_error(
    plot(sim, vars = c("susceptible", "recovery_rate"))
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
# VISUAL REGRESSION TESTS (expect_snapshot_plot)
# ============================================================================

test_that("plot() creates standard line plot for SIR simulation", {
  sim <- sir_sim()

  expect_snapshot_plot("sim-sir-default", plot(sim))
})

test_that("plot() respects showlegend", {
  sim <- sir_sim()

  expect_snapshot_plot("sim-showlegend-true", plot(sim, showlegend = TRUE))
  expect_snapshot_plot("sim-showlegend-false", plot(sim, showlegend = FALSE))
})

test_that("plot() respects vars argument", {
  sim <- sir_sim()

  expect_snapshot_plot("sim-single-variable", plot(sim, vars = "susceptible"))
  expect_snapshot_plot("sim-filtered-vars", plot(sim, vars = c("susceptible", "infected")))
})

test_that("plot() with custom palette", {
  sim <- sir_sim()

  expect_snapshot_plot("sim-custom-palette", plot(sim, palette = "Set 2"))
})

test_that("plot() with custom colors vector", {
  sim <- sir_sim()

  custom_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A")

  expect_snapshot_plot("sim-custom-colors", plot(sim, colors = custom_colors))
})

test_that("plot() with custom font family", {
  sim <- sir_sim()

  expect_snapshot_plot("sim-custom-font-family", plot(sim, font_family = "Arial"))
})

test_that("plot() with custom font size", {
  sim <- sir_sim()

  expect_snapshot_plot("sim-large-font-size", plot(sim, font_size = 20))
})

test_that("plot() with custom wrap width", {
  sfm <- sdbuildR()
  sfm <- update(sfm,
    name = "a",
    label = "Very Long Stock Name That Should Wrap", type = "stock"
  )
  stock_name_clean <- sfm$variables$name[1]
  sfm <- update(sfm,
    name = "b",
    label = "Long Flow Name That Should Also Wrap", type = "flow",
    from = !!stock_name_clean
  )
  sim <- simulate(sfm, only_stocks = FALSE)

  expect_snapshot_plot("sim-wrap-width-narrow", plot(sim, wrap_width = 10))
})

test_that("plot() with custom title, axis labels, and limits", {
  sim <- sir_sim()

  expect_snapshot_plot(
    "sim-custom-title-axes-limits",
    plot(sim,
      main = "Custom Simulation Title",
      xlab = "Custom X", ylab = "Custom Y", xlim = c(0, 50), ylim = c(0, 800)
    )
  )
})

test_that("plot() respects add_constants", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "Stock1", type = "stock")
  sfm <- update(sfm, "const_val", type = "constant", eqn = "100")
  sim <- simulate(sfm)

  expect_snapshot_plot("sim-with-constants", plot(sim, add_constants = TRUE))
  expect_snapshot_plot("sim-without-constants", plot(sim, add_constants = FALSE))
})

# ============================================================================
# EDGE CASES AND COMPLEX SCENARIOS
# ============================================================================

test_that("plot() shows legend for single-variable plot", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "Stock1", type = "stock")
  sim <- simulate(sfm)

  expect_snapshot_plot("sim-single-var-legend", plot(sim, showlegend = TRUE))
})

test_that("plot() works with both stocks and flow variables", {
  # SIR has susceptible (stock), infected (stock), recovered (stock)
  sim <- sir_sim(only_stocks = FALSE)

  expect_snapshot_plot("sim-only-stocks-false", plot(sim, showlegend = TRUE))
})

test_that("plot() handles variables with duplicate display labels", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "var1", type = "stock", label = "Same")
  sfm <- update(sfm, "var2", type = "stock", label = "Same")
  sim <- simulate(sfm)

  expect_snapshot_plot("sim-duplicate-labels", plot(sim))
})

test_that("plot() respects vars filtering for constants", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "S", type = "stock")
  sfm <- update(sfm, "I", type = "stock")
  sfm <- update(sfm, "const1", type = "constant", eqn = "50")
  sfm <- update(sfm, "const2", type = "constant", eqn = "100")
  sim <- simulate(sfm)

  # Request only S and const1
  expect_snapshot_plot(
    "sim-constants-filtered-vars",
    plot(sim, vars = c("S", "const1"), add_constants = TRUE)
  )
})

test_that("plot() with vars = constant automatically enables add_constants", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "Stock1", type = "stock")
  sfm <- update(sfm, "const_val", type = "constant", eqn = "75")
  sim <- simulate(sfm)

  # Even without add_constants = TRUE, specifying a constant in vars should include it
  expect_snapshot_plot(
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
