test_that("plot() creates a basic plot for simulation", {
  sim <- sir_sim()
  result <- plot(sim)
  expect_plotly(result)
  sfm <- sim[["object"]]
  df <- as.data.frame(sfm, properties = c("label"))
  stock_labels <- df$label[df$type == "stock"]
  n_stocks <- length(stock_labels)

  # Default is show only stocks and showlegend = TRUE
  trace_info <- plotly_trace_summary(result)
  expect_setequal(trace_info[["name"]], stock_labels)
  expect_true(all(trace_info[["showlegend"]]))
  legend_items <- plotly_legend_items(result)
  expect_setequal(legend_items[["name"]], stock_labels)
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
  # Object-level expectations: legend toggles should reflect in built Plotly object
  pl_true <- plot(sim, showlegend = TRUE)
  pl_false <- plot(sim, showlegend = FALSE)
  expect_plotly(pl_true)
  expect_plotly(pl_false)
  expect_true(nrow(plotly_legend_items(pl_true)) > 0)
  expect_equal(nrow(plotly_legend_items(pl_false)), 0)

  # Snapshots last
  expect_snapshot_plot("sim-showlegend-true", pl_true)
  expect_snapshot_plot("sim-showlegend-false", pl_false)
})

test_that("plot() respects vars argument", {
  sim <- sir_sim()
  # Object-level expectations: vars filtering should limit plotted trace names
  sfm <- sim[["object"]]
  names_df <- as.data.frame(sfm, properties = c("label"))
  sus_label <- names_df$label[names_df$name == "susceptible"]
  pl_single <- plot(sim, vars = "susceptible")
  trace_names <- plotly_trace_summary(pl_single)$name
  expect_setequal(trace_names, sus_label)

  sus_infected_labels <- names_df$label[names_df$name %in% c("susceptible", "infected")]
  pl_filtered <- plot(sim, vars = c("susceptible", "infected"))
  trace_names_filtered <- plotly_trace_summary(pl_filtered)$name
  expect_setequal(trace_names_filtered, sus_infected_labels)

  # Snapshots last
  expect_snapshot_plot("sim-single-variable", pl_single)
  expect_snapshot_plot("sim-filtered-vars", pl_filtered)
})

test_that("plot() with custom palette", {
  sim <- sir_sim()

  expect_snapshot_plot("sim-custom-palette", plot(sim, palette = "Set 2"))
})

test_that("plot() with custom colors vector", {
  sim <- sir_sim()

  custom_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A")
  # Object-level expectation: legend trace colors should match provided palette
  pl_colors <- plot(sim, colors = custom_colors)
  legend_items <- plotly_legend_items(pl_colors)
  if (nrow(legend_items) > 0) {
    # Normalize plotly colours to compare robustly (rgba vs named/hex)
    got <- vapply(legend_items$color, normalize_color_string, character(1))
    want <- vapply(custom_colors, normalize_color_string, character(1))
    expect_true(all(got %in% want))
  }

  # Snapshot last
  expect_snapshot_plot("sim-custom-colors", pl_colors)
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
  sfm <- sdbuildR("SIR")
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
