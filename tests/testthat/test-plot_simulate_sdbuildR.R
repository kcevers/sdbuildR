test_that("plot() creates a basic plot for simulation", {
  sim <- sir_sim()
  result <- plot(sim)
  expect_plotly(result)
  sfm <- sim[["object"]]
  df <- as.data.frame(sfm, properties = c("label"))
  stock_labels <- df$label[df$type == "stock"]
  n_stocks <- length(stock_labels)

  # Default is show only stocks and showlegend = TRUE
  trace_info <- plotly_traces(result)
  expect_setequal(trace_info[["name"]], stock_labels)
  expect_true(all(trace_info[["showlegend"]]))
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
  pl <- plot(sim)
  expect_plotly(pl)

  sfm <- sim[["object"]]
  df <- as.data.frame(sfm, properties = c("label", "type"))
  stock_labels <- df$label[df$type == "stock"]
  trace_info <- plotly_traces(pl)
  expect_setequal(trace_info[["name"]], stock_labels)
  expect_true(all(trace_info$showlegend))

  expect_snapshot_plot("sim-sir-default", pl)
})

test_that("plot.simulate_sdbuildR() respects showlegend", {
  sim <- sir_sim()
  # Object-level expectations: legend toggles should reflect in built Plotly object
  pl_true <- plot(sim, showlegend = TRUE)
  pl_false <- plot(sim, showlegend = FALSE)
  expect_plotly(pl_true)
  expect_plotly(pl_false)
  traces_true <- plotly_traces(pl_true)
  traces_false <- plotly_traces(pl_false)
  expect_true(nrow(traces_true) > 0)
  expect_true(all(traces_true$showlegend))
  expect_true(nrow(traces_false) > 0)
  expect_true(all(!(traces_false$showlegend)))

  # Snapshots last
  expect_snapshot_plot("sim-showlegend-true", pl_true)
  expect_snapshot_plot("sim-showlegend-false", pl_false)
})

test_that("plot.simulate_sdbuildR() respects vars argument", {
  sim <- sir_sim()
  # Object-level expectations: vars filtering should limit plotted trace names
  sfm <- sim[["object"]]
  names_df <- as.data.frame(sfm, properties = c("label"))
  sus_label <- names_df$label[names_df$name == "susceptible"]
  pl_single <- plot(sim, vars = "susceptible")
  trace_names <- plotly_traces(pl_single)$name
  expect_setequal(trace_names, sus_label)

  sus_infected_labels <- names_df$label[names_df$name %in% c("susceptible", "infected")]
  pl_filtered <- plot(sim, vars = c("susceptible", "infected"))
  trace_names_filtered <- plotly_traces(pl_filtered)$name
  expect_setequal(trace_names_filtered, sus_infected_labels)

  # Snapshots last
  expect_snapshot_plot("sim-single-variable", pl_single)
  expect_snapshot_plot("sim-filtered-vars", pl_filtered)
})

test_that("plot.simulate_sdbuildR() with custom palette", {
  sim <- sir_sim()
  sfm <- sim[["object"]]
  stock_labels <- as.data.frame(sfm, properties = c("label", "type"))$label
  pl <- plot(sim, palette = "Set 2")
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)

  # Unique colors should be assigned across traces when a palette is used
  expect_true(length(unique(traces$color)) == nrow(traces))

  expect_snapshot_plot("sim-custom-palette", pl)
})

test_that("plot.simulate_sdbuildR() with custom colors vector", {
  sim <- sir_sim()
  # Object-level expectation: legend trace colors reflect custom palette when exposed
  df <- as.data.frame(sim, direction = "long")
  vars <- unique(df$variable)
  names_df <- as.data.frame(sim[["object"]])
  label_names <- names_df$label[match(vars, names_df$name)]
  custom_colors <- stats::setNames(rainbow(length(label_names)), label_names)

  pl_colors <- plot(sim, colors = custom_colors, alpha = 1)
  expect_plotly(pl_colors)
  traces <- plotly_traces(pl_colors)
  expect_equal(length(traces[["name"]]), length(label_names))
  legend_check <- plotly_check_legend_colors(pl_colors, expected = custom_colors)
  expect_true(nrow(legend_check) > 0)
  expect_true(all(legend_check$ok))
  expect_true(all(legend_check$matches_expected))

  # Snapshot last
  expect_snapshot_plot("sim-custom-colors", pl_colors)
})

test_that("plot.simulate_sdbuildR() with custom font family", {
  sim <- sir_sim()
  pl <- plot(sim, font_family = "Courier New")
  layout <- plotly_layout(pl)
  expect_equal(layout$font$family, "Courier New")

  expect_snapshot_plot("sim-custom-font-family", pl)
})

test_that("plot.simulate_sdbuildR() with custom font size", {
  sim <- sir_sim()
  pl <- plot(sim, font_size = 20)
  layout <- plotly_layout(pl)
  expect_equal(layout$font$size, 20)

  expect_snapshot_plot("sim-large-font-size", pl)
})

test_that("plot.simulate_sdbuildR() with custom wrap width", {
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
  pl <- plot(sim, wrap_width = 10)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(all(grepl("<br", traces[["name"]])))

  expect_snapshot_plot("sim-wrap-width-narrow", pl)
})

test_that("plot.simulate_sdbuildR() with custom title, axis labels, and limits", {
  sim <- sir_sim()
  pl <- plot(sim,
    main = "Custom Simulation Title",
    xlab = "Custom X", ylab = "Custom Y", xlim = c(0, 50), ylim = c(0, 800)
  )
  layout <- plotly_layout(pl)
  expect_equal(layout$title, "Custom Simulation Title")
  expect_equal(layout$xaxis$title, "Custom X")
  expect_equal(layout$yaxis$title, "Custom Y")
  expect_equal(layout$xaxis$range, c(0, 50))
  expect_equal(layout$yaxis$range, c(0, 800))

  expect_snapshot_plot("sim-custom-title-axes-limits", pl)
})

test_that("plot.simulate_sdbuildR() respects show_constants", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "Stock1", type = "stock")
  sfm <- update(sfm, "const_val", type = "constant", eqn = "100")
  sim <- simulate(sfm)
  
  constants <- as.data.frame(sim[["object"]], type = "constants", properties = "label")
  const_label <- constants$label[constants$name == "const_val"]
  pl_with_constants <- plot(sim, show_constants = TRUE)
  expect_plotly(pl_with_constants)
  traces <- plotly_traces(pl_with_constants)
  expect_true(all(const_label %in% traces[["name"]]))

  pl_without_constants <- plot(sim, show_constants = FALSE)
  expect_plotly(pl_without_constants)
  traces_no_const <- plotly_traces(pl_without_constants)
  expect_true(all(!(const_label %in% traces_no_const[["name"]])))

  expect_snapshot_plot("sim-with-constants", pl_with_constants)
  expect_snapshot_plot("sim-without-constants", pl_without_constants)
})

# ============================================================================
# EDGE CASES AND COMPLEX SCENARIOS
# ============================================================================

test_that("plot.simulate_sdbuildR() shows legend for single-variable plot", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "Stock1", type = "stock")
  sim <- simulate(sfm)
  pl <- plot(sim, showlegend = TRUE)
  expect_plotly(pl)
  trace_info <- plotly_traces(pl)
  expect_equal(nrow(trace_info), 1L)
  expect_equal(trace_info$name, "Stock1")
  expect_true(all(trace_info$showlegend))
  expect_snapshot_plot("sim-single-var-legend", pl)
})

test_that("plot.simulate_sdbuildR() works with both stocks and flow variables", {
  # SIR has susceptible (stock), infected (stock), recovered (stock)
  sim <- sir_sim(only_stocks = FALSE)
  df <- as.data.frame(sim, direction = "long")
  var_names <- unique(df$variable)
  pl <- plot(sim, showlegend = TRUE)
  expect_plotly(pl)
  trace_info <- plotly_traces(pl)
  expect_equal(nrow(trace_info), length(var_names))
  expect_true(all(trace_info$showlegend))

  expect_snapshot_plot("sim-only-stocks-false", pl)
})

test_that("plot.simulate_sdbuildR() handles variables with duplicate display labels", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "var1", type = "stock", label = "Same")
  sfm <- update(sfm, "var2", type = "stock", label = "Same")
  sim <- simulate(sfm)
  pl <- plot(sim)
  traces <- plotly_traces(pl)
  expect_setequal(traces$name, c("Same (var1)", "Same (var2)"))
})

test_that("plot.simulate_sdbuildR() respects vars filtering for constants", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "S", type = "stock")
  sfm <- update(sfm, "I", type = "stock")
  sfm <- update(sfm, "const1", type = "constant", eqn = "50")
  sfm <- update(sfm, "const2", type = "constant", eqn = "100")
  sim <- simulate(sfm)

  # Request only S and const1
  pl <- plot(sim, vars = c("S", "const1"), show_constants = TRUE)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  constants <- as.data.frame(sim[["object"]], type = "constants", properties = "label")
  const_label <- constants$label[constants$name == "const1"]
  expect_true(sum(const_label == traces[["name"]]) == 1)
  expect_snapshot_plot("sim-constants-filtered-vars", pl)
})

test_that("plot.simulate_sdbuildR() with vars = constant automatically enables show_constants", {
  sfm <- sdbuildR()
  sfm <- update(sfm, "Stock1", type = "stock")
  sfm <- update(sfm, "const_val", type = "constant", eqn = "75")
  sim <- simulate(sfm)

  # Even without show_constants = TRUE, specifying a constant in vars should include it
  pl <- plot(sim, vars = c("Stock1", "const_val"), show_constants = FALSE)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(sum("const_val" == traces[["name"]]) == 1)
  expect_snapshot_plot("sim-vars-constant-show-constants", pl)
})

# ============================================================================
# DEFAULT BEHAVIOR TESTS
# ============================================================================

test_that("plot.simulate_sdbuildR() uses default titles", {
  sfm <- sdbuildR("SIR") |> meta(name = "My Model")
  sim <- simulate(sfm)

  pl <- plot(sim)
  layout <- plotly_layout(pl)
  expect_equal(layout$title, sfm$meta$name)
  expect_true(grepl("^Time", layout$xaxis$title))
  expect_equal(layout$yaxis$title, "")
  expect_plotly(pl)
})
