# ============================================================================
# METHOD EXISTENCE
# ============================================================================

test_that("plot.ensemble_sdbuildR method exists", {
  expect_true("plot.ensemble_sdbuildR" %in% methods("plot"))
})


# ============================================================================
# PARAMETER VALIDATION
# ============================================================================

test_that("plot.ensemble_sdbuildR() rejects invalid which", {
  sims <- make_r_ens()
  expect_error(plot(sims, which = "NA"), "ust be.*summary.*sims")
})

test_that("plot.ensemble_sdbuildR() validates central_tendency", {
  withr::local_pdf(NULL)
  sims <- make_r_ens()
  expect_no_error(plot(sims, central_tendency = "median"))
  expect_error(
    plot(sims, central_tendency = "medians"),
    "must be.*mean.*median.*FALSE"
  )
})

test_that("plot.ensemble_sdbuildR() rejects invalid condition with single condition", {
  sims <- make_r_ens()
  expect_error(plot(sims, condition = c(3, 6, 9)), "only one condition")
})

test_that("plot.ensemble_sdbuildR() rejects invalid condition with multiple conditions", {
  sims <- make_r_ens_2cond()
  expect_error(plot(sims, condition = 10), "be integers between")
})

test_that("plot.ensemble_sdbuildR() informs when sim used with summary", {
  withr::local_pdf(NULL)
  sims <- make_r_ens(save_sims = TRUE)
  expect_message(plot(sims, sim = 5), "sim.*argument is ignored")
})

test_that("plot.ensemble_sdbuildR() requires save_sims for which = 'sims'", {
  sims <- make_r_ens(save_sims = FALSE)
  expect_error(plot(sims, which = "sims"), "Individual simulation data is required")
})


# ============================================================================
# VISUAL REGRESSION TESTS (expect_snapshot_plot)
# ============================================================================

test_that("plot() default summary plot", {
  sims <- make_r_ens()
  pl <- plot(sims)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  expect_snapshot_plot("ens-summary-default", pl)
})

test_that("plot() sims plot (individual trajectories)", {
  sims <- make_r_ens(save_sims = TRUE)
  pl <- plot(sims, which = "sims")
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(nrow(traces) > 0)
  expect_snapshot_plot("ens-sims-default", pl)
})

test_that("plot() two conditions subplot grid", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, nrows = 2L, shareX = TRUE, shareY = TRUE)
  expect_plotly(pl)
  info <- plotly_subplot_grid(pl)
  expect_true(info$is_subplot)
  expect_equal(info$n_panels, 2L)
  expect_equal(info$nrows, 2L)
  expect_equal(info$ncols, 1L)
  expect_true(info$shareX)
  expect_true(is.na(info$shareY)) # shareY applies within a row
  expect_true(nrow(plotly_traces(pl)) > 0)

  expect_snapshot_plot("ens-two-conditions", pl)
})

test_that("plot.ensemble_sdbuildR() filtered condition shows single condition", {
  sims <- make_r_ens_2cond()

  pl <- plot(sims, condition = 2L)
  expect_plotly(pl)
  info <- plotly_subplot_grid(pl)
  # expect_false(info$is_subplot) # Still a subplot grid, but with only one subplot
  expect_equal(info$n_panels, 1L)
  expect_equal(info$nrows, 1L)
  expect_equal(info$ncols, 1L)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot("ens-filtered-condition-2", pl)
})

test_that("plot.ensemble_sdbuildR() with too many nrows", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, nrows = 3L) # More rows than conditions should be gracefully handled
  expect_plotly(pl)
  info <- plotly_subplot_grid(pl)
  # expect_false(info$is_subplot) # Still a subplot grid, but with only one subplot
  expect_equal(info$n_panels, 2L)
  expect_equal(info$nrows, 2L)
  expect_equal(info$ncols, 1L)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot("ens-too-many-nrows", pl)
})


test_that("plot.ensemble_sdbuildR() central_tendency = 'median'", {
  sims <- make_r_ens()
  pl <- plot(sims, central_tendency = "median")
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot("ens-central-tendency-median", pl)
})

test_that("plot.ensemble_sdbuildR() central_tendency = FALSE (no central line)", {
  sims <- make_r_ens()
  pl <- plot(sims, central_tendency = FALSE)
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot("ens-central-tendency-false", pl)
})

test_that("plot.ensemble_sdbuildR() showlegend = FALSE", {
  sims <- make_r_ens()
  # Object-level expectation: no legend items when disabled
  pl_noleg <- plot(sims, showlegend = FALSE)
  expect_plotly(pl_noleg)
  traces <- plotly_traces(pl_noleg)
  expect_true(nrow(traces) > 0)
  expect_true(all(!(traces$showlegend)))

  # Snapshot last
  expect_snapshot_plot("ens-showlegend-false", pl_noleg)
})


test_that("plot.ensemble_sdbuildR() label_subplots = TRUE shows condition labels", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, label_subplots = TRUE)
  expect_plotly(pl)
  layout <- plotly_layout(pl)
  annot <- unlist(lapply(layout$annotations, `[[`, "text"))
  expect_true(length(annot) > 1)
  expect_true(sum(grepl("^Condition", annot)) == 2)

  # Snapshot last
  expect_snapshot_plot("ens-label-subplots-true", pl)
})


test_that("plot.ensemble_sdbuildR() label_subplots = FALSE hides condition labels", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, label_subplots = FALSE)
  expect_plotly(pl)
  layout <- plotly_layout(pl)
  annot <- unlist(lapply(layout$annotations, `[[`, "text"))
  expect_true(length(annot) == 1) # Only annotation for time label
  expect_false(any(grepl("^Condition", annot)))

  # Snapshot last
  expect_snapshot_plot("ens-label-subplots-false", pl)
})

test_that("plot.ensemble_sdbuildR() nrows works", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, nrows = 1L)
  expect_plotly(pl)

  info <- plotly_subplot_grid(pl)
  expect_true(info$is_subplot)
  expect_equal(info$nrows, 1L)
  expect_equal(info$ncols, 2L)

  pl <- plot(sims, nrows = 2L)
  expect_plotly(pl)

  info <- plotly_subplot_grid(pl)
  expect_true(info$is_subplot)
  expect_equal(info$nrows, 2L)
  expect_equal(info$ncols, 1L)

  expect_snapshot_plot(
    c("ens-nrows-1", "ens-nrows-2"),
    list(
      plot(sims, nrows = 1L),
      plot(sims, nrows = 2L)
    )
  )
})

test_that("plot.ensemble_sdbuildR() shareX and shareY works", {
  # 4 conditions
  n <- 3
  nrows <- 2
  sims <- make_r_ens(n = n, conditions = list(
    "contact_rate" = c(1.5, 2.5),
    "recovery_rate" = c(0.1, 0.2)
  ), cross = TRUE)

  pl <- plot(sims, shareX = TRUE, shareY = TRUE, nrows = nrows)
  info <- plotly_subplot_grid(pl)
  expect_true(info$shareX)
  expect_true(info$shareY)

  pl <- plot(sims, shareX = TRUE, shareY = FALSE, nrows = nrows)
  info <- plotly_subplot_grid(pl)
  expect_true(info$shareX)
  expect_false(info$shareY)

  pl <- plot(sims, shareX = FALSE, shareY = TRUE, nrows = nrows)
  info <- plotly_subplot_grid(pl)
  expect_false(info$shareX)
  expect_true(info$shareY)

  pl <- plot(sims, shareX = FALSE, shareY = FALSE, nrows = nrows)
  info <- plotly_subplot_grid(pl)
  expect_false(info$shareX)
  expect_false(info$shareY)

  # Snapshot last
  expect_snapshot_plot(
    c(
      "ens-sharex-true-sharey-true",
      "ens-sharex-true-sharey-false",
      "ens-sharex-false-sharey-true",
      "ens-sharex-false-sharey-false"
    ),
    list(
      plot(sims, shareX = TRUE, shareY = TRUE, nrows = nrows),
      plot(sims, shareX = TRUE, shareY = FALSE, nrows = nrows),
      plot(sims, shareX = FALSE, shareY = TRUE, nrows = nrows),
      plot(sims, shareX = FALSE, shareY = FALSE, nrows = nrows)
    )
  )
})


test_that("plot.ensemble_sdbuildR() custom palette", {
  sims <- make_r_ens()
  pl <- plot(sims, palette = "Pastel 1")
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot("ens-custom-palette", pl)
})

test_that("plot() custom colors vector", {
  sims <- make_r_ens()
  # Object-level expectation: legend trace colors reflect custom palette when exposed
  df <- as.data.frame(sims, direction = "long")
  vars <- unique(df$variable)
  names_df <- as.data.frame(sims[["object"]])
  label_names <- names_df$label[match(vars, names_df$name)]
  custom_colors <- stats::setNames(rainbow(length(label_names)), label_names)

  pl_colors <- plot(sims, colors = custom_colors, alpha = 1)
  expect_plotly(pl_colors)
  traces <- plotly_traces(pl_colors)
  expect_equal(length(unique(traces[["name"]])), length(label_names))
  legend_check <- plotly_check_legend_colors(pl_colors, expected = custom_colors)
  expect_true(nrow(legend_check) > 0)
  expect_true(all(legend_check$ok))
  expect_true(all(legend_check$matches_expected))

  # Snapshot last
  expect_snapshot_plot(
    "ens-custom-colors",
    pl_colors
  )
})

test_that("plot() custom font family", {
  sims <- make_r_ens()
  pl <- plot(sims, font_family = "Arial")
  expect_plotly(pl)
  layout <- plotly_layout(pl)
  expect_equal(layout$font$family, "Arial")

  # Snapshot last
  expect_snapshot_plot("ens-custom-font-family", pl)
})

test_that("plot() custom font size", {
  sims <- make_r_ens()
  pl <- plot(sims, font_size = 20)
  expect_plotly(pl)
  layout <- plotly_layout(pl)
  expect_equal(layout$font$size, 20)

  # Snapshot last
  expect_snapshot_plot("ens-large-font-size", pl)
})

test_that("plot() narrow wrap_width wraps long labels", {
  sims <- make_r_ens()
  pl <- plot(sims, wrap_width = 1, show_constants = TRUE)
  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(any(grepl("<br", traces$name, fixed = TRUE)))
  expect_snapshot_plot("ens-wrap-width-narrow", pl)
})


test_that("plot.ensemble_sdbuildR() with show_constants = TRUE", {
  sims <- make_r_ens()
  constants <- as.data.frame(sims[["object"]], type = "constants", properties = "label")
  pl <- plot(sims, show_constants = TRUE)

  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(all(constants[["label"]] %in% traces[["name"]]))

  # Snapshot last
  expect_snapshot_plot("ens-show-constants", pl)
})
