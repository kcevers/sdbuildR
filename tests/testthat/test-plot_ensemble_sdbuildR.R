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
  expect_snapshot_plot("ens-summary-default", plot(sims))
})

test_that("plot() sims plot (individual trajectories)", {
  sims <- make_r_ens(save_sims = TRUE)
  expect_snapshot_plot("ens-sims-default", plot(sims, which = "sims"))
})

test_that("plot() two conditions subplot grid", {
  sims <- make_r_ens_2cond()
  expect_snapshot_plot("ens-two-conditions", plot(sims))
})

test_that("plot() filtered condition shows single condition", {
  sims <- make_r_ens_2cond()
  expect_snapshot_plot("ens-filtered-j2", plot(sims, condition = 2L))
})

test_that("plot() central_tendency = 'median'", {
  sims <- make_r_ens()
  expect_snapshot_plot(
    "ens-central-tendency-median",
    plot(sims, central_tendency = "median")
  )
})

test_that("plot() central_tendency = FALSE (no central line)", {
  sims <- make_r_ens()
  expect_snapshot_plot(
    "ens-central-tendency-false",
    plot(sims, central_tendency = FALSE)
  )
})

test_that("plot() showlegend = FALSE", {
  sims <- make_r_ens()
  # Object-level expectation: no legend items when disabled
  pl_noleg <- plot(sims, showlegend = FALSE)
  expect_plotly(pl_noleg)
  expect_equal(nrow(plotly_legend_items(pl_noleg)), 0)

  # Snapshot last
  expect_snapshot_plot("ens-showlegend-false", pl_noleg)
})


test_that("plot() label_subplots = TRUE shows condition labels", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, label_subplots = TRUE)
  layout <- plotly_layout_attrs(pl)
  annot <- unlist(lapply(layout$annotations, `[[`, "text"))
  expect_true(length(annot) > 1)
  expect_true(any(grepl("^Condition", annot)))

  # Snapshot last
  expect_snapshot_plot("ens-label-subplots-true", pl)
})


test_that("plot() label_subplots = FALSE hides condition labels", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, label_subplots = FALSE)
  layout <- plotly_layout_attrs(pl)
  annot <- unlist(lapply(layout$annotations, `[[`, "text"))
  expect_true(length(annot) == 1) # Only annotation for time label
  expect_false(any(grepl("^Condition", annot)))

  # Snapshot last
  expect_snapshot_plot("ens-label-subplots-false", pl)
})

test_that("plot() nrows = 1 stacks conditions in one column", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, nrows = 1L)
  expect_plotly(pl)
  expect_true(is_subplot(pl))

  # Snapshot last
  expect_snapshot_plot("ens-nrows-1", pl)
})

test_that("plot() shareX = TRUE gives shared x axes", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, shareX = TRUE)

  # Snapshot last
  expect_snapshot_plot("ens-sharex-true", pl)
})


test_that("plot() shareX = FALSE gives independent x axes", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, shareX = FALSE)

  # Snapshot last
  expect_snapshot_plot("ens-sharex-false", pl)
})

test_that("plot() shareY = TRUE gives shared y axes", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, shareY = TRUE)

  # Snapshot last
  expect_snapshot_plot("ens-sharey-true", pl)
})

test_that("plot() shareY = FALSE gives independent y axes", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, shareY = FALSE)

  # Snapshot last
  expect_snapshot_plot("ens-sharey-false", pl)
})

test_that("plot() custom palette", {
  sims <- make_r_ens()
  expect_snapshot_plot("ens-custom-palette", plot(sims, palette = "Pastel 1"))
})

test_that("plot() custom colors vector", {
  sims <- make_r_ens()
  # Object-level expectation: legend trace colors reflect custom palette when exposed
  pl_colors <- plot(sims, colors = c("steelblue", "coral", "green3"))
  li <- plotly_legend_items(pl_colors)
  if (nrow(li) > 0) {
    got <- vapply(li$color, normalize_color_string, character(1))
    want <- vapply(c("steelblue", "coral", "green3"), normalize_color_string, character(1))
    expect_true(all(got %in% want))
  }

  # Snapshot last
  expect_snapshot_plot(
    "ens-custom-colors",
    pl_colors
  )
})

test_that("plot() custom font family", {
  sims <- make_r_ens()
  pl <- plot(sims, font_family = "Arial")
  layout <- plotly_layout_attrs(pl)
  expect_equal(layout$font$family, "Arial")

  # Snapshot last
  expect_snapshot_plot("ens-custom-font-family", pl)
})

test_that("plot() custom font size", {
  sims <- make_r_ens()
  pl <- plot(sims, font_size = 20)
  layout <- plotly_layout_attrs(pl)
  expect_equal(layout$font$size, 20)

  # Snapshot last
  expect_snapshot_plot("ens-large-font-size", pl)
})

test_that("plot() narrow wrap_width wraps long labels", {
  sims <- make_r_ens()
  expect_snapshot_plot("ens-wrap-width-narrow", plot(sims, wrap_width = 10))
})


test_that("plot() ensemble with add_constants = TRUE", {
  sims <- make_r_ens()
  pl <- plot(sims, add_constants = TRUE)

  expect_plotly(pl)
  traces <- plotly_trace_summary(pl)
  # expect_true(nrow()) > 0)

  # Snapshot last
  expect_snapshot_plot("ens-add-constants", pl)
})
