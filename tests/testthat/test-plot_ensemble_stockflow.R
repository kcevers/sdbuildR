# ============================================================================
# METHOD EXISTENCE
# ============================================================================

test_that("plot.ensemble_stockflow method exists", {
  expect_true("plot.ensemble_stockflow" %in% methods("plot"))
})


# ============================================================================
# PARAMETER VALIDATION
# ============================================================================

test_that("plot.ensemble_stockflow() rejects invalid which", {
  sims <- make_r_ens(n = 2)
  expect_error(plot(sims, which = "NA"), "ust be.*summary.*sims")
})

test_that("plot.ensemble_stockflow() validates central", {
  withr::local_pdf(NULL)
  sims <- make_r_ens(n = 2)
  expect_no_error(plot(sims, central = "median"))
  expect_no_error(plot(sims, central = "none"))
  # Lenient matching: plurals and case variants are accepted.
  expect_no_error(plot(sims, central = "Medians"))
  expect_no_error(plot(sims, central = "MEAN"))
  expect_error(
    plot(sims, central = "medians2"),
    "must be.*mean.*median.*none"
  )
})

test_that("plot.ensemble_stockflow() validates spread", {
  withr::local_pdf(NULL)
  sims <- make_r_ens(n = 2, central = "mean", spread = c("quantile", "sd"))
  expect_no_error(plot(sims, spread = "sd"))
  expect_no_error(plot(sims, spread = "none"))
  # Lenient matching: plurals and case variants are accepted.
  expect_no_error(plot(sims, spread = "Quantiles"))
  expect_no_error(plot(sims, spread = "SDs"))
  expect_error(
    plot(sims, spread = "iqr"),
    "must be.*quantile.*sd.*none"
  )
})

test_that("plot.ensemble_stockflow() rejects invalid condition with single condition", {
  sims <- make_r_ens(n = 2)
  expect_error(plot(sims, condition = c(3, 6, 9)), "only one condition")
})

test_that("plot.ensemble_stockflow() rejects invalid condition with multiple conditions", {
  sims <- make_r_ens_2cond(n = 2)
  expect_error(plot(sims, condition = 10), "be integers between")
})

test_that("plot.ensemble_stockflow() informs when sim used with summary", {
  withr::local_pdf(NULL)
  sims <- make_r_ens(n = 2, save_sims = TRUE)
  expect_message(plot(sims, sim = 5), "sim.*argument is ignored")
})

test_that("plot.ensemble_stockflow() requires save_sims for which = 'sims'", {
  sims <- make_r_ens(n = 2, save_sims = FALSE)
  expect_error(plot(sims, which = "sims"), "Individual simulation data is required")
})


test_that("plot.ensemble_stockflow() validates central_line_width", {
  withr::local_pdf(NULL)
  sims <- make_r_ens(n = 2)
  expect_error(plot(sims, central_line_width = "thick"), "central_line_width")
  expect_error(plot(sims, central_line_width = -1), "positive")
})

test_that("plot.ensemble_stockflow() applies scalar and vector central_line_width", {
  withr::local_pdf(NULL)
  sims <- make_r_ens(n = 2)

  ct_widths <- function(p) {
    b <- plotly::plotly_build(p)
    w <- vapply(b[["x"]][["data"]], function(tr) {
      if (!is.null(tr[["line"]][["width"]])) as.numeric(tr[["line"]][["width"]])[1] else NA_real_
    }, numeric(1))
    w[!is.na(w)]
  }

  # Scalar applies to every central tendency line
  expect_true(all(ct_widths(plot(sims, which = "summary", central_line_width = 7)) == 7))

  # Vector with one value per variable
  nv <- length(unique(as.data.frame(sims, which = "summary")[["variable"]]))
  expect_setequal(
    ct_widths(plot(sims, which = "summary", central_line_width = seq_len(nv))),
    seq_len(nv)
  )

  # Too-short vector is rejected with the right argument name
  expect_error(
    plot(sims, which = "summary", central_line_width = rep(2, nv - 1)),
    "central_line_width"
  )
})


# ============================================================================
# BASIC OUTPUT TYPE (non-snapshot)
# ============================================================================

test_that("plot.ensemble_stockflow() creates a basic summary plot", {
  sims <- make_r_ens(n = 2)
  pl <- plot(sims)
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
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

test_that("plot() sims: legend colours match trajectory colours", {
  # Regression test: the legend is carried by the central-tendency traces while
  # the individual trajectories set their colour explicitly. Plotly silently
  # drops the `colors =` palette of an aesthetic trace when explicit-colour
  # traces are already present, which made the legend swatches disagree with the
  # trajectories. Compare per legendgroup, ignoring alpha.
  withr::local_pdf(NULL)
  sims <- make_r_ens(n = 5, save_sims = TRUE)
  b <- plotly::plotly_build(plot(sims, which = "sims"))

  # Normalise any plotly colour spec (#RRGGBB, #RRGGBBAA, or rgba(...)) to "r,g,b".
  to_rgb <- function(col) {
    if (grepl("^rgba?\\(", col)) {
      nums <- as.numeric(strsplit(gsub("[rgba() ]", "", col), ",")[[1]])
      return(paste(nums[1:3], collapse = ","))
    }
    paste(grDevices::col2rgb(substr(col, 1, 7))[, 1], collapse = ",")
  }

  # Key on the trace `name` (the variable label), which both the legend-carrying
  # and trajectory traces reliably carry; `legendgroup` is unset on plotly's
  # palette-mapped traces, so it cannot be used to line them up.
  legend_col <- list() # variable -> colour of the legend-carrying trace
  traj_col <- list() # variable -> colour of the trajectory traces
  for (tr in b[["x"]][["data"]]) {
    col <- tr[["line"]][["color"]]
    if (is.null(col) || is.null(tr[["name"]])) next
    if (isTRUE(tr[["showlegend"]])) {
      legend_col[[tr[["name"]]]] <- to_rgb(col)
    } else {
      traj_col[[tr[["name"]]]] <- to_rgb(col)
    }
  }

  expect_true(length(legend_col) > 0)
  for (v in names(legend_col)) {
    expect_identical(legend_col[[v]], traj_col[[v]])
  }
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

test_that("plot.ensemble_stockflow() filtered condition shows single condition", {
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

test_that("plot.ensemble_stockflow() with too many nrows", {
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


test_that("plot.ensemble_stockflow() central = 'median'", {
  sims <- make_r_ens(central = "median")
  pl <- plot(sims, central = "median")
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot("ens-central-tendency-median", pl)
})

test_that("plot.ensemble_stockflow() central = 'none' (no central line)", {
  sims <- make_r_ens()
  pl <- plot(sims, central = "none")
  expect_plotly(pl)
  expect_true(nrow(plotly_traces(pl)) > 0)
  expect_snapshot_plot("ens-central-tendency-false", pl)
})

test_that("plot.ensemble_stockflow() showlegend = FALSE", {
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


test_that("plot.ensemble_stockflow() label_subplots = TRUE shows condition labels", {
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


test_that("plot.ensemble_stockflow() label_subplots = FALSE hides condition labels", {
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

test_that("plot.ensemble_stockflow() nrows works", {
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

test_that("plot.ensemble_stockflow() shareX and shareY works", {
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


test_that("plot.ensemble_stockflow() custom palette", {
  sims <- make_r_ens()
  pl <- plot(sims, palette = "Greens")
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

test_that("plot.ensemble_stockflow() maps central traces to source summaries and named colors", {
  sims <- make_r_ens(n = 3)
  names_df <- as.data.frame(sims[["object"]])
  labels_all <- names_df[["label"]]
  labels <- names_df[["label"]][match(unique(sims[["summary"]][["variable"]]), names_df[["name"]])]
  colors <- stats::setNames(grDevices::rainbow(length(labels_all)), labels_all)
  colors <- rev(colors)

  pl <- plot(sims, colors = colors, webgl = FALSE)
  built_traces <- plotly::plotly_build(pl)[["x"]][["data"]]
  trace_info <- plotly_traces(pl)
  label_to_name <- stats::setNames(names_df[["name"]], names_df[["label"]])

  for (trace_label in labels) {
    variable <- unname(label_to_name[[trace_label]])
    expected_y <- sims[["summary"]][["mean"]][sims[["summary"]][["variable"]] == variable]
    matching <- which(vapply(built_traces, function(trace) {
      identical(trace[["name"]], trace_label) &&
        length(trace[["y"]]) == length(expected_y) &&
        isTRUE(all.equal(as.numeric(trace[["y"]]), as.numeric(expected_y)))
    }, logical(1)))

    expect_length(matching, 1L)
    expect_equal(
      trace_info[["color"]][matching],
      normalize_color_string(colors[[trace_label]])
    )
  }
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


test_that("plot.ensemble_stockflow() with show_constants = TRUE", {
  sims <- make_r_ens()
  constants <- as.data.frame(sims[["object"]], type = "constants", properties = "label")
  pl <- plot(sims, show_constants = TRUE)

  expect_plotly(pl)
  traces <- plotly_traces(pl)
  expect_true(all(constants[["label"]] %in% traces[["name"]]))

  # Snapshot last
  expect_snapshot_plot("ens-show-constants", pl)
})

# ============================================================================
# CONDITION DISPLAY AND TIME ANIMATION
# ============================================================================

test_that("plot.ensemble_stockflow(condition_display = 'slider') builds a slider", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, condition_display = "slider")
  expect_plotly(pl)

  layout <- plotly_layout(pl)
  expect_true(length(layout$sliders) > 0)
  expect_equal(length(layout$sliders[[1]]$steps), sims[["n_conditions"]])

  # Single parameter -> one control; steps swap the data (method = "restyle").
  # The parameter label lives in the slider title (currentvalue prefix), and the
  # tick labels carry only the bare parameter values.
  steps <- layout$sliders[[1]]$steps
  expect_true(all(vapply(steps, function(s) s$method, character(1)) == "restyle"))
  expect_match(layout$sliders[[1]]$currentvalue$prefix, "=")
  expect_false(grepl("=", steps[[1]]$label))

  # Only one condition's traces are drawn; the slider swaps their y data
  # client-side rather than hiding per-condition traces.
  expect_equal(
    length(plotly::plotly_build(pl)$x$data),
    length(plotly::plotly_build(plot(sims, condition = 1))$x$data)
  )
})

test_that("plot.ensemble_stockflow() shows one control per parameter when cross = TRUE", {
  sims <- make_r_ens(conditions = list(
    contact_rate = c(1, 2), recovery_rate = c(0.05, 0.1)
  ))
  expect_true(isTRUE(sims[["cross"]]))

  pl <- plot(sims, condition_display = "slider")
  layout <- plotly_layout(pl)
  expect_equal(length(layout$sliders), 2L) # one slider per parameter

  # Slider titles use the parameter labels; steps are the parameter's values.
  prefixes <- vapply(layout$sliders, function(s) s$currentvalue$prefix, character(1))
  expect_true(any(grepl("Contact rate", prefixes)))
  expect_true(any(grepl("Recovery rate", prefixes)))
  expect_equal(length(layout$sliders[[1]]$steps), 2L)

  # Cross-product swap requires the client-side hook.
  expect_false(is.null(pl$jsHooks$render))

  # Still only one condition's worth of traces.
  expect_equal(
    length(plotly::plotly_build(pl)$x$data),
    length(plotly::plotly_build(plot(sims, condition = 1))$x$data)
  )

  pd <- plot(sims, condition_display = "dropdown")
  expect_equal(length(plotly_layout(pd)$updatemenus), 2L)
  expect_false(is.null(pd$jsHooks$render))
})

test_that("plot.ensemble_stockflow() slider keeps a step per value but thins labels", {
  sims <- make_r_ens(conditions = list(contact_rate = seq(1, 25)))
  pl <- plot(sims, condition_display = "slider")
  layout <- plotly_layout(pl)

  steps <- layout$sliders[[1]]$steps
  # One jump per varied parameter value ...
  expect_equal(length(steps), 25L)

  labs <- vapply(steps, function(s) s$label, character(1))
  nonempty <- labs[nzchar(labs)]
  # ... but tick labels are thinned to a reasonable default when > 10 values.
  expect_lte(length(nonempty), 10L)
  # Endpoints stay labelled.
  expect_true(nzchar(labs[[1]]))
  expect_true(nzchar(labs[[length(labs)]]))
})

test_that("plot.ensemble_stockflow() control_options$max_labels tunes tick density", {
  sims <- make_r_ens(conditions = list(contact_rate = seq(1, 25)))

  pl <- plot(sims,
    condition_display = "slider",
    control_options = list(max_labels = 5)
  )
  steps <- plotly_layout(pl)$sliders[[1]]$steps
  labs <- vapply(steps, function(s) s$label, character(1))
  expect_equal(length(steps), 25L) # still one step per value
  expect_lte(length(labs[nzchar(labs)]), 5L) # but at most 5 labels
})

test_that("plot.ensemble_stockflow() rejects invalid control_options", {
  sims <- make_r_ens_2cond()
  expect_error(
    plot(sims, condition_display = "slider", control_options = list(foo = 1)),
    "control_options"
  )
  expect_error(
    plot(sims, condition_display = "slider", control_options = list(max_labels = 0)),
    "max_labels"
  )
})

test_that("plot.ensemble_stockflow(condition_display = 'dropdown') builds a dropdown", {
  sims <- make_r_ens_2cond()
  pl <- plot(sims, condition_display = "dropdown")
  expect_plotly(pl)

  layout <- plotly_layout(pl)
  expect_true(length(layout$updatemenus) > 0)
  expect_equal(length(layout$updatemenus[[1]]$buttons), sims[["n_conditions"]])
})

test_that("plot.ensemble_stockflow() condition controls work with which = 'sims'", {
  sims <- make_r_ens_2cond(save_sims = TRUE)
  pl <- plot(sims, which = "sims", condition_display = "slider")
  expect_plotly(pl)
  expect_true(length(plotly_layout(pl)$sliders) > 0)
})

test_that("plot.ensemble_stockflow(animation = 'time') builds frames for one condition", {
  sims <- make_r_ens(save_sims = TRUE)
  pl <- plot(sims, which = "sims", animation = "time")
  expect_plotly(pl)
  expect_true(length(plotly_frames(pl)) > 0)
})

test_that("plot.ensemble_stockflow() animates a single selected condition", {
  sims <- make_r_ens_2cond(save_sims = TRUE)
  pl <- plot(sims, which = "sims", condition = 1, animation = "time")
  expect_plotly(pl)
  expect_true(length(plotly_frames(pl)) > 0)
})

test_that("plot.ensemble_stockflow() rejects invalid / unsupported combinations", {
  sims <- make_r_ens_2cond(save_sims = TRUE)
  expect_error(plot(sims, condition_display = "tabs"), "condition_display")
  expect_error(plot(sims, animation = "fast"), "animation")
  expect_error(
    plot(sims, which = "sims", animation = "time", condition_display = "slider"),
    "not supported"
  )
  # Multiple conditions (subplots) cannot be animated together
  expect_error(plot(sims, which = "sims", animation = "time"), "not supported|multiple conditions")
  # Summary confidence ribbons cannot be animated
  expect_error(plot(sims, which = "summary", condition = 1, animation = "time"), "not supported|ribbons")
})

test_that("plot.ensemble_stockflow() webgl toggles trace type for which = 'sims'", {
  sims <- make_r_ens(n = 5, save_sims = TRUE)

  pl_gl <- plot(sims, which = "sims", webgl = TRUE)
  types_gl <- vapply(plotly::plotly_build(pl_gl)$x$data, function(d) d$type %||% "", character(1))
  expect_true(any(types_gl == "scattergl"))

  pl_svg <- plot(sims, which = "sims", webgl = FALSE)
  data_svg <- plotly::plotly_build(pl_svg)$x$data
  types_svg <- vapply(data_svg, function(d) d$type %||% "", character(1))
  expect_false(any(types_svg == "scattergl"))
  # Per-sim traces produce more traces than the webgl single-trace-per-variable
  expect_gt(length(data_svg), length(plotly::plotly_build(pl_gl)$x$data))
})

test_that("plot.ensemble_stockflow() obeys global webgl option", {
  sims <- make_r_ens(n = 5, save_sims = TRUE)

  withr::local_options(list(sdbuildR.webgl = TRUE))
  pl_gl <- plot(sims, which = "sims")
  types_gl <- vapply(plotly::plotly_build(pl_gl)$x$data, function(d) d$type %||% "", character(1))
  expect_true(any(types_gl == "scattergl"))

  withr::local_options(list(sdbuildR.webgl = FALSE))
  pl_svg <- plot(sims, which = "sims")
  data_svg <- plotly::plotly_build(pl_svg)$x$data
  types_svg <- vapply(data_svg, function(d) d$type %||% "", character(1))
  expect_false(any(types_svg == "scattergl"))
  # Per-sim traces produce more traces than the webgl single-trace-per-variable
  expect_gt(length(data_svg), length(plotly::plotly_build(pl_gl)$x$data))

  # Setting webgl explicitly overrides the global option
  pl_gl2 <- plot(sims, which = "sims", webgl = TRUE)
  types_gl2 <- vapply(plotly::plotly_build(pl_gl2)$x$data, function(d) d$type %||% "", character(1))
  expect_true(any(types_gl2 == "scattergl"))
})


test_that("plot.ensemble_stockflow() rejects non-logical webgl", {
  sims <- make_r_ens(n = 3, save_sims = TRUE)
  expect_error(plot(sims, which = "sims", webgl = "yes"), "webgl")
})
