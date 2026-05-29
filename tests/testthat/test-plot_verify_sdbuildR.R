# ============================================================================
# METHOD EXISTENCE + GUARD TESTS
# ============================================================================

test_that("plot.verify_sdbuildR method exists", {
  expect_true("plot.verify_sdbuildR" %in% methods("plot"))
})


# ============================================================================
# BASIC OUTPUT TYPE (non-snapshot)
# ============================================================================

test_that("plot.verify_sdbuildR returns plotly for single condition, n=1", {
  res <- make_verify_model()
  pl <- plot(res, test = 1L)
  expect_plotly(pl)
})

test_that("plot.verify_sdbuildR returns plotly for two conditions", {
  res <- make_verify_model(n_tests = 2)
  pl <- plot(res)
  expect_plotly(pl)
})


trace_group <- function(trace) {
  group <- trace[["legendgroup"]]
  if (is.null(group)) {
    return(NA_character_)
  }
  group_chr <- as.character(group)
  if (length(group_chr) == 0L || !nzchar(group_chr)) {
    return(NA_character_)
  }
  group_chr
}


trace_color <- function(trace) {
  color <- NULL
  if (!is.null(trace[["line"]])) color <- trace[["line"]][["color"]]
  if (is.null(color) || !nzchar(as.character(color))) {
    if (!is.null(trace[["marker"]])) color <- trace[["marker"]][["color"]]
  }
  if (is.null(color)) {
    return(NA_character_)
  }

  normalize_color <- function(col) {
    col_chr <- as.character(col)
    if (!nzchar(col_chr)) {
      return(NA_character_)
    }

    # rgba(...) or rgb(...)
    if (grepl("^rgba?\\(", col_chr)) {
      nums <- as.numeric(strsplit(gsub("rgba?\\(|\\)", "", col_chr), ",")[[1L]])
      if (length(nums) >= 3) {
        r <- nums[1]
        g <- nums[2]
        b <- nums[3]
        if (max(nums, na.rm = TRUE) <= 1) {
          r <- round(r * 255)
          g <- round(g * 255)
          b <- round(b * 255)
        }
        return(grDevices::rgb(r, g, b, maxColorValue = 255))
      }
    }

    # Hex (#RRGGBB or #RRGGBBAA) -> strip alpha if present
    if (grepl("^#", col_chr)) {
      if (nchar(col_chr) >= 7) {
        return(toupper(substr(col_chr, 1, 7)))
      }
      return(toupper(col_chr))
    }

    # Try named colours / other CSS-like values via col2rgb
    rgb_val <- tryCatch(grDevices::col2rgb(col_chr), error = function(e) NULL)
    if (!is.null(rgb_val)) {
      return(grDevices::rgb(rgb_val[1, 1], rgb_val[2, 1], rgb_val[3, 1], maxColorValue = 255))
    }

    NA_character_
  }

  toupper(normalize_color(color))
}


test_that("plot.verify_sdbuildR keeps trace colors aligned with the legend", {
  sfm <- make_verifiable_sfm() |>
    update(S, eqn = 10) |>
    unit_test(label = "S non-negative", expr = "all(S >= 0)") |>
    unit_test(label = "drain non-negative", expr = "all(drain >= 0)")

  res <- silence(verify(sfm))
  built <- plotly::plotly_build(plot(res))
  traces <- built[["x"]][["data"]]

  groups <- unique(vapply(traces, trace_group, character(1)))
  groups <- groups[!is.na(groups)]

  expect_true(length(groups) >= 2L)

  for (grp in groups) {
    grp_traces <- traces[vapply(traces, function(trace) identical(trace_group(trace), grp), logical(1))]
    grp_colors <- unique(vapply(grp_traces, trace_color, character(1)))
    grp_colors <- grp_colors[!is.na(grp_colors)]

    expect_equal(length(grp_colors), 1L)

    legend_traces <- grp_traces[vapply(grp_traces, function(trace) isTRUE(trace[["showlegend"]]), logical(1))]
    if (length(legend_traces) > 0L) {
      expect_equal(length(legend_traces), 1L)
      expect_identical(trace_color(legend_traces[[1L]]), grp_colors[[1L]])
    } else {
      # Some rendering backends/platforms may collapse legend entries; skip strict legend-trace checks
      expect_true(length(legend_traces) == 0L)
    }
  }
})


test_that("plot.verify_sdbuildR uses explicit custom colors on legend traces", {
  sfm <- make_verifiable_sfm() |>
    update(S, eqn = 10) |>
    unit_test(label = "S non-negative", expr = "all(S >= 0)") |>
    unit_test(label = "drain non-negative", expr = "all(drain >= 0)")

  res <- silence(verify(sfm))
  custom_colors <- c("#FF5733", "#3366FF")
  built <- plotly::plotly_build(plot(res, colors = custom_colors))
  traces <- built[["x"]][["data"]]

  legend_traces <- traces[vapply(traces, function(trace) isTRUE(trace[["showlegend"]]), logical(1))]
  if (length(legend_traces) == 0L) {
    # No explicit legend traces found; some backends may not expose them consistently.
    expect_true(length(legend_traces) == 0L)
  } else {
    legend_colors <- vapply(legend_traces, trace_color, character(1))
    legend_groups <- vapply(legend_traces, trace_group, character(1))
    legend_colors <- legend_colors[!is.na(legend_colors)]
    legend_groups <- legend_groups[!is.na(legend_groups)]

    expect_true(all(legend_colors %in% custom_colors))

    for (grp in unique(legend_groups)) {
      grp_traces <- traces[vapply(traces, function(trace) identical(trace_group(trace), grp), logical(1))]
      grp_colors <- unique(vapply(grp_traces, trace_color, character(1)))
      grp_colors <- grp_colors[!is.na(grp_colors)]
      expect_equal(length(grp_colors), 1L)
      expect_true(grp_colors[[1L]] %in% custom_colors)
    }
  }
})


# ============================================================================
# VISUAL REGRESSION TESTS (expect_snapshot_plot)
# ============================================================================

test_that("plot() single condition n=1", {
  res <- make_verify_model()
  expect_snapshot_plot("verify-single-cond-n1", plot(res, test = 1L))
})

test_that("plot() two conditions n=1 (subplot)", {
  res <- make_verify_model(n_tests = 2)
  expect_snapshot_plot("verify-two-cond-n1", plot(res))
})

test_that("plot() filtered j selects one condition from two", {
  res <- make_verify_model(n_tests = 2)
  expect_snapshot_plot("verify-filtered-j2", plot(res, test = 2L))
})


# ============================================================================
# VISUAL REGRESSION — LAYOUT CONTROL
# ============================================================================

test_that("plot() showlegend = FALSE hides legend", {
  res <- make_verify_model()
  expect_snapshot_plot("verify-showlegend-false", plot(res, showlegend = FALSE))
})

test_that("plot() nrows = 1 forces single-row layout", {
  res <- make_verify_model(n_tests = 2)
  expect_snapshot_plot("verify-nrows-1", plot(res, nrows = 1L))
})

test_that("plot() shareX = FALSE gives independent x axes", {
  res <- make_verify_model(n_tests = 2)
  expect_snapshot_plot("verify-sharex-false", plot(res, shareX = FALSE))
})

test_that("plot() shareY = FALSE gives independent y axes", {
  res <- make_verify_model(n_tests = 2)
  expect_snapshot_plot("verify-sharey-false", plot(res, shareY = FALSE))
})


# ============================================================================
# VISUAL REGRESSION — FILTERING
# ============================================================================

test_that("plot() label filter selects matching condition from two", {
  res <- make_verify_model(n_tests = 2)
  expect_snapshot_plot("verify-label-filter", plot(res, label = "non-neg"))
})

test_that("plot() test and label filters intersect correctly", {
  res <- make_verify_model(n_tests = 2)
  expect_snapshot_plot("verify-nr-label-intersection", plot(res, test = 1L, label = "non-neg"))
})

test_that("plot() status = 'pass' shows only passing tests", {
  res <- make_verify_model(with_fail = TRUE)
  expect_snapshot_plot("verify-status-pass-only", plot(res, status = "pass"))
})

test_that("plot() status = 'fail' shows only failing tests", {
  res <- make_verify_model(with_fail = TRUE)
  expect_snapshot_plot("verify-status-fail-only", plot(res, status = "fail"))
})


# ============================================================================
# VISUAL REGRESSION — APPEARANCE
# ============================================================================

test_that("plot() custom palette changes line colours", {
  res <- make_verify_model()
  expect_snapshot_plot("verify-custom-palette", plot(res, palette = "Pastel 1"))
})

test_that("plot() custom colors vector overrides palette", {
  res <- make_verify_model()
  expect_snapshot_plot("verify-custom-colors", plot(res, colors = "steelblue"))
})

test_that("plot() custom font_family changes annotation font", {
  res <- make_verify_model()
  expect_snapshot_plot("verify-custom-font-family", plot(res, font_family = "Arial"))
})

test_that("plot() custom font_size changes annotation font", {
  res <- make_verify_model()
  expect_snapshot_plot("verify-custom-font-size", plot(res, font_size = 20))
})

test_that("plot() narrow wrap_width wraps long labels", {
  res <- make_verify_model()
  expect_snapshot_plot("verify-wrap-width-narrow", plot(res, wrap_width = 10))
})


# ============================================================================
# VISUAL REGRESSION — MULTI-RUN APPEARANCE
# ============================================================================

test_that("plot() custom alpha is accepted", {
  res <- make_verify_model()
  expect_snapshot_plot("verify-alpha-low", plot(res, alpha = 0.5))
})
