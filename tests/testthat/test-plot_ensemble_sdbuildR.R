# ============================================================================
# METHOD EXISTENCE
# ============================================================================

test_that("plot.ensemble_sdbuildR method exists", {
  expect_true("plot.ensemble_sdbuildR" %in% methods("plot"))
})


# ============================================================================
# BASIC OUTPUT TYPE (non-snapshot)
# ============================================================================

test_that("plot.ensemble_sdbuildR() returns plotly", {
  withr::local_pdf(NULL)

  sims <- make_r_ens()
  expect_plotly(plot(sims))
})

test_that("plot.ensemble_sdbuildR() returns plotly for sims plot", {
  withr::local_pdf(NULL)
  sims <- make_r_ens(save_sims = TRUE)
  expect_plotly(plot(sims, which = "sims"))
})

test_that("plot.ensemble_sdbuildR() returns plotly for two conditions", {
  withr::local_pdf(NULL)
  sims <- make_r_ens_2cond()
  expect_plotly(plot(sims))
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
  expect_snapshot_plot("ens-central-tendency-median", plot(sims, central_tendency = "median"))
})

test_that("plot() central_tendency = FALSE (no central line)", {
  sims <- make_r_ens()
  expect_snapshot_plot("ens-central-tendency-false", plot(sims, central_tendency = FALSE))
})

test_that("plot() showlegend = FALSE", {
  sims <- make_r_ens()
  expect_snapshot_plot("ens-showlegend-false", plot(sims, showlegend = FALSE))
})

test_that("plot() label_subplots = FALSE hides condition labels", {
  sims <- make_r_ens_2cond()
  expect_snapshot_plot("ens-label-subplots-false", plot(sims, label_subplots = FALSE))
})

test_that("plot() nrows = 1 stacks conditions in one column", {
  sims <- make_r_ens_2cond()
  expect_snapshot_plot("ens-nrows-1", plot(sims, nrows = 1L))
})

test_that("plot() shareX = FALSE gives independent x axes", {
  sims <- make_r_ens_2cond()
  expect_snapshot_plot("ens-sharex-false", plot(sims, shareX = FALSE))
})

test_that("plot() shareY = FALSE gives independent y axes", {
  sims <- make_r_ens_2cond()
  expect_snapshot_plot("ens-sharey-false", plot(sims, shareY = FALSE))
})

test_that("plot() custom palette", {
  sims <- make_r_ens()
  expect_snapshot_plot("ens-custom-palette", plot(sims, palette = "Pastel 1"))
})

test_that("plot() custom colors vector", {
  sims <- make_r_ens()
  expect_snapshot_plot(
    "ens-custom-colors",
    plot(sims, colors = c("steelblue", "coral", "green3"))
  )
})

test_that("plot() custom font family", {
  sims <- make_r_ens()
  expect_snapshot_plot("ens-custom-font-family", plot(sims, font_family = "Arial"))
})

test_that("plot() custom font size", {
  sims <- make_r_ens()
  expect_snapshot_plot("ens-large-font-size", plot(sims, font_size = 20))
})

test_that("plot() narrow wrap_width wraps long labels", {
  sims <- make_r_ens()
  expect_snapshot_plot("ens-wrap-width-narrow", plot(sims, wrap_width = 10))
})

# f1 <- function(name) {
#   announce_snapshot_file(name = name)
#   skip("test1")
# }

# f2 <- function(name) {
#   announce_snapshot_file(name = name)
#   skip("test2")
# }

# test_that("test", {
#   sims <- make_r_ens()
#   f1("ens-test-1")
#   f2("ens-test-2")
# })
