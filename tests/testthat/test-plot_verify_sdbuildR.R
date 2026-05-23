# ============================================================================
# METHOD EXISTENCE + GUARD TESTS
# ============================================================================

test_that("plot.verify_sdbuildR method exists", {
  expect_true("plot.verify_sdbuildR" %in% methods("plot"))
})

test_that("plot.verify_sdbuildR errors when sims are NULL", {
  res <- silence(
    make_verifiable_sfm() |>
      unit_test(label = "S non-negative", expr = all(S >= 0)) |>
      verify()
  )
  expect_error(plot(res), regexp = "return_sims")
})


# ============================================================================
# BASIC OUTPUT TYPE (non-vdiffr)
# ============================================================================

test_that("plot.verify_sdbuildR returns plotly for single condition, n=1", {
  res <- make_verify_1cond()
  pl <- plot(res, nr = 1L)
  expect_plotly(pl)
})

test_that("plot.verify_sdbuildR returns plotly for two conditions", {
  res <- make_verify_2cond()
  pl <- plot(res)
  expect_plotly(pl)
})

test_that("plot.verify_sdbuildR returns plotly for n > 1", {
  res <- make_verify_1cond(n = 3L)
  pl <- plot(res)
  expect_plotly(pl)
})


# ============================================================================
# VISUAL REGRESSION TESTS (vdiffr)
# ============================================================================

test_that("plot() single condition n=1", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_1cond()
  vdiffr::expect_doppelganger("verify-single-cond-n1", plot(res, nr = 1L))
})

test_that("plot() two conditions n=1 (subplot)", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_2cond()
  vdiffr::expect_doppelganger("verify-two-cond-n1", plot(res))
})

test_that("plot() single condition n=3 (multi-run overlay)", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_1cond(n = 3L)
  vdiffr::expect_doppelganger("verify-single-cond-n3", plot(res))
})

test_that("plot() filtered j selects one condition from two", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_2cond()
  vdiffr::expect_doppelganger("verify-filtered-j2", plot(res, nr = 2L))
})

test_that("plot() filtered i selects subset of runs", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_1cond(n = 3L)
  vdiffr::expect_doppelganger("verify-filtered-i12", plot(res, i = 1:2))
})


# ============================================================================
# VISUAL REGRESSION — LAYOUT CONTROL
# ============================================================================

test_that("plot() showlegend = FALSE hides legend", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_1cond()
  vdiffr::expect_doppelganger("verify-showlegend-false", plot(res, showlegend = FALSE))
})

test_that("plot() nrows = 1 forces single-row layout", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_2cond()
  vdiffr::expect_doppelganger("verify-nrows-1", plot(res, nrows = 1L))
})

test_that("plot() shareX = FALSE gives independent x axes", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_2cond()
  vdiffr::expect_doppelganger("verify-sharex-false", plot(res, shareX = FALSE))
})

test_that("plot() shareY = FALSE gives independent y axes", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_2cond()
  vdiffr::expect_doppelganger("verify-sharey-false", plot(res, shareY = FALSE))
})


# ============================================================================
# VISUAL REGRESSION — FILTERING
# ============================================================================

test_that("plot() label filter selects matching condition from two", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_2cond()
  vdiffr::expect_doppelganger("verify-label-filter", plot(res, label = "non-neg"))
})

test_that("plot() status = 'pass' shows only passing tests", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_with_fail()
  vdiffr::expect_doppelganger("verify-status-pass-only", plot(res, status = "pass"))
})

test_that("plot() status = 'fail' shows only failing tests", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_with_fail()
  vdiffr::expect_doppelganger("verify-status-fail-only", plot(res, status = "fail"))
})


# ============================================================================
# VISUAL REGRESSION — APPEARANCE
# ============================================================================

test_that("plot() custom palette changes line colours", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_1cond()
  vdiffr::expect_doppelganger("verify-custom-palette", plot(res, palette = "Pastel 1"))
})

test_that("plot() custom colors vector overrides palette", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_1cond()
  vdiffr::expect_doppelganger("verify-custom-colors", plot(res, colors = "steelblue"))
})

test_that("plot() custom font_family changes annotation font", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_1cond()
  vdiffr::expect_doppelganger("verify-custom-font-family", plot(res, font_family = "Arial"))
})

test_that("plot() custom font_size changes annotation font", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_1cond()
  vdiffr::expect_doppelganger("verify-custom-font-size", plot(res, font_size = 20))
})

test_that("plot() narrow wrap_width wraps long labels", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_1cond()
  vdiffr::expect_doppelganger("verify-wrap-width-narrow", plot(res, wrap_width = 10))
})


# ============================================================================
# VISUAL REGRESSION — MULTI-RUN APPEARANCE
# ============================================================================

test_that("plot() low alpha reduces trajectory opacity", {
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")

  res <- make_verify_1cond(n = 3L)
  vdiffr::expect_doppelganger("verify-alpha-low", plot(res, alpha = 0.1))
})
