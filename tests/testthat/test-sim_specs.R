test_that("sim_specs() modifies start time", {
  sfm <- sdbuildR() |>
    sim_specs(start = 10)

  expect_equal(as.numeric(sfm$sim_specs$start), 10)
})

test_that("sim_specs() modifies stop time", {
  sfm <- sdbuildR() |>
    sim_specs(stop = 200)

  expect_equal(as.numeric(sfm$sim_specs$stop), 200)
})

test_that("sim_specs() modifies dt", {
  sfm <- sdbuildR() |>
    sim_specs(dt = 0.1)

  expect_equal(as.numeric(sfm$sim_specs$dt), 0.1)
})

test_that("sim_specs() modifies time_units", {
  sfm <- sdbuildR() |>
    sim_specs(time_units = "days")

  # Note: time_units get converted to abbreviations internally
  expect_true(sfm$sim_specs$time_units %in% c("d", "days"))
})

test_that("sim_specs() modifies method", {
  sfm <- sdbuildR() |>
    sim_specs(method = "rk4")

  expect_equal(sfm$sim_specs$method, "rk4")
})

test_that("sim_specs() preserves other fields", {
  sfm <- sdbuildR() |>
    sim_specs(start = 5)

  expect_true("stop" %in% names(sfm$sim_specs))
  expect_true("dt" %in% names(sfm$sim_specs))
})

test_that("sim_specs() returns sdbuildR object", {
  sfm <- sdbuildR() |>
    sim_specs(start = 0)

  expect_s3_class(sfm, "sdbuildR")
})

test_that("sim_specs() handles language parameter", {
  sfm <- sdbuildR() |>
    sim_specs(language = "R")

  expect_equal(sfm$sim_specs$language, "R")

  sfm <- sdbuildR() |>
    sim_specs(language = "Julia")

  expect_equal(sfm$sim_specs$language, "Julia")
})

test_that("sim_specs() modifies seed", {
  sfm <- sdbuildR() |>
    sim_specs(seed = 123)

  expect_equal(as.numeric(sfm$sim_specs$seed), 123)
})


cli::test_that_cli(configs = c("plain", "ansi"), "clean_language() rejects invalid language", {
  expect_snapshot(clean_language("python"), error = TRUE)
  expect_snapshot(clean_language("cpp"), error = TRUE)
})



test_that("sim_specs() sets basic parameters", {
  sfm <- sdbuildR()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- suppressWarnings(sim_specs(sfm1, start = 0, stop = 10, dt = 0.5))

  expect_equal(sfm2[["sim_specs"]][["start"]], "0.0")
  expect_equal(sfm2[["sim_specs"]][["stop"]], "10.0")
  expect_equal(sfm2[["sim_specs"]][["dt"]], "0.5")
})

test_that("sim_specs() validates start < stop", {
  sfm <- sdbuildR()
  sfm1 <- build(sfm, "Stock1", type = "stock")

  expect_error(
    sim_specs(sfm1, start = 10, stop = 5),
    "start.*smaller than.*stop"
  )
})

test_that("sim_specs() validates numeric parameters", {
  sfm <- sdbuildR()
  sfm1 <- build(sfm, "Stock1", type = "stock")

  expect_error(sim_specs(sfm1, start = "abc"), "numeric")
  expect_error(sim_specs(sfm1, stop = "xyz"), "numeric")
  expect_error(sim_specs(sfm1, dt = "foo"), "numeric")
})

test_that("sim_specs() validates language parameter", {
  sfm <- sdbuildR()
  sfm1 <- build(sfm, "Stock1", type = "stock")

  expect_no_error(sim_specs(sfm1, language = "R"))
  expect_no_error(sim_specs(sfm1, language = "Julia"))
})

test_that("sim_specs() validates method parameter", {
  sfm <- sdbuildR()
  sfm1 <- build(sfm, "Stock1", type = "stock")

  expect_no_error(sim_specs(sfm1, method = "euler", language = "R"))
  expect_no_error(sim_specs(sfm1, method = "rk4", language = "R"))
})

test_that("sim_specs() handles time_units", {
  sfm <- sdbuildR()
  sfm1 <- build(sfm, "Stock1", type = "stock")

  expect_no_error(sim_specs(sfm1, time_units = "days"))
  expect_no_error(sim_specs(sfm1, time_units = "hours"))
  expect_no_error(sim_specs(sfm1, time_units = "years"))
})

test_that("sim_specs() rejects invalid time_units", {
  sfm <- sdbuildR()
  sfm1 <- build(sfm, "Stock1", type = "stock")

  expect_error(sim_specs(sfm1, time_units = "invalid123"), "only contain letters")
})

test_that("sim_specs() sets save_at parameter", {
  sfm <- sdbuildR()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- sim_specs(sfm1, dt = 0.1, save_at = 1)

  expect_equal(sfm2[["sim_specs"]][["save_at"]], "1.0")
})

test_that("sim_specs() warns about large dt", {
  sfm <- sdbuildR()
  sfm1 <- build(sfm, "Stock1", type = "stock")

  expect_warning(
    sim_specs(sfm1, dt = 0.5),
    "Large timestep"
  )
})

test_that("sim_specs() returns sdbuildR object", {
  sfm <- sdbuildR()
  sfm1 <- build(sfm, "Stock1", type = "stock")
  sfm2 <- sim_specs(sfm1, start = 0, stop = 10)

  expect_s3_class(sfm2, "sdbuildR")
})

# --- Bug regression: save_at auto-correction must use sfm dt, not default ----

test_that("save_at auto-corrects to sfm dt (not function default) when save_at < sfm dt", {
  sfm <- sdbuildR() |>
    suppressWarnings(sim_specs(dt = 0.5))  # store dt = 0.5

  # Pass only save_at smaller than the stored dt — auto-correction should fire
  sfm2 <- expect_warning(
    sim_specs(sfm, save_at = 0.1),
    "save_at.*equal to.*dt|dt.*save_at"
  )
  # Must be 0.5 (the stored dt), NOT 0.01 (the function signature default)
  expect_equal(as.numeric(sfm2[["sim_specs"]][["save_at"]]), 0.5)
})

# --- New positive-value guards --------------------------------------------------

test_that("sim_specs() rejects non-positive dt", {
  sfm <- sdbuildR()
  expect_error(sim_specs(sfm, dt = 0),    "positive")
  expect_error(sim_specs(sfm, dt = -0.1), "positive")
})

test_that("sim_specs() rejects non-positive save_at", {
  sfm <- sdbuildR()
  expect_error(sim_specs(sfm, dt = 0.01, save_at = 0),  "positive")
  expect_error(sim_specs(sfm, dt = 0.01, save_at = -1), "positive")
})

# --- NSE support ---------------------------------------------------------------

test_that("sim_specs() accepts bare symbols via NSE", {
  sfm <- sdbuildR()

  expect_equal(
    sim_specs(sfm, language = R)[["sim_specs"]][["language"]], "R"
  )
  expect_equal(
    sim_specs(sfm, language = Julia)[["sim_specs"]][["language"]], "Julia"
  )
  expect_equal(
    sim_specs(sfm, method = rk4)[["sim_specs"]][["method"]], "rk4"
  )
  expect_true(
    sim_specs(sfm, time_units = years)[["sim_specs"]][["time_units"]] %in% c("yr", "years")
  )
})

test_that("sim_specs() supports !! injection for NSE args", {
  sfm  <- sdbuildR()
  lang <- "Julia"
  meth <- "rk4"

  expect_equal(
    sim_specs(sfm, language = !!lang)[["sim_specs"]][["language"]], "Julia"
  )
  expect_equal(
    sim_specs(sfm, method = !!meth)[["sim_specs"]][["method"]], "rk4"
  )
})
