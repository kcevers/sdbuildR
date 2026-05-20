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

test_that("sim_specs() stores vars and preserves order after deduplication", {
  sfm <- sdbuildR("SIR") |>
    sim_specs(vars = c("Susceptible", "Infected", "Susceptible"))

  expect_equal(sfm$sim_specs$vars, c("Susceptible", "Infected"))
})

test_that("sim_specs() rejects vars that are not time-varying", {
  sfm <- sdbuildR("SIR")
  expect_error(sim_specs(sfm, vars = c("Beta")), "time-varying")
})

test_that("sim_specs() rejects unknown vars", {
  sfm <- sdbuildR("SIR")
  expect_error(sim_specs(sfm, vars = c("does_not_exist")), "Unsupported|unknown")
})


cli::test_that_cli(configs = c("plain", "ansi"), "clean_language() rejects invalid language", {
  expect_snapshot(clean_language("python"), error = TRUE)
  expect_snapshot(clean_language("cpp"), error = TRUE)
})


test_that("sim_specs() sets basic parameters", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- suppressWarnings(sim_specs(sfm1, start = 0, stop = 10, dt = 0.5))

  expect_equal(sfm2[["sim_specs"]][["start"]], "0.0")
  expect_equal(sfm2[["sim_specs"]][["stop"]], "10.0")
  expect_equal(sfm2[["sim_specs"]][["dt"]], "0.5")
})

test_that("sim_specs() validates start < stop", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_error(
    sim_specs(sfm1, start = 10, stop = 5),
    "start.*smaller than.*stop"
  )
})

test_that("sim_specs() validates numeric parameters", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_error(sim_specs(sfm1, start = "abc"), "numeric")
  expect_error(sim_specs(sfm1, stop = "xyz"), "numeric")
  expect_error(sim_specs(sfm1, dt = "foo"), "numeric")
})

test_that("sim_specs() validates language parameter", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_no_error(sim_specs(sfm1, language = "R"))
  expect_no_error(sim_specs(sfm1, language = "Julia"))
})

test_that("sim_specs() validates method parameter", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_no_error(sim_specs(sfm1, method = "euler", language = "R"))
  expect_no_error(sim_specs(sfm1, method = "rk4", language = "R"))
})

test_that("sim_specs() handles time_units", {
  sfm <- sdbuildR()

  sfm <- expect_no_error(sim_specs(sfm, time_units = "days"))
  expect_equal(sfm[["sim_specs"]][["time_units"]], "days")
  sfm <- expect_no_error(sim_specs(sfm, time_units = "hours"))
  expect_equal(sfm[["sim_specs"]][["time_units"]], "hours")
  sfm <- expect_no_error(sim_specs(sfm, time_units = "years"))
  expect_equal(sfm[["sim_specs"]][["time_units"]], "years")
})

test_that("sim_specs() warns about large dt", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_warning(
    sim_specs(sfm1, dt = 0.5),
    "Large timestep"
  )
})

test_that("sim_specs() returns sdbuildR object", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- sim_specs(sfm1, start = 0, stop = 10)

  expect_s3_class(sfm2, "sdbuildR")
})

# --- New positive-value guards --------------------------------------------------

test_that("sim_specs() rejects non-positive dt", {
  sfm <- sdbuildR()
  expect_error(sim_specs(sfm, dt = 0), "positive")
  expect_error(sim_specs(sfm, dt = -0.1), "positive")
})

test_that("sim_specs() rejects non-positive save_at", {
  sfm <- sdbuildR()
  expect_error(sim_specs(sfm, save_at = 0), "positive")
  expect_error(sim_specs(sfm, save_at = -1), "positive")
})

# --- NSE support ---------------------------------------------------------------


test_that("sim_specs() supports !! injection for NSE args", {
  sfm <- sdbuildR()
  lang <- "Julia"
  meth <- "rk4"

  expect_equal(
    sim_specs(sfm, language = !!lang)[["sim_specs"]][["language"]], "Julia"
  )
  expect_equal(
    sim_specs(sfm, method = !!meth)[["sim_specs"]][["method"]], "rk4"
  )
})

# --- Save parameter: defaults ---------------------------------------------------

test_that("new sdbuildR() has save_type = 'all' and NULL save fields", {
  sfm <- sdbuildR()
  expect_equal(sfm[["sim_specs"]][["save_type"]], "all")
  expect_null(sfm[["sim_specs"]][["save_at"]])
  expect_null(sfm[["sim_specs"]][["save_n"]])
})

# --- Save parameter: save_at scalar (interval) ----------------------------------

test_that("sim_specs() stores save_at interval", {
  sfm2 <- sim_specs(sdbuildR(), save_at = 1)
  expect_equal(sfm2[["sim_specs"]][["save_type"]], "save_at")
  expect_equal(sfm2[["sim_specs"]][["save_at"]], "1.0")
  expect_null(sfm2[["sim_specs"]][["save_n"]])
})

test_that("sim_specs() auto-corrects save_at < dt", {
  expect_warning(
    sfm2 <- sim_specs(sdbuildR(), save_at = 0.001),
    "Automatically setting"
  )
  expect_equal(
    as.numeric(sfm2[["sim_specs"]][["save_at"]]),
    as.numeric(sfm2[["sim_specs"]][["dt"]])
  )
})

test_that("sim_specs() warns on save_at interval misalignment with stop", {
  # stop = 10, save_at = 3: 10 %% 3 = 1, stop not aligned
  expect_warning(
    sim_specs(sdbuildR(), stop = 10, save_at = 3),
    "Endpoint may be missing"
  )
})

test_that("sim_specs() does not warn when save_at aligns with stop", {
  # stop = 10, save_at = 2: 10 %% 2 = 0, aligned
  expect_no_warning(sim_specs(sdbuildR(), stop = 10, save_at = 2))
})

# --- Save parameter: save_at vector (explicit times) ----------------------------

test_that("sim_specs() stores save_at vector", {
  sfm2 <- sim_specs(sdbuildR(), save_at = c(1, 5, 10))
  expect_equal(sfm2[["sim_specs"]][["save_type"]], "save_at")
  expect_equal(length(sfm2[["sim_specs"]][["save_at"]]), 3L)
  expect_null(sfm2[["sim_specs"]][["save_n"]])
})

test_that("sim_specs() rejects save_at vector with out-of-range values", {
  expect_error(sim_specs(sdbuildR(), save_at = c(1, 200)), "out-of-range|within")
})

test_that("sim_specs() sorts and deduplicates save_at vector", {
  sfm2 <- sim_specs(sdbuildR(), save_at = c(10, 1, 5, 1))
  vals <- as.numeric(sfm2[["sim_specs"]][["save_at"]])
  expect_equal(vals, c(1, 5, 10))
})

# --- Save parameter: save_n -----------------------------------------------------

test_that("sim_specs() stores save_n", {
  sfm2 <- sim_specs(sdbuildR(), save_n = 100)
  expect_equal(sfm2[["sim_specs"]][["save_type"]], "save_n")
  expect_equal(as.integer(sfm2[["sim_specs"]][["save_n"]]), 100L)
  expect_null(sfm2[["sim_specs"]][["save_at"]])
})

test_that("sim_specs() save_n = 1 stores correctly", {
  sfm2 <- sim_specs(sdbuildR(), save_n = 1)
  expect_equal(sfm2[["sim_specs"]][["save_type"]], "save_n")
  expect_equal(as.integer(sfm2[["sim_specs"]][["save_n"]]), 1L)
})

test_that("sim_specs() rejects invalid save_n", {
  expect_error(sim_specs(sdbuildR(), save_n = 0), "positive integer")
  expect_error(sim_specs(sdbuildR(), save_n = -1), "positive integer")
  expect_error(sim_specs(sdbuildR(), save_n = "abc"), "positive integer")
})

# --- Save parameter: mutual exclusion -------------------------------------------

test_that("sim_specs() errors when both save_at and save_n provided", {
  expect_error(sim_specs(sdbuildR(), save_at = 1, save_n = 100), "Cannot specify both")
})

test_that("sim_specs() allows both save_at and save_n when one is NA", {
  expect_no_error(sim_specs(sdbuildR(), save_at = NA, save_n = 100))
  expect_no_error(sim_specs(sdbuildR(), save_at = 1, save_n = NA))
})

# --- Save parameter: reset to "all" with NA/NULL/"" ----------------------------

test_that("sim_specs() resetting save_at to NA gives save_type = all", {
  sfm <- sim_specs(sdbuildR(), save_at = 1)
  sfm2 <- sim_specs(sfm, save_at = NA)
  expect_equal(sfm2[["sim_specs"]][["save_type"]], "all")
  expect_null(sfm2[["sim_specs"]][["save_at"]])
  expect_null(sfm2[["sim_specs"]][["save_n"]])
})

test_that("sim_specs() resetting save_n to NA gives save_type = all", {
  sfm <- sim_specs(sdbuildR(), save_n = 50)
  sfm2 <- sim_specs(sfm, save_n = NA)
  expect_equal(sfm2[["sim_specs"]][["save_type"]], "all")
  expect_null(sfm2[["sim_specs"]][["save_at"]])
  expect_null(sfm2[["sim_specs"]][["save_n"]])
})

# --- Save parameter: overwriting -----------------------------------------------

test_that("sim_specs() save_n overwrites previous save_at", {
  sfm <- sim_specs(sdbuildR(), save_at = 1)
  sfm2 <- sim_specs(sfm, save_n = 50)
  expect_equal(sfm2[["sim_specs"]][["save_type"]], "save_n")
  expect_equal(as.integer(sfm2[["sim_specs"]][["save_n"]]), 50L)
  expect_null(sfm2[["sim_specs"]][["save_at"]])
})

test_that("sim_specs() save_at overwrites previous save_n", {
  sfm <- sim_specs(sdbuildR(), save_n = 50)
  sfm2 <- sim_specs(sfm, save_at = 2)
  expect_equal(sfm2[["sim_specs"]][["save_type"]], "save_at")
  expect_null(sfm2[["sim_specs"]][["save_n"]])
})
