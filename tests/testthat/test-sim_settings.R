test_that("sim_settings() modifies start time", {
  sfm <- sdbuildR() |>
    sim_settings(start = 10)

  expect_equal(as.numeric(sfm$sim_settings$start), 10)
})

test_that("sim_settings() modifies stop time", {
  sfm <- sdbuildR() |>
    sim_settings(stop = 200)

  expect_equal(as.numeric(sfm$sim_settings$stop), 200)
})

test_that("sim_settings() modifies dt", {
  sfm <- sdbuildR() |>
    sim_settings(dt = 0.1)

  expect_equal(as.numeric(sfm$sim_settings$dt), 0.1)
})

test_that("sim_settings() modifies method", {
  sfm <- sdbuildR() |>
    sim_settings(method = "rk4")

  expect_equal(sfm$sim_settings$method, "rk4")
})

test_that("sim_settings() preserves other fields", {
  sfm <- sdbuildR() |>
    sim_settings(start = 5)

  expect_true("stop" %in% names(sfm$sim_settings))
  expect_true("dt" %in% names(sfm$sim_settings))
})

test_that("sim_settings() returns sdbuildR object", {
  sfm <- sdbuildR() |>
    sim_settings(start = 0)

  expect_s3_class(sfm, "sdbuildR")
})

test_that("sim_settings() handles language parameter", {
  sfm <- sdbuildR() |>
    sim_settings(language = "R")

  expect_equal(sfm$sim_settings$language, "R")

  sfm <- sdbuildR() |>
    sim_settings(language = "Julia")

  expect_equal(sfm$sim_settings$language, "Julia")
})

test_that("sim_settings() modifies seed", {
  sfm <- sdbuildR() |>
    sim_settings(seed = 123)

  expect_equal(as.numeric(sfm$sim_settings$seed), 123)
})

test_that("sim_settings() stores vars and preserves order after deduplication", {
  sfm <- sdbuildR("SIR") |>
    sim_settings(vars = c("susceptible", "infected", "susceptible"))

  expect_equal(sfm$sim_settings$vars, c("susceptible", "infected"))
})

test_that("sim_settings() rejects unknown vars", {
  sfm <- sdbuildR("SIR")
  expect_error(sim_settings(sfm, vars = c("does_not_exist")), "Invalid variable name")
})


cli::test_that_cli(configs = c("plain", "ansi"), "clean_language() rejects invalid language", {
  expect_snapshot(clean_language("python"), error = TRUE)
  expect_snapshot(clean_language("cpp"), error = TRUE)
})


test_that("sim_settings() sets basic parameters", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- suppressWarnings(sim_settings(sfm1, start = 0, stop = 10, dt = 0.5))

  expect_equal(sfm2[["sim_settings"]][["start"]], "0.0")
  expect_equal(sfm2[["sim_settings"]][["stop"]], "10.0")
  expect_equal(sfm2[["sim_settings"]][["dt"]], "0.5")
})

test_that("sim_settings() validates start < stop", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_error(
    sim_settings(sfm1, start = 10, stop = 5),
    "start.*smaller than.*stop"
  )
})

test_that("sim_settings() validates numeric parameters", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_error(sim_settings(sfm1, start = "abc"), "numeric")
  expect_error(sim_settings(sfm1, stop = "xyz"), "numeric")
  expect_error(sim_settings(sfm1, dt = "foo"), "numeric")
})

test_that("sim_settings() validates language parameter", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_no_error(sim_settings(sfm1, language = "R"))
  expect_no_error(sim_settings(sfm1, language = "Julia"))
})

test_that("sim_settings() validates method parameter", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_no_error(sim_settings(sfm1, method = "euler", language = "R"))
  expect_no_error(sim_settings(sfm1, method = "rk4", language = "R"))
})

test_that("sim_settings() handles time_units", {
  sfm <- sdbuildR()

  sfm <- expect_no_error(sim_settings(sfm, time_units = "days"))
  expect_equal(sfm[["sim_settings"]][["time_units"]], "days")
  sfm <- expect_no_error(sim_settings(sfm, time_units = "hours"))
  expect_equal(sfm[["sim_settings"]][["time_units"]], "hours")
  sfm <- expect_no_error(sim_settings(sfm, time_units = "years"))
  expect_equal(sfm[["sim_settings"]][["time_units"]], "years")
})

test_that("sim_settings() warns about large dt", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")

  expect_warning(
    sim_settings(sfm1, dt = 0.5),
    "Large timestep"
  )
})

test_that("sim_settings() returns sdbuildR object", {
  sfm <- sdbuildR()
  sfm1 <- update(sfm, "Stock1", type = "stock")
  sfm2 <- sim_settings(sfm1, start = 0, stop = 10)

  expect_s3_class(sfm2, "sdbuildR")
})

# --- New positive-value guards --------------------------------------------------

test_that("sim_settings() rejects non-positive dt", {
  sfm <- sdbuildR()
  expect_error(sim_settings(sfm, dt = 0), "positive")
  expect_error(sim_settings(sfm, dt = -0.1), "positive")
})

test_that("sim_settings() rejects non-positive save_at", {
  sfm <- sdbuildR()
  expect_error(sim_settings(sfm, save_at = 0), "positive")
  expect_error(sim_settings(sfm, save_at = -1), "positive")
})

# --- NSE support ---------------------------------------------------------------


test_that("sim_settings() method and language accept string variables directly", {
  sfm <- sdbuildR()
  lang <- "Julia"
  meth <- "rk4"

  expect_equal(
    sim_settings(sfm, language = lang)[["sim_settings"]][["language"]], "Julia"
  )
  expect_equal(
    sim_settings(sfm, method = meth)[["sim_settings"]][["method"]], "rk4"
  )
})

# --- Save parameter: defaults ---------------------------------------------------

test_that("new sdbuildR() has save_type = 'all' and NULL save fields", {
  sfm <- sdbuildR()
  expect_equal(sfm[["sim_settings"]][["save_type"]], "all")
  expect_null(sfm[["sim_settings"]][["save_at"]])
  expect_null(sfm[["sim_settings"]][["save_n"]])
})

# --- Save parameter: save_at scalar (interval) ----------------------------------

test_that("sim_settings() stores save_at interval", {
  sfm2 <- sim_settings(sdbuildR(), save_at = 1)
  expect_equal(sfm2[["sim_settings"]][["save_type"]], "save_at")
  expect_equal(sfm2[["sim_settings"]][["save_at"]], "1.0")
  expect_null(sfm2[["sim_settings"]][["save_n"]])
})

test_that("sim_settings() auto-corrects save_at < dt", {
  expect_warning(
    sfm2 <- sim_settings(sdbuildR(), save_at = 0.001),
    "Automatically setting"
  )
  expect_equal(
    as.numeric(sfm2[["sim_settings"]][["save_at"]]),
    as.numeric(sfm2[["sim_settings"]][["dt"]])
  )
})

test_that("sim_settings() warns on save_at interval misalignment with stop", {
  # stop = 10, save_at = 3: 10 %% 3 = 1, stop not aligned
  expect_warning(
    sim_settings(sdbuildR(), stop = 10, save_at = 3),
    "Endpoint may be missing"
  )
})

test_that("sim_settings() does not warn when save_at aligns with stop", {
  # stop = 10, save_at = 2: 10 %% 2 = 0, aligned
  expect_no_warning(sim_settings(sdbuildR(), stop = 10, save_at = 2))
})

# --- Save parameter: save_at vector (explicit times) ----------------------------

test_that("sim_settings() stores save_at vector", {
  sfm2 <- sim_settings(sdbuildR(), save_at = c(1, 5, 10))
  expect_equal(sfm2[["sim_settings"]][["save_type"]], "save_at")
  expect_equal(length(sfm2[["sim_settings"]][["save_at"]]), 3L)
  expect_null(sfm2[["sim_settings"]][["save_n"]])
})

test_that("sim_settings() rejects save_at vector with out-of-range values", {
  expect_error(sim_settings(sdbuildR(), save_at = c(1, 200)), "out-of-range|within")
})

test_that("sim_settings() sorts and deduplicates save_at vector", {
  sfm2 <- sim_settings(sdbuildR(), save_at = c(10, 1, 5, 1))
  vals <- as.numeric(sfm2[["sim_settings"]][["save_at"]])
  expect_equal(vals, c(1, 5, 10))
})

# --- Save parameter: save_n -----------------------------------------------------

test_that("sim_settings() stores save_n", {
  sfm2 <- sim_settings(sdbuildR(), save_n = 100)
  expect_equal(sfm2[["sim_settings"]][["save_type"]], "save_n")
  expect_equal(as.integer(sfm2[["sim_settings"]][["save_n"]]), 100L)
  expect_null(sfm2[["sim_settings"]][["save_at"]])
})

test_that("sim_settings() save_n = 1 stores correctly", {
  sfm2 <- sim_settings(sdbuildR(), save_n = 1)
  expect_equal(sfm2[["sim_settings"]][["save_type"]], "save_n")
  expect_equal(as.integer(sfm2[["sim_settings"]][["save_n"]]), 1L)
})

test_that("sim_settings() rejects invalid save_n", {
  expect_error(sim_settings(sdbuildR(), save_n = 0), "positive integer")
  expect_error(sim_settings(sdbuildR(), save_n = -1), "positive integer")
  expect_error(sim_settings(sdbuildR(), save_n = "abc"), "positive integer")
})

# --- Save parameter: mutual exclusion -------------------------------------------

test_that("sim_settings() errors when both save_at and save_n provided", {
  expect_error(sim_settings(sdbuildR(), save_at = 1, save_n = 100), "Cannot specify both")
})

test_that("sim_settings() allows both save_at and save_n when one is NA", {
  expect_no_error(sim_settings(sdbuildR(), save_at = NA, save_n = 100))
  expect_no_error(sim_settings(sdbuildR(), save_at = 1, save_n = NA))
})

# --- Save parameter: reset to "all" with NA/NULL/"" ----------------------------

test_that("sim_settings() resetting save_at to NA gives save_type = all", {
  sfm <- sim_settings(sdbuildR(), save_at = 1)
  sfm2 <- sim_settings(sfm, save_at = NA)
  expect_equal(sfm2[["sim_settings"]][["save_type"]], "all")
  expect_null(sfm2[["sim_settings"]][["save_at"]])
  expect_null(sfm2[["sim_settings"]][["save_n"]])
})

test_that("sim_settings() resetting save_n to NA gives save_type = all", {
  sfm <- sim_settings(sdbuildR(), save_n = 50)
  sfm2 <- sim_settings(sfm, save_n = NA)
  expect_equal(sfm2[["sim_settings"]][["save_type"]], "all")
  expect_null(sfm2[["sim_settings"]][["save_at"]])
  expect_null(sfm2[["sim_settings"]][["save_n"]])
})

# --- Save parameter: overwriting -----------------------------------------------

test_that("sim_settings() save_n overwrites previous save_at", {
  sfm <- sim_settings(sdbuildR(), save_at = 1)
  sfm2 <- sim_settings(sfm, save_n = 50)
  expect_equal(sfm2[["sim_settings"]][["save_type"]], "save_n")
  expect_equal(as.integer(sfm2[["sim_settings"]][["save_n"]]), 50L)
  expect_null(sfm2[["sim_settings"]][["save_at"]])
})

test_that("sim_settings() save_at overwrites previous save_n", {
  sfm <- sim_settings(sdbuildR(), save_n = 50)
  sfm2 <- sim_settings(sfm, save_at = 2)
  expect_equal(sfm2[["sim_settings"]][["save_type"]], "save_at")
  expect_null(sfm2[["sim_settings"]][["save_n"]])
})

test_that("sim_settings() default save_sims is FALSE on new model", {
  sfm <- sdbuildR("SIR")
  expect_false(isTRUE(sfm[["sim_settings"]][["save_sims"]]))
})

test_that("verify() always retains sims regardless of save_sims", {
  sfm <- sdbuildR("SIR") |>
    unit_test(expr = all(susceptible >= 0))

  res <- verify(sfm)
  expect_s3_class(res, "verify_sdbuildR")
  expect_true(!is.null(res$sims)) # always present, no save_sims needed
})

test_that("ensemble respects sim_settings save_sims and per-call override via ...", {
  sfm <- sdbuildR("SIR") |>
    sim_settings(save_sims = TRUE)

  ens_keep <- ensemble(sfm, n = 2)
  expect_true(!is.null(ens_keep$df))

  ens_drop <- ensemble(sfm, n = 2, save_sims = FALSE)
  expect_null(ens_drop$df)
})

test_that("sim_settings() rejects invalid save_sims", {
  expect_error(sim_settings(sdbuildR(), save_sims = "notlogical"), "Invalid")
})
