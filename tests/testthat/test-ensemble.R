# Tests for ensemble() and plot.ensemble_sdbuildR()

# Input validation (no Julia execution needed) ----------------------------

test_that("ensemble() requires Julia language", {
  sfm <- make_basic_sfm() |> sim_specs(language = "R")
  expect_error(
    ensemble(sfm),
    "only supported for.*Julia",
    ignore.case = TRUE
  )
})

test_that("ensemble() validates n argument", {
  sfm <- make_ensemble_sfm()
  expect_error(ensemble(sfm, n = 0), "must be greater than")
  expect_error(ensemble(sfm, n = -5), "must be greater than")
  expect_error(ensemble(sfm, n = "ten"), "must be.*numeric")
})

test_that("ensemble() validates quantiles argument", {
  sfm <- make_ensemble_sfm()
  expect_error(ensemble(sfm, quantiles = 0.5), "at least.*2.*unique")
  expect_error(ensemble(sfm, quantiles = c(0.5, 0.5)), "at least.*2.*unique")
  expect_error(ensemble(sfm, quantiles = c(-0.1, 0.5)), "between.*0.*and.*1")
  expect_error(ensemble(sfm, quantiles = c(0.5, 1.1)), "between.*0.*and.*1")
  expect_error(ensemble(sfm, quantiles = "high"), "must be.*numeric")
})

test_that("ensemble() validates logical arguments", {
  sfm <- make_ensemble_sfm()
  expect_error(ensemble(sfm, cross = "yes"), "must be.*TRUE.*FALSE")
  expect_error(ensemble(sfm, return_sims = 1), "must be.*TRUE.*FALSE")
  expect_error(ensemble(sfm, only_stocks = "all"), "must be.*TRUE.*FALSE")
})

test_that("ensemble() validates range is a named list", {
  sfm <- make_ensemble_sfm()
  expect_error(ensemble(sfm, range = "S"), "must be a.*list")
  expect_error(ensemble(sfm, range = list()), "at least one parameter")
  expect_error(ensemble(sfm, range = list(0.1, 0.2)), "must be named")
})

test_that("ensemble() validates range elements are numeric", {
  sfm <- make_ensemble_sfm()
  expect_error(
    ensemble(sfm, range = list("S" = "abc")),
    "must be.*numeric"
  )
  expect_error(
    ensemble(sfm, range = list("S" = c(1, 2), "k" = c("a", "b"))),
    "must be.*numeric"
  )
})

test_that("ensemble() validates range names are unique", {
  sfm <- make_ensemble_sfm()
  expect_error(
    ensemble(sfm, range = list("S" = c(1, 2), "S" = c(3, 4))),
    "must be unique"
  )
})

test_that("ensemble() validates range names exist in model", {
  sfm <- make_ensemble_sfm()
  expect_error(
    ensemble(sfm, range = list("nonexistent" = c(1, 2))),
    "do not exist in the model"
  )
})

test_that("ensemble() rejects flows and auxiliaries in range", {
  sfm <- make_ensemble_sfm()
  expect_error(
    ensemble(sfm, range = list("Flow1" = c(1, 2))),
    "Cannot vary flows or auxiliaries"
  )
})

test_that("ensemble() validates equal range lengths when cross = FALSE", {
  sfm <- make_ensemble_sfm()
  expect_error(
    ensemble(sfm,
      range = list("S" = c(1, 2, 3), "k" = c(0.1, 0.2)),
      cross = FALSE
    ),
    "equal length"
  )
})


# Basic ensemble ----------------------------------------

test_that("ensemble() runs successfully", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )

  sims <- silence(ensemble(sfm))
  expect_true(sims[["success"]])
  expect_false(is.null(sims[["summary"]]))
})

test_that("ensemble() returns correct structure", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm, return_sims = TRUE))

  expected_fields <- c(
    "success", "df", "summary", "n", "n_total",
    "n_conditions", "conditions", "init", "constants",
    "script", "duration"
  )
  for (field in expected_fields) {
    expect_true(
      field %in% names(sims),
      info = paste("Missing field:", field)
    )
  }
})

test_that("ensemble() respects only_stocks = TRUE", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  df <- as.data.frame(sfm, properties = "eqn")
  n_stocks <- nrow(df[df[["type"]] == "stock", ])

  sims <- silence(ensemble(sfm, n = 15, 
  only_stocks = TRUE, return_sims = FALSE))
  expect_true(sims[["success"]])
  expect_equal(
    length(unique(sims[["summary"]][["variable"]])),
    n_stocks
  )
})

test_that("ensemble() returns all variables with only_stocks = FALSE", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  df <- as.data.frame(sfm, properties = "eqn")
  n_all <- nrow(df[df[["type"]] %in% c("stock", "flow", "aux"), ])

  sims <- silence(ensemble(sfm, n = 15, only_stocks = FALSE, return_sims = TRUE))
  expect_true(sims[["success"]])
  expect_false(is.null(sims[["summary"]]))
  expect_false(is.null(sims[["df"]]))
  expect_equal(
    length(unique(sims[["summary"]][["variable"]])),
    n_all
  )
})

test_that("ensemble() summary has consistent timeseries lengths", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm, only_stocks = FALSE))

  table_lengths <- with(sims[["summary"]], table(variable))
  expect_equal(length(unique(table_lengths)), 1)
})

test_that("ensemble() returns correct n properties", {
  skip_if_julia_not_ready()

  nr_sims <- 15
  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm, n = nr_sims, return_sims = TRUE))

  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims)
  expect_equal(sims[["n_conditions"]], 1)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1)
})

test_that("ensemble() returns constants and init summaries", {
  skip_if_julia_not_ready()

  nr_sims <- 15
  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm, n = nr_sims, return_sims = TRUE))

  expect_equal(
    sort(unique(sims[["constants"]][["summary"]][["variable"]])),
    c("a0", "a1", "a2")
  )
  expect_equal(
    sort(unique(sims[["init"]][["summary"]][["variable"]])),
    c("Compensatory_behaviour", "Food_intake", "Hunger")
  )
  expect_equal(
    max(as.numeric(sims[["constants"]][["df"]][["i"]])),
    nr_sims
  )
  expect_equal(
    max(as.numeric(sims[["init"]][["df"]][["i"]])),
    nr_sims
  )
})

test_that("ensemble() custom quantiles", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm, quantiles = c(0.1, 0.5, 0.9, 1)))
  expect_true(sims[["success"]])

  q_cols <- grep("^q", colnames(sims[["summary"]]), value = TRUE)
  expect_equal(length(q_cols), 4)
})


# Range / conditions ------------------------------------

test_that("ensemble() works with single variable range", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm,
    range = list("a2" = seq(0.2, 0.8, by = 0.05)),
    n = 10
  ))
  expect_true(sims[["success"]])
})

test_that("ensemble() crossed design computes correct conditions", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  n <- 3
  sims <- silence(ensemble(sfm,
    range = list(
      "a1" = c(1.1, 1.2, 1.3),
      "a2" = c(1.2, 1.3, 1.4)
    ),
    cross = TRUE, n = n, return_sims = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], n)
  expect_equal(sims[["n_total"]], n * 9)
  expect_equal(sims[["n_conditions"]], 9)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1:9)
})

test_that("ensemble() non-crossed design pairs values", {
  skip_if_julia_not_ready()

  nr_sims <- 15
  nr_cond <- 3
  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm,
    range = list(
      "a1" = c(1.1, 1.2, 1.3),
      "a2" = c(1.2, 1.3, 1.4)
    ),
    cross = FALSE, n = nr_sims, return_sims = TRUE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims * nr_cond)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1:nr_cond)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1:nr_cond)
})

test_that("ensemble() non-crossed design conditions data frame", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm,
    range = list(
      "a2" = c(0.2, 0.3, 0.4),
      "a1" = c(1.3, 1.4, 1.5)
    ),
    cross = FALSE, n = 10, return_sims = TRUE
  ))
  expect_true(sims[["success"]])

  cond_df <- as.data.frame(sims[["conditions"]])
  expect_equal(cond_df[["j"]], 1:3)
  # Range is alphabetically sorted, so a1 comes before a2
  expect_equal(cond_df[["a1"]], c(1.3, 1.4, 1.5))
  expect_equal(cond_df[["a2"]], c(0.2, 0.3, 0.4))
  expect_equal(unique(sims[["constants"]][["df"]][["i"]]), 1:10)
  expect_equal(unique(sims[["constants"]][["df"]][["j"]]), 1:3)
})

test_that("ensemble() range parameters are alphabetically sorted", {
  skip_if_julia_not_ready()

  # Regression test: in an earlier version, range parameter order was not preserved
  sfm <- sdbuildR() |>
    sim_specs(language = "Julia") |>
    sim_specs(stop = 12, dt = 0.1, save_at = 1, time_units = "month") |>
    meta(name = "Maya's Burnout") |>
    build("workload", "stock", eqn = 4) |>
    build("new_tasks", "flow",
      eqn = "workload * work_growth", to = "workload"
    ) |>
    build("work_growth", "constant", eqn = 1.5) |>
    build(c("sleep", "necessary_sleep", "worry_factor"),
      c("stock", "constant", "constant"),
      eqn = c("necessary_sleep", 8, 0.1)
    ) |>
    build("worry_about_work", "flow",
      eqn = "workload * worry_factor", from = "sleep"
    ) |>
    build("need_for_rest", "flow",
      eqn = "workload * necessary_sleep / sleep", from = "workload"
    )

  sims <- silence(ensemble(sfm,
    n = 10, return_sims = TRUE,
    range = list(
      "work_growth" = c(1.5),
      "necessary_sleep" = c(8)
    )
  ))
  expect_true(sims[["success"]])

  cond_df <- as.data.frame(sims[["conditions"]])
  expect_true("work_growth" %in% colnames(cond_df))
  expect_true("necessary_sleep" %in% colnames(cond_df))
  expect_equal(cond_df[["work_growth"]], 1.5)
  expect_equal(cond_df[["necessary_sleep"]], 8)
})

test_that("ensemble() with mixed stock and constant in range", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("predator_prey") |>
    build(c("predator", "prey"), eqn = "runif(1, 30, 50)") |>
    sim_specs(
      language = "Julia",
      dt = 0.1, save_at = 10,
      start = 0, stop = 200
    )

  nr_sims <- 5
  sims <- silence(ensemble(sfm,
    range = list(
      "prey" = c(40, 50, 60),
      "delta" = seq(0.015, 0.03, by = 0.005)
    ),
    cross = TRUE, n = nr_sims, return_sims = TRUE,
    only_stocks = TRUE
  ))
  expect_true(sims[["success"]])
  expect_false(is.null(sims[["summary"]]))
  expect_false(is.null(sims[["df"]]))
  expect_equal(length(unique(sims[["summary"]][["variable"]])), 2) # 2 stocks

  nr_cond <- 3 * 4
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_cond * nr_sims)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1:nr_cond)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1:nr_cond)
})


# Reproducibility ----------------------------------------

test_that("ensemble() is reproducible with seed", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("predator_prey") |>
    sim_specs(
      language = "Julia",
      start = 0, stop = 10, dt = 0.1,
      seed = 123
    ) |>
    build(c("predator", "prey"), eqn = "runif(1)")

  sims1 <- silence(ensemble(sfm, return_sims = TRUE))
  sims2 <- silence(ensemble(sfm, return_sims = TRUE))

  expect_equal(sims1[["df"]], sims2[["df"]])
  expect_equal(sims1[["summary"]], sims2[["summary"]])
})


# Edge cases --------------------------------------------

test_that("ensemble() works with single time point", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("predator_prey") |>
    sim_specs(
      language = "Julia",
      start = 0, stop = 5, dt = 0.1,
      save_n = 1
    ) |>
    build(c("predator", "prey"), eqn = "runif(1)")

  sims <- silence(ensemble(sfm))
  expect_true(sims[["success"]])
  expect_equal(length(unique(sims[["summary"]][["time"]])), 1)
  expect_no_error(expect_no_warning(expect_no_message(plot(sims))))

  # Also works with return_sims
  sims <- silence(ensemble(sfm, return_sims = TRUE))
  expect_true(sims[["success"]])
  expect_no_error(expect_no_warning(expect_no_message(plot(sims))))
  expect_no_error(expect_no_warning(expect_no_message(plot(sims, type = "sims"))))
})

test_that("ensemble() works with units", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("coffee_cup") |>
    sim_specs(language = "Julia", stop = 10, dt = 0.1) |>
    build("coffee_temperature", eqn = "runif(1, 20, 150)")

  nr_sims <- 15
  sims <- silence(ensemble(sfm, n = nr_sims, only_stocks = FALSE, return_sims = TRUE))
  expect_true(sims[["success"]])
  expect_false(is.null(sims[["summary"]]))
  expect_false(is.null(sims[["df"]]))
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1)
  expect_no_error(expect_no_message(plot(sims)))
  expect_no_error(expect_no_message(plot(sims, type = "sims")))
})

test_that("ensemble() works with interpolation function", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("logistic_model") |>
    sim_specs(
      language = "Julia",
      start = 0, stop = 50, dt = 0.1,
      save_n = 1
    ) |>
    build("X", eqn = "runif(1, 0, K)") |>
    build("input", "constant", eqn = "pulse(times, 10, width = dt, height = .01)") |>
    build("inflow2", "flow", eqn = "input(t)", to = "X")

  sims <- silence(ensemble(sfm))
  expect_true(sims[["success"]])
  expect_no_error(plot(sims))
})


# Verbose messages ----------------------------------------

test_that("ensemble() prints simulation count", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )

  # Basic ensemble
  expect_message(
    ensemble(sfm, n = 10, verbose = TRUE),
    "Running"
  )

  # Ensemble with range: 3 x 3 = 9 conditions, 15 per condition = 135 total
  expect_message(
    ensemble(sfm,
      range = list(
        "a1" = c(1.1, 1.2, 1.3),
        "a2" = c(1.2, 1.3, 1.4)
      ),
      cross = TRUE, n = 15, verbose = TRUE,
      return_sims = TRUE
    ),
    "135"
  )
})


# plot.ensemble_sdbuildR() --------------------------------

test_that("plot.ensemble_sdbuildR() renders summary plot", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm, n = 10))

  expect_no_error(expect_no_message(plot(sims)))
  expect_no_error(plot(sims, j = 1))
})

test_that("plot.ensemble_sdbuildR() rejects invalid type", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm))

  expect_error(plot(sims, type = "NA"), "must be.*summary.*sims")
})

test_that("plot.ensemble_sdbuildR() rejects invalid central_tendency", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm))

  expect_no_error(plot(sims, central_tendency = "median"))
  expect_error(
    plot(sims, central_tendency = "medians"),
    "must be.*mean.*median.*FALSE"
  )
})

test_that("plot.ensemble_sdbuildR() informs when i used with summary type", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm, n = 10, return_sims = TRUE))

  expect_message(
    plot(sims, i = 5),
    "i.*argument is ignored"
  )
})

test_that("plot.ensemble_sdbuildR() validates j index with single condition", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm))

  expect_error(
    plot(sims, j = c(3, 6, 9)),
    "only one condition"
  )
})

test_that("plot.ensemble_sdbuildR() validates j index with multiple conditions", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  nr_cond <- 3
  sims <- silence(ensemble(sfm,
    range = list(
      "a1" = c(1.1, 1.2, 1.3),
      "a2" = c(1.2, 1.3, 1.4)
    ),
    cross = FALSE, n = 15, return_sims = TRUE
  ))

  expect_error(
    plot(sims, j = nr_cond + 1),
    "must.*contain integers between"
  )
})

test_that("plot.ensemble_sdbuildR() requires return_sims for type = 'sims'", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm, return_sims = FALSE))

  expect_error(
    plot(sims, type = "sims"),
    "Individual simulation data is required"
  )
})

test_that("plot.ensemble_sdbuildR() renders sims plot", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  nr_sims <- 10
  sims <- silence(ensemble(sfm, n = nr_sims, return_sims = TRUE))

  expect_no_error(plot(sims, type = "sims", i = nr_sims - 1))
  expect_no_error(plot(sims, type = "sims"))
})

test_that("plot.ensemble_sdbuildR() renders with specific j and i", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("Crielaard2022") |> sim_specs(
    language = "Julia",
    start = 0, stop = 10, dt = 0.1,
    save_at = 1
  )
  sims <- silence(ensemble(sfm,
    range = list(
      "a1" = c(1.1, 1.2, 1.3),
      "a2" = c(1.2, 1.3, 1.4)
    ),
    cross = TRUE, n = 3, return_sims = TRUE
  ))

  expect_no_error(expect_no_message(plot(sims)))
  expect_no_error(plot(sims, j = c(3, 5, 8), nrows = 4))
  expect_no_error(plot(sims, i = 1:3, j = 3:8, type = "summary"))
  expect_no_error(expect_no_message(plot(sims, i = 1:2, type = "sims")))
  expect_no_error(plot(sims, j = 1:3, type = "sims"))
})


# Snapshot tests for error messages ----------------------------------------

cli::test_that_cli(configs = c("plain", "ansi"), "ensemble() error: invalid n", {
  sfm <- make_ensemble_sfm()
  expect_snapshot(ensemble(sfm, n = 0), error = TRUE)
})

cli::test_that_cli(configs = c("plain", "ansi"), "ensemble() error: invalid quantiles", {
  sfm <- make_ensemble_sfm()
  expect_snapshot(ensemble(sfm, quantiles = 0.5), error = TRUE)
})

cli::test_that_cli(configs = c("plain", "ansi"), "ensemble() error: non-numeric range", {
  sfm <- make_ensemble_sfm()
  expect_snapshot(ensemble(sfm, range = list("S" = "abc")), error = TRUE)
})

