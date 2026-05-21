# Tests for ensemble() and plot.ensemble_sdbuildR()

# Input validation ----------------------------

test_that("ensemble() passes additional arguments (language) to sim_specs", {
  sfm <- make_basic_sfm()
  expect_error(
    ensemble(sfm, language = "unsupported"),
    "Invalid.*language",
    ignore.case = TRUE
  )
})

test_that("ensemble() validates n argument", {
  sfm <- make_ensemble_error_sfm()
  expect_error(ensemble(sfm, n = 0), "must be greater than")
  expect_error(ensemble(sfm, n = -5), "must be greater than")
  expect_error(ensemble(sfm, n = "ten"), "must be.*numeric")
})

test_that("ensemble() validates quantiles argument", {
  sfm <- make_ensemble_error_sfm()
  expect_error(ensemble(sfm, quantiles = 0.5), "at least.*2.*unique")
  expect_error(ensemble(sfm, quantiles = c(0.5, 0.5)), "at least.*2.*unique")
  expect_error(ensemble(sfm, quantiles = c(-0.1, 0.5)), "between.*0.*and.*1")
  expect_error(ensemble(sfm, quantiles = c(0.5, 1.1)), "between.*0.*and.*1")
  expect_error(ensemble(sfm, quantiles = "high"), "must be.*numeric")
})

test_that("ensemble() validates logical arguments", {
  sfm <- make_ensemble_error_sfm()
  expect_error(ensemble(sfm, cross = "yes"), "must be.*TRUE.*FALSE")
  expect_error(ensemble(sfm, return_sims = 1), "must be.*TRUE.*FALSE")
  expect_error(ensemble(sfm, only_stocks = "all"), "must be.*TRUE.*FALSE")
})

test_that("ensemble() validates conditions is a named list", {
  sfm <- make_ensemble_error_sfm()
  expect_error(ensemble(sfm, conditions = "S"), "must be a.*list")
  expect_error(ensemble(sfm, conditions = list()), "at least one parameter")
  expect_error(ensemble(sfm, conditions = list(0.1, 0.2)), "must be named")
})

test_that("ensemble() validates conditions elements are numeric", {
  sfm <- make_ensemble_error_sfm()
  expect_error(
    ensemble(sfm, conditions = list("S" = "abc")),
    "must be.*numeric"
  )
  expect_error(
    ensemble(sfm, conditions = list("S" = c(1, 2), "k" = c("a", "b"))),
    "must be.*numeric"
  )
})

test_that("ensemble() validates conditions names are unique", {
  sfm <- make_ensemble_error_sfm()
  expect_error(
    ensemble(sfm, conditions = list("S" = c(1, 2), "S" = c(3, 4))),
    "must be unique"
  )
})

test_that("ensemble() validates conditions names exist in model", {
  sfm <- make_ensemble_error_sfm()
  expect_error(
    ensemble(sfm, conditions = list("nonexistent" = c(1, 2))),
    "do not exist in the model"
  )
})

test_that("ensemble() rejects flows and auxiliaries in conditions", {
  sfm <- make_ensemble_error_sfm()
  expect_error(
    ensemble(sfm, conditions = list("Flow1" = c(1, 2))),
    "Cannot vary flows or auxiliaries"
  )
})

test_that("ensemble() validates equal conditions lengths when cross = FALSE", {
  sfm <- make_ensemble_error_sfm()
  expect_error(
    ensemble(sfm,
      conditions = list("S" = c(1, 2, 3), "k" = c(0.1, 0.2)),
      cross = FALSE
    ),
    "equal length"
  )
})


# Basic ensemble ----------------------------------------

test_that("ensemble() runs successfully", {
  skip_if_julia_not_ready()
  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm, n = 3))
  expect_true(sims[["success"]])
  expect_false(is.null(sims[["summary"]]))
})

test_that("ensemble() returns correct structure", {
  skip_if_julia_not_ready()
  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm, n = 3, return_sims = TRUE))

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

test_that("ensemble() handles models with no constants", {
  skip_if_julia_not_ready()
  sfm <- make_basic_sfm() |>
    sim_specs(language = "jl", start = 0, stop = 10, dt = 0.1, save_at = 1)

  sims <- silence(ensemble(sfm, n = 3, return_sims = TRUE, verbose = FALSE))

  expect_true(sims[["success"]])
  expect_equal(nrow(sims[["constants"]][["df"]]), 0)
  expect_equal(nrow(sims[["constants"]][["summary"]]), 0)
  expect_equal(
    sort(names(sims[["constants"]][["df"]])),
    c("i", "j", "value", "variable")
  )
  expect_true(
    all(c("j", "variable", "mean") %in%
      names(sims[["constants"]][["summary"]]))
  )
})


test_that("ensemble() of model with only stocks", {
  skip_if_julia_not_ready()
  sfm <- sdbuildR() |>
    stock("Stock1", eqn = 100) |>
    stock("Stock2", eqn = 50) |>
    sim_specs(language = "Julia", start = 0, stop = 10, dt = 0.1, save_at = 1)

  sims <- silence(ensemble(sfm, n = 3, only_stocks = TRUE, return_sims = TRUE, verbose = FALSE))
  expect_true(sims[["success"]])
  expect_equal(
    sort(unique(sims[["summary"]][["variable"]])),
    c("Stock1", "Stock2")
  )
})


test_that("ensemble() respects only_stocks = TRUE", {
  skip_if_julia_not_ready()
  sfm <- make_jl_ensemble_sfm()
  df <- as.data.frame(sfm, properties = "eqn")
  n_stocks <- nrow(df[df[["type"]] == "stock", ])

  sims <- silence(ensemble(sfm,
    n = 3,
    only_stocks = TRUE, return_sims = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(
    length(unique(sims[["summary"]][["variable"]])),
    n_stocks
  )
})

test_that("ensemble() returns all variables with only_stocks = FALSE", {
  skip_if_julia_not_ready()
  sfm <- make_jl_ensemble_sfm()

  df <- as.data.frame(sfm, properties = "eqn")
  n_all <- nrow(df[df[["type"]] %in% c("stock", "flow", "aux"), ])

  sims <- silence(ensemble(sfm, n = 3, only_stocks = FALSE, return_sims = TRUE))
  expect_true(sims[["success"]])
  expect_false(is.null(sims[["summary"]]))
  expect_false(is.null(sims[["df"]]))
  expect_equal(
    length(unique(sims[["summary"]][["variable"]])),
    n_all
  )
})

test_that("ensemble() with Julia filters outputs to vars", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm() |>
    sim_specs(vars = c("Food_intake", "Hunger"))

  sims <- silence(ensemble(sfm, n = 3, return_sims = TRUE))

  expect_true(sims[["success"]])
  expect_equal(sort(unique(sims[["summary"]][["variable"]])), c("Food_intake", "Hunger"))
  expect_equal(sort(unique(sims[["df"]][["variable"]])), c("Food_intake", "Hunger"))
})

test_that("ensemble() with Julia vars overrides only_stocks", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm() |>
    sim_specs(only_stocks = TRUE, vars = c("Satiety"))

  sims <- silence(ensemble(sfm, n = 3, return_sims = TRUE))

  expect_true(sims[["success"]])
  expect_equal(unique(sims[["summary"]][["variable"]]), "Satiety")
  expect_equal(unique(sims[["df"]][["variable"]]), "Satiety")
})

test_that("ensemble() summary has consistent timeseries lengths", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm, n = 3, only_stocks = FALSE))

  table_lengths <- with(sims[["summary"]], table(variable))
  expect_equal(length(unique(table_lengths)), 1)
})

test_that("ensemble() returns correct n properties", {
  skip_if_julia_not_ready()

  nr_sims <- 3
  sfm <- make_jl_ensemble_sfm()

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

  nr_sims <- 3
  sfm <- make_jl_ensemble_sfm()

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

test_that("ensemble() only_stocks = FALSE returns full i coverage across variable classes", {
  skip_if_julia_not_ready()

  nr_sims <- 5
  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(
    sfm,
    n = nr_sims,
    only_stocks = FALSE,
    return_sims = TRUE
  ))

  expect_true(sims[["success"]])

  df <- sims[["df"]]
  constants_df <- sims[["constants"]][["df"]]
  init_df <- sims[["init"]][["df"]]

  stock_names <- as.data.frame(sfm, properties = "eqn")
  stock_names <- stock_names[stock_names[["type"]] == "stock", "name", drop = TRUE]
  non_stock_names <- setdiff(unique(df[["variable"]]), stock_names)

  expect_equal(sort(unique(df[["i"]])), seq_len(nr_sims))
  expect_equal(sort(unique(constants_df[["i"]])), seq_len(nr_sims))
  expect_equal(sort(unique(init_df[["i"]])), seq_len(nr_sims))

  for (nm in stock_names) {
    i_vals <- sort(unique(df[df[["variable"]] == nm, "i", drop = TRUE]))
    expect_equal(i_vals, seq_len(nr_sims))
  }

  expect_true(length(non_stock_names) > 0)
  for (nm in non_stock_names) {
    i_vals <- sort(unique(df[df[["variable"]] == nm, "i", drop = TRUE]))
    expect_equal(i_vals, seq_len(nr_sims))
  }
})

test_that("ensemble() only_stocks = TRUE keeps full i coverage for stocks constants and init", {
  skip_if_julia_not_ready()

  nr_sims <- 5
  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(
    sfm,
    n = nr_sims,
    only_stocks = TRUE,
    return_sims = TRUE
  ))

  expect_true(sims[["success"]])

  df <- sims[["df"]]
  constants_df <- sims[["constants"]][["df"]]
  init_df <- sims[["init"]][["df"]]

  stock_names <- as.data.frame(sfm, properties = "eqn")
  stock_names <- stock_names[stock_names[["type"]] == "stock", "name", drop = TRUE]

  expect_equal(sort(unique(df[["variable"]])), sort(stock_names))
  expect_equal(sort(unique(df[["i"]])), seq_len(nr_sims))
  expect_equal(sort(unique(constants_df[["i"]])), seq_len(nr_sims))
  expect_equal(sort(unique(init_df[["i"]])), seq_len(nr_sims))

  for (nm in stock_names) {
    i_vals <- sort(unique(df[df[["variable"]] == nm, "i", drop = TRUE]))
    expect_equal(i_vals, seq_len(nr_sims))
  }
})

test_that("ensemble() custom quantiles", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()
  sims <- silence(ensemble(sfm, n = 3, quantiles = c(0.1, 0.5, 0.9, 1)))
  expect_true(sims[["success"]])

  q_cols <- grep("^q", colnames(sims[["summary"]]), value = TRUE)
  expect_equal(length(q_cols), 4)
})


# Conditions ------------------------------------

test_that("ensemble() works with single variable conditions", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm,
    conditions = list("a2" = seq(0.2, 0.8, by = 0.05)),
    n = 3
  ))
  expect_true(sims[["success"]])
})

test_that("ensemble() crossed design computes correct conditions", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  n <- 3
  sims <- silence(ensemble(sfm,
    conditions = list(
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

  nr_sims <- 3
  nr_cond <- 3
  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm,
    conditions = list(
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

  sfm <- make_jl_ensemble_sfm()

  n <- 3
  sims <- silence(ensemble(sfm,
    conditions = list(
      "a2" = c(0.2, 0.3, 0.4),
      "a1" = c(1.3, 1.4, 1.5)
    ),
    cross = FALSE, n = n, return_sims = TRUE
  ))
  expect_true(sims[["success"]])

  cond_df <- as.data.frame(sims[["conditions"]])
  expect_equal(cond_df[["j"]], 1:3)
  # Conditions are alphabetically sorted, so a1 comes before a2
  expect_equal(cond_df[["a1"]], c(1.3, 1.4, 1.5))
  expect_equal(cond_df[["a2"]], c(0.2, 0.3, 0.4))
  expect_equal(unique(sims[["constants"]][["df"]][["i"]]), 1:n)
  expect_equal(unique(sims[["constants"]][["df"]][["j"]]), 1:3)
})

test_that("ensemble() conditions parameters are alphabetically sorted", {
  skip_if_julia_not_ready()

  # Regression test: in an earlier version, conditions parameter order was not preserved
  sfm <- sdbuildR() |>
    sim_specs(language = "Julia") |>
    sim_specs(stop = 12, dt = 0.1, save_at = 1, time_units = "month") |>
    meta(name = "Maya's Burnout") |>
    update("workload", "stock", eqn = 4) |>
    update("new_tasks", "flow",
      eqn = "workload * work_growth", to = "workload"
    ) |>
    update("work_growth", "constant", eqn = 1.5) |>
    update(c("sleep", "necessary_sleep", "worry_factor"),
      c("stock", "constant", "constant"),
      eqn = c("necessary_sleep", 8, 0.1)
    ) |>
    update("worry_about_work", "flow",
      eqn = "workload * worry_factor", from = "sleep"
    ) |>
    update("need_for_rest", "flow",
      eqn = "workload * necessary_sleep / sleep", from = "workload"
    )

  sims <- silence(ensemble(sfm,
    n = 3, return_sims = TRUE,
    conditions = list(
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

test_that("ensemble() with mixed stock and constant in conditions", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("predator_prey") |>
    update(c("predator", "prey"), eqn = "runif(1, 30, 50)") |>
    sim_specs(
      language = "Julia",
      dt = 0.1, save_at = 10,
      start = 0, stop = 200
    )

  nr_sims <- 3
  sims <- silence(ensemble(sfm,
    conditions = list(
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
    update(c("predator", "prey"), eqn = "runif(1)")

  sims1 <- silence(ensemble(sfm, n = 3, return_sims = TRUE))
  sims2 <- silence(ensemble(sfm, n = 3, return_sims = TRUE))

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
    update(c("predator", "prey"), eqn = "runif(1)")

  sims <- silence(ensemble(sfm, n = 3))
  expect_true(sims[["success"]])
  expect_equal(length(unique(sims[["summary"]][["time"]])), 1)
  expect_silent(plot(sims))

  # Also works with return_sims
  sims <- silence(ensemble(sfm, n = 3, return_sims = TRUE))
  expect_true(sims[["success"]])
  expect_silent(plot(sims))
  expect_silent(plot(sims, type = "sims"))
})

test_that("ensemble() works with interpolation function", {
  skip_if_julia_not_ready()

  sfm <- sdbuildR("logistic_model") |>
    sim_specs(
      language = "Julia",
      start = 0, stop = 50, dt = 0.1,
      save_n = 1
    ) |>
    update("X", eqn = "runif(1, 0, K)") |>
    update("input", "constant", eqn = "pulse(times, 10, width = dt, height = .01)") |>
    update("inflow2", "flow", eqn = "input(t)", to = "X")

  sims <- silence(ensemble(sfm, n = 3))
  expect_true(sims[["success"]])
  expect_silent(plot(sims))
})


# Verbose messages ----------------------------------------

test_that("ensemble() prints simulation count", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  # Basic ensemble
  expect_message(
    ensemble(sfm, n = 3, verbose = TRUE),
    "Starting"
  )

  # Ensemble with conditions: 3 x 3 = 9 conditions, 15 per condition = 135 total
  expect_message(
    ensemble(sfm,
      conditions = list(
        "a1" = c(1.1, 1.2, 1.3),
        "a2" = c(1.2, 1.3, 1.4)
      ),
      cross = TRUE, n = 3, verbose = TRUE,
      return_sims = TRUE
    ),
    "conditions"
  )
})


# plot.ensemble_sdbuildR() --------------------------------

test_that("plot.ensemble_sdbuildR() renders summary plot", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm, n = 3))

  expect_no_error(expect_no_message(plot(sims)))
  expect_no_error(plot(sims, j = 1))
})

test_that("plot.ensemble_sdbuildR() rejects invalid ...", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm, n = 3))

  expect_error(plot(sims, type = "NA"), "must be.*summary.*sims")

  expect_no_error(plot(sims, central_tendency = "median"))
  expect_error(
    plot(sims, central_tendency = "medians"),
    "must be.*mean.*median.*FALSE"
  )

  # plot.ensemble_sdbuildR() validates j index with single condition
  expect_error(
    plot(sims, j = c(3, 6, 9)),
    "only one condition"
  )
})


test_that("plot.ensemble_sdbuildR() informs when i used with summary type", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm, n = 3, return_sims = TRUE))

  expect_message(
    plot(sims, i = 5),
    "i.*argument is ignored"
  )
})


test_that("plot.ensemble_sdbuildR() validates j index with multiple conditions", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  nr_cond <- 3
  sims <- silence(ensemble(sfm,
    conditions = list(
      "a1" = c(1.1, 1.2, 1.3),
      "a2" = c(1.2, 1.3, 1.4)
    ),
    cross = FALSE, n = 3, return_sims = TRUE
  ))

  expect_error(
    plot(sims, j = nr_cond + 1),
    "must.*contain integers between"
  )
})

test_that("plot.ensemble_sdbuildR() requires return_sims for type = 'sims'", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm, n = 3, return_sims = FALSE))

  expect_error(
    plot(sims, type = "sims"),
    "Individual simulation data is required"
  )
})

test_that("plot.ensemble_sdbuildR() renders sims plot", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  nr_sims <- 3
  sims <- silence(ensemble(sfm, n = nr_sims, return_sims = TRUE))

  expect_no_error(plot(sims, type = "sims", i = nr_sims - 1))
  expect_no_error(plot(sims, type = "sims"))
})

test_that("plot.ensemble_sdbuildR() renders with specific j and i", {
  skip_if_julia_not_ready()

  sfm <- make_jl_ensemble_sfm()

  sims <- silence(ensemble(sfm,
    conditions = list(
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
  sfm <- make_ensemble_error_sfm()
  expect_snapshot(ensemble(sfm, n = 0), error = TRUE)
})

cli::test_that_cli(configs = c("plain", "ansi"), "ensemble() error: invalid quantiles", {
  sfm <- make_ensemble_error_sfm()
  expect_snapshot(ensemble(sfm, n = 3, quantiles = 0.5), error = TRUE)
})

cli::test_that_cli(configs = c("plain", "ansi"), "ensemble() error: non-numeric conditions", {
  sfm <- make_ensemble_error_sfm()
  expect_snapshot(ensemble(sfm, n = 3, conditions = list("S" = "abc")), error = TRUE)
})


# check_ensemble_sdbuildR() ---------------------------------------------------

# Error conditions require corrupted/wrong-class objects

test_that("check_ensemble_sdbuildR() rejects non-ensemble objects", {
  expect_error(check_ensemble_sdbuildR(list(success = TRUE)), "ensemble_sdbuildR")
  expect_error(check_ensemble_sdbuildR("string"), "ensemble_sdbuildR")
  expect_error(check_ensemble_sdbuildR(42), "ensemble_sdbuildR")
})

test_that("check_ensemble_sdbuildR() assesses success, summary, duration", {
  skip_if_julia_not_ready()
  sims0 <- sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  sims$success <- "yes"
  expect_error(check_ensemble_sdbuildR(sims), "success")

  sims$success <- c(TRUE, FALSE)
  expect_error(check_ensemble_sdbuildR(sims), "success")

  sims <- sims0
  sims$summary <- NULL
  expect_error(check_ensemble_sdbuildR(sims), "summary")

  sims$summary <- "not a df"
  expect_error(check_ensemble_sdbuildR(sims), "summary")

  sims <- sims0
  sims$duration <- NULL
  expect_error(check_ensemble_sdbuildR(sims), "duration")
})

test_that("check_ensemble_sdbuildR() passes for valid successful ensemble", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  expect_no_error(check_ensemble_sdbuildR(sims))
  expect_invisible(check_ensemble_sdbuildR(sims))
})


# print.ensemble_sdbuildR() ---------------------------------------------------

test_that("print() returns x invisibly", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  expect_invisible(silence(print(sims)))
  expect_identical(silence(withVisible(print(sims))$value), sims)
})

cli::test_that_cli(configs = "plain", "print() success output matches snapshot", {
  sfm <- make_r_ensemble_random_sfm() |>
    meta(name = "Demo model")

  sims <- silence(ensemble(sfm, n = 3, verbose = FALSE))
  sims$duration <- structure(1.234, class = "difftime", units = "secs")
  expect_snapshot(print(sims))
})

cli::test_that_cli(configs = "plain", "print() success with conditions lists changed parameters", {
  sfm <- make_r_ensemble_random_sfm() |>
    meta(name = "Demo model")

  sims <- silence(ensemble(sfm,
    n = 3,
    return_sims = TRUE,
    conditions = list(
      Effective_Contact_Rate = c(1.5, 2.5),
      Delay = c(1, 3)
    ),
    verbose = FALSE
  ))
  sims$duration <- structure(1.234, class = "difftime", units = "secs")
  expect_snapshot(print(sims))
})

cli::test_that_cli(configs = "plain", "ensemble setup rejects malformed equations early", {
  expect_error(
    sdbuildR() |>
      meta(name = "Demo model") |>
      update("S", type = "stock", eqn = "1 // 2") |>
      update("F1", type = "flow", eqn = "S", to = "S") |>
      sim_specs(language = "R"),
    "Could not parse the equation for .*S",
    ignore.case = TRUE
  )
})


# ensemble() object structure -------------------------------------------------

test_that("ensemble() success result has all required fields including error_message = NULL", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  required <- c(
    "success", "error_message", "df", "summary", "n", "n_total",
    "n_conditions", "conditions", "init", "constants", "script",
    "duration", "cross", "quantiles", "object"
  )
  for (field in required) {
    expect_true(field %in% names(sims), info = paste("Missing:", field))
  }
  expect_true(sims[["success"]])
  expect_null(sims[["error_message"]])
})

test_that("ensemble() success result has correct field types", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  expect_true(sims[["success"]])
  expect_null(sims[["error_message"]])
  expect_s3_class(sims[["summary"]], "data.frame")
  expect_true(is.numeric(sims[["n"]]))
  expect_true(is.numeric(sims[["n_total"]]))
  expect_true(is.numeric(sims[["n_conditions"]]))
  expect_true(is.character(sims[["script"]]))
})


# as.data.frame.ensemble_sdbuildR() -------------------------------------------


test_that("as.data.frame() default returns summary df", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  df <- as.data.frame(sims)
  expect_identical(df, sims[["summary"]])
  expect_s3_class(df, "data.frame")
})

test_that("as.data.frame() type = 'summary' returns summary df", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  df <- as.data.frame(sims, type = "summary")
  expect_identical(df, sims[["summary"]])
})

test_that("as.data.frame() type = 'sims' returns individual sims df", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, return_sims = TRUE))
  df <- as.data.frame(sims, type = "sims")
  expect_identical(df, sims[["df"]])
  expect_s3_class(df, "data.frame")
})

test_that("as.data.frame() type = 'sims' errors when return_sims = FALSE", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, return_sims = FALSE))
  expect_error(as.data.frame(sims, type = "sims"), "return_sims")
})

test_that("as.data.frame() summary has expected columns", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  df <- as.data.frame(sims)
  expect_true(all(c("j", "variable", "time", "mean", "median") %in% names(df)))
  q_cols <- grepl("^q", names(df))
  expect_gt(sum(q_cols), 0)
})

test_that("as.data.frame() individual sims df has expected columns", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, return_sims = TRUE))
  df <- as.data.frame(sims, type = "sims")
  expect_true(all(c("i", "j", "variable", "time", "value") %in% names(df)))
})

test_that("as.data.frame() direction = 'wide' widens summary", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  df_long <- as.data.frame(sims, direction = "long")
  df_wide <- as.data.frame(sims, direction = "wide")
  # Wide has fewer rows (one per j+time, not per j+time+variable)
  expect_lt(nrow(df_wide), nrow(df_long))
  # 'variable' column should be absorbed into column names
  expect_false("variable" %in% names(df_wide))
  # j and time remain as id columns
  expect_true("j" %in% names(df_wide))
  expect_true("time" %in% names(df_wide))
})

test_that("as.data.frame() direction = 'wide' widens individual sims", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, return_sims = TRUE))
  df_long <- as.data.frame(sims, type = "sims", direction = "long")
  df_wide <- as.data.frame(sims, type = "sims", direction = "wide")
  expect_lt(nrow(df_wide), nrow(df_long))
  expect_false("variable" %in% names(df_wide))
  expect_true(all(c("i", "j", "time") %in% names(df_wide)))
  # Variable names appear as columns (there are stocks in Crielaard2022)
  stock_names <- unique(df_long[["variable"]])
  expect_true(any(stock_names %in% names(df_wide)))
})

test_that("as.data.frame() rejects invalid direction", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  expect_error(as.data.frame(sims, direction = "diagonal"), "direction")
  expect_error(as.data.frame(sims, direction = ""), "direction")
})

test_that("as.data.frame() row.names sets row names", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  df <- as.data.frame(sims)
  rn <- paste0("row", seq_len(nrow(df)))
  df_named <- as.data.frame(sims, row.names = rn)
  expect_equal(rownames(df_named), rn)
})

test_that("as.data.frame() row.names length mismatch errors", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  expect_error(as.data.frame(sims, row.names = c("a", "b")), "mismatch|row.names|row names")
})

test_that("as.data.frame() preserves all conditions with multiple conditions", {
  skip_if_julia_not_ready()
  sfm <- make_jl_ensemble_sfm()
  n_cond <- 3
  sims <- silence(ensemble(sfm,
    conditions = list("a1" = c(1.1, 1.2, 1.3)),
    n = 3
  ))
  df <- as.data.frame(sims)
  expect_equal(sort(unique(df[["j"]])), seq_len(n_cond))
})


# head.ensemble_sdbuildR() / tail.ensemble_sdbuildR() ------------------------

test_that("head() and tail() return a data.frame with correct number of rows from summary", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  h <- head(sims, n = 4L)
  expect_s3_class(h, "data.frame")
  expect_equal(nrow(h), 4L)
  expect_equal(h, head(sims[["summary"]], 4L))

  t <- tail(sims, n = 4L)
  expect_s3_class(t, "data.frame")
  expect_equal(nrow(t), 4L)
  expect_equal(
    unname(as.matrix(t)),
    unname(as.matrix(tail(sims[["summary"]], 4L)))
  )
})


test_that("head() and tail() pass type = 'sims' through to individual sims", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, return_sims = TRUE))
  h <- head(sims, n = 3L, type = "sims")
  expect_equal(h, head(sims[["df"]], 3L))

  t <- tail(sims, n = 3L, type = "sims")
  expect_equal(
    unname(as.matrix(t)),
    unname(as.matrix(tail(sims[["df"]], 3L)))
  )
})


test_that("head() passes direction = 'wide' through", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  h_long <- head(sims, n = 3L, direction = "long")
  h_wide <- head(sims, n = 3L, direction = "wide")
  expect_false("variable" %in% names(h_wide))
  expect_true("variable" %in% names(h_long))
})


# summary.ensemble_sdbuildR() -------------------------------------------------

test_that("summary() returns the summary data.frame", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  s <- summary(sims)
  expect_identical(s, sims[["summary"]])
  expect_s3_class(s, "data.frame")
})

test_that("summary() result has expected columns", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  s <- summary(sims)
  expect_true(all(c("j", "variable", "time", "mean", "median") %in% names(s)))
  q_cols <- grep("^q", names(s), value = TRUE)
  expect_gt(length(q_cols), 0)
})

test_that("summary() result contains correct quantile columns for custom quantiles", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3, quantiles = c(0.1, 0.5, 0.9)))
  s <- summary(sims)
  q_cols <- grep("^q", names(s), value = TRUE)
  expect_equal(length(q_cols), 3)
})

test_that("summary() is consistent with as.data.frame() default", {
  skip_if_julia_not_ready()
  sims <- silence(ensemble(make_jl_ensemble_sfm(), n = 3))
  expect_identical(summary(sims), as.data.frame(sims))
})
