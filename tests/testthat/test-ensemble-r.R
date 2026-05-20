# Tests for ensemble_r() — R backend for ensemble simulations


# Basic R ensemble --------------------------------------------------------

test_that("ensemble() runs in R", {
  sfm <- make_r_ensemble_sfm()
  sims <- silence(ensemble(sfm, n = 3, verbose = FALSE))
  expect_s3_class(sims, "ensemble_sdbuildR")
  expect_true(sims[["success"]])
  expect_false(is.null(sims[["summary"]]))
})

test_that("ensemble() R returns correct structure", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 5, return_sims = TRUE, verbose = FALSE))

  expected_fields <- c(
    "success", "df", "summary", "n", "n_total",
    "n_conditions", "conditions", "init", "constants",
    "duration"
  )
  for (field in expected_fields) {
    expect_true(
      field %in% names(sims),
      info = paste("Missing field:", field)
    )
  }
})

test_that("ensemble() R handles models with no constants", {
  sfm <- make_basic_sfm() |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1, save_at = 1)

  sims <- silence(ensemble(sfm, n = 3, return_sims = TRUE, verbose = FALSE))

  expect_true(sims[["success"]])
  expect_equal(nrow(sims[["constants"]][["df"]]), 0)
  expect_equal(nrow(sims[["constants"]][["summary"]]), 0)
  expect_equal(
    names(sims[["constants"]][["df"]]),
    c("i", "j", "variable", "value")
  )
  expect_true(
    all(c("j", "variable", "mean", "median", "sd", "min", "max", "q0.025", "q0.975") %in%
      names(sims[["constants"]][["summary"]]))
  )
})

test_that("ensemble() R respects only_stocks = TRUE", {
  sfm <- make_r_ensemble_random_sfm()
  df <- as.data.frame(sfm, properties = "eqn")
  n_stocks <- nrow(df[df[["type"]] == "stock", ])

  sims <- silence(ensemble(sfm, n = 3, only_stocks = TRUE, verbose = FALSE))
  expect_true(sims[["success"]])
  expect_equal(
    length(unique(sims[["summary"]][["variable"]])),
    n_stocks
  )
})

test_that("ensemble() R returns all variables with only_stocks = FALSE", {
  sfm <- make_r_ensemble_random_sfm()
  df <- as.data.frame(sfm, properties = "eqn")
  n_all <- nrow(df[df[["type"]] %in% c("stock", "flow", "aux"), ])

  sims <- silence(ensemble(sfm,
    n = 3, only_stocks = FALSE,
    return_sims = TRUE, verbose = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(
    length(unique(sims[["summary"]][["variable"]])),
    n_all
  )
})

test_that("ensemble() R filters outputs to vars", {
  sfm <- make_r_ensemble_random_sfm() |>
    sim_specs(vars = c("Susceptible", "Infection_Rate"))

  sims <- silence(ensemble(sfm,
    n = 3,
    return_sims = TRUE,
    verbose = FALSE
  ))

  expect_true(sims[["success"]])
  expect_equal(sort(unique(sims[["summary"]][["variable"]])), c("Infection_Rate", "Susceptible"))
  expect_equal(sort(unique(sims[["df"]][["variable"]])), c("Infection_Rate", "Susceptible"))
})

test_that("ensemble() R returns correct n properties", {
  nr_sims <- 5
  sfm <- make_r_ensemble_random_sfm()

  sims <- silence(ensemble(sfm,
    n = nr_sims, return_sims = TRUE,
    verbose = FALSE
  ))
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims)
  expect_equal(sims[["n_conditions"]], 1)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1)
})

test_that("ensemble() R custom quantiles", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm,
    n = 3, quantiles = c(0.1, 0.5, 0.9, 1),
    verbose = FALSE
  ))
  expect_true(sims[["success"]])
  q_cols <- grep("^q", colnames(sims[["summary"]]), value = TRUE)
  expect_equal(length(q_cols), 4)
})


# Conditions in R ---------------------------------------------------------

test_that("ensemble() R works with single variable conditions", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm,
    conditions = list("Effective_Contact_Rate" = c(1.5, 2, 2.5)),
    n = 3, verbose = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n_conditions"]], 3)
})

test_that("ensemble() R crossed design computes correct conditions", {
  sfm <- make_r_ensemble_random_sfm()
  n <- 2
  sims <- silence(ensemble(sfm,
    conditions = list(
      "Effective_Contact_Rate" = c(1.5, 2.5),
      "Delay" = c(1, 3)
    ),
    cross = TRUE, n = n, verbose = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], n)
  expect_equal(sims[["n_total"]], n * 4)
  expect_equal(sims[["n_conditions"]], 4)
  expect_equal(sort(unique(sims[["summary"]][["j"]])), 1:4)
})

test_that("ensemble() R non-crossed design pairs values", {
  sfm <- make_r_ensemble_random_sfm()
  nr_sims <- 3
  nr_cond <- 3
  sims <- silence(ensemble(sfm,
    conditions = list(
      "Effective_Contact_Rate" = c(1.5, 2, 2.5),
      "Delay" = c(1, 2, 3)
    ),
    cross = FALSE, n = nr_sims, return_sims = TRUE, verbose = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims * nr_cond)
  expect_equal(sort(unique(sims[["df"]][["i"]])), 1:nr_sims)
  expect_equal(sort(unique(sims[["df"]][["j"]])), 1:nr_cond)
})

test_that("ensemble() R conditions data frame is correct", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm,
    conditions = list(
      "Effective_Contact_Rate" = c(1.5, 2, 2.5),
      "Delay" = c(1, 2, 3)
    ),
    cross = FALSE, n = 2, return_sims = TRUE, verbose = FALSE
  ))
  expect_true(sims[["success"]])

  cond_df <- as.data.frame(sims[["conditions"]])
  expect_equal(cond_df[["j"]], 1:3)
  # Alphabetically sorted: Delay before Effective_Contact_Rate
  expect_equal(cond_df[["Delay"]], c(1, 2, 3))
  expect_equal(cond_df[["Effective_Contact_Rate"]], c(1.5, 2, 2.5))
})


# Output compatibility ----------------------------------------------------

test_that("ensemble() R result works with plot()", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 3, verbose = FALSE))
  expect_no_error(plot(sims))
})

test_that("ensemble() R result works with as.data.frame()", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 3, return_sims = TRUE, verbose = FALSE))

  df_summary <- as.data.frame(sims, type = "summary")
  expect_s3_class(df_summary, "data.frame")
  expect_true(all(c("j", "variable", "time", "mean", "median") %in% names(df_summary)))

  df_sims <- as.data.frame(sims, type = "sims")
  expect_s3_class(df_sims, "data.frame")
  expect_true(all(c("i", "j", "variable", "time", "value") %in% names(df_sims)))
})

test_that("ensemble() R result works with head() and tail()", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 3, verbose = FALSE))
  h <- head(sims, n = 3L)
  t <- tail(sims, n = 3L)
  expect_s3_class(h, "data.frame")
  expect_s3_class(t, "data.frame")
  expect_equal(nrow(h), 3L)
  expect_equal(nrow(t), 3L)
})

test_that("ensemble() R result works with summary()", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 3, verbose = FALSE))
  s <- summary(sims)
  expect_identical(s, sims[["summary"]])
})

test_that("ensemble() R result works with print()", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 3, verbose = FALSE))
  expect_invisible(silence(print(sims)))
})


# Verbose messages --------------------------------------------------------

test_that("ensemble() R prints simulation count", {
  sfm <- make_r_ensemble_random_sfm()

  expect_message(
    ensemble(sfm, n = 5, verbose = TRUE),
    "Starting"
  )
})


# Edge cases --------------------------------------------------------------

test_that("ensemble() R works with n = 1", {
  sfm <- make_r_ensemble_sfm()
  sims <- silence(ensemble(sfm, n = 1, verbose = FALSE))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], 1)
  expect_equal(sims[["n_total"]], 1)
})

test_that("ensemble() R works with conditions and n = 1", {
  sfm <- make_r_ensemble_sfm()
  sims <- silence(ensemble(sfm,
    n = 1,
    conditions = list("Effective_Contact_Rate" = c(1.5, 2.5)),
    verbose = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], 1)
  expect_equal(sims[["n_total"]], 2)
  expect_equal(sims[["n_conditions"]], 2)
})


# Parallel execution via user-managed future plan -------------------------

test_that("ensemble() R runs sequentially with future::sequential plan", {
  skip_if_not_installed("future")

  future::plan(future::sequential)
  on.exit(future::plan(future::sequential), add = TRUE)

  sfm <- make_r_ensemble_sfm()
  sims <- silence(ensemble(sfm, n = 3, verbose = FALSE))

  expect_true(sims[["success"]])
  expect_equal(future::nbrOfWorkers(), 1L)
})

test_that("ensemble() R uses parallel path when future plan has multiple workers", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  sfm <- sdbuildR("SIR") |>
    update("Susceptible", eqn = "runif(1, 900, 1100)") |>
    sim_specs(language = "R", start = 0, stop = 10, dt = 0.1, save_at = 1)

  sims <- silence(ensemble(sfm, n = 4, verbose = FALSE))

  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], 4)
  expect_gt(future::nbrOfWorkers(), 1L)
})
