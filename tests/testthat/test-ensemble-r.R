# Tests for ensemble_r() — R backend for ensemble simulations


# Basic R ensemble --------------------------------------------------------

test_that("ensemble() runs in R", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 3, verbose = FALSE, save_sims = TRUE))

  # ensemble() R returns correct structure
  expect_successful_ensemble(sims, c(
    "success", "df", "summary", "n", "n_total",
    "n_conditions", "conditions", "init", "constants",
    "duration"
  ))
})

test_that("ensemble() R handles models with no constants", {
  sfm <- make_basic_sfm() |>
    sim_settings(language = "R", start = 0, stop = 10, dt = 0.1, save_at = 1)

  sims <- silence(ensemble(sfm, n = 3, save_sims = TRUE, verbose = FALSE))

  expect_true(sims[["success"]])
  expect_equal(nrow(sims[["constants"]][["df"]]), 0)
  expect_equal(nrow(sims[["constants"]][["summary"]]), 0)
  expect_equal(
    names(sims[["constants"]][["df"]]),
    c("sim", "condition", "variable", "value")
  )
  expect_true(
    all(c("condition", "variable", "mean", "median", "sd", "min", "max", "q0.025", "q0.975") %in%
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
    save_sims = TRUE, verbose = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(
    length(unique(sims[["summary"]][["variable"]])),
    n_all
  )
})

test_that("ensemble() R filters outputs to vars", {
  sfm <- make_r_ensemble_random_sfm() |>
    sim_settings(vars = c("susceptible", "new_infections"))

  sims <- silence(ensemble(sfm,
    n = 3,
    save_sims = TRUE,
    verbose = FALSE
  ))

  expect_true(sims[["success"]])
  expect_equal(sort(unique(sims[["summary"]][["variable"]])), c("new_infections", "susceptible"))
  expect_equal(sort(unique(sims[["df"]][["variable"]])), c("new_infections", "susceptible"))
})

test_that("ensemble() R returns correct n properties", {
  nr_sims <- 5
  sfm <- make_r_ensemble_random_sfm()

  sims <- silence(ensemble(sfm,
    n = nr_sims, save_sims = TRUE,
    verbose = FALSE
  ))
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims)
  expect_equal(sims[["n_conditions"]], 1)
  expect_unique_values(sims[["df"]], "sim", seq_len(nr_sims))
  expect_unique_values(sims[["df"]], "condition", 1)
  expect_unique_values(sims[["summary"]], "condition", 1)
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
    conditions = list("contact_rate" = c(1.5, 2, 2.5)),
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
      "contact_rate" = c(1.5, 2.5),
      "infection_rate" = c(1, 3)
    ),
    cross = TRUE, n = n, verbose = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], n)
  expect_equal(sims[["n_total"]], n * 4)
  expect_equal(sims[["n_conditions"]], 4)
  expect_equal(sort(unique(sims[["summary"]][["condition"]])), 1:4)
})

test_that("ensemble() R non-crossed design pairs values", {
  sfm <- make_r_ensemble_random_sfm()
  nr_sims <- 3
  nr_cond <- 3
  sims <- silence(ensemble(sfm,
    conditions = list(
      "contact_rate" = c(1.5, 2, 2.5),
      "infection_rate" = c(1, 2, 3)
    ),
    cross = FALSE, n = nr_sims, save_sims = TRUE, verbose = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], nr_sims)
  expect_equal(sims[["n_total"]], nr_sims * nr_cond)
  expect_unique_values(sims[["df"]], "sim", seq_len(nr_sims))
  expect_unique_values(sims[["df"]], "condition", seq_len(nr_cond))


  # ensemble() R conditions data frame is correct
  cond_df <- as.data.frame(sims[["conditions"]])
  expect_equal(cond_df[["condition"]], 1:nr_cond)
  # Alphabetically sorted: infection_rate before contact_rate
  expect_equal(names(cond_df), c("condition", "contact_rate", "infection_rate"))
  expect_equal(cond_df[["infection_rate"]], c(1, 2, 3))
  expect_equal(cond_df[["contact_rate"]], c(1.5, 2, 2.5))
})


# Output compatibility ----------------------------------------------------

test_that("ensemble() R result works with as.data.frame()", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 3, save_sims = TRUE, verbose = FALSE))

  df_summary <- as.data.frame(sims, which = "summary")
  expect_s3_class(df_summary, "data.frame")
  expect_true(all(c("condition", "variable", "time", "mean", "median") %in% names(df_summary)))

  df_sims <- as.data.frame(sims, which = "sims")
  expect_s3_class(df_sims, "data.frame")
  expect_true(all(c("sim", "condition", "variable", "time", "value") %in% names(df_sims)))
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
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 1, verbose = FALSE))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], 1)
  expect_equal(sims[["n_total"]], 1)
})

test_that("ensemble() R works with conditions and n = 1", {
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm,
    n = 1,
    conditions = list("contact_rate" = c(1.5, 2.5)),
    verbose = FALSE
  ))
  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], 1)
  expect_equal(sims[["n_total"]], 2)
  expect_equal(sims[["n_conditions"]], 2)
})

test_that("ensemble() in R respects seed", {
  sfm <- make_r_ensemble_random_sfm() |> sim_settings(seed = 123)

  # Should not modify global seed state
  withr::local_seed(123) # ensure .Random.seed exists before capturing it
  orig_seed <- .Random.seed

  sims1 <- silence(ensemble(sfm, n = 3, verbose = FALSE, save_sims = TRUE))
  sims2 <- silence(ensemble(sfm, n = 3, verbose = FALSE, save_sims = TRUE))

  new_seed <- .Random.seed
  expect_true(identical(orig_seed, new_seed))

  expect_equal(sims1[["summary"]], sims2[["summary"]])
  expect_equal(sims1[["df"]], sims2[["df"]])

  # Each simulation within an ensemble should be different
  cols <- c("time", "value")
  tol <- 1e-5
  df1a <- as.data.frame(sims1, which = "sims", sim = 1)
  df1b <- as.data.frame(sims1, which = "sims", sim = 2)
  df1c <- as.data.frame(sims1, which = "sims", sim = 3)
  expect_true(abs(sum(df1a[, cols] - df1b[, cols])) > tol)
  expect_true(abs(sum(df1a[, cols] - df1c[, cols])) > tol)
  expect_true(abs(sum(df1b[, cols] - df1c[, cols])) > tol)

  df2a <- as.data.frame(sims2, which = "sims", sim = 1)
  df2b <- as.data.frame(sims2, which = "sims", sim = 2)
  df2c <- as.data.frame(sims2, which = "sims", sim = 3)
  expect_true(abs(sum(df2a[, cols] - df2b[, cols])) > tol)
  expect_true(abs(sum(df2a[, cols] - df2c[, cols])) > tol)
  expect_true(abs(sum(df2b[, cols] - df2c[, cols])) > tol)
})


test_that("ensemble() in R with parallel execution respects seed", {
  skip_if_not_installed("future")
  skip_on_cran()

  sfm <- make_r_ensemble_random_sfm() |> sim_settings(seed = 123)

  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  # Should not modify global seed state
  withr::local_seed(123) # ensure .Random.seed exists before capturing it
  orig_seed <- .Random.seed

  sims1 <- silence(ensemble(sfm, n = 3, verbose = FALSE, save_sims = TRUE))
  sims2 <- silence(ensemble(sfm, n = 3, verbose = FALSE, save_sims = TRUE))

  new_seed <- .Random.seed
  expect_equal(orig_seed, new_seed)

  expect_equal(sims1[["summary"]], sims2[["summary"]])
  expect_equal(sims1[["df"]], sims2[["df"]])

  # Each simulation within an ensemble should be different
  cols <- c("time", "value")
  tol <- 1e-5
  df1a <- as.data.frame(sims1, which = "sims", sim = 1)
  df1b <- as.data.frame(sims1, which = "sims", sim = 2)
  df1c <- as.data.frame(sims1, which = "sims", sim = 3)
  expect_true(abs(sum(df1a[, cols] - df1b[, cols])) > tol)
  expect_true(abs(sum(df1a[, cols] - df1c[, cols])) > tol)
  expect_true(abs(sum(df1b[, cols] - df1c[, cols])) > tol)

  df2a <- as.data.frame(sims2, which = "sims", sim = 1)
  df2b <- as.data.frame(sims2, which = "sims", sim = 2)
  df2c <- as.data.frame(sims2, which = "sims", sim = 3)
  expect_true(abs(sum(df2a[, cols] - df2b[, cols])) > tol)
  expect_true(abs(sum(df2a[, cols] - df2c[, cols])) > tol)
  expect_true(abs(sum(df2b[, cols] - df2c[, cols])) > tol)
})


test_that("ensemble() in R without seed", {
  sfm <- make_r_ensemble_random_sfm() |> sim_settings(seed = NULL)
  sims1 <- silence(ensemble(sfm, n = 3, verbose = FALSE, save_sims = TRUE))
  sims2 <- silence(ensemble(sfm, n = 3, verbose = FALSE, save_sims = TRUE))

  tol <- 1e-5
  df1 <- as.data.frame(sims1, which = "summary")
  df2 <- as.data.frame(sims2, which = "summary")
  cols <- setdiff(colnames(df1), "variable")

  expect_true(abs(sum(df1[, cols] - df2[, cols])) > tol)
  cols <- c("time", "value")
  df1 <- as.data.frame(sims1, which = "sims")
  df2 <- as.data.frame(sims2, which = "sims")
  expect_true(abs(sum(df1[, cols] - df2[, cols])) > tol)
})

# Parallel execution via user-managed future plan -------------------------

test_that("ensemble() R runs sequentially with future::sequential plan", {
  skip_if_not_installed("future")

  future::plan(future::sequential)
  on.exit(future::plan(future::sequential), add = TRUE)

  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = 3, verbose = FALSE))

  expect_true(sims[["success"]])
  expect_equal(future::nbrOfWorkers(), 1L)
})

test_that("ensemble() R uses parallel path when future plan has multiple workers", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  n <- 4
  sfm <- make_r_ensemble_random_sfm()
  sims <- silence(ensemble(sfm, n = n, verbose = FALSE))

  expect_true(sims[["success"]])
  expect_equal(sims[["n"]], n)
  expect_gt(future::nbrOfWorkers(), 1L)
})


test_that("ensemble respects sim_settings save_sims and per-call override via ...", {
  sfm <- sdbuildR("SIR") |>
    sim_settings(save_sims = TRUE)

  ens_keep <- ensemble(sfm, n = 2)
  expect_true(!is.null(ens_keep$df))

  ens_drop <- ensemble(sfm, n = 2, save_sims = FALSE)
  expect_null(ens_drop$df)
})
