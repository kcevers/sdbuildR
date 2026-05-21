# Tests for export_model()

# ---- input validation ----

test_that("export_model() errors on non-sdbuildR input", {
  expect_error(export_model(list(), format = "desolve"), class = "rlang_error")
  expect_error(export_model(list(), format = "psychomodels"), class = "rlang_error")
})


test_that("export_model() errors on unknown format", {
  sfm <- sdbuildR("SIR")
  expect_error(export_model(sfm, format = "xmile"))
})


# ---- psychomodels format ----

test_that("export_model(format='psychomodels') returns JSON with expected fields", {
  sfm <- sdbuildR("SIR")
  out <- export_model(sfm,
    format = "psychomodels",
    publication_doi = "10.0000/example"
  )
  expect_type(out, "character")

  obj <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_type(obj, "list")
  expect_false(is.list(obj[[1]]))

  expected_fields <- c(
    "id", "title", "description", "explanation", "publication_doi",
    "publication_citation", "publication_citation_fetched_at",
    "publication_csl_fetched_at", "publication_csl_json",
    "code_repository_url", "data_url", "submission_remarks",
    "framework", "programming_language", "model_variable",
    "psychology_discipline", "software_package", "slug",
    "published_pending_moderation_at", "published_at", "created_at",
    "updated_at", "created_by", "updated_by", "published_by"
  )
  expect_true(all(expected_fields %in% names(obj)))
  expect_equal(obj[["title"]], sfm$meta$name)
  expect_match(obj[["slug"]], "^[a-z0-9-]+$")
  expect_match(obj[["explanation"]], "Model Equations \\(LaTeX\\)")
})


test_that("export_model(format='psychomodels') writes .json file, appending extension", {
  sfm <- sdbuildR("SIR")
  path <- tempfile(pattern = "psychomodel")

  out_path <- export_model(sfm,
    format = "psychomodels",
    publication_doi = "10.0000/example",
    file = path
  )

  expect_equal(out_path, paste0(path, ".json"))
  expect_true(file.exists(out_path))

  obj <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  expect_equal(obj[["publication_doi"]], "10.0000/example")
})


test_that("export_model(format='psychomodels') include_latex = FALSE", {
  sfm <- sdbuildR("SIR")
  out <- export_model(sfm,
    format = "psychomodels",
    explanation = "Base explanation",
    include_latex = FALSE
  )
  obj <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_equal(obj[["explanation"]], "Base explanation")
  expect_false(grepl("\\$\\$", obj[["explanation"]]))
})


# ---- deSolve format ----

test_that("export_model(format='desolve') returns named list", {
  sfm <- sdbuildR("SIR")
  ds <- export_model(sfm, format = "desolve")
  expect_type(ds, "list")
  expect_named(ds, c("model", "params", "init", "times", "method"),
    ignore.order = TRUE
  )
})


test_that("export_model(format='desolve') list elements have correct types", {
  sfm <- sdbuildR("SIR")
  ds <- export_model(sfm, format = "desolve")
  expect_type(ds$model, "closure")
  expect_true(is.numeric(ds$params) && !is.null(names(ds$params)))
  expect_true(is.numeric(ds$init) && !is.null(names(ds$init)))
  expect_true(is.numeric(ds$times))
  expect_type(ds$method, "character")
})


test_that("export_model(format='desolve') params are numeric and from constants", {
  sfm <- sdbuildR("SIR")
  ds <- export_model(sfm, format = "desolve")
  vars <- sfm[["variables"]]
  const_names <- vars$name[vars$type == "constant"]
  # params must be numeric and only contain constant names
  expect_true(is.numeric(ds$params))
  expect_true(all(names(ds$params) %in% const_names))
})


test_that("export_model(format='desolve') init matches sfm stock initial values", {
  sfm <- sdbuildR("SIR")
  ds <- export_model(sfm, format = "desolve")
  vars <- sfm[["variables"]]
  stock_names <- vars$name[vars$type == "stock"]
  expect_true(all(stock_names %in% names(ds$init)))
})


test_that("export_model(format='desolve') method matches sfm sim_specs", {
  sfm <- sdbuildR("SIR") |> sim_specs(method = "rk4")
  ds <- export_model(sfm, format = "desolve")
  expect_equal(ds$method, "rk4")
})


test_that("export_model(format='desolve') case-insensitive format argument", {
  sfm <- sdbuildR("SIR")
  ds1 <- export_model(sfm, format = "desolve")
  ds2 <- export_model(sfm, format = "deSolve")
  ds3 <- export_model(sfm, format = "DESOLVE")
  expect_named(ds1, names(ds2))
  expect_named(ds1, names(ds3))
})


test_that("export_model(format='desolve') writes .R file, appending extension", {
  sfm <- sdbuildR("SIR")
  path <- tempfile(pattern = "desolve_model")

  out_path <- export_model(sfm, format = "desolve", file = path)

  expect_equal(out_path, paste0(path, ".R"))
  expect_true(file.exists(out_path))
  content <- readLines(out_path)
  expect_true(any(grepl("library\\(deSolve\\)", content)))
  expect_true(any(grepl("model <- function", content)))
})


test_that("export_model(format='desolve') model function runs with deSolve::ode()", {
  sfm <- sdbuildR("SIR")
  ds <- export_model(sfm, format = "desolve")

  out <- expect_no_error(
    deSolve::ode(
      y = ds$init, times = ds$times,
      func = ds$model, parms = ds$params,
      method = "lsoda"
    )
  )
  expect_true(is.matrix(out) || inherits(out, "deSolve"))
  expect_true(nrow(out) > 1L)
})


test_that("export_model(format='desolve') produces valid deSolve output for logistic model", {
  log_fn <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dN <- r * N * (1 - N / K)
      list(c(dN))
    })
  }
  sfm <- import_desolve(
    log_fn, c(r = 0.3, K = 100),
    c(N = 10), seq(0, 20, by = 0.1)
  )
  sfm <- sim_specs(sfm, save_at = 1)
  ds <- export_model(sfm, format = "desolve")

  out <- expect_no_error(
    deSolve::ode(
      y = ds$init, times = ds$times,
      func = ds$model, parms = ds$params,
      method = ds$method
    )
  )
  # N should approach K
  final_N <- out[nrow(out), "N"]
  expect_true(abs(final_N - 100) < 5)
})
