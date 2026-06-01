# Tests for export_model()

# ---- input validation ----

test_that("export_model() errors on non-sdbuildR input", {
  expect_error(export_model(list(), format = "sdbuildR"), class = "rlang_error")
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


# ---- sdbuildR format ----

test_that("export_model(format='sdbuildR') returns character string with sdbuildR code", {
  sfm <- sdbuildR("SIR")
  out <- export_model(sfm, format = "sdbuildR")
  expect_type(out, "character")
  expect_match(out, "sdbuildR()", fixed = TRUE)
  expect_match(out, "stock(", fixed = TRUE)
})

test_that("export_model(format='sdbuildR') matches build_sdbuildR_code_() output", {
  sfm <- sdbuildR("SIR")
  out_new <- export_model(sfm, format = "sdbuildR")
  out_old <- suppressWarnings(build_sdbuildR_code_(sfm))
  expect_identical(out_new, out_old)
})

test_that("export_model(format='sdbuildR') writes .R file, appending extension", {
  sfm <- sdbuildR("SIR")
  path <- tempfile(pattern = "sdbuildR_model")
  out_path <- export_model(sfm, format = "sdbuildR", file = path)
  expect_equal(out_path, paste0(path, ".R"))
  expect_true(file.exists(out_path))
  content <- readLines(out_path)
  expect_true(any(grepl("sdbuildR()", content, fixed = TRUE)))
})


# ---- deSolve format ----

test_that("export_model(format='deSolve') returns character string with deSolve code", {
  sfm <- sdbuildR("SIR")
  out <- export_model(sfm, format = "deSolve")
  expect_type(out, "character")
  expect_match(out, "library(deSolve)", fixed = TRUE)
  expect_match(out, "deSolve::ode(", fixed = TRUE)
  expect_match(out, "ode_func", fixed = TRUE)
})

test_that("export_model(format='deSolve') output contains model variable names", {
  sfm <- sdbuildR("SIR")
  out <- export_model(sfm, format = "deSolve")
  expect_match(out, "susceptible", fixed = TRUE)
  expect_match(out, "infected", fixed = TRUE)
  expect_match(out, "recovered", fixed = TRUE)
  expect_match(out, "init = c(", fixed = TRUE)
})

test_that("export_model(format='deSolve') errors on Julia language", {
  sfm <- sim_settings(sdbuildR("SIR"), language = "Julia")
  expect_error(export_model(sfm, format = "deSolve"), class = "rlang_error")
})

test_that("export_model(format='deSolve') writes .R file, appending extension", {
  sfm <- sdbuildR("SIR")
  path <- tempfile(pattern = "desolve_model")
  out_path <- export_model(sfm, format = "deSolve", file = path)
  expect_equal(out_path, paste0(path, ".R"))
  expect_true(file.exists(out_path))
})
