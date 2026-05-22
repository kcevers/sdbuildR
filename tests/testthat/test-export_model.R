# Tests for export_model()

# ---- input validation ----

test_that("export_model() errors on non-sdbuildR input", {
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


 
