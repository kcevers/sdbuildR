test_that("export_model(format='psychomodels') returns single JSON object", {
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
  expect_equal(obj[["description"]], sfm$meta$caption)
  expect_equal(obj[["framework"]], "Ordinary Differential Equations")
  expect_match(obj[["slug"]], "^[a-z0-9-]+$")
  expect_match(obj[["explanation"]], "Model Equations \\(LaTeX\\)")
  expect_match(obj[["explanation"]], "\\$\\$")
})


test_that("export_model(format='psychomodels') writes json file and appends extension", {
  sfm <- sdbuildR("SIR")
  path <- tempfile(pattern = "psychomodel")

  out_path <- export_model(
    sfm,
    format          = "psychomodels",
    publication_doi = "10.0000/example",
    file            = path
  )

  expected_path <- paste0(path, ".json")
  expect_equal(out_path, expected_path)
  expect_true(file.exists(expected_path))

  obj <- jsonlite::fromJSON(expected_path, simplifyVector = FALSE)
  expect_equal(obj[["publication_doi"]], "10.0000/example")
  expect_match(obj[["explanation"]], "\\$\\$")
})


test_that("export_model(format='psychomodels') publication_doi is optional", {
  sfm <- sdbuildR("SIR")

  out <- export_model(sfm, format = "psychomodels")
  obj <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_equal(obj[["publication_doi"]], "")
})


test_that("export_model(format='psychomodels') supports include_latex = FALSE", {
  sfm <- sdbuildR("SIR")

  out <- export_model(
    sfm,
    format        = "psychomodels",
    explanation   = "Base explanation",
    include_latex = FALSE
  )

  obj <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_equal(obj[["explanation"]], "Base explanation")
  expect_false(grepl("\\$\\$", obj[["explanation"]]))
})
