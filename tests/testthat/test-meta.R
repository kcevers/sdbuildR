# Tests for meta and sim_settings manipulation

test_that("meta() modifies model name", {
  sfm <- sdbuildR() |>
    meta(name = "Test Model")

  expect_equal(sfm$meta$name, "Test Model")
})

test_that("meta() modifies author", {
  sfm <- sdbuildR() |>
    meta(author = "Test Author")

  expect_equal(sfm$meta$author, "Test Author")
})

test_that("meta() modifies version", {
  sfm <- sdbuildR() |>
    meta(version = "2.0")

  expect_equal(sfm$meta$version, "2.0")
})

test_that("meta() preserves other fields", {
  sfm <- sdbuildR() |>
    meta(name = "New Name")

  expect_true("author" %in% names(sfm$meta))
  expect_true("version" %in% names(sfm$meta))
})

test_that("meta() returns sdbuildR object", {
  sfm <- sdbuildR() |>
    meta(name = "Test")

  expect_s3_class(sfm, "sdbuildR")
})

# Error message tests ----

cli::test_that_cli(configs = c("plain", "ansi"), "meta() requires sfm argument", {
  expect_snapshot(meta(name = "Test"), error = TRUE)
})
